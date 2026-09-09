# TIMSS 2023 Grade 8: aggregate descriptive statistics
# Script version: 06_descriptive_statistics_v1.0_2026-09-04
#
# Purpose:
# - Complete the descriptive evidence needed for the Results chapter.
# - Use the same eligible_main_dp sample and analysis_weight_candidate as Round 4.
# - Produce aggregate outputs only.
#
# This script does not:
# - read original RData files;
# - refit any regression or multilevel model;
# - alter scripts or outputs from Rounds 1-5;
# - write student-level or school-level records to public directories.

required_packages <- c("here", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Missing package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Run this once in the R Console: install.packages(c(\"here\", \"readr\"))"
    ),
    call. = FALSE
  )
}

project_root <- here::here()
if (!file.exists(file.path(project_root, "TIMSS_2023.Rproj"))) {
  stop(
    "The RStudio Project root could not be verified. Open TIMSS_2023.Rproj and rerun the script.",
    call. = FALSE
  )
}

message("Project root: ", project_root)
message("Round 6 computes aggregate descriptive statistics only and fits no models.")

input_path <- here::here(
  "data_processed",
  "local_only",
  "timss_g8_analysis_data.rds"
)

public_output_dir <- here::here("data_metadata")
local_output_dir <- here::here("data_metadata", "local_only")
archive_root <- here::here(
  "data_metadata",
  "local_only",
  "archive",
  "06_descriptive_statistics"
)

dir.create(public_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(local_output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_path)) {
  stop(
    paste0(
      "Processed analysis data are missing: ",
      file.path(
        "data_processed",
        "local_only",
        "timss_g8_analysis_data.rds"
      )
    ),
    call. = FALSE
  )
}

output_paths <- list(
  continuous = here::here(
    "data_metadata",
    "analysis_continuous_descriptives.csv"
  ),
  categorical = here::here(
    "data_metadata",
    "analysis_categorical_descriptives.csv"
  ),
  pv_audit = here::here(
    "data_metadata",
    "local_only",
    "descriptive_pv_audit.csv"
  ),
  summary = here::here(
    "data_metadata",
    "local_only",
    "descriptive_statistics_summary.txt"
  ),
  session = here::here(
    "data_metadata",
    "local_only",
    "session_info_descriptive.txt"
  )
)

to_relative_path <- function(path) {
  root_normalized <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )
  path_normalized <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
  prefix <- paste0(root_normalized, "/")
  if (startsWith(path_normalized, prefix)) {
    substring(path_normalized, nchar(prefix) + 1L)
  } else {
    basename(path_normalized)
  }
}

archive_existing_outputs <- function(paths) {
  existing <- unlist(paths, use.names = FALSE)
  existing <- existing[file.exists(existing)]
  if (length(existing) == 0L) {
    return(invisible(character()))
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_dir <- file.path(archive_root, timestamp)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

  moved <- character()
  for (path in existing) {
    destination <- file.path(archive_dir, basename(path))
    copied <- file.copy(path, destination, overwrite = FALSE)
    if (!copied) {
      stop(
        "Unable to archive an existing Round 6 output: ",
        to_relative_path(path),
        call. = FALSE
      )
    }
    unlinked <- file.remove(path)
    if (!unlinked) {
      stop(
        "Unable to replace an existing Round 6 output after archiving: ",
        to_relative_path(path),
        call. = FALSE
      )
    }
    moved <- c(moved, destination)
  }

  message(
    "Archived ",
    length(moved),
    " previous Round 6 output file(s) under ",
    to_relative_path(archive_dir),
    "."
  )
  invisible(moved)
}

write_csv_checked <- function(data, path) {
  tryCatch(
    {
      readr::write_excel_csv(data, path, na = "")
    },
    error = function(e) {
      stop(
        "Unable to write aggregate CSV: ",
        to_relative_path(path),
        "; ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
    stop(
      "Aggregate CSV was not created or is empty: ",
      to_relative_path(path),
      call. = FALSE
    )
  }

  invisible(path)
}

write_text_checked <- function(lines, path) {
  connection <- file(
    path,
    open = "wt",
    encoding = "UTF-8"
  )
  writeLines(lines, connection, useBytes = TRUE)
  close(connection)

  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
    stop(
      "Text output was not created or is empty: ",
      to_relative_path(path),
      call. = FALSE
    )
  }

  invisible(path)
}

archive_existing_outputs(output_paths)

message("Reading the prepared local analysis data.")
analysis_data <- tryCatch(
  readRDS(input_path),
  error = function(e) {
    stop(
      "Unable to read the processed RDS: ",
      conditionMessage(e),
      call. = FALSE
    )
  }
)

if (!is.data.frame(analysis_data)) {
  stop("The processed RDS does not contain a data frame.", call. = FALSE)
}

domain_pvs <- list(
  data_probability = sprintf("BSMDAT%02d", 1:5),
  number = sprintf("BSMNUM%02d", 1:5),
  algebra = sprintf("BSMALG%02d", 1:5),
  geometry_measurement = sprintf("BSMGEO%02d", 1:5)
)

required_columns <- c(
  "system_id",
  "school_uid",
  "student_uid",
  "eligible_main_dp",
  "BSBGHER",
  "ict_access",
  "language_at_home",
  "analysis_weight_candidate",
  unlist(domain_pvs, use.names = FALSE)
)

missing_columns <- setdiff(required_columns, names(analysis_data))
if (length(missing_columns) > 0L) {
  stop(
    "The processed RDS is missing required column(s): ",
    paste(missing_columns, collapse = " | "),
    call. = FALSE
  )
}

source_systems <- sort(unique(as.character(analysis_data$system_id)))
source_students_n <- nrow(analysis_data)
source_schools_n <- length(unique(as.character(analysis_data$school_uid)))

eligible_flag <- analysis_data$eligible_main_dp
if (!is.logical(eligible_flag)) {
  eligible_flag <- as.logical(eligible_flag)
}
eligible_flag[is.na(eligible_flag)] <- FALSE

analytic_data <- analysis_data[eligible_flag, required_columns, drop = FALSE]

if (nrow(analytic_data) == 0L) {
  stop("No eligible_main_dp observations were found.", call. = FALSE)
}

analytic_systems <- sort(unique(as.character(analytic_data$system_id)))
analytic_students_n <- nrow(analytic_data)
analytic_schools_n <- length(unique(as.character(analytic_data$school_uid)))
excluded_systems <- setdiff(source_systems, analytic_systems)

if (anyNA(analytic_data$system_id) ||
    anyNA(analytic_data$school_uid) ||
    anyNA(analytic_data$student_uid)) {
  stop(
    "system_id, school_uid, or student_uid contains missing values in eligible_main_dp.",
    call. = FALSE
  )
}
if (anyDuplicated(as.character(analytic_data$student_uid)) > 0L) {
  stop(
    "student_uid is not unique in eligible_main_dp.",
    call. = FALSE
  )
}

expected_excluded_systems <- c("BRA", "CIV", "PSE")
if (length(source_systems) != 47L) {
  stop(
    "Unexpected source-system count: ",
    length(source_systems),
    "; expected 47.",
    call. = FALSE
  )
}
if (length(analytic_systems) != 44L) {
  stop(
    "Unexpected analytic-system count: ",
    length(analytic_systems),
    "; expected 44.",
    call. = FALSE
  )
}
if (!identical(sort(excluded_systems), sort(expected_excluded_systems))) {
  stop(
    "Unexpected excluded systems: ",
    paste(excluded_systems, collapse = " | "),
    ". Expected BRA | CIV | PSE.",
    call. = FALSE
  )
}
if (analytic_students_n != 272315L) {
  stop(
    "Unexpected eligible_main_dp student count: ",
    analytic_students_n,
    "; expected 272315.",
    call. = FALSE
  )
}
if (analytic_schools_n != 8171L) {
  stop(
    "Unexpected eligible_main_dp school count: ",
    analytic_schools_n,
    "; expected 8171.",
    call. = FALSE
  )
}

weights <- analytic_data$analysis_weight_candidate
if (!is.numeric(weights)) {
  stop("analysis_weight_candidate must be numeric.", call. = FALSE)
}
if (anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
  stop(
    "analysis_weight_candidate contains missing, non-finite, or non-positive values in the analytic sample.",
    call. = FALSE
  )
}

weighted_mean <- function(x, w) {
  if (!is.numeric(x)) {
    stop("A continuous descriptive variable is not numeric.", call. = FALSE)
  }
  keep <- !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  sum(w[keep] * x[keep]) / sum(w[keep])
}

weighted_sd_population <- function(x, w) {
  if (!is.numeric(x)) {
    stop("A continuous descriptive variable is not numeric.", call. = FALSE)
  }
  keep <- !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0
  if (sum(keep) < 2L) {
    return(NA_real_)
  }
  x_valid <- x[keep]
  w_valid <- w[keep]
  mean_value <- sum(w_valid * x_valid) / sum(w_valid)
  sqrt(sum(w_valid * (x_valid - mean_value)^2) / sum(w_valid))
}

safe_unweighted_mean <- function(x) {
  keep <- !is.na(x) & is.finite(x)
  if (!any(keep)) NA_real_ else mean(x[keep])
}

safe_unweighted_sd <- function(x) {
  keep <- !is.na(x) & is.finite(x)
  if (sum(keep) < 2L) NA_real_ else stats::sd(x[keep])
}

for (variable_name in c("BSBGHER", unlist(domain_pvs, use.names = FALSE))) {
  x <- analytic_data[[variable_name]]
  if (!is.numeric(x)) {
    stop(variable_name, " must be numeric in the analytic sample.", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(
      variable_name,
      " contains missing or non-finite values in eligible_main_dp.",
      call. = FALSE
    )
  }
  if (length(unique(x)) < 2L) {
    stop(variable_name, " has no valid variation.", call. = FALSE)
  }
}

ict_values <- sort(unique(as.character(analytic_data$ict_access)))
language_values <- sort(unique(as.character(analytic_data$language_at_home)))
expected_ict_values <- sort(c("No", "Yes"))
expected_language_values <- sort(c(
  "Always",
  "Almost always",
  "Sometimes",
  "Never"
))

if (!identical(ict_values, expected_ict_values)) {
  stop(
    "Unexpected ict_access categories: ",
    paste(ict_values, collapse = " | "),
    call. = FALSE
  )
}
if (!identical(language_values, expected_language_values)) {
  stop(
    "Unexpected language_at_home categories: ",
    paste(language_values, collapse = " | "),
    call. = FALSE
  )
}

message("Computing continuous descriptives across five plausible values.")

domain_labels <- c(
  data_probability = "Data and Probability achievement",
  number = "Number achievement",
  algebra = "Algebra achievement",
  geometry_measurement = "Geometry and Measurement achievement"
)

pv_audit_rows <- list()
continuous_rows <- list()

for (domain_name in names(domain_pvs)) {
  pv_names <- domain_pvs[[domain_name]]
  domain_records <- vector("list", length(pv_names))

  for (pv_index in seq_along(pv_names)) {
    variable_name <- pv_names[[pv_index]]
    x <- analytic_data[[variable_name]]

    record <- data.frame(
      variable_group = domain_name,
      display_label = unname(domain_labels[[domain_name]]),
      pv_number = pv_index,
      variable_name = variable_name,
      analytic_systems_n = length(analytic_systems),
      analytic_students_n = analytic_students_n,
      analytic_schools_n = analytic_schools_n,
      unweighted_mean = safe_unweighted_mean(x),
      unweighted_sd = safe_unweighted_sd(x),
      weighted_mean = weighted_mean(x, weights),
      weighted_sd = weighted_sd_population(x, weights),
      weight_variable = "analysis_weight_candidate",
      stringsAsFactors = FALSE
    )

    domain_records[[pv_index]] <- record
    pv_audit_rows[[length(pv_audit_rows) + 1L]] <- record
  }

  domain_data <- do.call(rbind, domain_records)
  continuous_rows[[length(continuous_rows) + 1L]] <- data.frame(
    variable_group = domain_name,
    variable_name = paste(pv_names, collapse = " | "),
    display_label = unname(domain_labels[[domain_name]]),
    variable_type = "plausible_value_family",
    plausible_values_n = length(pv_names),
    analytic_systems_n = length(analytic_systems),
    analytic_students_n = analytic_students_n,
    analytic_schools_n = analytic_schools_n,
    unweighted_mean = mean(domain_data$unweighted_mean),
    unweighted_sd = mean(domain_data$unweighted_sd),
    weighted_mean = mean(domain_data$weighted_mean),
    weighted_sd = mean(domain_data$weighted_sd),
    pv_weighted_mean_min = min(domain_data$weighted_mean),
    pv_weighted_mean_max = max(domain_data$weighted_mean),
    pv_weighted_sd_min = min(domain_data$weighted_sd),
    pv_weighted_sd_max = max(domain_data$weighted_sd),
    weight_variable = "analysis_weight_candidate",
    method_note = paste(
      "Weighted mean is the arithmetic mean of five PV-specific weighted means.",
      "Weighted SD is the arithmetic mean of five PV-specific weighted population SDs.",
      "Plausible values were not averaged at student level."
    ),
    stringsAsFactors = FALSE
  )
}

her <- analytic_data$BSBGHER
continuous_rows[[length(continuous_rows) + 1L]] <- data.frame(
  variable_group = "home_educational_resources",
  variable_name = "BSBGHER",
  display_label = "Home Educational Resources scale",
  variable_type = "continuous_scale",
  plausible_values_n = 0L,
  analytic_systems_n = length(analytic_systems),
  analytic_students_n = analytic_students_n,
  analytic_schools_n = analytic_schools_n,
  unweighted_mean = safe_unweighted_mean(her),
  unweighted_sd = safe_unweighted_sd(her),
  weighted_mean = weighted_mean(her, weights),
  weighted_sd = weighted_sd_population(her, weights),
  pv_weighted_mean_min = NA_real_,
  pv_weighted_mean_max = NA_real_,
  pv_weighted_sd_min = NA_real_,
  pv_weighted_sd_max = NA_real_,
  weight_variable = "analysis_weight_candidate",
  method_note = paste(
    "Weighted population SD uses sqrt(sum(w * (x - weighted_mean)^2) / sum(w)).",
    "The scale was not categorised or standardised."
  ),
  stringsAsFactors = FALSE
)

continuous_descriptives <- do.call(rbind, continuous_rows)
pv_audit <- do.call(rbind, pv_audit_rows)

if (any(!is.finite(continuous_descriptives$weighted_mean)) ||
    any(!is.finite(continuous_descriptives$weighted_sd))) {
  stop("At least one continuous descriptive statistic is non-finite.", call. = FALSE)
}

message("Computing weighted and unweighted categorical distributions.")

categorical_distribution <- function(
    x,
    variable_name,
    display_label,
    expected_levels,
    reference_category,
    weights) {
  x_character <- as.character(x)
  if (anyNA(x_character)) {
    stop(
      variable_name,
      " contains missing values in eligible_main_dp.",
      call. = FALSE
    )
  }

  total_n <- length(x_character)
  total_weight <- sum(weights)

  rows <- lapply(expected_levels, function(category) {
    selected <- x_character == category
    category_n <- sum(selected)
    category_weight <- sum(weights[selected])

    data.frame(
      variable_name = variable_name,
      display_label = display_label,
      reference_category = reference_category,
      category = category,
      analytic_systems_n = length(analytic_systems),
      analytic_students_n = analytic_students_n,
      analytic_schools_n = analytic_schools_n,
      unweighted_n = category_n,
      unweighted_percentage = 100 * category_n / total_n,
      weighted_sum = category_weight,
      weighted_percentage = 100 * category_weight / total_weight,
      weight_variable = "analysis_weight_candidate",
      method_note = paste(
        "Percentages use the common eligible_main_dp sample.",
        "Weighted percentages use the same candidate weight as Round 4."
      ),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

ict_distribution <- categorical_distribution(
  x = analytic_data$ict_access,
  variable_name = "ict_access",
  display_label = "Own computer or tablet at home",
  expected_levels = c("No", "Yes"),
  reference_category = "No",
  weights = weights
)

language_distribution <- categorical_distribution(
  x = analytic_data$language_at_home,
  variable_name = "language_at_home",
  display_label = "Frequency of speaking the language of the test at home",
  expected_levels = c("Always", "Almost always", "Sometimes", "Never"),
  reference_category = "Always",
  weights = weights
)

categorical_descriptives <- rbind(
  ict_distribution,
  language_distribution
)

category_checks <- aggregate(
  cbind(unweighted_percentage, weighted_percentage) ~ variable_name,
  data = categorical_descriptives,
  FUN = sum
)

if (any(abs(category_checks$unweighted_percentage - 100) > 1e-8) ||
    any(abs(category_checks$weighted_percentage - 100) > 1e-8)) {
  stop("Categorical percentages do not sum to 100%.", call. = FALSE)
}

system_weight_sums <- aggregate(
  analysis_weight_candidate ~ system_id,
  data = analytic_data,
  FUN = sum
)

validation <- data.frame(
  check_id = 1:10,
  check = c(
    "Processed RDS available",
    "Required columns available",
    "Source-system count",
    "Analytic-system count",
    "Expected excluded systems",
    "Common analytic sample size",
    "Positive finite analysis weights",
    "Complete and variable continuous measures",
    "Expected categorical levels",
    "Aggregate output integrity"
  ),
  status = rep("PASS", 10L),
  details = c(
    to_relative_path(input_path),
    paste(length(required_columns), "required columns verified"),
    paste(length(source_systems), "systems"),
    paste(length(analytic_systems), "systems"),
    paste(excluded_systems, collapse = " | "),
    paste(
      analytic_students_n,
      "students;",
      analytic_schools_n,
      "schools"
    ),
    paste(
      "minimum =",
      format(min(weights), digits = 8),
      "; maximum =",
      format(max(weights), digits = 8)
    ),
    "BSBGHER and 20 plausible values verified",
    paste(
      "ict_access:",
      paste(c("No", "Yes"), collapse = " | "),
      "; language_at_home:",
      paste(c("Always", "Almost always", "Sometimes", "Never"), collapse = " | ")
    ),
    paste(
      nrow(continuous_descriptives),
      "continuous rows;",
      nrow(categorical_descriptives),
      "categorical rows;",
      nrow(pv_audit),
      "PV audit rows"
    )
  ),
  stringsAsFactors = FALSE
)

message("Writing aggregate descriptive outputs.")
write_csv_checked(continuous_descriptives, output_paths$continuous)
write_csv_checked(categorical_descriptives, output_paths$categorical)
write_csv_checked(pv_audit, output_paths$pv_audit)

session_lines <- c(
  "TIMSS 2023 Grade 8 descriptive-statistics session information",
  paste0("Script version: 06_descriptive_statistics_v1.0_2026-09-04"),
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  capture.output(sessionInfo())
)
write_text_checked(session_lines, output_paths$session)

summary_lines <- c(
  "TIMSS 2023 Grade 8 aggregate descriptive-statistics summary",
  "Script version: 06_descriptive_statistics_v1.0_2026-09-04",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "Scope",
  paste0("- Source systems: ", length(source_systems)),
  paste0("- Source students: ", source_students_n),
  paste0("- Source schools: ", source_schools_n),
  paste0("- Analytic systems: ", length(analytic_systems)),
  paste0("- Analytic students: ", analytic_students_n),
  paste0("- Analytic schools: ", analytic_schools_n),
  paste0("- Excluded systems: ", paste(excluded_systems, collapse = " | ")),
  paste0(
    "- Student retention: ",
    format(round(100 * analytic_students_n / source_students_n, 3), nsmall = 3),
    "%"
  ),
  paste0(
    "- School retention: ",
    format(round(100 * analytic_schools_n / source_schools_n, 3), nsmall = 3),
    "%"
  ),
  "",
  "Weighting",
  "- Weight used: analysis_weight_candidate.",
  "- Round 3 formula: SENWGT * 500 / sum(SENWGT within each system).",
  "- Weighted means and percentages are invariant to the constant mean-one rescaling used during Round 4 model fitting.",
  paste0(
    "- Complete-case system weight totals range from ",
    format(min(system_weight_sums$analysis_weight_candidate), digits = 9),
    " to ",
    format(max(system_weight_sums$analysis_weight_candidate), digits = 9),
    "."
  ),
  "",
  "Plausible-value descriptive method",
  "- Each achievement domain was summarised separately for PV1-PV5.",
  "- The displayed domain mean is the arithmetic mean of five PV-specific weighted means.",
  "- The displayed domain SD is the arithmetic mean of five PV-specific weighted population SDs.",
  "- Plausible values were not averaged at student level.",
  "- No regression or multilevel model was fitted.",
  "- No sampling standard error or confidence interval was calculated for these descriptive statistics.",
  "- The descriptives do not implement full TIMSS JK2 variance estimation.",
  "",
  "Continuous descriptive results",
  capture.output(print(
    continuous_descriptives[
      , c(
        "display_label",
        "weighted_mean",
        "weighted_sd",
        "unweighted_mean",
        "unweighted_sd"
      )
    ],
    row.names = FALSE,
    digits = 5
  )),
  "",
  "Categorical descriptive results",
  capture.output(print(
    categorical_descriptives[
      , c(
        "display_label",
        "category",
        "unweighted_n",
        "unweighted_percentage",
        "weighted_percentage"
      )
    ],
    row.names = FALSE,
    digits = 5
  )),
  "",
  "Automatic validation",
  capture.output(print(validation, row.names = FALSE)),
  "",
  "Final status: PASS",
  "",
  "Output files",
  paste0("- ", to_relative_path(output_paths$continuous)),
  paste0("- ", to_relative_path(output_paths$categorical)),
  paste0("- ", to_relative_path(output_paths$pv_audit)),
  paste0("- ", to_relative_path(output_paths$summary)),
  paste0("- ", to_relative_path(output_paths$session))
)

write_text_checked(summary_lines, output_paths$summary)

required_outputs <- unlist(output_paths, use.names = FALSE)
output_ok <- file.exists(required_outputs) &
  !is.na(file.info(required_outputs)$size) &
  file.info(required_outputs)$size > 0

if (!all(output_ok)) {
  stop(
    "One or more required Round 6 output files were not created successfully.",
    call. = FALSE
  )
}

message("")
message("Round 6 aggregate descriptive statistics completed.")
message("Source systems: ", length(source_systems))
message("Achievement-model systems: ", length(analytic_systems))
message("Excluded systems: ", paste(excluded_systems, collapse = " | "))
message("Students described: ", analytic_students_n)
message("Schools described: ", analytic_schools_n)
message("Final status: PASS")
message("Output files:")
for (path in required_outputs) {
  message("  - ", to_relative_path(path))
}

rm(analysis_data, analytic_data)
invisible(gc())
