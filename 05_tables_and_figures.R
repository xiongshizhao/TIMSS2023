# TIMSS 2023 Grade 8 dissertation
# Round 5: publication-safe tables and figures from Round 4 aggregate outputs
#
# This script reads aggregate model outputs only. It does not read raw TIMSS
# files, the processed student-level RDS, or fitted model objects, and it does
# not fit or refit any statistical model.

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c(
  "here",
  "readr",
  "dplyr",
  "ggplot2",
  "officer",
  "flextable"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Missing package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Run this once in the R Console: install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(officer)
  library(flextable)
})

script_version <- "05_tables_and_figures_v1.3_2026-09-04"

stop_with <- function(...) {
  stop(paste0(...), call. = FALSE)
}

relative_path <- function(path) {
  root <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)
  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (identical(target, root)) {
    return(".")
  }
  if (startsWith(target, prefix)) {
    return(substring(target, nchar(prefix) + 1L))
  }
  basename(path)
}

assert_file <- function(path, label) {
  if (!file.exists(path)) {
    stop_with("Missing required input file: ", relative_path(path),
              " (", label, ").")
  }
  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    stop_with("Required input file is empty: ", relative_path(path), ".")
  }
  invisible(TRUE)
}

read_csv_checked <- function(path, label) {
  assert_file(path, label)
  result <- tryCatch(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    ),
    error = function(e) {
      stop_with(
        "Could not read ", label, " from ", relative_path(path),
        ": ", conditionMessage(e)
      )
    }
  )
  if (nrow(result) == 0L) {
    stop_with(label, " contains zero data rows: ", relative_path(path), ".")
  }
  result
}

assert_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop_with(
      label, " does not match the Round 4 schema. Missing column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

assert_numeric_columns <- function(data, columns, label) {
  invalid <- columns[!vapply(data[columns], is.numeric, logical(1))]
  if (length(invalid) > 0L) {
    stop_with(
      label, " has non-numeric required column(s): ",
      paste(invalid, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

assert_exact_values <- function(actual, expected, label) {
  actual <- sort(unique(stats::na.omit(as.character(actual))))
  expected <- sort(unique(as.character(expected)))
  if (!identical(actual, expected)) {
    stop_with(
      label, " differs from the Round 4 output. Expected: ",
      paste(expected, collapse = " | "), "; found: ",
      paste(actual, collapse = " | "), "."
    )
  }
  invisible(TRUE)
}

assert_finite_results <- function(data, label) {
  numeric_fields <- c(
    "pooled_estimate",
    "pooled_standard_error",
    "confidence_interval_lower",
    "confidence_interval_upper",
    "p_value"
  )
  bad <- !Reduce(
    `&`,
    lapply(data[numeric_fields], function(x) is.finite(x))
  )
  if (any(bad)) {
    stop_with(label, " contains missing or non-finite pooled results.")
  }
  if (any(data$confidence_interval_lower > data$confidence_interval_upper)) {
    stop_with(label, " contains a confidence interval with lower > upper.")
  }
  if (any(data$pooled_standard_error < 0)) {
    stop_with(label, " contains a negative pooled standard error.")
  }
  invisible(TRUE)
}

format_number <- function(x, digits = 2L) {
  formatC(x, format = "f", digits = digits)
}

format_count <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_p_value <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3))
  )
}

format_ci <- function(lower, upper, digits = 2L) {
  paste0(
    "[", format_number(lower, digits), ", ",
    format_number(upper, digits), "]"
  )
}

parse_note_value <- function(lines, prefix, required = TRUE) {
  hits <- grep(paste0("^", prefix), lines, value = TRUE)
  if (length(hits) == 0L) {
    if (required) {
      stop_with("Round 4 method notes do not contain: ", prefix)
    }
    return(NA_character_)
  }
  if (length(hits) > 1L) {
    stop_with("Round 4 method notes contain duplicate lines for: ", prefix)
  }
  trimws(sub(paste0("^", prefix), "", hits))
}

write_csv_checked <- function(data, path) {
  tryCatch(
    readr::write_excel_csv(data, path, na = ""),
    error = function(e) {
      stop_with(
        "Could not write output ", relative_path(path), ": ",
        conditionMessage(e)
      )
    }
  )
  if (!file.exists(path) || is.na(file.info(path)$size) ||
      file.info(path)$size <= 0) {
    stop_with("Output was not created or is empty: ", relative_path(path), ".")
  }
  invisible(TRUE)
}

archive_existing_outputs <- function(paths, archive_root) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0L) {
    return(invisible(character(0)))
  }
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_dir <- file.path(archive_root, paste0("05_tables_and_figures_", timestamp))
  if (!dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(archive_dir)) {
    stop_with("Could not create local archive directory for previous outputs.")
  }
  archived <- character(0)
  for (path in existing) {
    destination <- file.path(archive_dir, basename(path))
    copied <- file.copy(path, destination, overwrite = FALSE, copy.mode = TRUE)
    if (!isTRUE(copied)) {
      stop_with("Could not archive existing output: ", relative_path(path), ".")
    }
    if (unlink(path) != 0L) {
      stop_with("Could not replace archived output: ", relative_path(path), ".")
    }
    archived <- c(archived, destination)
  }
  message(
    "Archived ", length(archived), " previous output file(s) under ",
    relative_path(archive_dir), "."
  )
  invisible(archived)
}

save_plot_checked <- function(plot, pdf_path, png_path, width, height) {
  mac_quartz_pdf <- function(filename, width, height, ...) {
    grDevices::quartz(
      type = "pdf",
      file = filename,
      width = width,
      height = height,
      ...
    )
  }
  pdf_device <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    mac_quartz_pdf
  } else {
    "pdf"
  }
  tryCatch(
    {
      ggplot2::ggsave(
        filename = pdf_path,
        plot = plot,
        device = pdf_device,
        width = width,
        height = height,
        units = "in",
        bg = "white"
      )
      ggplot2::ggsave(
        filename = png_path,
        plot = plot,
        device = "png",
        width = width,
        height = height,
        units = "in",
        dpi = 300,
        bg = "white"
      )
    },
    error = function(e) {
      stop_with("Figure export failed: ", conditionMessage(e))
    }
  )
  for (path in c(pdf_path, png_path)) {
    if (!file.exists(path) || is.na(file.info(path)$size) ||
        file.info(path)$size <= 0) {
      stop_with("Figure file was not created or is empty: ",
                relative_path(path), ".")
    }
  }
  invisible(TRUE)
}

make_booktabs <- function(data, widths, left_columns, right_columns) {
  ft <- flextable::flextable(data)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "header")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  ft <- flextable::padding(ft, padding = 4, part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  if (length(left_columns) > 0L) {
    ft <- flextable::align(
      ft, j = left_columns, align = "left", part = "all"
    )
  }
  if (length(right_columns) > 0L) {
    ft <- flextable::align(
      ft, j = right_columns, align = "right", part = "all"
    )
  }
  for (column_name in names(widths)) {
    ft <- flextable::width(ft, j = column_name, width = widths[[column_name]])
  }
  flextable::set_table_properties(ft, layout = "fixed", width = 1)
}

add_table_to_document <- function(doc, number, title, ft, note) {
  number_paragraph <- officer::fpar(
    officer::ftext(
      paste0("Table ", number),
      officer::fp_text(
        font.family = "Arial", font.size = 10, bold = TRUE
      )
    )
  )
  title_paragraph <- officer::fpar(
    officer::ftext(
      title,
      officer::fp_text(
        font.family = "Arial", font.size = 10, italic = TRUE
      )
    )
  )
  note_paragraph <- officer::fpar(
    officer::ftext(
      "Note. ",
      officer::fp_text(
        font.family = "Arial", font.size = 8.5, italic = TRUE
      )
    ),
    officer::ftext(
      note,
      officer::fp_text(font.family = "Arial", font.size = 8.5)
    )
  )
  doc <- officer::body_add_fpar(doc, number_paragraph)
  doc <- officer::body_add_fpar(doc, title_paragraph)
  doc <- flextable::body_add_flextable(doc, value = ft, align = "left")
  officer::body_add_fpar(doc, note_paragraph)
}

main_direction <- function(lower, upper) {
  ifelse(
    lower > 0,
    "Positive association",
    ifelse(upper < 0, "Negative association", "No clear association")
  )
}

domain_direction <- function(lower, upper) {
  ifelse(
    lower > 0,
    "Stronger in Data and Probability",
    ifelse(
      upper < 0,
      "Weaker in Data and Probability",
      "No clear domain difference"
    )
  )
}

project_root <- here::here()
if (!identical(basename(normalizePath(project_root, mustWork = TRUE)),
               "TIMSS_2023")) {
  stop_with(
    "The RStudio Project root must be TIMSS_2023. Current project folder: ",
    basename(normalizePath(project_root, mustWork = TRUE)), "."
  )
}

message("Project: TIMSS_2023")
message("Round 5 reads aggregate Round 4 outputs only and fits no models.")

input_paths <- list(
  first_pass = here::here(
    "outputs", "model_results", "first_pass_dp_estimates.csv"
  ),
  main_multilevel = here::here(
    "outputs", "model_results", "main_multilevel_dp_estimates.csv"
  ),
  domain_difference = here::here(
    "outputs", "model_results", "domain_difference_estimates.csv"
  ),
  icc = here::here("outputs", "model_results", "icc_summary.csv"),
  diagnostics = here::here(
    "outputs", "model_results", "model_diagnostics.csv"
  ),
  sample = here::here(
    "outputs", "model_results", "model_sample_summary.csv"
  ),
  method_notes = here::here(
    "outputs", "model_results", "model_method_notes.txt"
  )
)

output_dirs <- list(
  tables = here::here("outputs", "tables"),
  figures_main = here::here("outputs", "figures", "main"),
  figures = here::here("outputs", "figures"),
  plot_data = here::here("outputs", "plot_data"),
  local_only = here::here("outputs", "local_only")
)

for (directory in output_dirs) {
  if (!dir.create(directory, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(directory)) {
    stop_with("Could not create output directory: ",
              relative_path(directory), ".")
  }
}

output_paths <- list(
  table_1 = file.path(
    output_dirs$tables, "Table_1_analysis_sample_and_icc.csv"
  ),
  table_2 = file.path(output_dirs$tables, "Table_2_dp_associations.csv"),
  table_3 = file.path(
    output_dirs$tables, "Table_3_domain_comparisons.csv"
  ),
  tables_docx = file.path(
    output_dirs$tables, "TIMSS_dissertation_results_tables.docx"
  ),
  figure_1_pdf = file.path(
    output_dirs$figures_main, "Figure_1_dp_associations.pdf"
  ),
  figure_1_png = file.path(
    output_dirs$figures_main, "Figure_1_dp_associations.png"
  ),
  figure_2_pdf = file.path(
    output_dirs$figures_main, "Figure_2_domain_comparisons.pdf"
  ),
  figure_2_png = file.path(
    output_dirs$figures_main, "Figure_2_domain_comparisons.png"
  ),
  figure_1_data = file.path(
    output_dirs$plot_data, "Figure_1_dp_associations_data.csv"
  ),
  figure_2_data = file.path(
    output_dirs$plot_data, "Figure_2_domain_comparisons_data.csv"
  ),
  figure_manifest = file.path(
    output_dirs$figures, "figure_manifest.csv"
  ),
  rq_evidence = file.path(
    output_dirs$tables, "rq_evidence_summary.csv"
  )
)

message("Checking Round 4 input files and schemas.")

first_pass <- read_csv_checked(
  input_paths$first_pass, "first-pass D&P estimates"
)
main_multilevel <- read_csv_checked(
  input_paths$main_multilevel, "main multilevel D&P estimates"
)
domain_difference <- read_csv_checked(
  input_paths$domain_difference, "domain-difference estimates"
)
icc_summary <- read_csv_checked(input_paths$icc, "ICC summary")
model_diagnostics <- read_csv_checked(
  input_paths$diagnostics, "model diagnostics"
)
model_sample_summary <- read_csv_checked(
  input_paths$sample, "model sample summary"
)
assert_file(input_paths$method_notes, "Round 4 method notes")
method_notes <- readLines(input_paths$method_notes, warn = FALSE, encoding = "UTF-8")
if (length(method_notes) == 0L) {
  stop_with("Round 4 method notes contain no lines.")
}

estimate_columns <- c(
  "model_id", "outcome", "systems_n", "students_n", "schools_n",
  "excluded_systems", "weight_variable", "term", "term_label",
  "term_role", "n_pv", "pool_status", "pooled_estimate",
  "pooled_standard_error", "degrees_of_freedom", "confidence_level",
  "confidence_interval_lower", "confidence_interval_upper", "p_value",
  "within_pv_variance", "between_pv_variance", "total_variance",
  "between_pv_fraction_of_total_variance", "pv1_estimate",
  "pv1_standard_error", "pv2_estimate", "pv2_standard_error",
  "pv3_estimate", "pv3_standard_error", "pv4_estimate",
  "pv4_standard_error", "pv5_estimate", "pv5_standard_error",
  "variance_method", "interpretation"
)

icc_columns <- c(
  "model_id", "outcome", "summary_level", "pv_number",
  "school_variance", "residual_variance", "icc", "icc_mean",
  "icc_minimum", "icc_maximum", "systems_n", "students_n",
  "schools_n", "excluded_systems", "method_note"
)

diagnostic_columns <- c(
  "model_id", "outcome", "pv_number", "sample_flag",
  "eligible_rows_supplied", "observations_used", "systems_used",
  "schools_used", "excluded_systems", "fit_success",
  "convergence_status", "convergence_message", "singular_fit",
  "optimizer", "elapsed_seconds", "fixed_effect_terms_present",
  "missing_coefficients", "school_variance", "residual_variance", "icc",
  "captured_warnings", "captured_messages", "error_message"
)

sample_columns <- c(
  "summary_type", "model_id", "sample_flag", "outcome", "systems_n",
  "students_n", "schools_n", "excluded_systems", "system_id",
  "included_in_achievement_models", "exclusion_reason", "variable",
  "category", "reference_category", "unweighted_n",
  "unweighted_percentage", "weighted_sum", "weighted_percentage"
)

assert_columns(first_pass, estimate_columns, "first_pass_dp_estimates.csv")
assert_columns(
  main_multilevel, estimate_columns, "main_multilevel_dp_estimates.csv"
)
assert_columns(
  domain_difference, estimate_columns, "domain_difference_estimates.csv"
)
assert_columns(icc_summary, icc_columns, "icc_summary.csv")
assert_columns(model_diagnostics, diagnostic_columns, "model_diagnostics.csv")
assert_columns(model_sample_summary, sample_columns, "model_sample_summary.csv")

estimate_numeric_columns <- c(
  "systems_n", "students_n", "schools_n", "n_pv", "pooled_estimate",
  "pooled_standard_error", "degrees_of_freedom", "confidence_level",
  "confidence_interval_lower", "confidence_interval_upper", "p_value",
  "within_pv_variance", "between_pv_variance", "total_variance",
  "between_pv_fraction_of_total_variance", "pv1_estimate",
  "pv1_standard_error", "pv2_estimate", "pv2_standard_error",
  "pv3_estimate", "pv3_standard_error", "pv4_estimate",
  "pv4_standard_error", "pv5_estimate", "pv5_standard_error"
)
for (item in list(
  list(data = first_pass, label = "first_pass_dp_estimates.csv"),
  list(data = main_multilevel, label = "main_multilevel_dp_estimates.csv"),
  list(data = domain_difference, label = "domain_difference_estimates.csv")
)) {
  assert_numeric_columns(item$data, estimate_numeric_columns, item$label)
  assert_finite_results(item$data, item$label)
  if (any(item$data$n_pv != 5L)) {
    stop_with(item$label, " contains a row not pooled from exactly five PVs.")
  }
  if (any(item$data$pool_status != "PASS")) {
    stop_with(item$label, " contains a non-PASS pooling status.")
  }
  if (any(item$data$p_value < 0 | item$data$p_value > 1)) {
    stop_with(item$label, " contains a p-value outside [0, 1].")
  }
  if (any(
    item$data$pooled_estimate < item$data$confidence_interval_lower |
      item$data$pooled_estimate > item$data$confidence_interval_upper
  )) {
    stop_with(item$label, " contains an estimate outside its confidence interval.")
  }
}

assert_numeric_columns(
  icc_summary,
  c(
    "pv_number", "school_variance", "residual_variance", "icc",
    "icc_mean", "icc_minimum", "icc_maximum", "systems_n",
    "students_n", "schools_n"
  ),
  "icc_summary.csv"
)
assert_numeric_columns(
  model_diagnostics,
  c(
    "pv_number", "eligible_rows_supplied", "observations_used",
    "systems_used", "schools_used", "elapsed_seconds", "school_variance",
    "residual_variance", "icc"
  ),
  "model_diagnostics.csv"
)
if (!is.logical(model_diagnostics$fit_success)) {
  stop_with("model_diagnostics.csv column fit_success must be logical.")
}
if (!is.logical(model_diagnostics$singular_fit)) {
  stop_with("model_diagnostics.csv column singular_fit must be logical or NA.")
}
assert_numeric_columns(
  model_sample_summary,
  c(
    "systems_n", "students_n", "schools_n", "unweighted_n",
    "unweighted_percentage", "weighted_sum", "weighted_percentage"
  ),
  "model_sample_summary.csv"
)
if (!is.logical(model_sample_summary$included_in_achievement_models)) {
  stop_with(
    paste0(
      "model_sample_summary.csv column included_in_achievement_models ",
      "must be logical or NA."
    )
  )
}

expected_terms <- c(
  "BSBGHER",
  "ict_accessYes",
  "language_at_homeAlmost always",
  "language_at_homeSometimes",
  "language_at_homeNever"
)
focal_terms <- c("BSBGHER", "ict_accessYes")
expected_domain_outcomes <- c(
  "Data and Probability minus Number",
  "Data and Probability minus Algebra",
  "Data and Probability minus Geometry and Measurement",
  paste0(
    "Data and Probability minus the mean of Number, Algebra, and ",
    "Geometry and Measurement"
  )
)
expected_model_ids <- c(
  "A_first_pass_dp",
  "B_null_dp",
  "C_main_dp",
  "D_dp_minus_number",
  "E_dp_minus_algebra",
  "F_dp_minus_geometry",
  "G_dp_minus_other_mean"
)

assert_exact_values(first_pass$model_id, "A_first_pass_dp",
                    "First-pass model ID")
assert_exact_values(main_multilevel$model_id, "C_main_dp",
                    "Main multilevel model ID")
assert_exact_values(first_pass$outcome, "Data and Probability",
                    "First-pass outcome")
assert_exact_values(main_multilevel$outcome, "Data and Probability",
                    "Main multilevel outcome")
assert_exact_values(first_pass$term, expected_terms,
                    "First-pass coefficient terms")
assert_exact_values(main_multilevel$term, expected_terms,
                    "Main multilevel coefficient terms")
assert_exact_values(domain_difference$model_id, expected_model_ids[4:7],
                    "Domain-difference model IDs")
assert_exact_values(domain_difference$outcome, expected_domain_outcomes,
                    "Domain-difference outcomes")
assert_exact_values(domain_difference$term, expected_terms,
                    "Domain-difference coefficient terms")

for (item in list(
  list(data = first_pass, label = "first_pass_dp_estimates.csv"),
  list(data = main_multilevel, label = "main_multilevel_dp_estimates.csv")
)) {
  term_counts <- item$data |>
    dplyr::count(term, name = "rows_n")
  if (nrow(item$data) != 5L || nrow(term_counts) != 5L ||
      any(term_counts$rows_n != 1L)) {
    stop_with(item$label, " must contain exactly one row for each retained term.")
  }
}

domain_cells <- domain_difference |>
  dplyr::count(outcome, term, name = "rows_n")
if (nrow(domain_cells) != 20L || any(domain_cells$rows_n != 1L)) {
  stop_with(
    "domain_difference_estimates.csv must contain one row for every ",
    "combination of four outcomes and five retained coefficient terms."
  )
}

assert_exact_values(model_diagnostics$model_id, expected_model_ids,
                    "Diagnostic model IDs")
assert_exact_values(
  model_diagnostics$outcome,
  c("Data and Probability", expected_domain_outcomes),
  "Diagnostic outcome labels"
)
if (nrow(model_diagnostics) != 35L) {
  stop_with("model_diagnostics.csv must contain 35 PV-specific model fits; found ",
            nrow(model_diagnostics), ".")
}
if (any(!model_diagnostics$fit_success)) {
  stop_with("At least one Round 4 diagnostic row has fit_success = FALSE.")
}
if (any(model_diagnostics$convergence_status != "CONVERGED")) {
  stop_with("At least one Round 4 model is not marked CONVERGED.")
}

icc_five_pv <- icc_summary |>
  dplyr::filter(summary_level == "five_pv_summary")
icc_pv_specific <- icc_summary |>
  dplyr::filter(summary_level == "pv_specific")
if (nrow(icc_five_pv) != 1L || nrow(icc_pv_specific) != 5L) {
  stop_with(
    "icc_summary.csv must contain five PV-specific rows and one ",
    "five-PV summary row."
  )
}
if (!all(is.finite(c(
  icc_five_pv$icc_mean,
  icc_five_pv$icc_minimum,
  icc_five_pv$icc_maximum
)))) {
  stop_with("The five-PV ICC summary is missing or non-finite.")
}
if (icc_five_pv$icc_minimum > icc_five_pv$icc_maximum) {
  stop_with("The ICC minimum is greater than the ICC maximum.")
}

system_sample_rows <- model_sample_summary |>
  dplyr::filter(summary_type == "system_sample")
if (nrow(system_sample_rows) == 0L || any(is.na(system_sample_rows$system_id))) {
  stop_with("model_sample_summary.csv has no usable system_sample rows.")
}
assert_exact_values(
  model_sample_summary$summary_type,
  c("model_sample", "factor_category", "system_sample", "grade_validation"),
  "Model-sample summary types"
)
assert_exact_values(
  model_sample_summary$model_id,
  c(expected_model_ids, "ALL_REQUIRED_MODELS", "NOT_MODELLED"),
  "Model-sample summary model IDs"
)
if (nrow(system_sample_rows) != 47L || anyDuplicated(system_sample_rows$system_id)) {
  stop_with(
    "model_sample_summary.csv must contain one system_sample row for each of 47 systems."
  )
}

source_systems_n <- dplyr::n_distinct(system_sample_rows$system_id)
analysis_systems_n <- unique(main_multilevel$systems_n)
analysis_students_n <- unique(main_multilevel$students_n)
analysis_schools_n <- unique(main_multilevel$schools_n)
excluded_systems_text <- unique(main_multilevel$excluded_systems)

if (length(analysis_systems_n) != 1L ||
    length(analysis_students_n) != 1L ||
    length(analysis_schools_n) != 1L ||
    length(excluded_systems_text) != 1L) {
  stop_with("Main multilevel sample fields are not internally consistent.")
}

excluded_systems <- trimws(unlist(strsplit(
  excluded_systems_text, "\\|", fixed = FALSE
)))

if (source_systems_n != 47L) {
  stop_with("Expected 47 source systems from Round 4; found ",
            source_systems_n, ".")
}
if (analysis_systems_n != 44L ||
    !setequal(excluded_systems, c("BRA", "CIV", "PSE"))) {
  stop_with(
    "Round 4 achievement scope is not the confirmed 44 systems with ",
    "BRA, CIV, and PSE excluded."
  )
}
if (analysis_students_n != 272315L || analysis_schools_n != 8171L) {
  stop_with(
    "Round 4 analytic counts differ from the confirmed values. Found ",
    format_count(analysis_students_n), " students and ",
    format_count(analysis_schools_n), " schools."
  )
}

estimate_sample_rows <- dplyr::bind_rows(
  first_pass,
  main_multilevel,
  domain_difference
)
if (any(estimate_sample_rows$systems_n != analysis_systems_n) ||
    any(estimate_sample_rows$students_n != analysis_students_n) ||
    any(estimate_sample_rows$schools_n != analysis_schools_n)) {
  stop_with("Round 4 estimate files do not use one consistent analytic sample.")
}
if (any(estimate_sample_rows$weight_variable != "analysis_weight_candidate")) {
  stop_with("Round 4 estimate files do not consistently use analysis_weight_candidate.")
}
if (any(model_diagnostics$systems_used != analysis_systems_n) ||
    any(model_diagnostics$observations_used != analysis_students_n) ||
    any(model_diagnostics$schools_used != analysis_schools_n)) {
  stop_with("Round 4 diagnostic rows do not use one consistent analytic sample.")
}

analysis_status <- parse_note_value(method_notes, "Final analysis status:")
warning_count_text <- parse_note_value(
  method_notes, "Recorded analysis warnings:", required = FALSE
)
warning_count <- suppressWarnings(as.integer(warning_count_text))
if (is.na(warning_count)) {
  warning_count <- 0L
}
if (!analysis_status %in% c("PASS", "PASS WITH WARNINGS")) {
  stop_with("Round 4 analysis status is not usable: ", analysis_status, ".")
}

model_fit_count <- nrow(model_diagnostics)
singular_fit_count <- sum(model_diagnostics$singular_fit %in% TRUE, na.rm = TRUE)
diagnostic_warning_count <- sum(
  !is.na(model_diagnostics$captured_warnings) &
    model_diagnostics$captured_warnings != "" &
    model_diagnostics$captured_warnings != "None"
)

if (singular_fit_count > 0L) {
  stop_with("Round 4 diagnostics contain singular model fits.")
}

message("Round 4 schema validation passed.")
message("Source systems: ", source_systems_n)
message("Achievement-model systems: ", analysis_systems_n)
message("Students: ", format_count(analysis_students_n))
message("Schools: ", format_count(analysis_schools_n))
message("Model fits: ", model_fit_count)
message("Round 4 status: ", analysis_status)

term_display <- c(
  BSBGHER = "Home educational resources (per scale point)",
  ict_accessYes = "Own computer or tablet at home: Yes vs No",
  `language_at_homeAlmost always` =
    "Language of test at home: Almost always vs Always",
  language_at_homeSometimes =
    "Language of test at home: Sometimes vs Always",
  language_at_homeNever =
    "Language of test at home: Never vs Always"
)

model_display <- c(
  A_first_pass_dp = "First-pass linear regression",
  C_main_dp = "Main multilevel model"
)

comparison_display <- c(
  `Data and Probability minus Number` = "D&P minus Number",
  `Data and Probability minus Algebra` = "D&P minus Algebra",
  `Data and Probability minus Geometry and Measurement` =
    "D&P minus Geometry and Measurement",
  `Data and Probability minus the mean of Number, Algebra, and Geometry and Measurement` =
    "D&P minus mean of the other three domains"
)

comparison_target <- c(
  `Data and Probability minus Number` = "Number",
  `Data and Probability minus Algebra` = "Algebra",
  `Data and Probability minus Geometry and Measurement` =
    "Geometry and Measurement",
  `Data and Probability minus the mean of Number, Algebra, and Geometry and Measurement` =
    "the mean of Number, Algebra, and Geometry and Measurement"
)

round4_warning_note <- if (analysis_status == "PASS WITH WARNINGS" ||
                           warning_count > 0L) {
  paste0(
    "Round 4 status was PASS WITH WARNINGS. The recorded warning concerns ",
    "material differences between first-pass and multilevel estimates for ",
    "BSBGHER and ict_accessYes; it is not a convergence or singular-fit failure."
  )
} else {
  "Round 4 status was PASS, with no recorded analysis warning."
}

variance_limitation <- paste0(
  "Standard errors and 95% confidence intervals are model-based and should ",
  "not be interpreted as full TIMSS JK2 design-based standard errors."
)

association_limitation <- paste0(
  "Results describe associations in the pooled 44-system analytic sample ",
  "and do not establish causation or identical system-specific associations."
)

table_1 <- tibble::tibble(
  item = c(
    "Source education systems",
    "Education systems included in achievement models",
    "Excluded education systems",
    "Students analysed",
    "Schools analysed",
    "PV-specific model fits",
    "Mean school-level ICC across five plausible values",
    "Observed ICC range across plausible values",
    "Round 4 analysis status"
  ),
  value = c(
    as.character(source_systems_n),
    as.character(analysis_systems_n),
    paste(excluded_systems, collapse = ", "),
    format_count(analysis_students_n),
    format_count(analysis_schools_n),
    as.character(model_fit_count),
    format_number(icc_five_pv$icc_mean, 3),
    paste0(
      format_number(icc_five_pv$icc_minimum, 3), " to ",
      format_number(icc_five_pv$icc_maximum, 3)
    ),
    analysis_status
  ),
  note = c(
    "Prepared source coverage before achievement eligibility restrictions.",
    "Systems contributing to all reported achievement models.",
    "Content-domain plausible values had no valid values for these systems.",
    "Identical complete-case sample used for the reported model comparisons.",
    "Unique school_uid groups in the analytic sample.",
    "Seven model families multiplied by five plausible values.",
    "Arithmetic mean of five weighted null-model ICCs.",
    "Observed minimum and maximum; Rubin pooling was not applied to ICC.",
    paste0(
      model_fit_count, "/", model_fit_count,
      " fits converged; ", singular_fit_count, " singular fits; ",
      diagnostic_warning_count, " PV-specific model-warning rows."
    )
  )
)

table_2_source <- dplyr::bind_rows(first_pass, main_multilevel) |>
  dplyr::mutate(
    model = unname(model_display[model_id]),
    predictor = unname(term_display[term]),
    model_order = match(model_id, names(model_display)),
    predictor_order = match(term, expected_terms)
  ) |>
  dplyr::arrange(model_order, predictor_order)

if (any(is.na(table_2_source$model)) || any(is.na(table_2_source$predictor))) {
  stop_with("A Table 2 model or predictor label could not be mapped.")
}

table_2 <- table_2_source |>
  dplyr::transmute(
    model,
    predictor,
    estimate = round(pooled_estimate, 2),
    standard_error = round(pooled_standard_error, 2),
    ci_lower = round(confidence_interval_lower, 2),
    ci_upper = round(confidence_interval_upper, 2),
    confidence_interval_95 = format_ci(
      confidence_interval_lower, confidence_interval_upper, 2
    ),
    p_value = format_p_value(p_value)
  )

table_3_source <- domain_difference |>
  dplyr::filter(term %in% focal_terms) |>
  dplyr::mutate(
    comparison = unname(comparison_display[outcome]),
    predictor = unname(term_display[term]),
    comparison_order = match(outcome, expected_domain_outcomes),
    predictor_order = match(term, focal_terms)
  ) |>
  dplyr::arrange(comparison_order, predictor_order)

if (nrow(table_3_source) != 8L ||
    any(is.na(table_3_source$comparison)) ||
    any(is.na(table_3_source$predictor))) {
  stop_with("Table 3 must contain eight mapped focal domain contrasts.")
}

table_3 <- table_3_source |>
  dplyr::transmute(
    comparison,
    predictor,
    estimate = round(pooled_estimate, 2),
    standard_error = round(pooled_standard_error, 2),
    ci_lower = round(confidence_interval_lower, 2),
    ci_upper = round(confidence_interval_upper, 2),
    confidence_interval_95 = format_ci(
      confidence_interval_lower, confidence_interval_upper, 2
    ),
    p_value = format_p_value(p_value)
  )

figure_1_data <- main_multilevel |>
  dplyr::filter(term %in% focal_terms) |>
  dplyr::mutate(
    figure_id = "Figure 1",
    predictor = ifelse(
      term == "BSBGHER", "Home educational resources", "ICT access"
    ),
    predictor_label = unname(term_display[term])
  ) |>
  dplyr::transmute(
    figure_id,
    predictor,
    predictor_label,
    term,
    estimate = pooled_estimate,
    standard_error = pooled_standard_error,
    ci_lower = confidence_interval_lower,
    ci_upper = confidence_interval_upper,
    p_value,
    systems_n,
    students_n,
    schools_n,
    excluded_systems
  )

if (nrow(figure_1_data) != 2L) {
  stop_with("Figure 1 requires exactly two focal coefficients.")
}

figure_2_data <- table_3_source |>
  dplyr::mutate(
    figure_id = "Figure 2",
    predictor = ifelse(
      term == "BSBGHER", "Home educational resources", "ICT access"
    ),
    predictor_label = unname(term_display[term])
  ) |>
  dplyr::transmute(
    figure_id,
    comparison,
    outcome,
    predictor,
    predictor_label,
    term,
    estimate = pooled_estimate,
    standard_error = pooled_standard_error,
    ci_lower = confidence_interval_lower,
    ci_upper = confidence_interval_upper,
    p_value,
    systems_n,
    students_n,
    schools_n,
    excluded_systems
  )

if (nrow(figure_2_data) != 8L) {
  stop_with("Figure 2 requires exactly eight focal domain contrasts.")
}

if (any(grepl("^factor\\(system_id\\)",
              c(figure_1_data$term, figure_2_data$term)))) {
  stop_with("Education-system fixed-effect coefficients entered plot data.")
}

main_evidence_statement <- function(term, estimate, lower, upper) {
  if (term == "BSBGHER") {
    if (lower > 0) {
      return(paste0(
        "In the primary multilevel model, a one-point increase in the Home ",
        "Educational Resources scale was associated with a ",
        format_number(estimate, 2),
        "-point higher Data and Probability score (95% CI ",
        format_number(lower, 2), " to ", format_number(upper, 2),
        ") in the pooled 44-system analytic sample."
      ))
    }
    return(paste0(
      "The primary multilevel estimate for Home Educational Resources was ",
      format_number(estimate, 2), " points (95% CI ",
      format_number(lower, 2), " to ", format_number(upper, 2),
      "); the interval did not show a clear association."
    ))
  }
  if (lower <= 0 && upper >= 0) {
    return(paste0(
      "Having one's own computer or tablet at home (Yes versus No) did not ",
      "show a clear adjusted association with Data and Probability ",
      "achievement in the pooled 44-system sample (estimate ",
      format_number(estimate, 2), "; 95% CI ",
      format_number(lower, 2), " to ", format_number(upper, 2), ")."
    ))
  }
  paste0(
    "Having one's own computer or tablet at home (Yes versus No) was ",
    "associated with a ", format_number(estimate, 2),
    "-point difference in Data and Probability achievement (95% CI ",
    format_number(lower, 2), " to ", format_number(upper, 2),
    ") in the pooled 44-system sample."
  )
}

domain_evidence_statement <- function(predictor, comparison, estimate,
                                      lower, upper) {
  if (lower > 0) {
    return(paste0(
      "The ", predictor, " association was estimated to be ",
      format_number(estimate, 2), " points stronger in Data and Probability ",
      "than in ", comparison, " (95% CI ", format_number(lower, 2),
      " to ", format_number(upper, 2), ") in the pooled 44-system sample."
    ))
  }
  if (upper < 0) {
    return(paste0(
      "The ", predictor, " association was estimated to be ",
      format_number(abs(estimate), 2), " points weaker in Data and Probability ",
      "than in ", comparison, " (95% CI ", format_number(lower, 2),
      " to ", format_number(upper, 2), ") in the pooled 44-system sample."
    ))
  }
  paste0(
    "The estimated difference in the ", predictor,
    " association between Data and Probability and ", comparison, " was ",
    format_number(estimate, 2), " points (95% CI ",
    format_number(lower, 2), " to ", format_number(upper, 2),
    "); because the interval included zero, there was no clear evidence of ",
    "a domain difference."
  )
}

rq_questions <- c(
  RQ1 = paste0(
    "How is Grade 8 achievement in TIMSS 2023 Data and Probability ",
    "associated with students' access to home educational resources?"
  ),
  RQ2 = paste0(
    "How is Grade 8 achievement in TIMSS 2023 Data and Probability ",
    "associated with ICT-related access or opportunities?"
  ),
  RQ3 = paste0(
    "Are the associations of home educational resources and ICT-related ",
    "access or opportunities with achievement stronger in TIMSS 2023 Data ",
    "and Probability than in Number, Algebra, and Geometry and Measurement?"
  )
)

evidence_limitation <- paste0(
  association_limitation, " ", variance_limitation,
  " The equal-system-contribution weight is a candidate model weight."
)

main_evidence <- main_multilevel |>
  dplyr::filter(term %in% focal_terms) |>
  dplyr::mutate(
    rq_id = ifelse(term == "BSBGHER", "RQ1", "RQ2"),
    research_question = unname(rq_questions[rq_id]),
    predictor = ifelse(
      term == "BSBGHER",
      "Home Educational Resources scale",
      "Own computer or tablet at home: Yes versus No"
    ),
    comparison = "Data and Probability main multilevel model",
    estimate = pooled_estimate,
    standard_error = pooled_standard_error,
    ci_lower = confidence_interval_lower,
    ci_upper = confidence_interval_upper,
    p_value = p_value,
    direction = main_direction(ci_lower, ci_upper),
    evidence_statement = mapply(
      main_evidence_statement,
      term,
      estimate,
      ci_lower,
      ci_upper,
      USE.NAMES = FALSE
    ),
    table_id = "Table 2",
    figure_id = "Figure 1",
    limitation = evidence_limitation
  ) |>
  dplyr::select(
    rq_id, research_question, predictor, comparison, estimate,
    standard_error, ci_lower, ci_upper, p_value, direction,
    evidence_statement, table_id, figure_id, limitation
  )

domain_evidence <- table_3_source |>
  dplyr::mutate(
    rq_id = "RQ3",
    research_question = unname(rq_questions["RQ3"]),
    predictor = ifelse(
      term == "BSBGHER",
      "Home Educational Resources scale",
      "Own computer or tablet at home: Yes versus No"
    ),
    comparison = unname(comparison_display[outcome]),
    estimate = pooled_estimate,
    standard_error = pooled_standard_error,
    ci_lower = confidence_interval_lower,
    ci_upper = confidence_interval_upper,
    p_value = p_value,
    direction = domain_direction(ci_lower, ci_upper),
    evidence_statement = mapply(
      domain_evidence_statement,
      ifelse(
        term == "BSBGHER", "home educational resources", "ICT access"
      ),
      unname(comparison_target[outcome]),
      estimate,
      ci_lower,
      ci_upper,
      USE.NAMES = FALSE
    ),
    table_id = "Table 3",
    figure_id = "Figure 2",
    limitation = evidence_limitation
  ) |>
  dplyr::select(
    rq_id, research_question, predictor, comparison, estimate,
    standard_error, ci_lower, ci_upper, p_value, direction,
    evidence_statement, table_id, figure_id, limitation
  )

rq_evidence_summary <- dplyr::bind_rows(main_evidence, domain_evidence)
if (nrow(rq_evidence_summary) != 10L ||
    any(!is.finite(rq_evidence_summary$estimate)) ||
    any(rq_evidence_summary$ci_lower > rq_evidence_summary$ci_upper)) {
  stop_with("RQ evidence summary failed its row-count or numeric validation.")
}

caption_1 <- paste0(
  "Pooled fixed-effect estimates from five plausible-value versions of the ",
  "primary Data and Probability school-random-intercept model. The model ",
  "includes education-system fixed effects and covers ", analysis_systems_n,
  " achievement-model systems, ", format_count(analysis_students_n),
  " students, and ", format_count(analysis_schools_n), " schools."
)
note_1 <- paste0(
  "Points are pooled estimates and whiskers are model-based 95% confidence ",
  "intervals. Models use the equal-system-contribution candidate weight. ",
  variance_limitation, " ", association_limitation,
  " The predictors use different units, so their coefficient magnitudes ",
  "should not be compared directly. Round 4 status: ", analysis_status, "."
)

caption_2 <- paste0(
  "Pooled fixed-effect estimates from five same-index plausible-value ",
  "domain-difference models with education-system fixed effects and a school ",
  "random intercept. The models cover ", analysis_systems_n,
  " achievement-model systems, ", format_count(analysis_students_n),
  " students, and ", format_count(analysis_schools_n), " schools."
)
note_2 <- paste0(
  "Positive estimates indicate that the association is stronger in Data and ",
  "Probability; negative estimates indicate that it is weaker. An interval ",
  "containing zero is not clear evidence of a domain difference. Models use ",
  "the equal-system-contribution candidate weight. Whiskers are model-based ",
  "95% confidence intervals. ", variance_limitation, " ",
  association_limitation, " Round 4 status: ", analysis_status, "."
)

alt_text_1 <- paste0(
  "Dot-and-whisker plot with two coefficients from the main Data and ",
  "Probability multilevel model. Home educational resources is positive at ",
  format_number(
    figure_1_data$estimate[figure_1_data$term == "BSBGHER"], 2
  ),
  " with a 95% interval that does not cross zero. ICT access is ",
  format_number(
    figure_1_data$estimate[figure_1_data$term == "ict_accessYes"], 2
  ),
  " with a 95% interval that crosses zero."
)

her_clear_n <- sum(
  figure_2_data$term == "BSBGHER" & figure_2_data$ci_lower > 0
)
ict_clear_n <- sum(
  figure_2_data$term == "ict_accessYes" & figure_2_data$ci_lower > 0
)
alt_text_2 <- paste0(
  "Faceted dot-and-whisker plot of eight domain contrasts. All ", her_clear_n,
  " home educational resources contrasts are positive with intervals above ",
  "zero. ", ict_clear_n,
  " of four ICT contrasts has an interval above zero; the other ICT intervals ",
  "cross zero."
)

figure_manifest <- tibble::tibble(
  figure_id = c("Figure 1", "Figure 2"),
  rq_id = c("RQ1 | RQ2", "RQ3"),
  title = c(
    "Associations with Data and Probability Achievement",
    "Differences in Resource-Achievement Associations Across Mathematics Domains"
  ),
  pdf_file = c(
    "outputs/figures/main/Figure_1_dp_associations.pdf",
    "outputs/figures/main/Figure_2_domain_comparisons.pdf"
  ),
  png_file = c(
    "outputs/figures/main/Figure_1_dp_associations.png",
    "outputs/figures/main/Figure_2_domain_comparisons.png"
  ),
  plot_data_file = c(
    "outputs/plot_data/Figure_1_dp_associations_data.csv",
    "outputs/plot_data/Figure_2_domain_comparisons_data.csv"
  ),
  caption = c(caption_1, caption_2),
  note = c(note_1, note_2),
  alt_text = c(alt_text_1, alt_text_2),
  interpretation_supported = c(
    paste0(
      "Direction, magnitude, and model-based uncertainty of adjusted ",
      "associations in the pooled analytic sample."
    ),
    paste0(
      "Direction, magnitude, and model-based uncertainty of differences in ",
      "associations between Data and Probability and the specified domains."
    )
  ),
  interpretation_not_supported = c(
    paste0(
      "Causal effects, full TIMSS JK2 inference, global generalisation, ",
      "identical system-specific associations, or direct comparison of ",
      "coefficient magnitudes across predictors."
    ),
    paste0(
      "Causal effects, full TIMSS JK2 inference, global generalisation, ",
      "identical system-specific differences, or comparisons among Number, ",
      "Algebra, and Geometry and Measurement."
    )
  )
)

archive_existing_outputs(
  unname(unlist(output_paths, use.names = FALSE)),
  file.path(output_dirs$local_only, "archive")
)

message("Writing public-safe table and plot-data CSV files.")
write_csv_checked(table_1, output_paths$table_1)
write_csv_checked(table_2, output_paths$table_2)
write_csv_checked(table_3, output_paths$table_3)
write_csv_checked(figure_1_data, output_paths$figure_1_data)
write_csv_checked(figure_2_data, output_paths$figure_2_data)
write_csv_checked(figure_manifest, output_paths$figure_manifest)
write_csv_checked(rq_evidence_summary, output_paths$rq_evidence)

figure_1_plot_data <- figure_1_data |>
  dplyr::mutate(
    predictor_label = factor(
      predictor_label,
      levels = rev(unname(term_display[focal_terms]))
    )
  )

figure_1_plot <- ggplot2::ggplot(
  figure_1_plot_data,
  ggplot2::aes(x = estimate, y = predictor_label)
) +
  ggplot2::geom_vline(
    xintercept = 0, colour = "#666666", linewidth = 0.5,
    linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = ci_lower, xend = ci_upper, yend = predictor_label),
    colour = "#0072B2", linewidth = 0.9
  ) +
  ggplot2::geom_point(colour = "#0072B2", size = 3.0) +
  ggplot2::labs(
    title = "Associations with Data and Probability Achievement",
    subtitle = "Primary multilevel model",
    x = "Pooled achievement-score coefficient (95% CI)",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_family = "sans", base_size = 11) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle = ggplot2::element_text(colour = "#555555"),
    axis.text.y = ggplot2::element_text(colour = "#222222"),
    plot.title.position = "plot",
    plot.margin = ggplot2::margin(10, 12, 10, 10)
  )

figure_2_plot_data <- figure_2_data |>
  dplyr::mutate(
    comparison = factor(
      comparison,
      levels = rev(unname(comparison_display[expected_domain_outcomes]))
    ),
    predictor = factor(
      predictor,
      levels = c("Home educational resources", "ICT access")
    )
  )

figure_2_plot <- ggplot2::ggplot(
  figure_2_plot_data,
  ggplot2::aes(x = estimate, y = comparison, colour = predictor)
) +
  ggplot2::geom_vline(
    xintercept = 0, colour = "#666666", linewidth = 0.5,
    linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = ci_lower, xend = ci_upper, yend = comparison),
    linewidth = 0.9
  ) +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::facet_wrap(
    ggplot2::vars(predictor), ncol = 1, scales = "fixed"
  ) +
  ggplot2::scale_colour_manual(
    values = c(
      "Home educational resources" = "#0072B2",
      "ICT access" = "#D55E00"
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    title = paste0(
      "Differences in Resource-Achievement Associations\n",
      "Across Mathematics Domains"
    ),
    subtitle = "Positive values indicate a stronger association in Data and Probability",
    x = "Pooled domain-difference coefficient (95% CI)",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_family = "sans", base_size = 10.5) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", colour = "#222222"),
    strip.background = ggplot2::element_rect(
      fill = "#F2F2F2", colour = NA
    ),
    plot.title = ggplot2::element_text(face = "bold", size = 12.5),
    plot.subtitle = ggplot2::element_text(colour = "#555555"),
    axis.text.y = ggplot2::element_text(colour = "#222222"),
    plot.title.position = "plot",
    plot.margin = ggplot2::margin(10, 12, 10, 10)
  )

message("Exporting two figures as vector PDF and 300 dpi PNG.")
save_plot_checked(
  figure_1_plot,
  output_paths$figure_1_pdf,
  output_paths$figure_1_png,
  width = 7.2,
  height = 4.6
)
save_plot_checked(
  figure_2_plot,
  output_paths$figure_2_pdf,
  output_paths$figure_2_png,
  width = 7.2,
  height = 7.4
)

table_1_docx <- table_1 |>
  dplyr::rename(Item = item, Value = value, Note = note)
table_2_docx <- table_2 |>
  dplyr::select(
    Model = model,
    Predictor = predictor,
    Estimate = estimate,
    `Standard error` = standard_error,
    `95% CI` = confidence_interval_95,
    `p-value` = p_value
  )
table_3_docx <- table_3 |>
  dplyr::select(
    Comparison = comparison,
    Predictor = predictor,
    Estimate = estimate,
    `Standard error` = standard_error,
    `95% CI` = confidence_interval_95,
    `p-value` = p_value
  )

ft_1 <- make_booktabs(
  table_1_docx,
  widths = c(Item = 2.45, Value = 1.25, Note = 3.5),
  left_columns = c("Item", "Note"),
  right_columns = "Value"
)
ft_2 <- make_booktabs(
  table_2_docx,
  widths = c(
    Model = 1.20,
    Predictor = 2.40,
    Estimate = 0.65,
    `Standard error` = 0.75,
    `95% CI` = 1.50,
    `p-value` = 0.70
  ),
  left_columns = c("Model", "Predictor"),
  right_columns = c("Estimate", "Standard error", "95% CI", "p-value")
)
ft_3 <- make_booktabs(
  table_3_docx,
  widths = c(
    Comparison = 2.15,
    Predictor = 1.80,
    Estimate = 0.72,
    `Standard error` = 0.78,
    `95% CI` = 1.15,
    `p-value` = 0.60
  ),
  left_columns = c("Comparison", "Predictor"),
  right_columns = c("Estimate", "Standard error", "95% CI", "p-value")
)

table_1_note <- paste0(
  "The ICC is the arithmetic mean and observed range across five ",
  "plausible-value-specific weighted null models with education-system fixed ",
  "effects, a school random intercept, and the equal-system-contribution ",
  "candidate weight. Rubin's rules were not applied to the ICC. ",
  variance_limitation, " ", association_limitation, " ",
  round4_warning_note
)

table_2_note <- paste0(
  "Estimates are pooled across five plausible-value-specific models using ",
  "Rubin-style fixed-effect combination. Both models include ",
  "education-system fixed effects and the equal-system-contribution candidate ",
  "weight; the main multilevel model additionally includes a school random ",
  "intercept. Language-at-home rows are adjustment variables, not focal ",
  "research-question estimates. Intercepts and system coefficients are ",
  "omitted. ", variance_limitation, " ", association_limitation, " ",
  round4_warning_note
)

table_3_note <- paste0(
  "Each estimate is pooled across five same-index plausible-value difference ",
  "models with education-system fixed effects, a school random intercept, and ",
  "the equal-system-contribution candidate weight. Positive estimates indicate ",
  "a stronger association in Data and Probability; negative estimates indicate ",
  "a weaker association. A 95% confidence interval containing zero is not ",
  "clear evidence of a domain difference. ", variance_limitation, " ",
  association_limitation, " ", round4_warning_note
)

docx_success <- TRUE
docx_error <- NA_character_

message("Creating the Word document with three academic tables.")
tryCatch(
  {
    doc <- officer::read_docx()
    section_properties <- officer::prop_section(
      page_size = officer::page_size(orient = "portrait"),
      page_margins = officer::page_mar(
        top = 0.65,
        bottom = 0.65,
        left = 0.65,
        right = 0.65,
        header = 0.3,
        footer = 0.3,
        gutter = 0
      )
    )
    doc <- officer::body_set_default_section(doc, section_properties)
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          "TIMSS 2023 Grade 8 Dissertation Results Tables",
          officer::fp_text(
            font.family = "Arial", font.size = 14, bold = TRUE
          )
        )
      )
    )
    doc <- add_table_to_document(
      doc,
      1,
      "Analysis Sample and Multilevel Structure",
      ft_1,
      table_1_note
    )
    doc <- officer::body_add_break(doc)
    doc <- add_table_to_document(
      doc,
      2,
      paste0(
        "Associations of Home Educational Resources and ICT Access with ",
        "Data and Probability Achievement"
      ),
      ft_2,
      table_2_note
    )
    doc <- officer::body_add_break(doc)
    doc <- add_table_to_document(
      doc,
      3,
      paste0(
        "Differences in Resource-Achievement Associations Between Data and ",
        "Probability and Other Mathematics Domains"
      ),
      ft_3,
      table_3_note
    )
    print(doc, target = output_paths$tables_docx)
    if (!file.exists(output_paths$tables_docx) ||
        is.na(file.info(output_paths$tables_docx)$size) ||
        file.info(output_paths$tables_docx)$size <= 0) {
      stop("The DOCX file was not created or is empty.")
    }
  },
  error = function(e) {
    docx_success <<- FALSE
    docx_error <<- conditionMessage(e)
  }
)

if (!docx_success) {
  warning(
    paste0(
      "The CSV tables and figures were created, but the DOCX failed: ",
      docx_error
    ),
    call. = FALSE
  )
}

required_csv_outputs <- c(
  output_paths$table_1,
  output_paths$table_2,
  output_paths$table_3,
  output_paths$figure_1_data,
  output_paths$figure_2_data,
  output_paths$figure_manifest,
  output_paths$rq_evidence
)
required_figure_outputs <- c(
  output_paths$figure_1_pdf,
  output_paths$figure_1_png,
  output_paths$figure_2_pdf,
  output_paths$figure_2_png
)

missing_critical_outputs <- c(
  required_csv_outputs[!file.exists(required_csv_outputs)],
  required_figure_outputs[!file.exists(required_figure_outputs)]
)
empty_critical_outputs <- c(
  required_csv_outputs[
    file.exists(required_csv_outputs) & file.info(required_csv_outputs)$size <= 0
  ],
  required_figure_outputs[
    file.exists(required_figure_outputs) &
      file.info(required_figure_outputs)$size <= 0
  ]
)

if (length(missing_critical_outputs) > 0L ||
    length(empty_critical_outputs) > 0L) {
  stop_with(
    "One or more critical Round 5 outputs are missing or empty: ",
    paste(
      vapply(
        unique(c(missing_critical_outputs, empty_critical_outputs)),
        relative_path,
        character(1)
      ),
      collapse = " | "
    ),
    "."
  )
}

validation_checks <- tibble::tibble(
  check = c(
    "Required Round 4 inputs",
    "Round 4 CSV schemas",
    "Expected model IDs and terms",
    "Finite pooled estimates and ordered confidence intervals",
    "Four required domain outcomes",
    "Figure 1 focal rows",
    "Figure 2 focal rows",
    "System fixed effects excluded from plots",
    "Three public-safe CSV tables",
    "Word tables document",
    "Four PDF/PNG figure files",
    "Two plot-data CSV files",
    "Figure manifest",
    "RQ evidence summary"
  ),
  status = c(
    rep("PASS", 9),
    ifelse(docx_success, "PASS", "WARNING"),
    rep("PASS", 4)
  ),
  details = c(
    "Seven aggregate Round 4 inputs read successfully.",
    "All required columns and numeric types were present.",
    "Seven model families and the documented retained terms were verified.",
    "All pooled estimates, standard errors, confidence limits, and p-values were finite.",
    "Number, Algebra, Geometry and Measurement, and other-domain mean verified.",
    paste0(nrow(figure_1_data), " focal rows."),
    paste0(nrow(figure_2_data), " focal rows."),
    "No factor(system_id) term entered either plot dataset.",
    "Table 1, Table 2, and Table 3 CSV files created.",
    ifelse(
      docx_success,
      "TIMSS_dissertation_results_tables.docx created.",
      paste0("DOCX creation warning: ", docx_error)
    ),
    "Two vector PDFs and two 300 dpi PNGs created.",
    "Figure 1 and Figure 2 aggregate plot-data CSV files created.",
    paste0(nrow(figure_manifest), " manifest rows created."),
    paste0(nrow(rq_evidence_summary), " structured evidence rows created.")
  )
)

round5_warnings <- character(0)
if (analysis_status == "PASS WITH WARNINGS") {
  round5_warnings <- c(round5_warnings, round4_warning_note)
}
if (!docx_success) {
  round5_warnings <- c(
    round5_warnings,
    paste0("DOCX creation failed: ", docx_error)
  )
}

final_status <- if (length(round5_warnings) > 0L) {
  "PASS WITH WARNINGS"
} else {
  "PASS"
}

message("")
message("Round 5 tables and figures completed.")
message("Source systems: ", source_systems_n)
message("Achievement-model systems: ", analysis_systems_n)
message("Excluded systems: ", paste(excluded_systems, collapse = " | "))
message("Students represented: ", format_count(analysis_students_n))
message("Schools represented: ", format_count(analysis_schools_n))
message("Round 4 model fits summarised: ", model_fit_count)
message("Round 5 final status: ", final_status)
message("Validation checks:")
print(validation_checks, n = nrow(validation_checks))
if (length(round5_warnings) > 0L) {
  message("Recorded warning(s):")
  for (item in round5_warnings) {
    message("  - ", item)
  }
}
message("Output files:")
for (path in unname(unlist(output_paths, use.names = FALSE))) {
  if (file.exists(path)) {
    message("  - ", relative_path(path))
  }
}

invisible(list(
  status = final_status,
  validation = validation_checks,
  tables = list(table_1 = table_1, table_2 = table_2, table_3 = table_3),
  plot_data = list(figure_1 = figure_1_data, figure_2 = figure_2_data),
  figure_manifest = figure_manifest,
  rq_evidence_summary = rq_evidence_summary
))
