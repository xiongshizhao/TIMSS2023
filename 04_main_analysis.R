# TIMSS 2023 Grade 8: main analysis
#
# Scope:
#   - reads only the local processed RDS produced by R/03_prepare_analysis_data.R;
#   - fits the locked first-pass, null multilevel, main multilevel, and four
#     domain-difference model families;
#   - fits every model separately for all five plausible values;
#   - pools fixed-effect estimates after fitting, without averaging PV scores;
#   - uses the equal-system-contribution candidate weight created in Round 3;
#   - retains education-system fixed effects and a school random intercept;
#   - writes only aggregate, public-safe model outputs;
#   - does not produce figures, formatted dissertation tables, additional
#     models, system-specific results, or causal interpretations.

local({
  options(stringsAsFactors = FALSE)
  script_version <- "04_main_analysis_v1.1_2026-09-04"

  required_packages <- c("here", "readr", "lme4")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0L) {
    stop(
      paste0(
        "Missing package(s): ", paste(missing_packages, collapse = ", "),
        ". Run this once in the R Console: install.packages(c(",
        paste(sprintf("\"%s\"", missing_packages), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(lme4)
  })

  project_root <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)
  rproj_files <- list.files(
    project_root,
    pattern = "\\.Rproj$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(rproj_files) == 0L) {
    stop(
      paste(
        "No .Rproj file was found at the project root.",
        "Open the TIMSS_2023 RStudio Project and run the script again."
      ),
      call. = FALSE
    )
  }

  message("Project root verified: ", basename(project_root))
  message("Script version: ", script_version)
  message("Round 4 runs the locked main analysis only.")
  message("No raw RData file will be read.")

  relative_paths <- list(
    analysis_data = "data_processed/local_only/timss_g8_analysis_data.rds",
    result_dir = "outputs/model_results",
    local_output_dir = "outputs/local_only",
    archive_dir = "outputs/local_only/analysis_archive"
  )
  paths <- lapply(relative_paths, here::here)

  output_paths <- list(
    model_objects = here::here(
      "outputs", "local_only", "main_analysis_models.rds"
    ),
    first_pass = here::here(
      "outputs", "model_results", "first_pass_dp_estimates.csv"
    ),
    main_multilevel = here::here(
      "outputs", "model_results", "main_multilevel_dp_estimates.csv"
    ),
    domain_difference = here::here(
      "outputs", "model_results", "domain_difference_estimates.csv"
    ),
    icc_summary = here::here(
      "outputs", "model_results", "icc_summary.csv"
    ),
    model_diagnostics = here::here(
      "outputs", "model_results", "model_diagnostics.csv"
    ),
    model_sample_summary = here::here(
      "outputs", "model_results", "model_sample_summary.csv"
    ),
    method_notes = here::here(
      "outputs", "model_results", "model_method_notes.txt"
    ),
    session_info = here::here(
      "outputs", "local_only", "session_info_analysis.txt"
    )
  )

  if (!file.exists(paths$analysis_data)) {
    stop(
      "Processed RDS is missing: ", relative_paths$analysis_data,
      call. = FALSE
    )
  }

  for (directory_name in c("result_dir", "local_output_dir", "archive_dir")) {
    directory <- paths[[directory_name]]
    if (!dir.exists(directory)) {
      created <- dir.create(directory, recursive = TRUE, showWarnings = FALSE)
      if (!created || !dir.exists(directory)) {
        stop(
          "Could not create output directory: ",
          relative_paths[[directory_name]],
          call. = FALSE
        )
      }
    }
  }

  check_directory_writable <- function(directory, label) {
    test_file <- tempfile(pattern = "write_test_", tmpdir = directory)
    writable <- tryCatch({
      writeLines("test", test_file, useBytes = TRUE)
      file.exists(test_file)
    }, error = function(e) FALSE)
    if (file.exists(test_file)) {
      unlink(test_file)
    }
    if (!writable) {
      stop("Output directory is not writable: ", label, call. = FALSE)
    }
    invisible(TRUE)
  }

  check_directory_writable(paths$result_dir, relative_paths$result_dir)
  check_directory_writable(paths$local_output_dir, relative_paths$local_output_dir)
  check_directory_writable(paths$archive_dir, relative_paths$archive_dir)

  to_relative_path <- function(path) {
    normalized <- normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    )
    prefix <- paste0(project_root, "/")
    if (startsWith(normalized, prefix)) {
      substring(normalized, nchar(prefix) + 1L)
    } else {
      basename(path)
    }
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  existing_outputs <- output_paths[vapply(
    output_paths,
    file.exists,
    logical(1)
  )]
  if (length(existing_outputs) > 0L) {
    message("Archiving ", length(existing_outputs), " existing Round 4 output(s).")
    for (old_path in unname(existing_outputs)) {
      archive_path <- file.path(
        paths$archive_dir,
        paste0(timestamp, "_", basename(old_path))
      )
      moved <- file.rename(old_path, archive_path)
      if (!moved) {
        stop(
          "Could not archive existing output: ", to_relative_path(old_path),
          call. = FALSE
        )
      }
    }
  }

  collapse_text <- function(x, empty = "None") {
    x <- unique(trimws(as.character(x)))
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0L) empty else paste(x, collapse = " | ")
  }

  format_number <- function(x, digits = 6L) {
    if (length(x) == 0L || is.na(x) || !is.finite(x)) {
      return("NA")
    }
    formatC(x, format = "fg", digits = digits, flag = "#")
  }

  write_csv_checked <- function(x, path) {
    if (!is.data.frame(x)) {
      stop("CSV output is not a data frame: ", basename(path), call. = FALSE)
    }
    invalid_columns <- which(vapply(
      x,
      function(column) is.list(column) || is.matrix(column),
      logical(1)
    ))
    if (length(invalid_columns) > 0L) {
      stop(
        "CSV output contains list or matrix columns: ", basename(path),
        call. = FALSE
      )
    }

    character_values <- unlist(
      lapply(x[vapply(x, is.character, logical(1))], identity),
      use.names = FALSE
    )
    character_values <- character_values[!is.na(character_values)]
    absolute_path_pattern <- "(^|[[:space:]])(/Users/|/home/|/workspace/|[A-Za-z]:[/\\\\])"
    if (any(grepl(absolute_path_pattern, character_values, perl = TRUE))) {
      stop(
        "Public output contains a local absolute path: ", basename(path),
        call. = FALSE
      )
    }

    tryCatch(
      readr::write_excel_csv(x, path, na = ""),
      error = function(e) {
        stop(
          "Could not write CSV output ", basename(path), ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (!file.exists(path) || file.info(path)$size <= 0L) {
      stop("CSV output was not generated: ", basename(path), call. = FALSE)
    }
    invisible(TRUE)
  }

  write_text_checked <- function(lines, path) {
    tryCatch(
      writeLines(enc2utf8(as.character(lines)), path, useBytes = TRUE),
      error = function(e) {
        stop(
          "Could not write text output ", basename(path), ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (!file.exists(path) || file.info(path)$size <= 0L) {
      stop("Text output was not generated: ", basename(path), call. = FALSE)
    }
    invisible(TRUE)
  }

  warning_log <- character()
  add_analysis_warning <- function(message_text) {
    warning_log <<- unique(c(warning_log, as.character(message_text)))
    message("WARNING: ", message_text)
    invisible(TRUE)
  }

  analysis_data <- tryCatch(
    readRDS(paths$analysis_data),
    error = function(e) {
      stop(
        "Could not read the processed RDS: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.data.frame(analysis_data)) {
    stop("The processed RDS does not contain a data frame.", call. = FALSE)
  }

  dp_pvs <- sprintf("BSMDAT%02d", 1:5)
  number_pvs <- sprintf("BSMNUM%02d", 1:5)
  algebra_pvs <- sprintf("BSMALG%02d", 1:5)
  geometry_pvs <- sprintf("BSMGEO%02d", 1:5)
  difference_pvs <- list(
    D_dp_minus_number = paste0("dp_minus_number_pv", 1:5),
    E_dp_minus_algebra = paste0("dp_minus_algebra_pv", 1:5),
    F_dp_minus_geometry = paste0("dp_minus_geometry_pv", 1:5),
    G_dp_minus_other_mean = paste0("dp_minus_other_mean_pv", 1:5)
  )
  difference_labels <- c(
    D_dp_minus_number = "Data and Probability minus Number",
    E_dp_minus_algebra = "Data and Probability minus Algebra",
    F_dp_minus_geometry = "Data and Probability minus Geometry and Measurement",
    G_dp_minus_other_mean = paste(
      "Data and Probability minus the mean of Number, Algebra,",
      "and Geometry and Measurement"
    )
  )

  required_columns <- unique(c(
    "system_id", "school_uid", "student_uid", "IDGRADER",
    dp_pvs, number_pvs, algebra_pvs, geometry_pvs,
    unlist(difference_pvs, use.names = FALSE),
    "BSBGHER", "ict_access", "language_at_home",
    "TOTWGT", "SENWGT", "analysis_weight_candidate",
    "JKZONE", "JKREP",
    "eligible_main_dp", "eligible_domain_comparison"
  ))
  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop(
      "The processed RDS is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(analysis_data) != 323920L) {
    stop(
      "Unexpected processed-data row count: ", nrow(analysis_data),
      "; expected 323920 from Round 3.",
      call. = FALSE
    )
  }
  if (length(unique(analysis_data$system_id)) != 47L) {
    stop(
      "Unexpected processed-data system count: ",
      length(unique(analysis_data$system_id)),
      "; expected 47 from Round 3.",
      call. = FALSE
    )
  }

  if (!is.logical(analysis_data$eligible_main_dp) ||
      !is.logical(analysis_data$eligible_domain_comparison)) {
    stop("Eligibility flags must be logical variables.", call. = FALSE)
  }
  if (anyNA(analysis_data$eligible_main_dp) ||
      anyNA(analysis_data$eligible_domain_comparison)) {
    stop("Eligibility flags contain missing values.", call. = FALSE)
  }

  numeric_required <- c(
    dp_pvs, number_pvs, algebra_pvs, geometry_pvs,
    unlist(difference_pvs, use.names = FALSE),
    "BSBGHER", "TOTWGT", "SENWGT", "analysis_weight_candidate",
    "JKZONE", "JKREP"
  )
  nonnumeric_required <- numeric_required[
    !vapply(analysis_data[numeric_required], is.numeric, logical(1))
  ]
  if (length(nonnumeric_required) > 0L) {
    stop(
      "Required numeric variable(s) are not numeric: ",
      paste(nonnumeric_required, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyNA(analysis_data$system_id) ||
      anyNA(analysis_data$school_uid) ||
      anyNA(analysis_data$student_uid)) {
    stop(
      "system_id, school_uid, or student_uid contains missing values.",
      call. = FALSE
    )
  }
  if (anyDuplicated(analysis_data$student_uid)) {
    stop("student_uid is not unique in the processed RDS.", call. = FALSE)
  }

  expected_ict_levels <- c("No", "Yes")
  expected_language_levels <- c(
    "Always", "Almost always", "Sometimes", "Never"
  )
  if (!is.factor(analysis_data$ict_access)) {
    stop("ict_access is not a factor in the processed RDS.", call. = FALSE)
  }
  if (!is.factor(analysis_data$language_at_home)) {
    stop("language_at_home is not a factor in the processed RDS.", call. = FALSE)
  }
  if (!identical(levels(analysis_data$ict_access), expected_ict_levels)) {
    stop(
      "Unexpected ict_access levels: ",
      paste(levels(analysis_data$ict_access), collapse = " | "),
      call. = FALSE
    )
  }
  if (!identical(
    levels(analysis_data$language_at_home),
    expected_language_levels
  )) {
    stop(
      "Unexpected language_at_home levels: ",
      paste(levels(analysis_data$language_at_home), collapse = " | "),
      call. = FALSE
    )
  }

  source_weight_names <- c(
    "TOTWGT", "SENWGT", "analysis_weight_candidate"
  )
  for (weight_name in source_weight_names) {
    values <- analysis_data[[weight_name]]
    if (anyNA(values) || any(!is.finite(values)) || any(values <= 0)) {
      stop(
        "Invalid values were found in ", weight_name, ".",
        call. = FALSE
      )
    }
  }
  if (anyNA(analysis_data$JKZONE) || anyNA(analysis_data$JKREP)) {
    stop("JKZONE or JKREP contains missing values.", call. = FALSE)
  }
  if (!all(analysis_data$JKREP %in% c(0, 1))) {
    stop("JKREP contains values other than 0 and 1.", call. = FALSE)
  }

  expected_candidate <- ave(
    analysis_data$SENWGT,
    analysis_data$system_id,
    FUN = function(weight) weight * 500 / sum(weight)
  )
  candidate_difference <- abs(
    analysis_data$analysis_weight_candidate - expected_candidate
  )
  maximum_candidate_difference <- max(candidate_difference)
  if (!is.finite(maximum_candidate_difference) ||
      maximum_candidate_difference > 1e-10) {
    stop(
      "analysis_weight_candidate does not reproduce the Round 3 formula.",
      call. = FALSE
    )
  }

  weight_system_summary <- do.call(
    rbind,
    lapply(sort(unique(analysis_data$system_id)), function(system_code) {
      rows <- analysis_data$system_id == system_code
      senate_to_total <- analysis_data$SENWGT[rows] /
        analysis_data$TOTWGT[rows]
      candidate_to_senate <- analysis_data$analysis_weight_candidate[rows] /
        analysis_data$SENWGT[rows]
      data.frame(
        system_id = system_code,
        student_n = sum(rows),
        TOTWGT_sum = sum(analysis_data$TOTWGT[rows]),
        SENWGT_sum = sum(analysis_data$SENWGT[rows]),
        candidate_weight_sum = sum(
          analysis_data$analysis_weight_candidate[rows]
        ),
        senate_to_total_ratio_range = diff(range(senate_to_total)),
        candidate_to_senate_ratio_range = diff(range(candidate_to_senate)),
        stringsAsFactors = FALSE
      )
    })
  )
  row.names(weight_system_summary) <- NULL

  if (max(weight_system_summary$candidate_to_senate_ratio_range) > 1e-10) {
    stop(
      "Candidate weights are not proportional to SENWGT within systems.",
      call. = FALSE
    )
  }
  main_rows <- analysis_data$eligible_main_dp
  domain_rows <- analysis_data$eligible_domain_comparison
  if (!any(main_rows)) {
    stop("No observations are eligible for the D&P main analysis.", call. = FALSE)
  }
  if (!any(domain_rows)) {
    stop("No observations are eligible for domain comparisons.", call. = FALSE)
  }

  main_model_columns <- unique(c(
    "student_uid", "system_id", "school_uid", dp_pvs,
    "BSBGHER", "ict_access", "language_at_home",
    "analysis_weight_candidate"
  ))
  domain_model_columns <- unique(c(
    "student_uid", "system_id", "school_uid",
    unlist(difference_pvs, use.names = FALSE),
    "BSBGHER", "ict_access", "language_at_home",
    "analysis_weight_candidate"
  ))
  main_data <- analysis_data[main_rows, main_model_columns, drop = FALSE]
  domain_data <- analysis_data[
    domain_rows,
    domain_model_columns,
    drop = FALSE
  ]

  if (!identical(
    sort(main_data$student_uid),
    sort(domain_data$student_uid)
  )) {
    stop(
      paste(
        "eligible_main_dp and eligible_domain_comparison do not identify",
        "the same students; comparable models would use different samples."
      ),
      call. = FALSE
    )
  }

  expected_excluded_systems <- c("BRA", "CIV", "PSE")
  source_systems <- sort(unique(analysis_data$system_id))
  main_systems <- sort(unique(main_data$system_id))
  domain_systems <- sort(unique(domain_data$system_id))
  excluded_systems <- setdiff(source_systems, main_systems)
  if (length(main_systems) != 44L ||
      length(domain_systems) != 44L ||
      !identical(main_systems, domain_systems) ||
      !identical(sort(excluded_systems), sort(expected_excluded_systems))) {
    stop(
      paste0(
        "Unexpected achievement-model system coverage. Main systems=",
        length(main_systems), "; domain systems=", length(domain_systems),
        "; excluded=", collapse_text(excluded_systems), "."
      ),
      call. = FALSE
    )
  }

  if (nrow(main_data) != 272315L || nrow(domain_data) != 272315L) {
    stop(
      paste0(
        "Unexpected eligible sample size. Main=", nrow(main_data),
        "; domain=", nrow(domain_data), "; expected=272315."
      ),
      call. = FALSE
    )
  }

  if (anyNA(main_data[dp_pvs])) {
    stop("D&P plausible values are incomplete in the main sample.", call. = FALSE)
  }
  if (anyNA(domain_data[unlist(difference_pvs, use.names = FALSE)])) {
    stop(
      "Domain-difference plausible values are incomplete in the comparison sample.",
      call. = FALSE
    )
  }

  analysis_variables <- c(
    "BSBGHER", "ict_access", "language_at_home",
    "analysis_weight_candidate", "system_id", "school_uid"
  )
  if (anyNA(main_data[analysis_variables])) {
    stop("The main complete-case sample still contains missing model values.", call. = FALSE)
  }
  if (anyNA(domain_data[analysis_variables])) {
    stop("The domain complete-case sample still contains missing model values.", call. = FALSE)
  }

  main_data$ict_access <- factor(
    as.character(main_data$ict_access),
    levels = expected_ict_levels
  )
  domain_data$ict_access <- factor(
    as.character(domain_data$ict_access),
    levels = expected_ict_levels
  )
  main_data$language_at_home <- factor(
    as.character(main_data$language_at_home),
    levels = expected_language_levels
  )
  domain_data$language_at_home <- factor(
    as.character(domain_data$language_at_home),
    levels = expected_language_levels
  )
  main_data$system_id <- factor(main_data$system_id, levels = main_systems)
  domain_data$system_id <- factor(domain_data$system_id, levels = domain_systems)
  main_data$school_uid <- factor(main_data$school_uid)
  domain_data$school_uid <- factor(domain_data$school_uid)

  if (length(unique(main_data$ict_access)) < 2L) {
    stop("ict_access has no valid variation in the main sample.", call. = FALSE)
  }
  if (length(unique(main_data$language_at_home)) < 2L) {
    stop(
      "language_at_home has no valid variation in the main sample.",
      call. = FALSE
    )
  }
  if (length(unique(main_data$BSBGHER)) < 2L) {
    stop("BSBGHER has no valid variation in the main sample.", call. = FALSE)
  }

  grade_values <- sort(unique(
    analysis_data$IDGRADER[!is.na(analysis_data$IDGRADER)]
  ))
  grade_missing_n <- sum(is.na(analysis_data$IDGRADER))
  if (length(grade_values) == 0L) {
    add_analysis_warning(
      "IDGRADER has no valid values; it remains excluded from all models."
    )
  }

  # lme4 does not normalize prior weights. A single constant rescaling is used
  # within the common analytic sample so that the mean model weight equals 1.
  # This does not alter relative student weights or coefficient estimates.
  model_weight_scale <- mean(main_data$analysis_weight_candidate)
  main_data$model_weight <- main_data$analysis_weight_candidate /
    model_weight_scale
  domain_data$model_weight <- domain_data$analysis_weight_candidate /
    model_weight_scale

  if (anyNA(main_data$model_weight) ||
      any(!is.finite(main_data$model_weight)) ||
      any(main_data$model_weight <= 0)) {
    stop("Invalid model weights were created for the main sample.", call. = FALSE)
  }
  if (anyNA(domain_data$model_weight) ||
      any(!is.finite(domain_data$model_weight)) ||
      any(domain_data$model_weight <= 0)) {
    stop(
      "Invalid model weights were created for the domain sample.",
      call. = FALSE
    )
  }
  if (max(abs(main_data$model_weight -
      main_data$analysis_weight_candidate / model_weight_scale)) > 1e-12) {
    stop("Model-weight rescaling failed validation.", call. = FALSE)
  }

  message("Processed source systems: ", length(source_systems))
  message("Achievement-model systems: ", length(main_systems))
  message("Excluded systems: ", paste(excluded_systems, collapse = " | "))
  message("Main students: ", nrow(main_data))
  message("Main schools: ", length(unique(main_data$school_uid)))
  message(
    "ict_access levels: ",
    paste(levels(main_data$ict_access), collapse = " | "),
    "; reference: ", levels(main_data$ict_access)[1]
  )
  message(
    "language_at_home levels: ",
    paste(levels(main_data$language_at_home), collapse = " | "),
    "; reference: ", levels(main_data$language_at_home)[1]
  )

  factor_category_summary <- function(data, variable_name, reference_category) {
    values <- data[[variable_name]]
    weight <- data$analysis_weight_candidate
    categories <- levels(values)
    result <- do.call(rbind, lapply(categories, function(category) {
      rows <- !is.na(values) & values == category
      data.frame(
        summary_type = "factor_category",
        model_id = "C_main_dp",
        sample_flag = "eligible_main_dp",
        outcome = "Data and Probability",
        systems_n = length(unique(data$system_id)),
        students_n = nrow(data),
        schools_n = length(unique(data$school_uid)),
        excluded_systems = paste(excluded_systems, collapse = " | "),
        system_id = NA_character_,
        included_in_achievement_models = NA,
        exclusion_reason = NA_character_,
        variable = variable_name,
        category = category,
        reference_category = reference_category,
        unweighted_n = sum(rows),
        unweighted_percentage = 100 * sum(rows) / sum(!is.na(values)),
        weighted_sum = sum(weight[rows]),
        weighted_percentage = 100 * sum(weight[rows]) /
          sum(weight[!is.na(values)]),
        stringsAsFactors = FALSE
      )
    }))
    row.names(result) <- NULL
    result
  }

  ict_category_summary <- factor_category_summary(
    main_data,
    "ict_access",
    "No"
  )
  language_category_summary <- factor_category_summary(
    main_data,
    "language_at_home",
    "Always"
  )

  message("Unweighted and weighted ict_access category counts:")
  print(ict_category_summary[, c(
    "category", "unweighted_n", "unweighted_percentage",
    "weighted_sum", "weighted_percentage"
  )], row.names = FALSE)
  message("Unweighted and weighted language_at_home category counts:")
  print(language_category_summary[, c(
    "category", "unweighted_n", "unweighted_percentage",
    "weighted_sum", "weighted_percentage"
  )], row.names = FALSE)

  expected_focal_terms <- c(
    "BSBGHER",
    "ict_accessYes",
    "language_at_homeAlmost always",
    "language_at_homeSometimes",
    "language_at_homeNever"
  )
  term_labels <- c(
    BSBGHER = "Home Educational Resources scale",
    ict_accessYes = "Own computer or tablet at home: Yes versus No",
    `language_at_homeAlmost always` = paste(
      "Language of test at home: Almost always versus Always"
    ),
    language_at_homeSometimes = paste(
      "Language of test at home: Sometimes versus Always"
    ),
    language_at_homeNever = paste(
      "Language of test at home: Never versus Always"
    )
  )
  term_roles <- c(
    BSBGHER = "primary_predictor",
    ict_accessYes = "primary_predictor",
    `language_at_homeAlmost always` = "control",
    language_at_homeSometimes = "control",
    language_at_homeNever = "control"
  )

  fixed_formula_text <- paste(
    "BSBGHER + ict_access + language_at_home +",
    "factor(system_id)"
  )
  null_formula_text <- "factor(system_id) + (1 | school_uid)"
  multilevel_formula_text <- paste(
    fixed_formula_text,
    "+ (1 | school_uid)"
  )

  create_formula <- function(outcome, right_hand_side) {
    # lme4 may add a weights binding to a formula environment. Use a fresh,
    # mutable environment with base R as its parent; baseenv() itself is locked.
    formula_environment <- new.env(parent = baseenv())
    formula_object <- stats::as.formula(
      paste(outcome, "~", right_hand_side),
      env = formula_environment
    )
    formula_object
  }

  captured_fit <- function(expression) {
    captured_warnings <- character()
    captured_messages <- character()
    error_message <- ""
    value <- tryCatch(
      withCallingHandlers(
        expression,
        warning = function(w) {
          captured_warnings <<- c(
            captured_warnings,
            conditionMessage(w)
          )
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          captured_messages <<- c(
            captured_messages,
            conditionMessage(m)
          )
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      }
    )
    list(
      value = value,
      warnings = unique(captured_warnings),
      messages = unique(captured_messages),
      error = error_message
    )
  }

  compress_model_object <- function(model_object) {
    serialized <- serialize(model_object, connection = NULL, version = 3)
    compressed <- memCompress(serialized, type = "gzip")
    rm(serialized)
    compressed
  }

  empty_coefficient_table <- function() {
    data.frame(
      term = character(),
      estimate = numeric(),
      standard_error = numeric(),
      stringsAsFactors = FALSE
    )
  }

  diagnostic_row <- function(
      model_id, outcome_label, pv_number, sample_flag, data,
      fit_success, convergence_status, convergence_message,
      singular_fit, optimizer, elapsed_seconds, fixed_terms,
      missing_coefficients, school_variance, residual_variance, icc,
      captured_warnings, captured_messages, error_message) {
    data.frame(
      model_id = model_id,
      outcome = outcome_label,
      pv_number = as.integer(pv_number),
      sample_flag = sample_flag,
      eligible_rows_supplied = nrow(data),
      observations_used = if (fit_success) nrow(data) else NA_integer_,
      systems_used = length(unique(data$system_id)),
      schools_used = length(unique(data$school_uid)),
      excluded_systems = paste(excluded_systems, collapse = " | "),
      fit_success = fit_success,
      convergence_status = convergence_status,
      convergence_message = convergence_message,
      singular_fit = singular_fit,
      optimizer = optimizer,
      elapsed_seconds = elapsed_seconds,
      fixed_effect_terms_present = collapse_text(fixed_terms),
      missing_coefficients = collapse_text(missing_coefficients),
      school_variance = school_variance,
      residual_variance = residual_variance,
      icc = icc,
      captured_warnings = collapse_text(captured_warnings),
      captured_messages = collapse_text(captured_messages),
      error_message = if (nzchar(error_message)) error_message else "None",
      stringsAsFactors = FALSE
    )
  }

  fit_lm_one <- function(
      data, outcome, outcome_label, pv_number, model_id, sample_flag) {
    formula_object <- create_formula(outcome, fixed_formula_text)
    start_time <- proc.time()[["elapsed"]]
    captured <- captured_fit(
      stats::lm(
        formula = formula_object,
        data = data,
        weights = model_weight,
        na.action = stats::na.fail,
        model = TRUE,
        x = FALSE,
        y = FALSE,
        qr = TRUE
      )
    )
    elapsed <- proc.time()[["elapsed"]] - start_time

    if (is.null(captured$value)) {
      diagnostics <- diagnostic_row(
        model_id, outcome_label, pv_number, sample_flag, data,
        FALSE, "FAILED", captured$error,
        NA, "stats::lm weighted least squares", elapsed,
        character(), expected_focal_terms,
        NA_real_, NA_real_, NA_real_,
        captured$warnings, captured$messages, captured$error
      )
      return(list(
        success = FALSE,
        coefficients = empty_coefficient_table(),
        diagnostics = diagnostics,
        model_blob = raw()
      ))
    }

    fit <- captured$value
    coefficient_matrix <- summary(fit)$coefficients
    coefficient_table <- data.frame(
      term = row.names(coefficient_matrix),
      estimate = coefficient_matrix[, "Estimate"],
      standard_error = coefficient_matrix[, "Std. Error"],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    fixed_terms <- coefficient_table$term
    missing_coefficients <- setdiff(expected_focal_terms, fixed_terms)
    required_rows <- match(expected_focal_terms, coefficient_table$term)
    required_finite <- length(missing_coefficients) == 0L && all(
      is.finite(coefficient_table$estimate[required_rows]) &
        is.finite(coefficient_table$standard_error[required_rows]) &
        coefficient_table$standard_error[required_rows] > 0
    )
    fit_success <- isTRUE(required_finite) && stats::nobs(fit) == nrow(data)
    diagnostics <- diagnostic_row(
      model_id, outcome_label, pv_number, sample_flag, data,
      fit_success,
      if (fit_success) "CONVERGED" else "FAILED",
      if (fit_success) "None" else "Required coefficient or sample check failed",
      NA, "stats::lm weighted least squares", elapsed,
      fixed_terms, missing_coefficients,
      NA_real_, stats::sigma(fit)^2, NA_real_,
      captured$warnings, captured$messages, ""
    )
    model_blob <- compress_model_object(fit)
    rm(fit)
    gc(verbose = FALSE)

    list(
      success = fit_success,
      coefficients = coefficient_table,
      diagnostics = diagnostics,
      model_blob = model_blob
    )
  }

  fit_lmer_one <- function(
      data, outcome, outcome_label, pv_number, model_id, sample_flag,
      null_model = FALSE) {
    right_hand_side <- if (null_model) {
      null_formula_text
    } else {
      multilevel_formula_text
    }
    expected_terms <- if (null_model) character() else expected_focal_terms
    formula_object <- create_formula(outcome, right_hand_side)
    control_object <- lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 200000L),
      restart_edge = TRUE,
      calc.derivs = TRUE
    )

    start_time <- proc.time()[["elapsed"]]
    captured <- captured_fit(
      lme4::lmer(
        formula = formula_object,
        data = data,
        weights = model_weight,
        REML = FALSE,
        na.action = stats::na.fail,
        control = control_object
      )
    )
    elapsed <- proc.time()[["elapsed"]] - start_time

    if (is.null(captured$value)) {
      diagnostics <- diagnostic_row(
        model_id, outcome_label, pv_number, sample_flag, data,
        FALSE, "FAILED", captured$error,
        NA, "bobyqa; maxfun=200000", elapsed,
        character(), expected_terms,
        NA_real_, NA_real_, NA_real_,
        captured$warnings, captured$messages, captured$error
      )
      return(list(
        success = FALSE,
        coefficients = empty_coefficient_table(),
        diagnostics = diagnostics,
        model_blob = raw()
      ))
    }

    fit <- captured$value
    fixed_estimates <- lme4::fixef(fit)
    fixed_se <- sqrt(diag(stats::vcov(fit)))
    coefficient_table <- data.frame(
      term = names(fixed_estimates),
      estimate = as.numeric(fixed_estimates),
      standard_error = as.numeric(fixed_se[names(fixed_estimates)]),
      stringsAsFactors = FALSE
    )
    fixed_terms <- coefficient_table$term
    missing_coefficients <- setdiff(expected_terms, fixed_terms)

    optimizer <- as.character(fit@optinfo$optimizer)
    optimizer_code <- fit@optinfo$conv$opt
    optimizer_ok <- is.null(optimizer_code) || all(optimizer_code == 0)
    lme4_messages <- unlist(
      fit@optinfo$conv$lme4$messages,
      use.names = FALSE
    )
    lme4_messages <- lme4_messages[
      !is.na(lme4_messages) & nzchar(lme4_messages)
    ]
    convergence_ok <- optimizer_ok && length(lme4_messages) == 0L

    required_finite <- TRUE
    if (length(expected_terms) > 0L) {
      required_rows <- match(expected_terms, coefficient_table$term)
      required_finite <- length(missing_coefficients) == 0L && all(
        is.finite(coefficient_table$estimate[required_rows]) &
          is.finite(coefficient_table$standard_error[required_rows]) &
          coefficient_table$standard_error[required_rows] > 0
      )
    }

    variance_table <- as.data.frame(lme4::VarCorr(fit))
    school_rows <- variance_table$grp == "school_uid" &
      is.na(variance_table$var2)
    residual_rows <- variance_table$grp == "Residual"
    school_variance <- if (any(school_rows)) {
      variance_table$vcov[which(school_rows)[1]]
    } else {
      NA_real_
    }
    residual_variance <- if (any(residual_rows)) {
      variance_table$vcov[which(residual_rows)[1]]
    } else {
      NA_real_
    }
    icc <- if (
      is.finite(school_variance) && is.finite(residual_variance) &&
        school_variance + residual_variance > 0
    ) {
      school_variance / (school_variance + residual_variance)
    } else {
      NA_real_
    }
    singular_fit <- lme4::isSingular(fit, tol = 1e-4)
    fit_success <- convergence_ok && isTRUE(required_finite) &&
      stats::nobs(fit) == nrow(data) &&
      is.finite(school_variance) && is.finite(residual_variance)

    convergence_message <- collapse_text(c(
      if (!optimizer_ok) {
        paste0("Optimizer code: ", collapse_text(optimizer_code))
      } else {
        character()
      },
      lme4_messages
    ))
    diagnostics <- diagnostic_row(
      model_id, outcome_label, pv_number, sample_flag, data,
      fit_success,
      if (fit_success) "CONVERGED" else "FAILED",
      convergence_message,
      singular_fit, collapse_text(optimizer), elapsed,
      fixed_terms, missing_coefficients,
      school_variance, residual_variance, icc,
      captured$warnings, captured$messages, ""
    )
    model_blob <- compress_model_object(fit)
    rm(fit)
    gc(verbose = FALSE)

    list(
      success = fit_success,
      coefficients = coefficient_table,
      diagnostics = diagnostics,
      model_blob = model_blob
    )
  }

  pooled_result_prototype <- function() {
    data.frame(
      model_id = character(),
      outcome = character(),
      systems_n = integer(),
      students_n = integer(),
      schools_n = integer(),
      excluded_systems = character(),
      weight_variable = character(),
      term = character(),
      term_label = character(),
      term_role = character(),
      n_pv = integer(),
      pool_status = character(),
      pooled_estimate = numeric(),
      pooled_standard_error = numeric(),
      degrees_of_freedom = numeric(),
      confidence_level = numeric(),
      confidence_interval_lower = numeric(),
      confidence_interval_upper = numeric(),
      p_value = numeric(),
      within_pv_variance = numeric(),
      between_pv_variance = numeric(),
      total_variance = numeric(),
      between_pv_fraction_of_total_variance = numeric(),
      pv1_estimate = numeric(),
      pv1_standard_error = numeric(),
      pv2_estimate = numeric(),
      pv2_standard_error = numeric(),
      pv3_estimate = numeric(),
      pv3_standard_error = numeric(),
      pv4_estimate = numeric(),
      pv4_standard_error = numeric(),
      pv5_estimate = numeric(),
      pv5_standard_error = numeric(),
      variance_method = character(),
      interpretation = character(),
      stringsAsFactors = FALSE
    )
  }

  pool_five_models <- function(
      records, model_id, outcome_label, interpretation_type) {
    output_rows <- list()
    for (term in expected_focal_terms) {
      estimates <- rep(NA_real_, 5L)
      standard_errors <- rep(NA_real_, 5L)
      for (pv_number in 1:5) {
        record <- records[[pv_number]]
        if (isTRUE(record$success)) {
          coefficient_row <- record$coefficients[
            record$coefficients$term == term,
            ,
            drop = FALSE
          ]
          if (nrow(coefficient_row) == 1L) {
            estimates[pv_number] <- coefficient_row$estimate
            standard_errors[pv_number] <- coefficient_row$standard_error
          }
        }
      }

      complete <- all(is.finite(estimates)) &&
        all(is.finite(standard_errors)) &&
        all(standard_errors > 0)
      if (complete) {
        m <- 5L
        q_bar <- mean(estimates)
        u_bar <- mean(standard_errors^2)
        between <- stats::var(estimates)
        between_component <- (1 + 1 / m) * between
        total <- u_bar + between_component
        pooled_se <- sqrt(total)
        if (between_component <= .Machine$double.eps) {
          degrees_freedom <- Inf
        } else {
          degrees_freedom <- (m - 1) *
            (1 + u_bar / between_component)^2
        }
        critical_value <- if (is.finite(degrees_freedom)) {
          stats::qt(0.975, df = degrees_freedom)
        } else {
          stats::qnorm(0.975)
        }
        test_statistic <- q_bar / pooled_se
        p_value <- if (is.finite(degrees_freedom)) {
          2 * stats::pt(-abs(test_statistic), df = degrees_freedom)
        } else {
          2 * stats::pnorm(-abs(test_statistic))
        }
        between_fraction <- if (total > 0) {
          between_component / total
        } else {
          NA_real_
        }
        pool_status <- "PASS"
      } else {
        q_bar <- NA_real_
        u_bar <- NA_real_
        between <- NA_real_
        total <- NA_real_
        pooled_se <- NA_real_
        degrees_freedom <- NA_real_
        critical_value <- NA_real_
        p_value <- NA_real_
        between_fraction <- NA_real_
        pool_status <- "FAIL"
      }

      interpretation <- if (term_roles[[term]] == "control") {
        "Control coefficient; not a focal research-question estimate."
      } else if (identical(interpretation_type, "difference")) {
        paste(
          "Positive values indicate a stronger association in D&P;",
          "negative values indicate a weaker association in D&P;",
          "an interval containing zero is not clear evidence of a difference."
        )
      } else if (identical(model_id, "A_first_pass_dp")) {
        paste(
          "Associational first-pass benchmark; the school random intercept",
          "is not included."
        )
      } else {
        "Adjusted associational estimate from the primary D&P multilevel model."
      }

      output_rows[[length(output_rows) + 1L]] <- data.frame(
        model_id = model_id,
        outcome = outcome_label,
        systems_n = length(main_systems),
        students_n = nrow(main_data),
        schools_n = length(unique(main_data$school_uid)),
        excluded_systems = paste(excluded_systems, collapse = " | "),
        weight_variable = "analysis_weight_candidate",
        term = term,
        term_label = unname(term_labels[[term]]),
        term_role = unname(term_roles[[term]]),
        n_pv = sum(is.finite(estimates) & is.finite(standard_errors)),
        pool_status = pool_status,
        pooled_estimate = q_bar,
        pooled_standard_error = pooled_se,
        degrees_of_freedom = degrees_freedom,
        confidence_level = 0.95,
        confidence_interval_lower = q_bar - critical_value * pooled_se,
        confidence_interval_upper = q_bar + critical_value * pooled_se,
        p_value = p_value,
        within_pv_variance = u_bar,
        between_pv_variance = between,
        total_variance = total,
        between_pv_fraction_of_total_variance = between_fraction,
        pv1_estimate = estimates[1],
        pv1_standard_error = standard_errors[1],
        pv2_estimate = estimates[2],
        pv2_standard_error = standard_errors[2],
        pv3_estimate = estimates[3],
        pv3_standard_error = standard_errors[3],
        pv4_estimate = estimates[4],
        pv4_standard_error = standard_errors[4],
        pv5_estimate = estimates[5],
        pv5_standard_error = standard_errors[5],
        variance_method = paste(
          "Rubin-style five-PV combination of within-PV model-based",
          "standard errors with large-sample Rubin degrees of freedom;",
          "not full TIMSS JK2 variance"
        ),
        interpretation = interpretation,
        stringsAsFactors = FALSE
      )
    }
    result <- if (length(output_rows) == 0L) {
      pooled_result_prototype()
    } else {
      do.call(rbind, output_rows)
    }
    row.names(result) <- NULL
    result
  }

  model_blobs <- list()
  first_pass_records <- vector("list", 5L)
  null_records <- vector("list", 5L)
  main_records <- vector("list", 5L)
  difference_records <- lapply(difference_pvs, function(x) {
    vector("list", 5L)
  })
  all_diagnostic_rows <- list()

  store_fit_record <- function(record, storage_name) {
    if (length(record$model_blob) > 0L) {
      model_blobs[[storage_name]] <<- record$model_blob
    }
    record$model_blob <- NULL
    all_diagnostic_rows[[length(all_diagnostic_rows) + 1L]] <<-
      record$diagnostics
    record
  }

  for (pv_number in 1:5) {
    message("Fitting Model A, D&P PV ", pv_number, "/5.")
    record <- fit_lm_one(
      main_data,
      dp_pvs[pv_number],
      "Data and Probability",
      pv_number,
      "A_first_pass_dp",
      "eligible_main_dp"
    )
    first_pass_records[[pv_number]] <- store_fit_record(
      record,
      paste0("A_first_pass_dp_pv", pv_number)
    )
  }

  for (pv_number in 1:5) {
    message("Fitting Model B, D&P null multilevel PV ", pv_number, "/5.")
    record <- fit_lmer_one(
      main_data,
      dp_pvs[pv_number],
      "Data and Probability",
      pv_number,
      "B_null_dp",
      "eligible_main_dp",
      null_model = TRUE
    )
    null_records[[pv_number]] <- store_fit_record(
      record,
      paste0("B_null_dp_pv", pv_number)
    )
  }

  for (pv_number in 1:5) {
    message("Fitting Model C, D&P main multilevel PV ", pv_number, "/5.")
    record <- fit_lmer_one(
      main_data,
      dp_pvs[pv_number],
      "Data and Probability",
      pv_number,
      "C_main_dp",
      "eligible_main_dp",
      null_model = FALSE
    )
    main_records[[pv_number]] <- store_fit_record(
      record,
      paste0("C_main_dp_pv", pv_number)
    )
  }

  for (difference_model_id in names(difference_pvs)) {
    outcome_label <- unname(difference_labels[[difference_model_id]])
    for (pv_number in 1:5) {
      message(
        "Fitting Model ", difference_model_id,
        ", PV ", pv_number, "/5."
      )
      record <- fit_lmer_one(
        domain_data,
        difference_pvs[[difference_model_id]][pv_number],
        outcome_label,
        pv_number,
        difference_model_id,
        "eligible_domain_comparison",
        null_model = FALSE
      )
      difference_records[[difference_model_id]][[pv_number]] <-
        store_fit_record(
          record,
          paste0(difference_model_id, "_pv", pv_number)
        )
    }
  }

  model_diagnostics <- do.call(rbind, all_diagnostic_rows)
  row.names(model_diagnostics) <- NULL

  first_pass_estimates <- pool_five_models(
    first_pass_records,
    "A_first_pass_dp",
    "Data and Probability",
    "association"
  )
  main_multilevel_estimates <- pool_five_models(
    main_records,
    "C_main_dp",
    "Data and Probability",
    "association"
  )
  difference_estimate_parts <- lapply(
    names(difference_records),
    function(model_id) {
      pool_five_models(
        difference_records[[model_id]],
        model_id,
        unname(difference_labels[[model_id]]),
        "difference"
      )
    }
  )
  domain_difference_estimates <- do.call(
    rbind,
    difference_estimate_parts
  )
  row.names(domain_difference_estimates) <- NULL

  null_diagnostics <- model_diagnostics[
    model_diagnostics$model_id == "B_null_dp",
    ,
    drop = FALSE
  ]
  icc_pv_rows <- data.frame(
    model_id = "B_null_dp",
    outcome = "Data and Probability",
    summary_level = "pv_specific",
    pv_number = null_diagnostics$pv_number,
    school_variance = null_diagnostics$school_variance,
    residual_variance = null_diagnostics$residual_variance,
    icc = null_diagnostics$icc,
    icc_mean = NA_real_,
    icc_minimum = NA_real_,
    icc_maximum = NA_real_,
    systems_n = null_diagnostics$systems_used,
    students_n = null_diagnostics$observations_used,
    schools_n = null_diagnostics$schools_used,
    excluded_systems = paste(excluded_systems, collapse = " | "),
    method_note = paste(
      "PV-specific variance components from the weighted null model;",
      "model-based, not full TIMSS JK2 variance."
    ),
    stringsAsFactors = FALSE
  )
  valid_icc <- null_diagnostics$icc[
    null_diagnostics$fit_success & is.finite(null_diagnostics$icc)
  ]
  if (length(valid_icc) == 5L) {
    icc_summary_row <- data.frame(
      model_id = "B_null_dp",
      outcome = "Data and Probability",
      summary_level = "five_pv_summary",
      pv_number = NA_integer_,
      school_variance = mean(null_diagnostics$school_variance),
      residual_variance = mean(null_diagnostics$residual_variance),
      icc = NA_real_,
      icc_mean = mean(valid_icc),
      icc_minimum = min(valid_icc),
      icc_maximum = max(valid_icc),
      systems_n = length(main_systems),
      students_n = nrow(main_data),
      schools_n = length(unique(main_data$school_uid)),
      excluded_systems = paste(excluded_systems, collapse = " | "),
      method_note = paste(
        "Arithmetic mean and observed range across five PV-specific ICCs;",
        "Rubin's rules were not applied to the derived ICC."
      ),
      stringsAsFactors = FALSE
    )
  } else {
    icc_summary_row <- data.frame(
      model_id = "B_null_dp",
      outcome = "Data and Probability",
      summary_level = "five_pv_summary",
      pv_number = NA_integer_,
      school_variance = NA_real_,
      residual_variance = NA_real_,
      icc = NA_real_,
      icc_mean = NA_real_,
      icc_minimum = NA_real_,
      icc_maximum = NA_real_,
      systems_n = length(main_systems),
      students_n = nrow(main_data),
      schools_n = length(unique(main_data$school_uid)),
      excluded_systems = paste(excluded_systems, collapse = " | "),
      method_note = "FAIL: fewer than five valid PV-specific ICCs.",
      stringsAsFactors = FALSE
    )
  }
  icc_summary <- rbind(icc_pv_rows, icc_summary_row)
  row.names(icc_summary) <- NULL

  model_family_definitions <- data.frame(
    summary_type = "model_sample",
    model_id = c(
      "A_first_pass_dp", "B_null_dp", "C_main_dp",
      names(difference_pvs)
    ),
    sample_flag = c(
      rep("eligible_main_dp", 3L),
      rep("eligible_domain_comparison", 4L)
    ),
    outcome = c(
      rep("Data and Probability", 3L),
      unname(difference_labels[names(difference_pvs)])
    ),
    stringsAsFactors = FALSE
  )
  model_sample_rows <- do.call(rbind, lapply(
    seq_len(nrow(model_family_definitions)),
    function(index) {
      definition <- model_family_definitions[index, , drop = FALSE]
      data_used <- if (definition$sample_flag == "eligible_main_dp") {
        main_data
      } else {
        domain_data
      }
      data.frame(
        summary_type = definition$summary_type,
        model_id = definition$model_id,
        sample_flag = definition$sample_flag,
        outcome = definition$outcome,
        systems_n = length(unique(data_used$system_id)),
        students_n = nrow(data_used),
        schools_n = length(unique(data_used$school_uid)),
        excluded_systems = paste(excluded_systems, collapse = " | "),
        system_id = NA_character_,
        included_in_achievement_models = NA,
        exclusion_reason = NA_character_,
        variable = NA_character_,
        category = NA_character_,
        reference_category = NA_character_,
        unweighted_n = NA_integer_,
        unweighted_percentage = NA_real_,
        weighted_sum = sum(data_used$analysis_weight_candidate),
        weighted_percentage = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  ))
  system_sample_rows <- do.call(rbind, lapply(source_systems, function(code) {
    rows <- analysis_data$system_id == code &
      analysis_data$eligible_main_dp &
      analysis_data$eligible_domain_comparison
    included <- code %in% main_systems
    data.frame(
      summary_type = "system_sample",
      model_id = "ALL_REQUIRED_MODELS",
      sample_flag = "eligible_main_dp_and_domain_comparison",
      outcome = "All required achievement outcomes",
      systems_n = as.integer(included),
      students_n = sum(rows),
      schools_n = length(unique(analysis_data$school_uid[rows])),
      excluded_systems = paste(excluded_systems, collapse = " | "),
      system_id = code,
      included_in_achievement_models = included,
      exclusion_reason = if (included) {
        "None"
      } else {
        "No valid content-domain plausible values"
      },
      variable = NA_character_,
      category = NA_character_,
      reference_category = NA_character_,
      unweighted_n = NA_integer_,
      unweighted_percentage = NA_real_,
      weighted_sum = sum(analysis_data$analysis_weight_candidate[rows]),
      weighted_percentage = NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  grade_validation_row <- data.frame(
    summary_type = "grade_validation",
    model_id = "NOT_MODELLED",
    sample_flag = "full_prepared_source",
    outcome = "Not applicable",
    systems_n = length(source_systems),
    students_n = nrow(analysis_data),
    schools_n = length(unique(analysis_data$school_uid)),
    excluded_systems = paste(excluded_systems, collapse = " | "),
    system_id = NA_character_,
    included_in_achievement_models = NA,
    exclusion_reason = NA_character_,
    variable = "IDGRADER",
    category = paste0("Observed codes: ", collapse_text(grade_values)),
    reference_category = "Not applicable",
    unweighted_n = sum(!is.na(analysis_data$IDGRADER)),
    unweighted_percentage = 100 * mean(!is.na(analysis_data$IDGRADER)),
    weighted_sum = NA_real_,
    weighted_percentage = NA_real_,
    stringsAsFactors = FALSE
  )

  model_sample_summary <- rbind(
    model_sample_rows,
    ict_category_summary,
    language_category_summary,
    system_sample_rows,
    grade_validation_row
  )
  row.names(model_sample_summary) <- NULL

  singular_rows <- model_diagnostics[
    !is.na(model_diagnostics$singular_fit) &
      model_diagnostics$singular_fit,
    ,
    drop = FALSE
  ]
  if (nrow(singular_rows) > 0L) {
    add_analysis_warning(paste0(
      "Singular fits were detected in ", nrow(singular_rows),
      " model(s): ", collapse_text(singular_rows$model_id), "."
    ))
  }

  fitting_warning_rows <- model_diagnostics[
    model_diagnostics$captured_warnings != "None",
    ,
    drop = FALSE
  ]
  if (nrow(fitting_warning_rows) > 0L) {
    add_analysis_warning(paste0(
      "Model-fitting warnings were captured in ",
      nrow(fitting_warning_rows), " fit(s); see model_diagnostics.csv."
    ))
  }

  nonconverged_rows <- model_diagnostics[
    model_diagnostics$convergence_status != "CONVERGED",
    ,
    drop = FALSE
  ]
  boundary_rows <- model_diagnostics[
    is.finite(model_diagnostics$school_variance) &
      model_diagnostics$school_variance <= 1e-8,
    ,
    drop = FALSE
  ]
  if (nrow(boundary_rows) > 0L) {
    add_analysis_warning(paste0(
      "Boundary or near-zero school variance was detected in ",
      nrow(boundary_rows), " model(s)."
    ))
  }
  very_small_icc_rows <- model_diagnostics[
    is.finite(model_diagnostics$icc) & model_diagnostics$icc < 0.001,
    ,
    drop = FALSE
  ]
  if (nrow(very_small_icc_rows) > 0L) {
    add_analysis_warning(paste0(
      "ICC below 0.001 was detected in ",
      nrow(very_small_icc_rows), " model(s)."
    ))
  }

  sample_sizes_by_model <- split(
    model_diagnostics$observations_used,
    model_diagnostics$model_id
  )
  differing_sample_models <- names(sample_sizes_by_model)[vapply(
    sample_sizes_by_model,
    function(values) {
      values <- values[is.finite(values)]
      length(unique(values)) > 1L
    },
    logical(1)
  )]
  if (length(differing_sample_models) > 0L) {
    add_analysis_warning(paste0(
      "PV-specific sample sizes differ for model(s): ",
      paste(differing_sample_models, collapse = " | "), "."
    ))
  }

  focal_terms <- c("BSBGHER", "ict_accessYes")
  first_focal <- first_pass_estimates[
    first_pass_estimates$term %in% focal_terms,
    c("term", "pooled_estimate"),
    drop = FALSE
  ]
  main_focal <- main_multilevel_estimates[
    main_multilevel_estimates$term %in% focal_terms,
    c("term", "pooled_estimate"),
    drop = FALSE
  ]
  comparison <- merge(
    first_focal,
    main_focal,
    by = "term",
    suffixes = c("_first_pass", "_multilevel"),
    all = TRUE
  )
  comparison$absolute_difference <- abs(
    comparison$pooled_estimate_first_pass -
      comparison$pooled_estimate_multilevel
  )
  comparison$relative_difference <- comparison$absolute_difference /
    pmax(abs(comparison$pooled_estimate_multilevel), 0.5)
  materially_different <- comparison[
    is.finite(comparison$absolute_difference) &
      comparison$absolute_difference > 0.5 &
      comparison$relative_difference > 0.25,
    ,
    drop = FALSE
  ]
  if (nrow(materially_different) > 0L) {
    add_analysis_warning(paste0(
      "First-pass and multilevel focal estimates differ by more than 25% ",
      "and 0.5 score points for: ",
      paste(materially_different$term, collapse = " | "), "."
    ))
  }

  validation <- data.frame(
    check_id = integer(),
    check = character(),
    status = character(),
    details = character(),
    stringsAsFactors = FALSE
  )
  add_validation <- function(check_id, check, status, details) {
    validation <<- rbind(
      validation,
      data.frame(
        check_id = as.integer(check_id),
        check = check,
        status = status,
        details = details,
        stringsAsFactors = FALSE
      )
    )
  }

  add_validation(
    1L,
    "Processed source structure",
    if (nrow(analysis_data) == 323920L &&
        length(source_systems) == 47L) "PASS" else "FAIL",
    paste0("Rows=", nrow(analysis_data), "; source systems=", length(source_systems))
  )
  add_validation(
    2L,
    "Common analytic sample",
    if (nrow(main_data) == 272315L &&
        identical(sort(main_data$student_uid),
                  sort(domain_data$student_uid))) "PASS" else "FAIL",
    paste0(
      "Main rows=", nrow(main_data), "; domain rows=", nrow(domain_data),
      "; systems=", length(main_systems), "; excluded=",
      collapse_text(excluded_systems)
    )
  )
  add_validation(
    3L,
    "Weight relationship and validity",
    if (maximum_candidate_difference <= 1e-10 &&
        all(main_data$model_weight > 0) &&
        all(is.finite(main_data$model_weight))) "PASS" else "FAIL",
    paste0(
      "Maximum candidate-formula difference=",
      format_number(maximum_candidate_difference),
      "; mean model weight=", format_number(mean(main_data$model_weight))
    )
  )
  add_validation(
    4L,
    "Required model count",
    if (nrow(model_diagnostics) == 35L &&
        length(model_blobs) == 35L) "PASS" else "FAIL",
    paste0(
      "Model fits recorded=", nrow(model_diagnostics),
      "; model objects stored=", length(model_blobs),
      "; expected=35"
    )
  )
  add_validation(
    5L,
    "Required model convergence",
    if (nrow(nonconverged_rows) == 0L) "PASS" else "FAIL",
    paste0("Non-converged or failed models=", nrow(nonconverged_rows))
  )
  add_validation(
    6L,
    "Five PV fits per model family",
    if (all(table(model_diagnostics$model_id) == 5L)) "PASS" else "FAIL",
    paste0(
      paste(names(table(model_diagnostics$model_id)),
            as.integer(table(model_diagnostics$model_id)), sep = "="),
      collapse = "; "
    )
  )
  missing_focal_rows <- model_diagnostics[
    model_diagnostics$model_id != "B_null_dp" &
      model_diagnostics$missing_coefficients != "None",
    ,
    drop = FALSE
  ]
  add_validation(
    7L,
    "Required focal coefficients",
    if (nrow(missing_focal_rows) == 0L) "PASS" else "FAIL",
    paste0("Models with missing focal or control coefficients=", nrow(missing_focal_rows))
  )
  pooled_outputs <- rbind(
    first_pass_estimates,
    main_multilevel_estimates,
    domain_difference_estimates
  )
  add_validation(
    8L,
    "Exactly five PV estimates pooled",
    if (nrow(pooled_outputs) == 30L &&
        all(pooled_outputs$n_pv == 5L) &&
        all(pooled_outputs$pool_status == "PASS")) "PASS" else "FAIL",
    paste0(
      "Pooled coefficient rows=", nrow(pooled_outputs),
      "; rows with five PVs=", sum(pooled_outputs$n_pv == 5L)
    )
  )
  pooled_numeric_columns <- c(
    "pooled_estimate", "pooled_standard_error",
    "confidence_interval_lower", "confidence_interval_upper",
    "within_pv_variance", "between_pv_variance", "total_variance"
  )
  pooled_finite <- nrow(pooled_outputs) == 30L && all(vapply(
    pooled_outputs[pooled_numeric_columns],
    function(column) all(is.finite(column)),
    logical(1)
  ))
  add_validation(
    9L,
    "Finite pooled estimates",
    if (pooled_finite) "PASS" else "FAIL",
    paste0("All required pooled numeric values finite=", pooled_finite)
  )
  add_validation(
    10L,
    "ICC summary across five PVs",
    if (length(valid_icc) == 5L && all(valid_icc >= 0 & valid_icc <= 1)) {
      "PASS"
    } else {
      "FAIL"
    },
    paste0(
      "Valid ICC values=", length(valid_icc),
      "; mean=", format_number(if (length(valid_icc) > 0L) mean(valid_icc) else NA_real_)
    )
  )
  add_validation(
    11L,
    "PV-specific sample consistency",
    if (length(differing_sample_models) == 0L) "PASS" else "WARNING",
    paste0(
      "Model families with differing PV-specific samples=",
      length(differing_sample_models)
    )
  )
  add_validation(
    12L,
    "Singular and boundary diagnostics",
    if (nrow(singular_rows) == 0L && nrow(boundary_rows) == 0L) {
      "PASS"
    } else {
      "WARNING"
    },
    paste0(
      "Singular fits=", nrow(singular_rows),
      "; boundary fits=", nrow(boundary_rows)
    )
  )

  model_archive <- list(
    archive_format = "gzip-compressed serialized R model objects",
    restore_instruction = paste(
      "Use unserialize(memDecompress(archive$compressed_models[[name]],",
      "type = 'gzip')) to restore one fitted model."
    ),
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    analysis_specification = list(
      source_systems = source_systems,
      model_systems = main_systems,
      excluded_systems = excluded_systems,
      main_student_n = nrow(main_data),
      main_school_n = length(unique(main_data$school_uid)),
      weight_source = "analysis_weight_candidate",
      model_weight_formula = paste(
        "analysis_weight_candidate divided by its mean in the common",
        "analytic sample"
      ),
      pv_count = 5L,
      factor_references = c(
        ict_access = "No",
        language_at_home = "Always",
        system_id = levels(main_data$system_id)[1]
      )
    ),
    compressed_models = model_blobs,
    model_diagnostics = model_diagnostics,
    first_pass_estimates = first_pass_estimates,
    main_multilevel_estimates = main_multilevel_estimates,
    domain_difference_estimates = domain_difference_estimates,
    icc_summary = icc_summary
  )
  tryCatch(
    saveRDS(model_archive, output_paths$model_objects, compress = FALSE),
    error = function(e) {
      stop(
        "Could not save local model archive: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!file.exists(output_paths$model_objects) ||
      file.info(output_paths$model_objects)$size <= 0L) {
    stop("The local model archive was not generated.", call. = FALSE)
  }

  write_csv_checked(first_pass_estimates, output_paths$first_pass)
  write_csv_checked(main_multilevel_estimates, output_paths$main_multilevel)
  write_csv_checked(
    domain_difference_estimates,
    output_paths$domain_difference
  )
  write_csv_checked(icc_summary, output_paths$icc_summary)
  write_csv_checked(model_diagnostics, output_paths$model_diagnostics)
  write_csv_checked(model_sample_summary, output_paths$model_sample_summary)

  pre_note_output_names <- c(
    "model_objects", "first_pass", "main_multilevel",
    "domain_difference", "icc_summary", "model_diagnostics",
    "model_sample_summary"
  )
  pre_note_outputs_exist <- vapply(
    output_paths[pre_note_output_names],
    function(path) file.exists(path) && file.info(path)$size > 0L,
    logical(1)
  )
  add_validation(
    13L,
    "Model archive and aggregate outputs",
    if (all(pre_note_outputs_exist)) "PASS" else "FAIL",
    paste0(
      paste(
        names(pre_note_outputs_exist),
        ifelse(pre_note_outputs_exist, "present", "missing"),
        sep = "="
      ),
      collapse = "; "
    )
  )

  if (any(validation$status == "FAIL")) {
    final_status <- "FAIL"
  } else if (any(validation$status == "WARNING") ||
      length(warning_log) > 0L) {
    final_status <- "PASS WITH WARNINGS"
  } else {
    final_status <- "PASS"
  }

  method_note_lines <- c(
    "TIMSS 2023 Grade 8 main-analysis method notes",
    paste0("Script version: ", script_version),
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "Analysis scope",
    "- The analysis reads only data_processed/local_only/timss_g8_analysis_data.rds.",
    "- The prepared source contains 47 education systems.",
    paste0(
      "- Achievement models contain ", length(main_systems),
      " systems, ", nrow(main_data), " students, and ",
      length(unique(main_data$school_uid)), " schools."
    ),
    paste0(
      "- Systems excluded because their content-domain PVs contain no valid values: ",
      paste(excluded_systems, collapse = " | "), "."
    ),
    "- All reported relationships are associational and must not be described causally.",
    "",
    "Variables and factor references",
    "- BSBGHER is the continuous Home Educational Resources predictor.",
    "- ict_access is a factor; No is the reference and Yes is the comparison.",
    paste0(
      "- language_at_home is a factor with levels ",
      paste(expected_language_levels, collapse = " | "),
      "; Always is the reference."
    ),
    "- IDGRADER is not used as a model predictor.",
    paste0(
      "- IDGRADER validation found observed code(s): ",
      collapse_text(grade_values), "; missing rows: ", grade_missing_n, "."
    ),
    "- factor(system_id) is included as education-system fixed effects.",
    "- school_uid is used as the school random-intercept grouping variable.",
    "",
    "Weighting",
    "- TOTWGT and SENWGT are retained unchanged in the processed RDS.",
    "- The fitted models use analysis_weight_candidate from Round 3.",
    "- Round 3 formula: SENWGT * 500 / sum(SENWGT within each system).",
    paste0(
      "- Full-source SENWGT system totals range from ",
      format_number(min(weight_system_summary$SENWGT_sum)), " to ",
      format_number(max(weight_system_summary$SENWGT_sum)), "."
    ),
    paste0(
      "- Full-source candidate-weight system totals range from ",
      format_number(min(weight_system_summary$candidate_weight_sum)), " to ",
      format_number(max(weight_system_summary$candidate_weight_sum)), "."
    ),
    paste(
      "- The within-system SENWGT/TOTWGT ratio range was calculated as a",
      "relationship check; exact proportionality was not required because",
      "the two retained variables are distinct official weight forms."
    ),
    paste0(
      "- In the 44-system complete-case sample, candidate-weight system totals range from ",
      format_number(min(system_sample_rows$weighted_sum[
        system_sample_rows$included_in_achievement_models
      ])), " to ",
      format_number(max(system_sample_rows$weighted_sum[
        system_sample_rows$included_in_achievement_models
      ])), "."
    ),
    paste0(
      "- Maximum absolute difference from the Round 3 candidate formula: ",
      format_number(maximum_candidate_difference), "."
    ),
    paste0(
      "- For model fitting, candidate weights are divided by the common-sample mean ",
      "(", format_number(model_weight_scale), ") so that their mean is 1."
    ),
    paste(
      "- This constant rescaling preserves relative weights but avoids treating",
      "the unnormalised lme4 prior-weight scale as substantively meaningful."
    ),
    "",
    "Variance-estimation limitation",
    paste(
      "- stats::lm and lme4 use model-based standard errors with the specified",
      "prior weights."
    ),
    paste(
      "- JKZONE and JKREP were validated but thousands of replicate-weighted",
      "multilevel refits were deliberately not attempted."
    ),
    paste(
      "- The reported multilevel standard errors must not be described as full",
      "TIMSS JK2 design-based standard errors."
    ),
    paste(
      "- Sampling weights, system fixed effects, and the school random intercept",
      "address different parts of the design, but do not reproduce full JK2 variance."
    ),
    "",
    "Model sequence",
    "- Model A: weighted D&P linear regression benchmark with system fixed effects.",
    "- Model B: weighted D&P null school-random-intercept model with system fixed effects.",
    "- Model C: weighted D&P main school-random-intercept model.",
    paste(
      "- Models D-G: four weighted domain-difference school-random-intercept",
      "models on the identical complete-case sample."
    ),
    "- All multilevel models use maximum likelihood, bobyqa, and maxfun=200000.",
    "- System fixed-effect coefficients remain in each fitted model but are omitted from focal CSV outputs.",
    "",
    "Plausible-value combination",
    "- Five separate models are fitted; the five PV scores are never averaged before fitting.",
    "- Q_bar is the mean of five coefficient estimates.",
    "- U_bar is the mean of five squared within-PV standard errors.",
    "- B is the sample variance of the five estimates.",
    "- T = U_bar + (1 + 1/5) * B; pooled SE = sqrt(T).",
    paste(
      "- The reported degrees of freedom use the large-sample Rubin formula",
      "and do not claim an lme4-specific finite-sample denominator correction."
    ),
    paste(
      "- P-values are calculated from the pooled estimate and total variance;",
      "the five p-values are not averaged."
    ),
    paste(
      "- ICC is reported as the arithmetic mean and observed range across five",
      "PV-specific null models; Rubin's rules are not applied to ICC."
    ),
    paste(
      "- A first-pass versus multilevel discrepancy warning is recorded when",
      "the absolute difference exceeds 0.5 score points and 25 percent."
    ),
    "- School variance at or below 1e-8 and ICC below 0.001 trigger warnings.",
    "",
    "Domain-difference interpretation",
    paste(
      "- A positive HER or ICT coefficient means its association is stronger in",
      "Data and Probability than in the named comparison domain."
    ),
    "- A negative coefficient means the association is weaker in Data and Probability.",
    "- A 95% confidence interval containing zero is not clear evidence of a domain difference.",
    "",
    "Model-object storage",
    paste(
      "- Fitted lm and lmerMod objects are serialized and individually gzip-compressed",
      "inside outputs/local_only/main_analysis_models.rds."
    ),
    "- The model archive is local only and must not be uploaded or committed.",
    "",
    "Automatic validation",
    capture.output(print(validation, row.names = FALSE)),
    "",
    paste0("Recorded analysis warnings: ", length(warning_log)),
    if (length(warning_log) > 0L) {
      paste0("- ", warning_log)
    } else {
      "- None"
    },
    "",
    paste0("Final analysis status: ", final_status)
  )
  write_text_checked(method_note_lines, output_paths$method_notes)

  session_lines <- c(
    "TIMSS 2023 Grade 8 main-analysis session information",
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Project name: ", basename(project_root)),
    paste0("Final analysis status: ", final_status),
    paste0("Achievement-model systems: ", length(main_systems)),
    paste0("Students analysed: ", nrow(main_data)),
    paste0("Schools analysed: ", length(unique(main_data$school_uid))),
    paste0("Excluded systems: ", paste(excluded_systems, collapse = " | ")),
    paste0("R version: ", R.version.string),
    paste0("Platform: ", R.version$platform),
    paste0(
      "Operating system: ", Sys.info()[["sysname"]], " ",
      Sys.info()[["release"]]
    ),
    paste0("Locale: ", paste(Sys.getlocale(), collapse = " | ")),
    paste0("here version: ", as.character(utils::packageVersion("here"))),
    paste0("readr version: ", as.character(utils::packageVersion("readr"))),
    paste0("lme4 version: ", as.character(utils::packageVersion("lme4"))),
    "",
    "Complete sessionInfo() output:",
    capture.output(sessionInfo())
  )
  write_text_checked(session_lines, output_paths$session_info)

  final_outputs_exist <- vapply(
    output_paths,
    function(path) file.exists(path) && file.info(path)$size > 0L,
    logical(1)
  )
  if (!all(final_outputs_exist)) {
    stop(
      "One or more required Round 4 outputs were not generated: ",
      paste(names(final_outputs_exist)[!final_outputs_exist], collapse = ", "),
      call. = FALSE
    )
  }

  message("Round 4 main analysis completed.")
  message("Source systems: ", length(source_systems))
  message("Achievement-model systems: ", length(main_systems))
  message("Excluded systems: ", paste(excluded_systems, collapse = " | "))
  message("Students analysed: ", nrow(main_data))
  message("Schools analysed: ", length(unique(main_data$school_uid)))
  message("Model fits recorded: ", nrow(model_diagnostics))
  message("Final analysis status: ", final_status)
  message("Output files:")
  for (path in output_paths) {
    message("  - ", to_relative_path(path))
  }

  rm(
    analysis_data, main_data, domain_data,
    first_pass_records, null_records, main_records,
    difference_records, model_blobs, model_archive
  )
  gc(verbose = FALSE)

  if (identical(final_status, "FAIL")) {
    stop(
      paste(
        "Round 4 completed with FAIL status.",
        "Review model_diagnostics.csv and model_method_notes.txt."
      ),
      call. = FALSE
    )
  }
})
