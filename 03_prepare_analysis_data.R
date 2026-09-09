# TIMSS 2023 Grade 8: prepare the minimal analysis dataset
#
# Scope:
#   - reads Grade 8 BSG files only;
#   - retains only identifiers, the four mathematics content-domain PV sets,
#     the selected HER, ICT, and language variables, and required design data;
#   - converts only attribute-declared missing values to NA;
#   - creates analysis identifiers, eligibility flags, and same-index PV
#     difference outcomes;
#   - saves one local-only student-level RDS and public-safe aggregate metadata;
#   - does not run regressions, multilevel models, PV pooling, or figures.

local({
  options(stringsAsFactors = FALSE)

  required_packages <- c("here", "readr")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0L) {
    stop(
      paste0(
        "Missing package(s): ", paste(missing_packages, collapse = ", "),
        ". Run this once in the R Console: ",
        "install.packages(c(\"here\", \"readr\"))"
      ),
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(here)
    library(readr)
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
      "No .Rproj file was found at the project root. Open the TIMSS_2023 RStudio Project and run the script again.",
      call. = FALSE
    )
  }

  message("Project root: ", project_root)
  message("Round 3 prepares analysis data only; no statistical models or figures are produced.")
  message("Raw input is restricted to Grade 8 BSG files.")

  relative_paths <- list(
    bsg_dir = "data_raw/local_only/BSG_student_questionnaire_grade8",
    processed_dir = "data_processed/local_only",
    metadata_dir = "data_metadata",
    local_metadata_dir = "data_metadata/local_only",
    archive_dir = "data_metadata/local_only/prepare_archive"
  )
  paths <- lapply(relative_paths, here::here)

  output_paths <- list(
    analysis_data = here::here(
      "data_processed", "local_only", "timss_g8_analysis_data.rds"
    ),
    system_sample_summary = here::here(
      "data_metadata", "analysis_system_sample_summary.csv"
    ),
    variable_dictionary = here::here(
      "data_metadata", "analysis_variable_dictionary.csv"
    ),
    missingness_summary = here::here(
      "data_metadata", "analysis_missingness_summary.csv"
    ),
    weight_summary = here::here(
      "data_metadata", "analysis_weight_summary.csv"
    ),
    exclusion_summary = here::here(
      "data_metadata", "analysis_exclusion_summary.csv"
    ),
    prepare_summary = here::here(
      "data_metadata", "local_only", "prepare_analysis_summary.txt"
    ),
    session_info = here::here(
      "data_metadata", "local_only", "session_info_prepare.txt"
    )
  )

  if (!dir.exists(paths$bsg_dir)) {
    stop(
      "Required BSG directory is missing: ", relative_paths$bsg_dir,
      call. = FALSE
    )
  }
  for (directory_name in c(
    "processed_dir", "metadata_dir", "local_metadata_dir", "archive_dir"
  )) {
    directory <- paths[[directory_name]]
    if (!dir.exists(directory)) {
      created <- dir.create(directory, recursive = TRUE, showWarnings = FALSE)
      if (!created || !dir.exists(directory)) {
        stop(
          "Could not create required directory: ",
          relative_paths[[directory_name]],
          call. = FALSE
        )
      }
    }
  }

  check_directory_writable <- function(directory, relative_label) {
    test_file <- tempfile(pattern = "write_test_", tmpdir = directory)
    writable <- tryCatch({
      writeLines("test", test_file, useBytes = TRUE)
      file.exists(test_file)
    }, error = function(e) FALSE)
    if (file.exists(test_file)) unlink(test_file)
    if (!writable) {
      stop("Output directory is not writable: ", relative_label, call. = FALSE)
    }
    invisible(TRUE)
  }
  check_directory_writable(paths$processed_dir, relative_paths$processed_dir)
  check_directory_writable(paths$metadata_dir, relative_paths$metadata_dir)
  check_directory_writable(
    paths$local_metadata_dir,
    relative_paths$local_metadata_dir
  )

  to_relative_path <- function(path) {
    normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
    prefix <- paste0(project_root, "/")
    if (startsWith(normalized, prefix)) {
      substring(normalized, nchar(prefix) + 1L)
    } else if (identical(normalized, project_root)) {
      "."
    } else {
      normalized
    }
  }

  warning_log <- character()
  record_warning <- function(message_text) {
    warning_log <<- unique(c(warning_log, message_text))
    warning(message_text, call. = FALSE, immediate. = TRUE)
    invisible(TRUE)
  }

  clean_message <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(gsub("[[:space:]]+", " ", x))
  }

  archive_existing_metadata <- function() {
    metadata_outputs <- output_paths[setdiff(
      names(output_paths),
      "analysis_data"
    )]
    existing <- metadata_outputs[
      vapply(metadata_outputs, file.exists, logical(1))
    ]
    if (length(existing) == 0L) return(invisible(NULL))

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    archive_run_dir <- file.path(paths$archive_dir, timestamp)
    if (!dir.create(archive_run_dir, recursive = TRUE, showWarnings = FALSE) &&
        !dir.exists(archive_run_dir)) {
      stop("Could not create the preparation archive directory.", call. = FALSE)
    }
    for (source_path in existing) {
      destination <- file.path(archive_run_dir, basename(source_path))
      copied <- file.copy(source_path, destination, overwrite = FALSE)
      if (!copied) {
        stop(
          "Could not archive an existing preparation output: ",
          to_relative_path(source_path),
          call. = FALSE
        )
      }
    }
    invisible(NULL)
  }
  archive_existing_metadata()

  write_csv_checked <- function(data, path) {
    invalid <- vapply(data, function(x) is.list(x) || is.matrix(x), logical(1))
    if (any(invalid)) {
      stop(
        "Cannot write CSV because list or matrix columns remain: ",
        paste(names(data)[invalid], collapse = ", "),
        call. = FALSE
      )
    }
    tryCatch(
      readr::write_excel_csv(data, path, na = ""),
      error = function(e) {
        stop(
          "Could not write metadata output ", to_relative_path(path), ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (!file.exists(path) || file.info(path)$size <= 0L) {
      stop("Metadata output was not created: ", to_relative_path(path), call. = FALSE)
    }
    invisible(TRUE)
  }

  write_text_checked <- function(lines, path) {
    tryCatch(
      writeLines(enc2utf8(as.character(lines)), path, useBytes = TRUE),
      error = function(e) {
        stop(
          "Could not write text output ", to_relative_path(path), ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (!file.exists(path) || file.info(path)$size <= 0L) {
      stop("Text output was not created: ", to_relative_path(path), call. = FALSE)
    }
    invisible(TRUE)
  }

  first_attribute <- function(x, candidates) {
    for (candidate in candidates) {
      value <- attr(x, candidate, exact = TRUE)
      if (!is.null(value)) return(value)
    }
    NULL
  }

  atomic_values <- function(x) {
    out <- tryCatch(unclass(x), error = function(e) x)
    attributes(out) <- NULL
    as.vector(out)
  }

  missing_details <- function(x) {
    list(
      values = first_attribute(x, c("na_values", "missing.values", "missings")),
      range = first_attribute(x, c("na_range", "missing.range"))
    )
  }

  code_is_in <- function(raw, candidates) {
    if (is.null(candidates) || length(candidates) == 0L) {
      return(rep(FALSE, length(raw)))
    }
    as.character(raw) %in% as.character(candidates)
  }

  declared_missing_mask <- function(x) {
    raw <- atomic_values(x)
    details <- missing_details(x)
    mask <- is.na(raw)
    mask <- mask | code_is_in(raw, details$values)
    if (!is.null(details$range) && length(details$range) >= 2L) {
      numeric_raw <- suppressWarnings(as.numeric(raw))
      numeric_range <- suppressWarnings(as.numeric(details$range[1:2]))
      if (all(is.finite(numeric_range))) {
        mask <- mask | (
          !is.na(numeric_raw) &
            numeric_raw >= min(numeric_range) &
            numeric_raw <= max(numeric_range)
        )
      }
    }
    mask[is.na(mask)] <- FALSE
    mask
  }

  clean_numeric_variable <- function(x, variable_name) {
    if (!is.numeric(x)) {
      stop(variable_name, " is not numeric.", call. = FALSE)
    }
    raw <- suppressWarnings(as.numeric(atomic_values(x)))
    if (length(raw) != length(x)) {
      stop("Could not safely convert ", variable_name, " to numeric.", call. = FALSE)
    }
    missing_mask <- declared_missing_mask(x)
    declared_converted <- sum(missing_mask & !is.na(raw))
    raw[missing_mask] <- NA_real_
    if (any(!is.finite(raw) & !is.na(raw))) {
      stop(variable_name, " contains non-finite values.", call. = FALSE)
    }
    list(values = raw, declared_missing_converted_n = declared_converted)
  }

  clean_character_variable <- function(x, variable_name) {
    raw <- atomic_values(x)
    missing_mask <- declared_missing_mask(x)
    if (is.numeric(raw)) {
      values <- format(raw, scientific = FALSE, trim = TRUE, digits = 22L)
    } else {
      values <- trimws(as.character(raw))
    }
    values[missing_mask | !nzchar(values)] <- NA_character_
    if (length(values) != length(x)) {
      stop("Could not safely convert ", variable_name, " to character.", call. = FALSE)
    }
    list(
      values = values,
      declared_missing_converted_n = sum(missing_mask & !is.na(raw))
    )
  }

  safe_stat <- function(x, function_name) {
    valid <- x[!is.na(x)]
    if (length(valid) == 0L) return(NA_real_)
    switch(
      function_name,
      sum = sum(valid),
      mean = mean(valid),
      min = min(valid),
      max = max(valid),
      stop("Unknown summary function.", call. = FALSE)
    )
  }

  count_unique_nonmissing <- function(x) {
    length(unique(x[!is.na(x)]))
  }

  collapse_unique <- function(x) {
    values <- sort(unique(as.character(x[!is.na(x)])))
    if (length(values) == 0L) "" else paste(values, collapse = " | ")
  }

  select_main_data_frame <- function(environment, object_names, required_names) {
    tabular_names <- object_names[vapply(
      object_names,
      function(name) {
        object <- get(name, envir = environment, inherits = FALSE)
        is.data.frame(object)
      },
      logical(1)
    )]
    if (length(tabular_names) == 0L) {
      stop("No data frame was found in the RData file.", call. = FALSE)
    }

    scores <- vapply(tabular_names, function(name) {
      object <- get(name, envir = environment, inherits = FALSE)
      sum(required_names %in% toupper(names(object)))
    }, integer(1))
    row_counts <- vapply(tabular_names, function(name) {
      nrow(get(name, envir = environment, inherits = FALSE))
    }, integer(1))

    best_score <- max(scores)
    candidates <- tabular_names[scores == best_score]
    candidate_rows <- row_counts[match(candidates, tabular_names)]
    best_rows <- max(candidate_rows)
    finalists <- candidates[candidate_rows == best_rows]
    if (length(finalists) != 1L) {
      stop(
        "The RData file contains multiple equally plausible main data frames.",
        call. = FALSE
      )
    }
    finalists[[1]]
  }

  pv_groups <- list(
    data_probability = sprintf("BSMDAT%02d", 1:5),
    number = sprintf("BSMNUM%02d", 1:5),
    algebra = sprintf("BSMALG%02d", 1:5),
    geometry_measurement = sprintf("BSMGEO%02d", 1:5)
  )
  all_pv_names <- unname(unlist(pv_groups, use.names = FALSE))

  id_names <- c("CTY", "IDCNTRY", "IDSCHOOL", "IDSTUD")
  grade_name <- "IDGRADER"
  selected_research_names <- c("BSBGHER", "BSBG05A", "BSBG03")
  weight_design_names <- c("TOTWGT", "SENWGT", "JKZONE", "JKREP")
  core_required_names <- c(
    id_names,
    all_pv_names,
    weight_design_names
  )
  requested_names <- unique(c(
    core_required_names,
    grade_name,
    selected_research_names
  ))

  bsg_files <- list.files(
    paths$bsg_dir,
    pattern = "^BSG[A-Z0-9]{3}M8\\.RData$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  bsg_files <- sort(bsg_files)
  if (length(bsg_files) == 0L) {
    stop("No Grade 8 BSG RData files were found.", call. = FALSE)
  }
  if (length(bsg_files) != 47L) {
    stop(
      "Expected 47 Grade 8 BSG files but found ", length(bsg_files), ".",
      call. = FALSE
    )
  }

  extract_system_code <- function(path) {
    upper_name <- toupper(basename(path))
    code <- sub(
      "^BSG([A-Z0-9]{3})M8\\.RDATA$",
      "\\1",
      upper_name,
      perl = TRUE
    )
    if (!grepl("^[A-Z0-9]{3}$", code)) {
      stop("Could not extract the system code from file name: ", basename(path), call. = FALSE)
    }
    code
  }
  file_system_codes <- vapply(bsg_files, extract_system_code, character(1))
  if (anyDuplicated(file_system_codes)) {
    stop("Duplicate system codes were found in BSG file names.", call. = FALSE)
  }

  message("Grade 8 BSG files found: ", length(bsg_files))

  prepared_parts <- vector("list", length(bsg_files))
  system_summary_parts <- vector("list", length(bsg_files))
  missingness_parts <- list()
  file_read_records <- vector("list", length(bsg_files))
  internal_system_map <- vector("list", length(bsg_files))

  add_missingness_row <- function(
      system_code, variable_name, values, declared_missing_converted_n,
      variable_role, source_or_derived) {
    n_total <- length(values)
    n_missing <- sum(is.na(values))
    data.frame(
      system_id = system_code,
      variable_name = variable_name,
      variable_role = variable_role,
      source_or_derived = source_or_derived,
      unweighted_n = n_total,
      valid_n = n_total - n_missing,
      missing_n = n_missing,
      missing_percentage = if (n_total > 0L) 100 * n_missing / n_total else NA_real_,
      declared_missing_converted_n = declared_missing_converted_n,
      all_missing = n_total > 0L && n_missing == n_total,
      distinct_valid_values = count_unique_nonmissing(values),
      minimum = if (is.numeric(values)) safe_stat(values, "min") else NA_real_,
      maximum = if (is.numeric(values)) safe_stat(values, "max") else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  for (file_index in seq_along(bsg_files)) {
    file_path <- bsg_files[[file_index]]
    system_code <- file_system_codes[[file_index]]
    message(
      "Processing system ", file_index, "/", length(bsg_files), ": ",
      system_code, " (", basename(file_path), ")"
    )

    file_warnings <- character()
    data_environment <- new.env(parent = emptyenv())
    loaded_names <- tryCatch(
      withCallingHandlers(
        load(file_path, envir = data_environment),
        warning = function(w) {
          file_warnings <<- c(file_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        stop(
          "Could not read ", basename(file_path), ": ", conditionMessage(e),
          call. = FALSE
        )
      }
    )

    main_object_name <- tryCatch(
      select_main_data_frame(
        data_environment,
        loaded_names,
        core_required_names
      ),
      error = function(e) {
        stop(
          "Could not identify the main data frame in ", basename(file_path),
          ": ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
    source_data <- get(main_object_name, envir = data_environment, inherits = FALSE)
    uppercase_names <- toupper(names(source_data))
    if (anyDuplicated(uppercase_names)) {
      stop(
        "Case-insensitive duplicate column names were found in ",
        basename(file_path), ".",
        call. = FALSE
      )
    }
    names(source_data) <- uppercase_names

    missing_core <- setdiff(core_required_names, names(source_data))
    if (length(missing_core) > 0L) {
      stop(
        "Core variable(s) missing in ", basename(file_path), ": ",
        paste(missing_core, collapse = ", "),
        call. = FALSE
      )
    }
    missing_research <- setdiff(selected_research_names, names(source_data))
    if (length(missing_research) > 0L) {
      record_warning(paste0(
        "System ", system_code, " is missing selected research variable(s): ",
        paste(missing_research, collapse = ", "),
        ". Missing columns were created as NA and eligibility flags will be false."
      ))
      for (variable_name in missing_research) {
        source_data[[variable_name]] <- rep(NA_real_, nrow(source_data))
      }
    }
    if (!grade_name %in% names(source_data)) {
      record_warning(paste0(
        "System ", system_code, " is missing ", grade_name,
        "; grade validation will be unavailable for this system."
      ))
      source_data[[grade_name]] <- rep(NA_real_, nrow(source_data))
    }

    source_data <- source_data[, requested_names, drop = FALSE]
    n_students <- nrow(source_data)
    if (n_students == 0L) {
      stop("The main data frame contains zero rows for system ", system_code, ".", call. = FALSE)
    }

    cty_result <- clean_character_variable(source_data$CTY, "CTY")
    if (anyNA(cty_result$values)) {
      stop("CTY contains missing values in ", basename(file_path), ".", call. = FALSE)
    }
    cty_values <- sort(unique(toupper(cty_result$values[!is.na(cty_result$values)])))
    if (length(cty_values) != 1L || !identical(cty_values[[1]], system_code)) {
      stop(
        "File-name system code and CTY do not match for ", basename(file_path),
        ".",
        call. = FALSE
      )
    }

    idcntry_result <- clean_numeric_variable(source_data$IDCNTRY, "IDCNTRY")
    if (anyNA(idcntry_result$values)) {
      stop("IDCNTRY contains missing values in ", basename(file_path), ".", call. = FALSE)
    }
    idcntry_values <- unique(idcntry_result$values[!is.na(idcntry_result$values)])
    if (length(idcntry_values) != 1L) {
      stop(
        "IDCNTRY does not contain exactly one non-missing system code in ",
        basename(file_path), ".",
        call. = FALSE
      )
    }
    internal_system_map[[file_index]] <- data.frame(
      system_id = system_code,
      idcntry_code = idcntry_values[[1]],
      stringsAsFactors = FALSE
    )

    school_result <- clean_character_variable(source_data$IDSCHOOL, "IDSCHOOL")
    student_result <- clean_character_variable(source_data$IDSTUD, "IDSTUD")
    grade_result <- clean_numeric_variable(source_data[[grade_name]], grade_name)

    if (anyNA(school_result$values) || anyNA(student_result$values)) {
      stop(
        "Missing school or student identifiers were found in system ",
        system_code, ".",
        call. = FALSE
      )
    }

    cleaned_numeric <- list()
    numeric_names <- c(
      all_pv_names,
      selected_research_names,
      weight_design_names
    )
    converted_missing_counts <- setNames(
      integer(length(numeric_names)),
      numeric_names
    )
    for (variable_name in numeric_names) {
      result <- clean_numeric_variable(source_data[[variable_name]], variable_name)
      cleaned_numeric[[variable_name]] <- result$values
      converted_missing_counts[[variable_name]] <-
        result$declared_missing_converted_n
    }

    invalid_ict_codes <- setdiff(
      sort(unique(cleaned_numeric$BSBG05A[!is.na(cleaned_numeric$BSBG05A)])),
      c(1, 2)
    )
    if (length(invalid_ict_codes) > 0L) {
      stop(
        "Unconfirmed BSBG05A code(s) were found in system ", system_code,
        ". Review the official questionnaire before continuing.",
        call. = FALSE
      )
    }
    invalid_language_codes <- setdiff(
      sort(unique(cleaned_numeric$BSBG03[!is.na(cleaned_numeric$BSBG03)])),
      1:4
    )
    if (length(invalid_language_codes) > 0L) {
      stop(
        "Unconfirmed BSBG03 code(s) were found in system ", system_code,
        ". Review the official questionnaire before continuing.",
        call. = FALSE
      )
    }

    for (weight_name in c("TOTWGT", "SENWGT")) {
      weight_values <- cleaned_numeric[[weight_name]]
      if (anyNA(weight_values) || any(!is.finite(weight_values)) ||
          any(weight_values <= 0)) {
        stop(
          weight_name, " contains missing, non-finite, or non-positive values in system ",
          system_code, ".",
          call. = FALSE
        )
      }
    }
    for (design_name in c("JKZONE", "JKREP")) {
      design_values <- cleaned_numeric[[design_name]]
      if (anyNA(design_values) || any(!is.finite(design_values))) {
        stop(
          design_name, " contains missing or non-finite values in system ",
          system_code, ".",
          call. = FALSE
        )
      }
    }

    senate_weight_sum <- sum(cleaned_numeric$SENWGT)
    if (!is.finite(senate_weight_sum) || senate_weight_sum <= 0) {
      stop("SENWGT has an invalid system total in system ", system_code, ".", call. = FALSE)
    }
    # Candidate equal-system weight:
    #   w_ij(candidate) = SENWGT_ij * 500 / sum_j(SENWGT_ij)
    # The transformation preserves within-system relative weights and gives
    # every complete BSG system an exact total contribution of 500 before
    # analysis-specific complete-case filtering. It is retained as a candidate;
    # no post-complete-case renormalisation is performed in this script.
    analysis_weight_candidate <- cleaned_numeric$SENWGT * 500 / senate_weight_sum

    system_id <- rep(system_code, n_students)
    school_uid <- paste(system_id, school_result$values, sep = "::")
    student_uid <- paste(
      system_id,
      school_result$values,
      student_result$values,
      sep = "::"
    )
    if (anyDuplicated(student_uid)) {
      stop(
        "The system-school-student composite key is not unique in system ",
        system_code, ".",
        call. = FALSE
      )
    }

    prepared <- data.frame(
      system_id = system_id,
      IDCNTRY = idcntry_result$values,
      IDSCHOOL = school_result$values,
      IDSTUD = student_result$values,
      school_uid = school_uid,
      student_uid = student_uid,
      IDGRADER = grade_result$values,
      stringsAsFactors = FALSE
    )
    for (variable_name in all_pv_names) {
      prepared[[variable_name]] <- cleaned_numeric[[variable_name]]
    }
    prepared$BSBGHER <- cleaned_numeric$BSBGHER
    prepared$BSBG05A <- cleaned_numeric$BSBG05A
    prepared$ict_access <- factor(
      prepared$BSBG05A,
      levels = c(2, 1),
      labels = c("No", "Yes")
    )
    prepared$BSBG03 <- cleaned_numeric$BSBG03
    prepared$language_at_home <- factor(
      prepared$BSBG03,
      levels = 1:4,
      labels = c("Always", "Almost always", "Sometimes", "Never")
    )
    prepared$TOTWGT <- cleaned_numeric$TOTWGT
    prepared$SENWGT <- cleaned_numeric$SENWGT
    prepared$analysis_weight_candidate <- analysis_weight_candidate
    prepared$JKZONE <- cleaned_numeric$JKZONE
    prepared$JKREP <- cleaned_numeric$JKREP

    difference_names <- character()
    for (pv_index in 1:5) {
      suffix <- sprintf("%02d", pv_index)
      output_suffix <- paste0("pv", pv_index)
      data_probability <- prepared[[paste0("BSMDAT", suffix)]]
      number <- prepared[[paste0("BSMNUM", suffix)]]
      algebra <- prepared[[paste0("BSMALG", suffix)]]
      geometry <- prepared[[paste0("BSMGEO", suffix)]]

      current_names <- c(
        paste0("dp_minus_number_", output_suffix),
        paste0("dp_minus_algebra_", output_suffix),
        paste0("dp_minus_geometry_", output_suffix),
        paste0("dp_minus_other_mean_", output_suffix)
      )
      prepared[[current_names[[1]]]] <- data_probability - number
      prepared[[current_names[[2]]]] <- data_probability - algebra
      prepared[[current_names[[3]]]] <- data_probability - geometry
      prepared[[current_names[[4]]]] <- data_probability -
        (number + algebra + geometry) / 3
      difference_names <- c(difference_names, current_names)
    }

    complete_variables <- function(variable_names) {
      stats::complete.cases(prepared[, variable_names, drop = FALSE])
    }
    design_eligibility_names <- c(
      "system_id", "school_uid", "student_uid",
      "SENWGT", "JKZONE", "JKREP"
    )
    prepared$eligible_rq1 <- complete_variables(c(
      pv_groups$data_probability,
      "BSBGHER",
      design_eligibility_names
    ))
    prepared$eligible_rq2 <- complete_variables(c(
      pv_groups$data_probability,
      "ict_access",
      design_eligibility_names
    ))
    prepared$eligible_main_dp <- complete_variables(c(
      pv_groups$data_probability,
      "BSBGHER", "ict_access", "language_at_home",
      design_eligibility_names
    ))
    prepared$eligible_domain_comparison <- complete_variables(c(
      all_pv_names,
      "BSBGHER", "ict_access", "language_at_home",
      design_eligibility_names
    ))

    domain_available <- vapply(pv_groups, function(variable_names) {
      all(vapply(
        variable_names,
        function(variable_name) any(!is.na(prepared[[variable_name]])),
        logical(1)
      ))
    }, logical(1))
    unavailable_domains <- names(domain_available)[!domain_available]
    if (length(unavailable_domains) > 0L) {
      record_warning(paste0(
        "System ", system_code,
        " has no valid values in content-domain PV group(s): ",
        paste(unavailable_domains, collapse = ", "),
        ". The system remains in the prepared RDS but will not be eligible for the affected analyses."
      ))
    }

    if (count_unique_nonmissing(prepared$BSBGHER) < 2L) {
      record_warning(paste0(
        "BSBGHER has fewer than two valid values in system ", system_code, "."
      ))
    }
    if (count_unique_nonmissing(prepared$ict_access) < 2L) {
      record_warning(paste0(
        "ICT access has fewer than two valid categories in system ", system_code, "."
      ))
    }
    if (count_unique_nonmissing(prepared$language_at_home) < 2L) {
      record_warning(paste0(
        "The language control has fewer than two valid categories in system ",
        system_code, "."
      ))
    }

    missingness_roles <- c(
      setNames(rep("plausible_value", length(all_pv_names)), all_pv_names),
      BSBGHER = "home_educational_resources",
      BSBG05A = "ICT_access",
      BSBG03 = "language_control",
      TOTWGT = "original_student_weight",
      SENWGT = "senate_weight",
      JKZONE = "jackknife_zone",
      JKREP = "jackknife_replicate"
    )
    for (variable_name in names(missingness_roles)) {
      missingness_parts[[length(missingness_parts) + 1L]] <- add_missingness_row(
        system_code,
        variable_name,
        prepared[[variable_name]],
        converted_missing_counts[[variable_name]],
        missingness_roles[[variable_name]],
        "source"
      )
    }
    missingness_parts[[length(missingness_parts) + 1L]] <- add_missingness_row(
      system_code,
      grade_name,
      prepared[[grade_name]],
      grade_result$declared_missing_converted_n,
      "grade_validation",
      "source"
    )
    missingness_parts[[length(missingness_parts) + 1L]] <- add_missingness_row(
      system_code,
      "analysis_weight_candidate",
      prepared$analysis_weight_candidate,
      0L,
      "candidate_equal_system_weight",
      "derived"
    )
    for (variable_name in difference_names) {
      missingness_parts[[length(missingness_parts) + 1L]] <- add_missingness_row(
        system_code,
        variable_name,
        prepared[[variable_name]],
        0L,
        "domain_difference_outcome",
        "derived"
      )
    }

    system_summary_parts[[file_index]] <- data.frame(
      system_id = system_code,
      idcntry_code = idcntry_values[[1]],
      source_file = basename(file_path),
      source_object = main_object_name,
      original_student_n = n_students,
      original_school_n = length(unique(prepared$school_uid)),
      standardized_grade_codes = collapse_unique(prepared$IDGRADER),
      grade_missing_n = sum(is.na(prepared$IDGRADER)),
      file_code_matches_CTY = TRUE,
      data_probability_PVs_available = domain_available[["data_probability"]],
      number_PVs_available = domain_available[["number"]],
      algebra_PVs_available = domain_available[["algebra"]],
      geometry_measurement_PVs_available = domain_available[["geometry_measurement"]],
      all_four_domain_PV_groups_available = all(domain_available),
      stringsAsFactors = FALSE
    )
    file_read_records[[file_index]] <- data.frame(
      system_id = system_code,
      source_file = basename(file_path),
      read_success = TRUE,
      warning_count = length(file_warnings),
      warning_message = paste(unique(clean_message(file_warnings)), collapse = " | "),
      stringsAsFactors = FALSE
    )
    if (length(file_warnings) > 0L) {
      warning_log <- unique(c(
        warning_log,
        paste0(
          "Warnings while reading ", basename(file_path), ": ",
          paste(unique(clean_message(file_warnings)), collapse = " | ")
        )
      ))
    }

    prepared_parts[[file_index]] <- prepared
    rm(source_data, prepared, data_environment)
    invisible(gc(verbose = FALSE))
  }

  system_map <- do.call(rbind, internal_system_map)
  if (anyDuplicated(system_map$idcntry_code)) {
    stop(
      "More than one file-name system code maps to the same IDCNTRY value.",
      call. = FALSE
    )
  }

  analysis_data <- do.call(rbind, prepared_parts)
  row.names(analysis_data) <- NULL
  system_sample_summary <- do.call(rbind, system_summary_parts)
  row.names(system_sample_summary) <- NULL
  analysis_missingness_summary <- do.call(rbind, missingness_parts)
  row.names(analysis_missingness_summary) <- NULL
  file_read_summary <- do.call(rbind, file_read_records)

  if (length(unique(analysis_data$system_id)) != 47L) {
    stop("The combined prepared data do not contain exactly 47 systems.", call. = FALSE)
  }
  if (anyNA(analysis_data$school_uid) || anyNA(analysis_data$student_uid)) {
    stop("Prepared composite identifiers contain missing values.", call. = FALSE)
  }
  if (anyDuplicated(analysis_data$student_uid)) {
    stop("The combined student_uid is not unique.", call. = FALSE)
  }
  system_student_key <- paste(
    analysis_data$system_id,
    analysis_data$IDSTUD,
    sep = "::"
  )
  schools_per_system_student <- tapply(
    analysis_data$school_uid,
    system_student_key,
    function(x) length(unique(x))
  )
  students_linked_to_multiple_schools <- sum(
    schools_per_system_student > 1L,
    na.rm = TRUE
  )
  if (students_linked_to_multiple_schools > 0L) {
    stop(
      "At least one system-student identifier is linked to multiple schools.",
      call. = FALSE
    )
  }

  eligibility_names <- c(
    "eligible_rq1", "eligible_rq2",
    "eligible_main_dp", "eligible_domain_comparison"
  )
  exclusion_parts <- list()
  for (system_code in sort(unique(analysis_data$system_id))) {
    system_rows <- analysis_data$system_id == system_code
    system_school_ids <- analysis_data$school_uid[system_rows]
    for (eligibility_name in eligibility_names) {
      eligible_values <- analysis_data[[eligibility_name]][system_rows]
      original_n <- sum(system_rows)
      eligible_n <- sum(eligible_values)
      excluded_n <- original_n - eligible_n
      exclusion_parts[[length(exclusion_parts) + 1L]] <- data.frame(
        system_id = system_code,
        eligibility_flag = eligibility_name,
        original_student_n = original_n,
        eligible_student_n = eligible_n,
        excluded_student_n = excluded_n,
        exclusion_percentage = 100 * excluded_n / original_n,
        schools_before_filtering = length(unique(system_school_ids)),
        schools_after_filtering = length(unique(system_school_ids[eligible_values])),
        no_eligible_cases = eligible_n == 0L,
        very_small_eligible_sample = eligible_n > 0L && eligible_n < 100L,
        very_few_retained_schools = eligible_n > 0L &&
          length(unique(system_school_ids[eligible_values])) < 10L,
        stringsAsFactors = FALSE
      )
    }
  }
  analysis_exclusion_summary <- do.call(rbind, exclusion_parts)
  row.names(analysis_exclusion_summary) <- NULL

  weight_parts <- lapply(
    sort(unique(analysis_data$system_id)),
    function(system_code) {
      rows <- analysis_data$system_id == system_code
      eligible_rows <- rows & analysis_data$eligible_main_dp
      data.frame(
        system_id = system_code,
        student_n = sum(rows),
        TOTWGT_sum = safe_stat(analysis_data$TOTWGT[rows], "sum"),
        TOTWGT_mean = safe_stat(analysis_data$TOTWGT[rows], "mean"),
        TOTWGT_minimum = safe_stat(analysis_data$TOTWGT[rows], "min"),
        TOTWGT_maximum = safe_stat(analysis_data$TOTWGT[rows], "max"),
        SENWGT_sum_before_normalisation = safe_stat(
          analysis_data$SENWGT[rows], "sum"
        ),
        SENWGT_mean_before_normalisation = safe_stat(
          analysis_data$SENWGT[rows], "mean"
        ),
        SENWGT_minimum_before_normalisation = safe_stat(
          analysis_data$SENWGT[rows], "min"
        ),
        SENWGT_maximum_before_normalisation = safe_stat(
          analysis_data$SENWGT[rows], "max"
        ),
        analysis_weight_sum_after_normalisation = safe_stat(
          analysis_data$analysis_weight_candidate[rows], "sum"
        ),
        analysis_weight_mean_after_normalisation = safe_stat(
          analysis_data$analysis_weight_candidate[rows], "mean"
        ),
        analysis_weight_minimum_after_normalisation = safe_stat(
          analysis_data$analysis_weight_candidate[rows], "min"
        ),
        analysis_weight_maximum_after_normalisation = safe_stat(
          analysis_data$analysis_weight_candidate[rows], "max"
        ),
        analysis_weight_sum_eligible_main_dp = safe_stat(
          analysis_data$analysis_weight_candidate[eligible_rows], "sum"
        ),
        analysis_weight_missing_n = sum(
          is.na(analysis_data$analysis_weight_candidate[rows])
        ),
        analysis_weight_nonpositive_n = sum(
          analysis_data$analysis_weight_candidate[rows] <= 0,
          na.rm = TRUE
        ),
        analysis_weight_nonfinite_n = sum(
          !is.finite(analysis_data$analysis_weight_candidate[rows]) &
            !is.na(analysis_data$analysis_weight_candidate[rows])
        ),
        absolute_deviation_from_target_500 = abs(
          safe_stat(analysis_data$analysis_weight_candidate[rows], "sum") - 500
        ),
        stringsAsFactors = FALSE
      )
    }
  )
  analysis_weight_summary <- do.call(rbind, weight_parts)
  row.names(analysis_weight_summary) <- NULL

  systems_with_valid <- function(variable_name) {
    valid_by_system <- tapply(
      !is.na(analysis_data[[variable_name]]),
      analysis_data$system_id,
      any
    )
    sum(valid_by_system, na.rm = TRUE)
  }

  dictionary_rows <- list()
  add_dictionary_row <- function(
      role, variable_name, source_variable, label, measurement_level,
      coding_or_scale, transformation, use_status, used_in,
      documentation_source) {
    dictionary_rows[[length(dictionary_rows) + 1L]] <<- data.frame(
      role = role,
      variable_name = variable_name,
      source_variable = source_variable,
      file_type = "BSG",
      variable_label = label,
      measurement_level = measurement_level,
      coding_or_scale = coding_or_scale,
      transformation = transformation,
      systems_with_any_valid_value = systems_with_valid(variable_name),
      primary_or_supporting = use_status,
      used_in = used_in,
      documentation_source = documentation_source,
      stringsAsFactors = FALSE
    )
  }

  add_dictionary_row(
    "education_system_identifier", "system_id", "CTY and file name",
    "Three-character education-system code", "identifier",
    "Validated three-character code",
    "File-name code cross-checked against CTY", "required",
    "all preparation and analysis stages", "TIMSS 2023 User Guide"
  )
  add_dictionary_row(
    "school_identifier", "school_uid", "IDSCHOOL",
    "Education-system-specific school identifier", "identifier",
    "system_id::IDSCHOOL",
    "Composite identifier created without altering IDSCHOOL", "required",
    "school clustering", "TIMSS 2023 User Guide"
  )
  add_dictionary_row(
    "student_identifier", "student_uid", "IDSTUD and IDSCHOOL",
    "Education-system-school-student identifier", "identifier",
    "system_id::IDSCHOOL::IDSTUD",
    "Composite identifier created for integrity checks", "preparation_only",
    "deduplication and validation", "TIMSS 2023 User Guide"
  )
  add_dictionary_row(
    "grade_validation", "IDGRADER", "IDGRADER",
    "Standardized Grade ID", "categorical_identifier",
    "Official TIMSS standardized grade code",
    "Attribute-declared missing values converted to NA", "validation_only",
    "sample validation; excluded from models", "TIMSS 2023 Grade 8 Codebook"
  )

  domain_labels <- c(
    data_probability = "Data and Probability plausible value",
    number = "Number plausible value",
    algebra = "Algebra plausible value",
    geometry_measurement = "Geometry and Measurement plausible value"
  )
  for (domain_name in names(pv_groups)) {
    for (variable_name in pv_groups[[domain_name]]) {
      add_dictionary_row(
        paste0(domain_name, "_outcome"),
        variable_name,
        variable_name,
        paste(domain_labels[[domain_name]], substring(variable_name, nchar(variable_name) - 1L)),
        "continuous_plausible_value",
        "TIMSS mathematics reporting scale; five PVs retained separately",
        "Only attribute-declared missing values converted to NA",
        if (domain_name == "data_probability") "primary_outcome" else "comparison_outcome",
        if (domain_name == "data_probability") "RQ1, RQ2, and RQ3" else "RQ3",
        "TIMSS 2023 Grade 8 Codebook and Technical Report Chapters 11-13"
      )
    }
  }

  add_dictionary_row(
    "home_educational_resources", "BSBGHER", "BSBGHER",
    "Home Educational Resources scale", "continuous_scale",
    "Official international scale",
    "Attribute-declared missing values converted to NA; no standardisation",
    "primary_predictor", "RQ1, main Data and Probability model, and RQ3",
    "TIMSS 2023 Grade 8 Codebook and Student Derived Variables"
  )
  add_dictionary_row(
    "ICT_access", "ict_access", "BSBG05A",
    "Access to own computer or tablet at home", "binary_categorical",
    "No is the reference category; Yes is the comparison category",
    "Official codes 1=Yes and 2=No converted to a labelled factor; declared missing values converted to NA",
    "primary_predictor", "RQ2, main Data and Probability model, and RQ3",
    "TIMSS 2023 Grade 8 Student Questionnaire item 5A and Codebook"
  )
  add_dictionary_row(
    "language_control", "language_at_home", "BSBG03",
    "Frequency of speaking the language of the test at home",
    "categorical",
    "Always; Almost always; Sometimes; Never",
    "Official codes 1-4 converted to a factor; declared missing values converted to NA",
    "primary_control", "main Data and Probability model and RQ3",
    "TIMSS 2023 Grade 8 Student Questionnaire item 3 and Codebook"
  )
  add_dictionary_row(
    "original_student_weight", "TOTWGT", "TOTWGT",
    "Total Student Weight", "continuous_weight",
    "Official student population weight",
    "Retained unchanged after attribute-declared missing-value checking",
    "supporting", "weight checks and population-oriented summaries",
    "TIMSS 2023 User Guide"
  )
  add_dictionary_row(
    "senate_weight", "SENWGT", "SENWGT",
    "Senate Weight", "continuous_weight",
    "Official weight scaled to approximately 500 per education system",
    "Retained unchanged after validation", "primary_weight_candidate",
    "pooled analysis subject to final model implementation checks",
    "TIMSS 2023 User Guide"
  )
  add_dictionary_row(
    "candidate_equal_system_weight", "analysis_weight_candidate", "SENWGT",
    "Candidate equal-system analysis weight", "continuous_weight",
    "Exact full-sample system total of 500",
    "SENWGT * 500 / system sum of SENWGT; no complete-case renormalisation",
    "candidate_only", "retained for verification before R/04_main_analysis.R",
    "Derived from the official Senate Weight rule in the TIMSS 2023 User Guide"
  )
  add_dictionary_row(
    "jackknife_zone", "JKZONE", "JKZONE",
    "Jackknife Zone", "design_variable", "Official numeric code",
    "Retained unchanged", "required", "design-corrected variance estimation",
    "TIMSS 2023 User Guide"
  )
  add_dictionary_row(
    "jackknife_replicate", "JKREP", "JKREP",
    "Jackknife Replicate Code", "design_variable", "Official numeric code",
    "Retained unchanged", "required", "design-corrected variance estimation",
    "TIMSS 2023 User Guide"
  )

  for (pv_index in 1:5) {
    suffix <- sprintf("%02d", pv_index)
    output_suffix <- paste0("pv", pv_index)
    difference_definitions <- list(
      list(
        name = paste0("dp_minus_number_", output_suffix),
        source = paste0("BSMDAT", suffix, " - BSMNUM", suffix),
        label = "Data and Probability minus Number"
      ),
      list(
        name = paste0("dp_minus_algebra_", output_suffix),
        source = paste0("BSMDAT", suffix, " - BSMALG", suffix),
        label = "Data and Probability minus Algebra"
      ),
      list(
        name = paste0("dp_minus_geometry_", output_suffix),
        source = paste0("BSMDAT", suffix, " - BSMGEO", suffix),
        label = "Data and Probability minus Geometry and Measurement"
      ),
      list(
        name = paste0("dp_minus_other_mean_", output_suffix),
        source = paste0(
          "BSMDAT", suffix, " - mean(BSMNUM", suffix, ", BSMALG", suffix,
          ", BSMGEO", suffix, ")"
        ),
        label = "Data and Probability minus the mean of the other three domains"
      )
    )
    for (definition in difference_definitions) {
      add_dictionary_row(
        "domain_difference_outcome",
        definition$name,
        definition$source,
        paste(definition$label, "plausible value", pv_index),
        "continuous_derived_plausible_value",
        "Same-index multidimensional plausible-value contrast",
        definition$source,
        "comparison_outcome",
        "RQ3; five estimates must be pooled in R/04_main_analysis.R",
        "TIMSS 2023 Technical Report Chapters 11-13"
      )
    }
  }

  eligibility_descriptions <- c(
    eligible_rq1 = "Complete D&P PVs, BSBGHER, system/school/student IDs, SENWGT, JKZONE, and JKREP",
    eligible_rq2 = "Complete D&P PVs, ict_access, system/school/student IDs, SENWGT, JKZONE, and JKREP",
    eligible_main_dp = "Complete D&P PVs, BSBGHER, ict_access, language_at_home, IDs, SENWGT, JKZONE, and JKREP",
    eligible_domain_comparison = "Complete four-domain PVs, BSBGHER, ict_access, language_at_home, IDs, SENWGT, JKZONE, and JKREP"
  )
  for (flag_name in names(eligibility_descriptions)) {
    add_dictionary_row(
      "analysis_eligibility_flag", flag_name, eligibility_descriptions[[flag_name]],
      paste("Eligibility for", flag_name), "logical",
      "TRUE or FALSE",
      paste("TRUE when:", eligibility_descriptions[[flag_name]]),
      "preparation_only", "complete-case sample definition",
      "Locked Round 2B analysis specification"
    )
  }
  analysis_variable_dictionary <- do.call(rbind, dictionary_rows)
  row.names(analysis_variable_dictionary) <- NULL

  expected_analysis_columns <- unique(c(
    "system_id", "IDCNTRY", "IDSCHOOL", "IDSTUD",
    "school_uid", "student_uid", "IDGRADER",
    all_pv_names,
    "BSBGHER", "BSBG05A", "ict_access",
    "BSBG03", "language_at_home",
    "TOTWGT", "SENWGT", "analysis_weight_candidate",
    "JKZONE", "JKREP",
    unlist(lapply(1:5, function(index) c(
      paste0("dp_minus_number_pv", index),
      paste0("dp_minus_algebra_pv", index),
      paste0("dp_minus_geometry_pv", index),
      paste0("dp_minus_other_mean_pv", index)
    ))),
    eligibility_names
  ))
  unexpected_columns <- setdiff(names(analysis_data), expected_analysis_columns)
  missing_analysis_columns <- setdiff(expected_analysis_columns, names(analysis_data))
  if (length(unexpected_columns) > 0L || length(missing_analysis_columns) > 0L) {
    stop(
      "The prepared dataset column set does not match the locked specification.",
      call. = FALSE
    )
  }
  analysis_data <- analysis_data[, expected_analysis_columns, drop = FALSE]

  tryCatch(
    saveRDS(analysis_data, output_paths$analysis_data, compress = "gzip"),
    error = function(e) {
      stop(
        "Could not save the processed analysis RDS: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!file.exists(output_paths$analysis_data) ||
      file.info(output_paths$analysis_data)$size <= 0L) {
    stop("The processed analysis RDS was not generated.", call. = FALSE)
  }

  system_sample_summary <- system_sample_summary[order(
    system_sample_summary$system_id
  ), , drop = FALSE]
  analysis_missingness_summary <- analysis_missingness_summary[order(
    analysis_missingness_summary$system_id,
    analysis_missingness_summary$variable_name
  ), , drop = FALSE]
  analysis_weight_summary <- analysis_weight_summary[order(
    analysis_weight_summary$system_id
  ), , drop = FALSE]
  analysis_exclusion_summary <- analysis_exclusion_summary[order(
    analysis_exclusion_summary$system_id,
    match(analysis_exclusion_summary$eligibility_flag, eligibility_names)
  ), , drop = FALSE]

  write_csv_checked(system_sample_summary, output_paths$system_sample_summary)
  write_csv_checked(analysis_variable_dictionary, output_paths$variable_dictionary)
  write_csv_checked(analysis_missingness_summary, output_paths$missingness_summary)
  write_csv_checked(analysis_weight_summary, output_paths$weight_summary)
  write_csv_checked(analysis_exclusion_summary, output_paths$exclusion_summary)

  session_lines <- c(
    "TIMSS 2023 Grade 8 analysis-data preparation session information",
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Project root: ", project_root),
    paste0("R version: ", R.version.string),
    paste0("Platform: ", R.version$platform),
    paste0("Operating system: ", Sys.info()[["sysname"]], " ", Sys.info()[["release"]]),
    paste0("Locale: ", paste(Sys.getlocale(), collapse = " | ")),
    paste0("here version: ", as.character(utils::packageVersion("here"))),
    paste0("readr version: ", as.character(utils::packageVersion("readr"))),
    "",
    "Complete sessionInfo() output:",
    capture.output(sessionInfo())
  )
  write_text_checked(session_lines, output_paths$session_info)

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
    1L, "Target BSG file count",
    if (length(bsg_files) == 47L) "PASS" else "FAIL",
    paste0("Files found=", length(bsg_files), "; expected=47")
  )
  add_validation(
    2L, "Successful system reads",
    if (sum(file_read_summary$read_success) == 47L) "PASS" else "FAIL",
    paste0("Successful reads=", sum(file_read_summary$read_success), "/47")
  )
  add_validation(
    3L, "Systems retained in processed RDS",
    if (length(unique(analysis_data$system_id)) == 47L) "PASS" else "FAIL",
    paste0("Systems in RDS=", length(unique(analysis_data$system_id)), "/47")
  )
  core_coverage_counts <- tapply(
    analysis_missingness_summary$valid_n > 0L,
    analysis_missingness_summary$variable_name,
    sum
  )
  add_validation(
    4L, "Selected predictor and control coverage",
    if (all(core_coverage_counts[c("BSBGHER", "BSBG05A", "BSBG03")] == 47L)) {
      "PASS"
    } else {
      "WARNING"
    },
    paste0(
      "Systems with valid BSBGHER/BSBG05A/BSBG03=",
      paste(core_coverage_counts[c("BSBGHER", "BSBG05A", "BSBG03")], collapse = "/")
    )
  )
  structural_pv_pass <- all(all_pv_names %in% names(analysis_data))
  add_validation(
    5L, "Four content-domain PV structures",
    if (structural_pv_pass) "PASS" else "FAIL",
    paste0("Expected PV columns=20; present=", sum(all_pv_names %in% names(analysis_data)))
  )
  add_validation(
    6L, "Five PVs per content domain",
    if (all(vapply(pv_groups, length, integer(1)) == 5L)) "PASS" else "FAIL",
    paste0(
      "D&P/Number/Algebra/Geometry counts=",
      paste(vapply(pv_groups, length, integer(1)), collapse = "/")
    )
  )
  add_validation(
    7L, "Composite student-key uniqueness",
    if (!anyDuplicated(analysis_data$student_uid) &&
        students_linked_to_multiple_schools == 0L) "PASS" else "FAIL",
    paste0(
      "Duplicate student_uid=", sum(duplicated(analysis_data$student_uid)),
      "; system-student IDs linked to multiple schools=",
      students_linked_to_multiple_schools
    )
  )
  variation_by_system <- function(variable_name) {
    tapply(
      analysis_data[[variable_name]],
      analysis_data$system_id,
      count_unique_nonmissing
    )
  }
  her_variation <- variation_by_system("BSBGHER")
  ict_variation <- variation_by_system("ict_access")
  add_validation(
    8L, "HER and ICT valid variation",
    if (all(her_variation >= 2L) && all(ict_variation >= 2L)) "PASS" else "WARNING",
    paste0(
      "Systems with HER/ICT variation=",
      sum(her_variation >= 2L), "/", sum(ict_variation >= 2L), "; expected=47/47"
    )
  )
  language_variation <- variation_by_system("language_at_home")
  add_validation(
    9L, "Language-control categories",
    if (all(language_variation >= 2L)) "PASS" else "WARNING",
    paste0("Systems with at least two valid language categories=", sum(language_variation >= 2L), "/47")
  )
  weight_invalid_n <- sum(
    is.na(analysis_data$analysis_weight_candidate) |
      !is.finite(analysis_data$analysis_weight_candidate) |
      analysis_data$analysis_weight_candidate <= 0
  )
  add_validation(
    10L, "Weight validity",
    if (weight_invalid_n == 0L) "PASS" else "FAIL",
    paste0("Missing, non-finite, or non-positive candidate weights=", weight_invalid_n)
  )
  contribution_pass <- all(
    analysis_weight_summary$absolute_deviation_from_target_500 < 1e-6
  )
  add_validation(
    11L, "Full-sample equal-system candidate contribution",
    if (contribution_pass) "PASS" else "FAIL",
    paste0(
      "Systems with candidate weight sum equal to 500 within tolerance=",
      sum(analysis_weight_summary$absolute_deviation_from_target_500 < 1e-6),
      "/47"
    )
  )
  high_loss_rows <- analysis_exclusion_summary[
    analysis_exclusion_summary$exclusion_percentage > 20 &
      !analysis_exclusion_summary$no_eligible_cases,
    ,
    drop = FALSE
  ]
  add_validation(
    12L, "Complete-case sample loss",
    if (nrow(high_loss_rows) == 0L) "PASS" else "WARNING",
    paste0("System-eligibility rows with more than 20% loss and some retained cases=", nrow(high_loss_rows))
  )
  no_case_rows <- analysis_exclusion_summary[
    analysis_exclusion_summary$no_eligible_cases,
    ,
    drop = FALSE
  ]
  add_validation(
    13L, "Systems with eligible cases",
    if (nrow(no_case_rows) == 0L) "PASS" else "WARNING",
    paste0(
      "System-eligibility rows with zero eligible cases=", nrow(no_case_rows),
      "; affected systems=", collapse_unique(no_case_rows$system_id)
    )
  )
  few_school_rows <- analysis_exclusion_summary[
    analysis_exclusion_summary$very_few_retained_schools,
    ,
    drop = FALSE
  ]
  add_validation(
    14L, "Retained schools",
    if (nrow(few_school_rows) == 0L) "PASS" else "WARNING",
    paste0("System-eligibility rows retaining fewer than 10 schools=", nrow(few_school_rows))
  )
  add_validation(
    15L, "Processed RDS generated",
    if (file.exists(output_paths$analysis_data) &&
        file.info(output_paths$analysis_data)$size > 0L) "PASS" else "FAIL",
    paste0(
      "RDS=", to_relative_path(output_paths$analysis_data),
      "; size_bytes=", file.info(output_paths$analysis_data)$size
    )
  )

  required_pre_summary_outputs <- output_paths[c(
    "system_sample_summary", "variable_dictionary", "missingness_summary",
    "weight_summary", "exclusion_summary", "session_info"
  )]
  output_exists <- vapply(required_pre_summary_outputs, function(path) {
    file.exists(path) && file.info(path)$size > 0L
  }, logical(1))
  add_validation(
    16L, "Required aggregate and session outputs generated",
    if (all(output_exists)) "PASS" else "FAIL",
    paste0(
      paste(names(output_exists), ifelse(output_exists, "present", "missing"), sep = "="),
      collapse = "; "
    )
  )

  if (any(validation$status == "FAIL")) {
    final_status <- "FAIL"
  } else if (any(validation$status == "WARNING") || length(warning_log) > 0L) {
    final_status <- "PASS WITH WARNINGS"
  } else {
    final_status <- "PASS"
  }

  eligibility_totals <- do.call(rbind, lapply(eligibility_names, function(flag_name) {
    data.frame(
      eligibility_flag = flag_name,
      eligible_students = sum(analysis_data[[flag_name]]),
      systems_with_eligible_cases = sum(tapply(
        analysis_data[[flag_name]],
        analysis_data$system_id,
        any
      )),
      stringsAsFactors = FALSE
    )
  }))

  summary_lines <- c(
    "TIMSS 2023 Grade 8 analysis-data preparation summary",
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Project root: ", project_root),
    "",
    "Locked preparation specification:",
    "- Raw input: BSG only.",
    "- Target systems retained in the local RDS: 47.",
    "- Primary HER variable: BSBGHER.",
    "- Primary ICT variable: BSBG05A, represented by ict_access.",
    "- Language control: BSBG03, represented by language_at_home.",
    "- Grade validation variable: IDGRADER; excluded from models.",
    "- Content-domain PVs: five separate PVs for D&P, Number, Algebra, and Geometry and Measurement.",
    "- Student-level complete cases are marked by flags and are not deleted from the RDS.",
    "- No regression, multilevel model, PV pooling, or figure was produced.",
    "",
    "Weight handling:",
    "- TOTWGT and SENWGT are retained unchanged.",
    "- Candidate formula: analysis_weight_candidate = SENWGT * 500 / system sum(SENWGT).",
    "- The candidate preserves relative within-system weights and sets the full-sample system total to 500.",
    "- No post-complete-case weight renormalisation was performed.",
    "- R/04_main_analysis.R must verify the final multilevel and design-variance implementation before using the candidate weight.",
    "",
    "PV-difference handling:",
    "- Differences use only the same plausible-value index across domains.",
    "- No plausible values were averaged across the five imputations.",
    "- The mean in dp_minus_other_mean_pv1-pv5 is the within-index mean of Number, Algebra, and Geometry and Measurement.",
    "- Five model estimates must be pooled in R/04_main_analysis.R.",
    "",
    paste0("BSG files found and read: ", nrow(file_read_summary), "/47"),
    paste0("Systems stored in the RDS: ", length(unique(analysis_data$system_id))),
    paste0("Students stored in the RDS: ", nrow(analysis_data)),
    paste0("Schools represented in the RDS: ", length(unique(analysis_data$school_uid))),
    paste0(
      "Systems with all four nonempty content-domain PV groups: ",
      sum(system_sample_summary$all_four_domain_PV_groups_available), "/47"
    ),
    paste0(
      "Systems without all four nonempty content-domain PV groups: ",
      collapse_unique(system_sample_summary$system_id[
        !system_sample_summary$all_four_domain_PV_groups_available
      ])
    ),
    "",
    "Eligibility totals:",
    capture.output(print(eligibility_totals, row.names = FALSE)),
    "",
    "Automatic validation:",
    capture.output(print(validation, row.names = FALSE)),
    "",
    paste0("Recorded warnings: ", length(warning_log)),
    if (length(warning_log) > 0L) {
      paste0("- ", warning_log)
    } else {
      "- None"
    },
    "",
    paste0("Final preparation status: ", final_status),
    "",
    "Output files:",
    paste0("- ", vapply(output_paths, to_relative_path, character(1))),
    "",
    "Privacy safeguards:",
    "- Actual school and student identifiers appear only in the local-only RDS.",
    "- Public CSV outputs contain only system-level aggregates and variable definitions.",
    "- No raw RData file was modified.",
    "- The local RDS must not be uploaded to a web AI service or committed to GitHub."
  )
  write_text_checked(summary_lines, output_paths$prepare_summary)

  all_required_outputs <- output_paths
  all_outputs_exist <- vapply(all_required_outputs, function(path) {
    file.exists(path) && file.info(path)$size > 0L
  }, logical(1))
  if (!all(all_outputs_exist)) {
    stop(
      "One or more required preparation outputs were not generated: ",
      paste(names(all_outputs_exist)[!all_outputs_exist], collapse = ", "),
      call. = FALSE
    )
  }
  if (identical(final_status, "FAIL")) {
    stop(
      "Analysis-data preparation completed with a FAIL status. Review prepare_analysis_summary.txt before continuing.",
      call. = FALSE
    )
  }

  message("Analysis-data preparation completed.")
  message("Systems stored: ", length(unique(analysis_data$system_id)))
  message("Student records stored locally: ", nrow(analysis_data))
  message("Final preparation status: ", final_status)
  message("Local-only RDS: ", to_relative_path(output_paths$analysis_data))
  message("Aggregate outputs:")
  for (name in c(
    "system_sample_summary", "variable_dictionary", "missingness_summary",
    "weight_summary", "exclusion_summary", "prepare_summary", "session_info"
  )) {
    message("  - ", to_relative_path(output_paths[[name]]))
  }
})
