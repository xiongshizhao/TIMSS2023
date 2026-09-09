# TIMSS 2023 Grade 8: system harmonisation and pre-merge audit
#
# Scope:
#   - audits candidate-variable availability, schema, labels, missingness,
#     plausible-value structure, weights/design variables, and BSA/BSG keys;
#   - processes one system and one RData file at a time;
#   - never saves student records or actual identifiers;
#   - never performs a full BSA/BSG merge, modelling, PV averaging, or pooling.
# Revision 2026-09-04:
#   - prevents NA audit-group rows from entering the TOTWGT validation subset;
#   - compares value-label mapping signatures between systems rather than
#     treating repeated same-label codes within every system as a conflict;
#   - uses English-only messages and validation labels.

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

  project_root <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)
  message("Project root: ", project_root)
  message(paste(
    "Round 2A performs harmonisation and pre-merge auditing only;",
    "it does not save student records or merged data."
  ))

  relative_paths <- list(
    bsa_dir = "data_raw/local_only/BSA_achievement_grade8",
    bsg_dir = "data_raw/local_only/BSG_student_questionnaire_grade8",
    metadata_dir = "data_metadata",
    local_metadata_dir = "data_metadata/local_only",
    archive_dir = "data_metadata/local_only/harmonisation_archive"
  )
  paths <- lapply(relative_paths, here::here)

  input_paths <- list(
    raw_file_manifest = here::here("data_metadata", "raw_file_manifest.csv"),
    variable_inventory = here::here(
      "data_metadata", "local_only", "variable_inventory.csv"
    ),
    country_variable_coverage = here::here(
      "data_metadata", "country_variable_coverage.csv"
    ),
    candidate_variable_hits = here::here(
      "data_metadata", "candidate_variable_hits.csv"
    ),
    inventory_summary = here::here(
      "data_metadata", "local_only", "inventory_summary.txt"
    ),
    inventory_session_info = here::here(
      "data_metadata", "local_only", "session_info.txt"
    )
  )

  output_paths <- list(
    system_file_pairing = here::here(
      "data_metadata", "system_file_pairing.csv"
    ),
    candidate_variable_system_coverage = here::here(
      "data_metadata", "candidate_variable_system_coverage.csv"
    ),
    premerge_key_audit = here::here(
      "data_metadata", "premerge_key_audit.csv"
    ),
    pv_weight_design_audit = here::here(
      "data_metadata", "pv_weight_design_audit.csv"
    ),
    system_eligibility_premerge = here::here(
      "data_metadata", "system_eligibility_premerge.csv"
    ),
    variable_schema_audit = here::here(
      "data_metadata", "local_only", "variable_schema_audit.csv"
    ),
    value_label_audit = here::here(
      "data_metadata", "local_only", "value_label_audit.csv"
    ),
    candidate_missingness_distribution = here::here(
      "data_metadata", "local_only", "candidate_missingness_distribution.csv"
    ),
    harmonisation_issue_log = here::here(
      "data_metadata", "local_only", "harmonisation_issue_log.csv"
    ),
    harmonisation_summary = here::here(
      "data_metadata", "local_only", "harmonisation_summary.txt"
    ),
    session_info_harmonisation = here::here(
      "data_metadata", "local_only", "session_info_harmonisation.txt"
    )
  )

  for (directory in paths[c("bsa_dir", "bsg_dir")]) {
    if (!dir.exists(directory)) {
      stop("Required raw-data directory is missing: ", directory, call. = FALSE)
    }
  }
  for (directory in paths[c("metadata_dir", "local_metadata_dir")]) {
    if (!dir.exists(directory)) {
      ok <- dir.create(directory, recursive = TRUE, showWarnings = FALSE)
      if (!ok || !dir.exists(directory)) {
        stop("Could not create output directory: ", directory, call. = FALSE)
      }
    }
  }

  missing_inputs <- names(input_paths)[!vapply(input_paths, file.exists, logical(1))]
  if (length(missing_inputs) > 0L) {
    stop(
      "Required first-round metadata file(s) are missing: ",
      paste(missing_inputs, collapse = ", "),
      call. = FALSE
    )
  }

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

  check_directory_writable <- function(directory, label) {
    test_file <- tempfile(pattern = "write_test_", tmpdir = directory)
    ok <- tryCatch({
      writeLines("test", test_file, useBytes = TRUE)
      file.exists(test_file)
    }, error = function(e) FALSE)
    if (file.exists(test_file)) unlink(test_file)
    if (!ok) stop("Output directory is not writable: ", label, call. = FALSE)
    invisible(TRUE)
  }
  check_directory_writable(paths$metadata_dir, relative_paths$metadata_dir)
  check_directory_writable(
    paths$local_metadata_dir, relative_paths$local_metadata_dir
  )

  read_metadata_csv <- function(path, label) {
    out <- tryCatch(
      readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE,
        name_repair = "minimal"
      ),
      error = function(e) {
        stop(
          "Could not read first-round metadata: ", label, "; ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    as.data.frame(out, stringsAsFactors = FALSE)
  }

  assert_columns <- function(data, required, label) {
    absent <- setdiff(required, names(data))
    if (length(absent) > 0L) {
      stop(
        label, " is missing required column(s): ",
        paste(absent, collapse = ", "),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }

  raw_manifest <- read_metadata_csv(
    input_paths$raw_file_manifest, "raw_file_manifest.csv"
  )
  variable_inventory_first <- read_metadata_csv(
    input_paths$variable_inventory, "variable_inventory.csv"
  )
  first_coverage <- read_metadata_csv(
    input_paths$country_variable_coverage, "country_variable_coverage.csv"
  )
  candidate_hits_first <- read_metadata_csv(
    input_paths$candidate_variable_hits, "candidate_variable_hits.csv"
  )
  inventory_summary_lines <- readLines(
    input_paths$inventory_summary, warn = FALSE, encoding = "UTF-8"
  )
  inventory_session_lines <- readLines(
    input_paths$inventory_session_info, warn = FALSE, encoding = "UTF-8"
  )

  assert_columns(
    raw_manifest,
    c(
      "relative_path", "file_name", "file_type", "country_system_code",
      "object_name", "object_kind", "rows", "columns",
      "main_candidate_status", "read_success", "read_status"
    ),
    "raw_file_manifest.csv"
  )
  assert_columns(
    variable_inventory_first,
    c(
      "file_type", "country_system_code", "source_file", "source_object",
      "variable_name", "variable_label", "r_class", "storage_type",
      "is_factor", "is_labelled", "has_value_labels", "value_label_count",
      "has_missing_attribute", "missing_attribute_type"
    ),
    "variable_inventory.csv"
  )
  assert_columns(
    first_coverage,
    c(
      "country_system_code", "file_type", "candidate_category",
      "candidate_subcategory", "variable_name", "present", "candidate_status"
    ),
    "country_variable_coverage.csv"
  )
  assert_columns(
    candidate_hits_first,
    c(
      "country_system_code", "file_type", "source_file", "source_object",
      "variable_name", "candidate_category", "candidate_subcategory",
      "candidate_status", "official_source"
    ),
    "candidate_variable_hits.csv"
  )

  if (!any(grepl(
    "^Final overall validation:[[:space:]]*PASS$",
    inventory_summary_lines
  ))) {
    stop(
      "inventory_summary.txt does not confirm first-round ",
      "Final overall validation: PASS.",
      call. = FALSE
    )
  }

  raw_manifest$file_type <- toupper(as.character(raw_manifest$file_type))
  raw_manifest$country_system_code <- toupper(
    as.character(raw_manifest$country_system_code)
  )
  raw_manifest$read_success <- as.logical(raw_manifest$read_success)
  raw_manifest$full_path <- vapply(
    raw_manifest$relative_path,
    function(x) here::here(as.character(x)),
    character(1)
  )

  manifest_main <- raw_manifest[
    raw_manifest$read_success %in% TRUE &
      raw_manifest$main_candidate_status == "single_tabular_candidate",
    ,
    drop = FALSE
  ]
  if (nrow(manifest_main) == 0L) {
    stop("No usable main tabular object was found in the first-round manifest.", call. = FALSE)
  }
  missing_raw_files <- manifest_main$relative_path[
    !file.exists(manifest_main$full_path)
  ]
  if (length(missing_raw_files) > 0L) {
    stop(
      "Raw file(s) recorded by the manifest do not exist: ",
      paste(utils::head(missing_raw_files, 10L), collapse = ", "),
      call. = FALSE
    )
  }

  duplicate_manifest_keys <- duplicated(
    manifest_main[c("country_system_code", "file_type")]
  ) | duplicated(
    manifest_main[c("country_system_code", "file_type")],
    fromLast = TRUE
  )
  if (any(duplicate_manifest_keys)) {
    stop(
      paste(
        "The manifest contains more than one main file for a",
        "system/file_type combination; the audit cannot continue safely."
      ),
      call. = FALSE
    )
  }

  systems <- sort(unique(manifest_main$country_system_code))
  actual_system_count <- length(systems)
  message("First-round metadata read successfully. Actual systems: ", actual_system_count)
  message("Main BSA files: ", sum(manifest_main$file_type == "BSA"))
  message("Main BSG files: ", sum(manifest_main$file_type == "BSG"))

  clean_scalar <- function(x) {
    if (is.null(x) || length(x) == 0L) return(NA_character_)
    value <- paste(as.character(x), collapse = " | ")
    if (!nzchar(value)) NA_character_ else value
  }

  blank_if_na <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }

  definition_key <- function(variable_name, category, subcategory) {
    paste(
      toupper(blank_if_na(variable_name)),
      blank_if_na(category),
      blank_if_na(subcategory),
      sep = "\r"
    )
  }

  status_rank <- c(
    official_document_confirmed = 1L,
    official_document_confirmed_candidate = 2L,
    keyword_only_candidate = 3L,
    unresolved_multiple_keyword_categories = 4L,
    unresolved_not_found_in_inventory = 5L,
    unresolved = 6L
  )
  choose_registry_status <- function(x) {
    values <- unique(blank_if_na(x))
    values <- values[nzchar(values)]
    if (length(values) == 0L) return("unresolved")
    ranks <- status_rank[values]
    ranks[is.na(ranks)] <- max(status_rank) + 1L
    values[which.min(ranks)]
  }

  registry_base <- unique(first_coverage[c(
    "variable_name", "candidate_category", "candidate_subcategory"
  )])
  registry_base$variable_name <- toupper(
    as.character(registry_base$variable_name)
  )
  registry_base$definition_key <- definition_key(
    registry_base$variable_name,
    registry_base$candidate_category,
    registry_base$candidate_subcategory
  )

  candidate_hits_first$variable_name <- toupper(
    as.character(candidate_hits_first$variable_name)
  )
  candidate_hits_first$definition_key <- definition_key(
    candidate_hits_first$variable_name,
    candidate_hits_first$candidate_category,
    candidate_hits_first$candidate_subcategory
  )
  first_coverage$variable_name <- toupper(
    as.character(first_coverage$variable_name)
  )
  first_coverage$definition_key <- definition_key(
    first_coverage$variable_name,
    first_coverage$candidate_category,
    first_coverage$candidate_subcategory
  )
  status_records <- rbind(
    candidate_hits_first[, c("definition_key", "candidate_status")],
    first_coverage[, c("definition_key", "candidate_status")]
  )
  status_map <- tapply(
    status_records$candidate_status,
    status_records$definition_key,
    choose_registry_status
  )
  source_map <- tapply(
    blank_if_na(candidate_hits_first$official_source),
    candidate_hits_first$definition_key,
    function(x) {
      values <- sort(unique(x[nzchar(x)]))
      if (length(values) == 0L) NA_character_ else paste(values, collapse = " | ")
    }
  )
  registry_base$registry_status <- as.vector(
    status_map[match(registry_base$definition_key, names(status_map))],
    mode = "character"
  )
  registry_base$registry_status[is.na(registry_base$registry_status)] <- "unresolved"
  registry_base$official_source <- as.vector(
    source_map[match(registry_base$definition_key, names(source_map))],
    mode = "character"
  )
  registry_base$official_source[!nzchar(blank_if_na(
    registry_base$official_source
  ))] <- NA_character_
  source_by_variable <- tapply(
    blank_if_na(candidate_hits_first$official_source),
    candidate_hits_first$variable_name,
    function(x) {
      values <- sort(unique(x[nzchar(x)]))
      if (length(values) == 0L) NA_character_ else paste(values, collapse = " | ")
    }
  )
  missing_source <- is.na(registry_base$official_source)
  registry_base$official_source[missing_source] <- as.vector(
    source_by_variable[
      match(
        registry_base$variable_name[missing_source],
        names(source_by_variable)
      )
    ],
    mode = "character"
  )

  core_categories <- c(
    "identifiers", "plausible_values", "weights", "replication"
  )
  registry_base$gating_role <- ifelse(
    registry_base$registry_status == "official_document_confirmed" &
      registry_base$candidate_category %in% core_categories,
    "CORE_TECHNICAL",
    ifelse(
      registry_base$registry_status == "official_document_confirmed_candidate" &
        registry_base$candidate_category %in% c("home_resources", "ICT"),
      "RESEARCH_CANDIDATE",
      ifelse(
        registry_base$registry_status %in% c(
          "keyword_only_candidate", "unresolved_multiple_keyword_categories"
        ),
        "NON_GATING_KEYWORD",
        "NON_GATING_CANDIDATE"
      )
    )
  )
  candidate_registry <- registry_base[order(
    registry_base$candidate_category,
    registry_base$candidate_subcategory,
    registry_base$variable_name
  ), , drop = FALSE]
  row.names(candidate_registry) <- NULL
  candidate_names <- sort(unique(candidate_registry$variable_name))
  message("Distinct candidate variable names: ", length(candidate_names))
  message(
    "Broad keyword/unresolved candidate definitions: ",
    sum(candidate_registry$gating_role == "NON_GATING_KEYWORD")
  )

  # Conservative, non-exclusionary screening thresholds agreed for this audit.
  low_valid_n_threshold <- 30L
  low_valid_proportion_threshold <- 0.10

  normalise_label <- function(x) {
    x <- blank_if_na(x)
    x <- tolower(trimws(gsub("[[:space:]]+", " ", x)))
    x <- gsub("[[:punct:]]+", " ", x)
    trimws(gsub("[[:space:]]+", " ", x))
  }

  first_attribute <- function(x, candidates) {
    for (name in candidates) {
      value <- attr(x, name, exact = TRUE)
      if (!is.null(value)) return(value)
    }
    NULL
  }

  atomic_values <- function(x) {
    out <- tryCatch(unclass(x), error = function(e) x)
    attributes(out) <- NULL
    as.vector(out)
  }

  serialise_vector <- function(x, sort_values = TRUE) {
    if (is.null(x) || length(x) == 0L) return("")
    values <- as.character(x)
    values[is.na(values)] <- "<NA>"
    if (sort_values) values <- sort(values)
    paste(values, collapse = " || ")
  }

  value_label_records <- function(x) {
    labels_attribute <- first_attribute(x, c("labels", "value.labels"))
    if (!is.null(labels_attribute) && length(labels_attribute) > 0L) {
      codes <- as.character(unname(labels_attribute))
      labels <- names(labels_attribute)
      if (is.null(labels)) labels <- rep("", length(codes))
      return(data.frame(
        category_order = seq_along(codes),
        category_code = codes,
        category_label = as.character(labels),
        label_source = "value_labels_attribute",
        stringsAsFactors = FALSE
      ))
    }
    if (is.factor(x)) {
      levels_x <- levels(x)
      return(data.frame(
        category_order = seq_along(levels_x),
        category_code = as.character(seq_along(levels_x)),
        category_label = as.character(levels_x),
        label_source = "factor_levels",
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      category_order = integer(),
      category_code = character(),
      category_label = character(),
      label_source = character(),
      stringsAsFactors = FALSE
    )
  }

  missing_attribute_details <- function(x) {
    attrs <- attributes(x)
    attr_names <- if (is.null(attrs)) character() else names(attrs)
    candidate_names_local <- unique(c(
      intersect(
        attr_names,
        c("na_values", "na_range", "missing.values", "missing.range", "missings")
      ),
      grep("^(na_|missing)", attr_names, value = TRUE, ignore.case = TRUE)
    ))
    candidate_names_local <- sort(candidate_names_local)
    values <- first_attribute(x, c("na_values", "missing.values", "missings"))
    range <- first_attribute(x, c("na_range", "missing.range"))
    signature_parts <- character()
    if (length(candidate_names_local) > 0L) {
      for (name in candidate_names_local) {
        signature_parts <- c(
          signature_parts,
          paste0(name, "=", serialise_vector(attr(x, name, exact = TRUE)))
        )
      }
    }
    list(
      attribute_names = paste(candidate_names_local, collapse = " | "),
      values = values,
      range = range,
      signature = paste(signature_parts, collapse = " ; ")
    )
  }

  code_is_in <- function(raw, candidates) {
    if (is.null(candidates) || length(candidates) == 0L) {
      return(rep(FALSE, length(raw)))
    }
    as.character(raw) %in% as.character(candidates)
  }

  declared_missing_mask <- function(x, details) {
    raw <- atomic_values(x)
    mask <- is.na(x)
    if (length(mask) != length(raw)) mask <- is.na(raw)
    mask <- mask | code_is_in(raw, details$values)
    if (!is.null(details$range) && length(details$range) >= 2L) {
      numeric_raw <- suppressWarnings(as.numeric(raw))
      numeric_range <- suppressWarnings(as.numeric(details$range[1:2]))
      if (all(is.finite(numeric_range))) {
        mask <- mask | (
          !is.na(numeric_raw) & numeric_raw >= min(numeric_range) &
            numeric_raw <= max(numeric_range)
        )
      }
    }
    mask[is.na(mask)] <- FALSE
    mask
  }

  possible_missing_regex <- paste0(
    "missing|omitted|not[[:space:]_-]*administered|not[[:space:]_-]*reached|",
    "invalid|no[[:space:]_-]*response|not[[:space:]_-]*available|",
    "not[[:space:]_-]*applicable|system[[:space:]_-]*missing"
  )

  possible_missing_label_mask <- function(x, label_records) {
    raw <- atomic_values(x)
    if (nrow(label_records) == 0L) return(rep(FALSE, length(raw)))
    flagged <- grepl(
      possible_missing_regex,
      label_records$category_label,
      ignore.case = TRUE,
      perl = TRUE
    )
    codes <- label_records$category_code[flagged]
    code_is_in(raw, codes)
  }

  value_label_signatures <- function(records) {
    if (nrow(records) == 0L) {
      return(list(mapping = "", order = "", count = 0L))
    }
    normalized <- normalise_label(records$category_label)
    mapping_pairs <- paste0(records$category_code, "=", normalized)
    list(
      mapping = paste(sort(mapping_pairs), collapse = " || "),
      order = paste(mapping_pairs, collapse = " || "),
      count = nrow(records)
    )
  }

  quality_flag_from_counts <- function(unweighted_n, valid_n) {
    if (unweighted_n <= 0L || valid_n == 0L) return("ALL_MISSING")
    if (valid_n < low_valid_n_threshold) return("LOW_VALID_N")
    if (valid_n / unweighted_n < low_valid_proportion_threshold) {
      return("LOW_VALID_PROPORTION")
    }
    "OK"
  }

  empty_occurrence_schema <- function() {
    data.frame(
      country_system_code = character(), file_type = character(),
      source_file = character(), source_object = character(),
      variable_name = character(), registry_categories = character(),
      registry_statuses = character(), r_class = character(),
      storage_type = character(), is_numeric = logical(),
      is_factor = logical(), is_ordered = logical(), is_labelled = logical(),
      variable_label_exact = character(), variable_label_normalized = character(),
      value_label_count = integer(), value_label_signature = character(),
      value_label_order_signature = character(), factor_levels_signature = character(),
      missing_attribute_type = character(), missing_attribute_signature = character(),
      unweighted_n = integer(), valid_n = integer(), missing_n = integer(),
      missing_percentage = numeric(), possible_special_missing_n = integer(),
      unique_nonmissing_n = integer(), quality_flag = character(),
      schema_signature = character(), variable_error = character(),
      stringsAsFactors = FALSE
    )
  }

  empty_value_labels <- function() {
    data.frame(
      country_system_code = character(), file_type = character(),
      source_file = character(), source_object = character(),
      variable_name = character(), category_order = integer(),
      category_code = character(), category_label = character(),
      category_label_normalized = character(), label_source = character(),
      is_declared_missing = logical(), possible_missing_label = logical(),
      category_observed = logical(), observed_category_n = integer(),
      observed_category_proportion_of_valid = numeric(),
      stringsAsFactors = FALSE
    )
  }

  empty_distribution <- function() {
    data.frame(
      country_system_code = character(), file_type = character(),
      variable_name = character(), registry_categories = character(),
      summary_type = character(), category_code = character(),
      category_label = character(), unweighted_n = integer(),
      valid_n = integer(), missing_n = integer(), missing_percentage = numeric(),
      declared_or_R_missing_n = integer(), possible_special_missing_n = integer(),
      unique_nonmissing_n = integer(), category_n = integer(),
      category_proportion_of_valid = numeric(), mean = numeric(), sd = numeric(),
      minimum = numeric(), p25 = numeric(), median = numeric(),
      p75 = numeric(), maximum = numeric(), quality_flag = character(),
      stringsAsFactors = FALSE
    )
  }

  empty_pv_design <- function() {
    data.frame(
      country_system_code = character(), file_type = character(),
      audit_category = character(), audit_group = character(),
      variable_name = character(), analysis_role = character(),
      expected_count = integer(), present_count = integer(),
      present_variables = character(), missing_variables = character(),
      all_present = logical(), all_numeric = logical(),
      all_missing_variable_count = integer(), nonmissing_n_min = integer(),
      nonmissing_n_max = integer(), schema_consistent_across_systems = logical(),
      audit_status = character(), notes = character(),
      stringsAsFactors = FALSE
    )
  }

  empty_file_status <- function() {
    data.frame(
      country_system_code = character(), file_type = character(),
      source_file = character(), source_object = character(),
      read_success = logical(), warning_count = integer(),
      warning_message = character(), error_message = character(),
      rows = integer(), columns = integer(),
      stringsAsFactors = FALSE
    )
  }

  registry_for_variable <- function(variable_name) {
    rows <- candidate_registry[
      candidate_registry$variable_name == toupper(variable_name),
      ,
      drop = FALSE
    ]
    list(
      categories = paste(sort(unique(rows$candidate_category)), collapse = " | "),
      statuses = paste(sort(unique(rows$registry_status)), collapse = " | ")
    )
  }

  summarize_candidate_variable <- function(
      x, system_code, file_type, source_file, source_object, variable_name) {
    registry_info <- registry_for_variable(variable_name)
    label_exact <- clean_scalar(first_attribute(
      x, c("label", "variable.label", "var.label")
    ))
    label_normalized <- normalise_label(label_exact)
    label_records <- value_label_records(x)
    label_signatures <- value_label_signatures(label_records)
    missing_details <- missing_attribute_details(x)
    missing_mask <- declared_missing_mask(x, missing_details)
    possible_mask <- possible_missing_label_mask(x, label_records)
    raw <- atomic_values(x)
    unweighted_n <- length(raw)
    valid_mask <- !missing_mask
    valid_n <- sum(valid_mask)
    missing_n <- unweighted_n - valid_n
    possible_special_missing_n <- sum(possible_mask & valid_mask)
    valid_raw <- raw[valid_mask]
    unique_nonmissing_n <- length(unique(valid_raw))
    quality_flag <- quality_flag_from_counts(unweighted_n, valid_n)
    factor_levels_signature <- if (is.factor(x)) {
      paste(levels(x), collapse = " || ")
    } else {
      ""
    }
    is_labelled <- inherits(
      x, c("haven_labelled", "haven_labelled_spss", "labelled")
    ) || !is.null(first_attribute(x, c("labels", "value.labels")))
    schema_signature <- paste(
      paste(class(x), collapse = " | "),
      typeof(x),
      is.numeric(x),
      is.factor(x),
      is.ordered(x),
      is_labelled,
      label_signatures$count,
      factor_levels_signature,
      sep = "\r"
    )

    schema <- data.frame(
      country_system_code = system_code,
      file_type = file_type,
      source_file = source_file,
      source_object = source_object,
      variable_name = toupper(variable_name),
      registry_categories = registry_info$categories,
      registry_statuses = registry_info$statuses,
      r_class = paste(class(x), collapse = " | "),
      storage_type = typeof(x),
      is_numeric = is.numeric(x),
      is_factor = is.factor(x),
      is_ordered = is.ordered(x),
      is_labelled = is_labelled,
      variable_label_exact = label_exact,
      variable_label_normalized = label_normalized,
      value_label_count = as.integer(label_signatures$count),
      value_label_signature = label_signatures$mapping,
      value_label_order_signature = label_signatures$order,
      factor_levels_signature = factor_levels_signature,
      missing_attribute_type = if (nzchar(missing_details$attribute_names)) {
        missing_details$attribute_names
      } else {
        NA_character_
      },
      missing_attribute_signature = missing_details$signature,
      unweighted_n = as.integer(unweighted_n),
      valid_n = as.integer(valid_n),
      missing_n = as.integer(missing_n),
      missing_percentage = if (unweighted_n > 0L) 100 * missing_n / unweighted_n else NA_real_,
      possible_special_missing_n = as.integer(possible_special_missing_n),
      unique_nonmissing_n = as.integer(unique_nonmissing_n),
      quality_flag = quality_flag,
      schema_signature = schema_signature,
      variable_error = NA_character_,
      stringsAsFactors = FALSE
    )

    value_labels <- empty_value_labels()
    if (nrow(label_records) > 0L) {
      declared_values <- as.character(missing_details$values)
      range_numeric <- suppressWarnings(as.numeric(missing_details$range))
      code_numeric <- suppressWarnings(as.numeric(label_records$category_code))
      in_range <- rep(FALSE, nrow(label_records))
      if (length(range_numeric) >= 2L && all(is.finite(range_numeric[1:2]))) {
        in_range <- !is.na(code_numeric) &
          code_numeric >= min(range_numeric[1:2]) &
          code_numeric <= max(range_numeric[1:2])
      }
      value_labels <- data.frame(
        country_system_code = system_code,
        file_type = file_type,
        source_file = source_file,
        source_object = source_object,
        variable_name = toupper(variable_name),
        category_order = as.integer(label_records$category_order),
        category_code = label_records$category_code,
        category_label = label_records$category_label,
        category_label_normalized = normalise_label(label_records$category_label),
        label_source = label_records$label_source,
        is_declared_missing = label_records$category_code %in% declared_values | in_range,
        possible_missing_label = grepl(
          possible_missing_regex,
          label_records$category_label,
          ignore.case = TRUE,
          perl = TRUE
        ),
        category_observed = vapply(
          label_records$category_code,
          function(code) any(as.character(valid_raw) == code, na.rm = TRUE),
          logical(1)
        ),
        observed_category_n = vapply(
          label_records$category_code,
          function(code) sum(as.character(valid_raw) == code, na.rm = TRUE),
          integer(1)
        ),
        observed_category_proportion_of_valid = vapply(
          label_records$category_code,
          function(code) {
            if (valid_n == 0L) return(NA_real_)
            sum(as.character(valid_raw) == code, na.rm = TRUE) / valid_n
          },
          numeric(1)
        ),
        stringsAsFactors = FALSE
      )
    }

    variable_upper <- toupper(variable_name)
    is_identifier <- "identifiers" %in% strsplit(
      registry_info$categories, " \\| "
    )[[1]]
    is_pv <- grepl(
      "^BSM(MAT|NUM|ALG|GEO|DAT)[0-9]{2}$",
      variable_upper
    )

    is_official_continuous_scale <- is.numeric(x) &&
      grepl(
        "/SCL\\s*$",
        blank_if_na(label_exact),
        ignore.case = TRUE,
        perl = TRUE
      )

    categorical <- is.factor(x) || (
      !is_official_continuous_scale &&
        nrow(label_records) > 0L &&
        unique_nonmissing_n <= 50L
    )
    summary_type <- if (is_identifier) {
      "identifier_missingness_only"
    } else if (is_pv) {
      "pv_missingness_only"
    } else if (categorical) {
      "categorical"
    } else if (is.numeric(x)) {
      "continuous"
    } else {
      "nonidentifying_missingness_only"
    }

    distribution <- data.frame(
      country_system_code = system_code,
      file_type = file_type,
      variable_name = variable_upper,
      registry_categories = registry_info$categories,
      summary_type = summary_type,
      category_code = NA_character_,
      category_label = NA_character_,
      unweighted_n = as.integer(unweighted_n),
      valid_n = as.integer(valid_n),
      missing_n = as.integer(missing_n),
      missing_percentage = if (unweighted_n > 0L) 100 * missing_n / unweighted_n else NA_real_,
      declared_or_R_missing_n = as.integer(missing_n),
      possible_special_missing_n = as.integer(possible_special_missing_n),
      unique_nonmissing_n = as.integer(unique_nonmissing_n),
      category_n = NA_integer_,
      category_proportion_of_valid = NA_real_,
      mean = NA_real_, sd = NA_real_, minimum = NA_real_, p25 = NA_real_,
      median = NA_real_, p75 = NA_real_, maximum = NA_real_,
      quality_flag = quality_flag,
      stringsAsFactors = FALSE
    )

    if (identical(summary_type, "continuous") && valid_n > 0L) {
      numeric_values <- suppressWarnings(as.numeric(valid_raw))
      numeric_values <- numeric_values[is.finite(numeric_values)]
      if (length(numeric_values) > 0L) {
        quantiles <- as.numeric(stats::quantile(
          numeric_values,
          probs = c(0.25, 0.50, 0.75),
          na.rm = TRUE,
          names = FALSE,
          type = 7
        ))
        distribution$mean <- mean(numeric_values)
        distribution$sd <- if (length(numeric_values) > 1L) {
          stats::sd(numeric_values)
        } else {
          NA_real_
        }
        distribution$minimum <- min(numeric_values)
        distribution$p25 <- quantiles[1]
        distribution$median <- quantiles[2]
        distribution$p75 <- quantiles[3]
        distribution$maximum <- max(numeric_values)
      }
    }

    if (identical(summary_type, "categorical") && valid_n > 0L) {
      observed_codes <- sort(unique(as.character(valid_raw)), na.last = TRUE)
      label_lookup <- setNames(
        label_records$category_label,
        label_records$category_code
      )
      category_rows <- lapply(observed_codes, function(code) {
        count <- sum(as.character(valid_raw) == code, na.rm = TRUE)
        label <- unname(label_lookup[code])
        if (length(label) == 0L || is.na(label)) label <- NA_character_
        row <- distribution[1, , drop = FALSE]
        row$summary_type <- "category"
        row$category_code <- code
        row$category_label <- label
        row$category_n <- as.integer(count)
        row$category_proportion_of_valid <- count / valid_n
        row
      })
      distribution <- rbind(distribution, do.call(rbind, category_rows))
      row.names(distribution) <- NULL
    }

    list(
      schema = schema,
      value_labels = value_labels,
      distribution = distribution
    )
  }

  pv_groups <- list(
    data_probability = sprintf("BSMDAT%02d", 1:5),
    number = sprintf("BSMNUM%02d", 1:5),
    algebra = sprintf("BSMALG%02d", 1:5),
    geometry_measurement = sprintf("BSMGEO%02d", 1:5),
    overall_mathematics = sprintf("BSMMAT%02d", 1:5)
  )
  weight_variables <- c("TOTWGT", "SENWGT", "HOUWGT")
  design_variables <- c(
    "JKZONE", "JKREP",
    "WGTFAC1", "WGTADJ1", "WGTFAC2", "WGTADJ2",
    "WGTFAC3", "WGTADJ3"
  )

  make_pv_design_rows <- function(
      data, uppercase_names, system_code, file_type, schema_rows) {
    output <- list()
    counter <- 0L

    for (group_name in names(pv_groups)) {
      expected <- pv_groups[[group_name]]
      present <- intersect(expected, uppercase_names)
      missing <- setdiff(expected, uppercase_names)
      matched_schema <- schema_rows[
        schema_rows$variable_name %in% present,
        ,
        drop = FALSE
      ]
      all_numeric <- length(present) == length(expected) &&
        all(matched_schema$is_numeric %in% TRUE)
      all_missing_count <- sum(matched_schema$valid_n == 0L, na.rm = TRUE)
      variable_error_count <- sum(
        nzchar(blank_if_na(matched_schema$variable_error))
      )
      is_core_domain <- group_name != "overall_mathematics"
      status <- if (
        length(missing) > 0L || !all_numeric || all_missing_count > 0L ||
          variable_error_count > 0L
      ) {
        if (is_core_domain) "FAIL" else "WARNING"
      } else {
        "PASS"
      }
      counter <- counter + 1L
      output[[counter]] <- data.frame(
        country_system_code = system_code,
        file_type = file_type,
        audit_category = "plausible_values",
        audit_group = group_name,
        variable_name = NA_character_,
        analysis_role = if (is_core_domain) "content_domain_candidate" else "distinguish_only",
        expected_count = length(expected),
        present_count = length(present),
        present_variables = paste(present, collapse = " | "),
        missing_variables = paste(missing, collapse = " | "),
        all_present = length(missing) == 0L,
        all_numeric = all_numeric,
        all_missing_variable_count = as.integer(all_missing_count),
        nonmissing_n_min = if (nrow(matched_schema) > 0L) min(matched_schema$valid_n) else NA_integer_,
        nonmissing_n_max = if (nrow(matched_schema) > 0L) max(matched_schema$valid_n) else NA_integer_,
        schema_consistent_across_systems = NA,
        audit_status = status,
        notes = if (is_core_domain) {
          "Five PVs retained separately; no PV means, averaging, pooling, or models computed."
        } else {
          "Overall mathematics PVs are audited only to prevent confusion with content-domain PVs."
        },
        stringsAsFactors = FALSE
      )
    }

    audit_single <- function(variable_name, category, group, role, required) {
      present <- variable_name %in% uppercase_names
      matched <- schema_rows[
        schema_rows$variable_name == variable_name,
        ,
        drop = FALSE
      ]
      numeric_ok <- present && nrow(matched) == 1L &&
        isTRUE(matched$is_numeric[[1]])
      all_missing_count <- if (present && nrow(matched) == 1L) {
        as.integer(isTRUE(matched$valid_n[[1]] == 0L))
      } else {
        0L
      }
      variable_error <- present && nrow(matched) == 1L &&
        nzchar(blank_if_na(matched$variable_error[[1]]))
      status <- if (
        !present || !numeric_ok || all_missing_count > 0L || variable_error
      ) {
        if (required) "FAIL" else "WARNING"
      } else {
        "PASS"
      }
      data.frame(
        country_system_code = system_code,
        file_type = file_type,
        audit_category = category,
        audit_group = group,
        variable_name = variable_name,
        analysis_role = role,
        expected_count = 1L,
        present_count = as.integer(present),
        present_variables = if (present) variable_name else "",
        missing_variables = if (present) "" else variable_name,
        all_present = present,
        all_numeric = numeric_ok,
        all_missing_variable_count = all_missing_count,
        nonmissing_n_min = if (nrow(matched) == 1L) matched$valid_n[[1]] else NA_integer_,
        nonmissing_n_max = if (nrow(matched) == 1L) matched$valid_n[[1]] else NA_integer_,
        schema_consistent_across_systems = NA,
        audit_status = status,
        notes = if (required) "Required structural audit variable." else "Informational design candidate; not selected as final.",
        stringsAsFactors = FALSE
      )
    }

    for (variable_name in weight_variables) {
      counter <- counter + 1L
      output[[counter]] <- audit_single(
        variable_name,
        "weights",
        "student_sampling_weights",
        if (variable_name == "TOTWGT") "required_student_weight_candidate" else "alternative_weight_candidate",
        required = variable_name == "TOTWGT"
      )
    }
    for (variable_name in design_variables) {
      counter <- counter + 1L
      output[[counter]] <- audit_single(
        variable_name,
        "replication_design",
        if (variable_name %in% c("JKZONE", "JKREP")) "jackknife" else "other_design_components",
        if (variable_name %in% c("JKZONE", "JKREP")) "required_replication_candidate" else "informational_design_candidate",
        required = variable_name %in% c("JKZONE", "JKREP")
      )
    }
    do.call(rbind, output)
  }

  make_key_vector <- function(data, uppercase_names, key_columns) {
    indices <- match(key_columns, uppercase_names)
    if (any(is.na(indices))) {
      return(list(
        columns_present = FALSE,
        missing_columns = key_columns[is.na(indices)],
        key = NULL,
        missing_rows = nrow(data),
        unique_count = NA_integer_,
        duplicate_key_count = NA_integer_,
        duplicate_row_excess = NA_integer_,
        duplicate_keys = character()
      ))
    }
    components <- lapply(indices, function(index) {
      x <- data[[index]]
      # Factors need their displayed levels, while haven_labelled_spss vectors
      # must first be reduced to their underlying atomic values because their
      # own as.character() method deliberately rejects direct conversion.
      values <- if (is.factor(x)) {
        as.character(x)
      } else {
        as.character(atomic_values(x))
      }
      values[is.na(values)] <- ""
      trimws(values)
    })
    missing_rows <- Reduce(`|`, lapply(components, function(x) !nzchar(x)))
    key <- do.call(paste, c(components, sep = "\r"))
    key[missing_rows] <- NA_character_
    valid <- key[!is.na(key)]
    counts <- table(valid, useNA = "no")
    duplicate_keys <- names(counts)[counts > 1L]
    list(
      columns_present = TRUE,
      missing_columns = character(),
      key = key,
      missing_rows = sum(missing_rows),
      unique_count = length(counts),
      duplicate_key_count = length(duplicate_keys),
      duplicate_row_excess = if (length(duplicate_keys) > 0L) {
        sum(counts[duplicate_keys] - 1L)
      } else {
        0L
      },
      duplicate_keys = duplicate_keys
    )
  }

  inspect_one_file <- function(file_row, candidate_names_local) {
    warning_messages <- character()
    error_message <- NULL
    data_env <- new.env(parent = emptyenv())
    loaded_names <- tryCatch(
      withCallingHandlers(
        load(file_row$full_path, envir = data_env),
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        error_message <<- conditionMessage(e)
        character()
      }
    )

    if (!is.null(error_message)) {
      return(list(
        file_status = data.frame(
          country_system_code = file_row$country_system_code,
          file_type = file_row$file_type,
          source_file = file_row$file_name,
          source_object = file_row$object_name,
          read_success = FALSE,
          warning_count = length(warning_messages),
          warning_message = paste(unique(warning_messages), collapse = " | "),
          error_message = error_message,
          rows = NA_integer_, columns = NA_integer_,
          stringsAsFactors = FALSE
        ),
        schema = empty_occurrence_schema(),
        value_labels = empty_value_labels(),
        distribution = empty_distribution(),
        pv_design = empty_pv_design(),
        keys = list(),
        school_id = NULL
      ))
    }

    object_name <- as.character(file_row$object_name)
    if (!object_name %in% loaded_names) {
      uppercase_loaded <- toupper(loaded_names)
      matched_index <- match(toupper(object_name), uppercase_loaded)
      if (!is.na(matched_index)) object_name <- loaded_names[[matched_index]]
    }
    if (!exists(object_name, envir = data_env, inherits = FALSE)) {
      error_message <- paste0("Main object does not exist: ", file_row$object_name)
      rm(data_env)
      gc(verbose = FALSE)
      return(list(
        file_status = data.frame(
          country_system_code = file_row$country_system_code,
          file_type = file_row$file_type,
          source_file = file_row$file_name,
          source_object = file_row$object_name,
          read_success = FALSE,
          warning_count = length(warning_messages),
          warning_message = paste(unique(warning_messages), collapse = " | "),
          error_message = error_message,
          rows = NA_integer_, columns = NA_integer_,
          stringsAsFactors = FALSE
        ),
        schema = empty_occurrence_schema(),
        value_labels = empty_value_labels(),
        distribution = empty_distribution(),
        pv_design = empty_pv_design(),
        keys = list(),
        school_id = NULL
      ))
    }

    data <- get(object_name, envir = data_env, inherits = FALSE)
    if (!(is.data.frame(data) || is.matrix(data))) {
      error_message <- paste0(
        "Main object is not tabular: ", paste(class(data), collapse = " | ")
      )
      rm(data, data_env)
      gc(verbose = FALSE)
      return(list(
        file_status = data.frame(
          country_system_code = file_row$country_system_code,
          file_type = file_row$file_type,
          source_file = file_row$file_name,
          source_object = object_name,
          read_success = FALSE,
          warning_count = length(warning_messages),
          warning_message = paste(unique(warning_messages), collapse = " | "),
          error_message = error_message,
          rows = NA_integer_, columns = NA_integer_,
          stringsAsFactors = FALSE
        ),
        schema = empty_occurrence_schema(),
        value_labels = empty_value_labels(),
        distribution = empty_distribution(),
        pv_design = empty_pv_design(),
        keys = list(),
        school_id = NULL
      ))
    }

    uppercase_names <- toupper(names(data))
    duplicate_upper_names <- unique(uppercase_names[duplicated(uppercase_names)])
    if (length(duplicate_upper_names) > 0L) {
      warning_messages <- c(
        warning_messages,
        paste0(
          "Case-insensitive duplicate variable names: ",
          paste(duplicate_upper_names, collapse = ", ")
        )
      )
    }

    present_candidates <- intersect(candidate_names_local, uppercase_names)
    schema_list <- list()
    value_label_list <- list()
    distribution_list <- list()
    variable_errors <- character()

    for (variable_name in present_candidates) {
      index <- match(variable_name, uppercase_names)
      result <- tryCatch(
        withCallingHandlers(
          summarize_candidate_variable(
            data[[index]],
            file_row$country_system_code,
            file_row$file_type,
            file_row$file_name,
            object_name,
            variable_name
          ),
          warning = function(w) {
            warning_messages <<- c(
              warning_messages,
              paste0(variable_name, ": ", conditionMessage(w))
            )
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          variable_errors <<- c(
            variable_errors,
            paste0(variable_name, ": ", conditionMessage(e))
          )
          registry_info <- registry_for_variable(variable_name)
          list(
            schema = data.frame(
              country_system_code = file_row$country_system_code,
              file_type = file_row$file_type,
              source_file = file_row$file_name,
              source_object = object_name,
              variable_name = variable_name,
              registry_categories = registry_info$categories,
              registry_statuses = registry_info$statuses,
              r_class = paste(class(data[[index]]), collapse = " | "),
              storage_type = typeof(data[[index]]),
              is_numeric = is.numeric(data[[index]]),
              is_factor = is.factor(data[[index]]),
              is_ordered = is.ordered(data[[index]]),
              is_labelled = NA,
              variable_label_exact = NA_character_,
              variable_label_normalized = NA_character_,
              value_label_count = NA_integer_,
              value_label_signature = NA_character_,
              value_label_order_signature = NA_character_,
              factor_levels_signature = NA_character_,
              missing_attribute_type = NA_character_,
              missing_attribute_signature = NA_character_,
              unweighted_n = length(data[[index]]),
              valid_n = NA_integer_, missing_n = NA_integer_,
              missing_percentage = NA_real_, possible_special_missing_n = NA_integer_,
              unique_nonmissing_n = NA_integer_, quality_flag = "UNRESOLVED",
              schema_signature = NA_character_,
              variable_error = conditionMessage(e),
              stringsAsFactors = FALSE
            ),
            value_labels = empty_value_labels(),
            distribution = empty_distribution()
          )
        }
      )
      schema_list[[length(schema_list) + 1L]] <- result$schema
      if (nrow(result$value_labels) > 0L) {
        value_label_list[[length(value_label_list) + 1L]] <- result$value_labels
      }
      if (nrow(result$distribution) > 0L) {
        distribution_list[[length(distribution_list) + 1L]] <- result$distribution
      }
    }

    schema <- if (length(schema_list) > 0L) {
      do.call(rbind, schema_list)
    } else {
      empty_occurrence_schema()
    }
    value_labels <- if (length(value_label_list) > 0L) {
      do.call(rbind, value_label_list)
    } else {
      empty_value_labels()
    }
    distribution <- if (length(distribution_list) > 0L) {
      do.call(rbind, distribution_list)
    } else {
      empty_distribution()
    }

    keys <- list(
      IDSTUD = make_key_vector(data, uppercase_names, "IDSTUD"),
      IDCNTRY_IDSTUD = make_key_vector(
        data, uppercase_names, c("IDCNTRY", "IDSTUD")
      )
    )
    school_index <- match("IDSCHOOL", uppercase_names)
    school_id <- if (!is.na(school_index)) {
      x <- data[[school_index]]
      if (is.factor(x)) {
        as.character(x)
      } else {
        as.character(atomic_values(x))
      }
    } else {
      NULL
    }

    pv_design <- make_pv_design_rows(
      data,
      uppercase_names,
      file_row$country_system_code,
      file_row$file_type,
      schema
    )

    file_status <- data.frame(
      country_system_code = file_row$country_system_code,
      file_type = file_row$file_type,
      source_file = file_row$file_name,
      source_object = object_name,
      read_success = TRUE,
      warning_count = length(unique(warning_messages)),
      warning_message = paste(unique(warning_messages), collapse = " | "),
      error_message = if (length(variable_errors) > 0L) {
        paste(variable_errors, collapse = " | ")
      } else {
        NA_character_
      },
      rows = nrow(data),
      columns = ncol(data),
      stringsAsFactors = FALSE
    )

    rm(data, data_env)
    gc(verbose = FALSE)
    list(
      file_status = file_status,
      schema = schema,
      value_labels = value_labels,
      distribution = distribution,
      pv_design = pv_design,
      keys = keys,
      school_id = school_id
    )
  }

  key_summary_row <- function(
      system_code, key_definition, key_columns, bsa_result, bsg_result) {
    bsa_key <- bsa_result$keys[[key_definition]]
    bsg_key <- bsg_result$keys[[key_definition]]
    bsa_read <- isTRUE(bsa_result$file_status$read_success[[1]])
    bsg_read <- isTRUE(bsg_result$file_status$read_success[[1]])

    if (!bsa_read || !bsg_read || is.null(bsa_key) || is.null(bsg_key)) {
      return(data.frame(
        country_system_code = system_code,
        key_definition = paste(key_columns, collapse = " + "),
        bsa_key_columns_present = FALSE,
        bsg_key_columns_present = FALSE,
        bsa_rows = if (bsa_read) bsa_result$file_status$rows[[1]] else NA_integer_,
        bsg_rows = if (bsg_read) bsg_result$file_status$rows[[1]] else NA_integer_,
        bsa_missing_key_rows = NA_integer_, bsg_missing_key_rows = NA_integer_,
        bsa_unique_key_count = NA_integer_, bsg_unique_key_count = NA_integer_,
        bsa_duplicate_key_count = NA_integer_, bsg_duplicate_key_count = NA_integer_,
        bsa_duplicate_row_excess = NA_integer_, bsg_duplicate_row_excess = NA_integer_,
        common_unique_key_count = NA_integer_, only_bsa_unique_key_count = NA_integer_,
        only_bsg_unique_key_count = NA_integer_, bsa_match_percentage = NA_real_,
        bsg_match_percentage = NA_real_, many_to_many_key_count = NA_integer_,
        school_id_comparable_common_keys = NA_integer_,
        school_id_mismatch_common_keys = NA_integer_,
        possible_many_to_many = NA, audit_status = "FAIL",
        audit_note = "BSA or BSG could not be read or key result was unavailable.",
        stringsAsFactors = FALSE
      ))
    }

    bsa_valid <- unique(bsa_key$key[!is.na(bsa_key$key)])
    bsg_valid <- unique(bsg_key$key[!is.na(bsg_key$key)])
    common <- intersect(bsa_valid, bsg_valid)
    only_bsa <- setdiff(bsa_valid, bsg_valid)
    only_bsg <- setdiff(bsg_valid, bsa_valid)
    many_to_many_keys <- intersect(
      bsa_key$duplicate_keys,
      bsg_key$duplicate_keys
    )
    school_comparable <- NA_integer_
    school_mismatch <- NA_integer_

    if (
      identical(key_definition, "IDCNTRY_IDSTUD") &&
        bsa_key$duplicate_key_count == 0L &&
        bsg_key$duplicate_key_count == 0L &&
        !is.null(bsa_result$school_id) &&
        !is.null(bsg_result$school_id)
    ) {
      bsa_map <- setNames(bsa_result$school_id, bsa_key$key)
      bsg_map <- setNames(bsg_result$school_id, bsg_key$key)
      bsa_school <- bsa_map[common]
      bsg_school <- bsg_map[common]
      comparable <- !is.na(bsa_school) & !is.na(bsg_school) &
        nzchar(bsa_school) & nzchar(bsg_school)
      school_comparable <- sum(comparable)
      school_mismatch <- sum(
        comparable & as.character(bsa_school) != as.character(bsg_school)
      )
      rm(bsa_map, bsg_map, bsa_school, bsg_school)
    }

    columns_present <- bsa_key$columns_present && bsg_key$columns_present
    unique_ok <- bsa_key$duplicate_key_count == 0L &&
      bsg_key$duplicate_key_count == 0L
    no_missing_keys <- bsa_key$missing_rows == 0L &&
      bsg_key$missing_rows == 0L
    full_match <- length(only_bsa) == 0L && length(only_bsg) == 0L
    school_ok <- is.na(school_mismatch) || school_mismatch == 0L
    status <- if (
      !columns_present || !unique_ok || length(many_to_many_keys) > 0L
    ) {
      "FAIL"
    } else if (!no_missing_keys || !full_match || !school_ok) {
      "WARNING"
    } else {
      "PASS"
    }
    note <- if (!columns_present) {
      "One or more key columns are absent."
    } else if (length(many_to_many_keys) > 0L) {
      "The same duplicated key occurs in both BSA and BSG; formal join is prohibited."
    } else if (!unique_ok) {
      "Key is not unique in at least one file; review before any join."
    } else if (!no_missing_keys) {
      "At least one key contains an R missing or blank component; review before any join."
    } else if (!full_match) {
      "Unique keys are not fully matched across BSA and BSG."
    } else if (!school_ok) {
      "School identifier differs for at least one common student key."
    } else {
      "Keys are unique and fully matched; this is an audit result, not a performed join."
    }

    data.frame(
      country_system_code = system_code,
      key_definition = paste(key_columns, collapse = " + "),
      bsa_key_columns_present = bsa_key$columns_present,
      bsg_key_columns_present = bsg_key$columns_present,
      bsa_rows = bsa_result$file_status$rows[[1]],
      bsg_rows = bsg_result$file_status$rows[[1]],
      bsa_missing_key_rows = as.integer(bsa_key$missing_rows),
      bsg_missing_key_rows = as.integer(bsg_key$missing_rows),
      bsa_unique_key_count = as.integer(bsa_key$unique_count),
      bsg_unique_key_count = as.integer(bsg_key$unique_count),
      bsa_duplicate_key_count = as.integer(bsa_key$duplicate_key_count),
      bsg_duplicate_key_count = as.integer(bsg_key$duplicate_key_count),
      bsa_duplicate_row_excess = as.integer(bsa_key$duplicate_row_excess),
      bsg_duplicate_row_excess = as.integer(bsg_key$duplicate_row_excess),
      common_unique_key_count = length(common),
      only_bsa_unique_key_count = length(only_bsa),
      only_bsg_unique_key_count = length(only_bsg),
      bsa_match_percentage = if (length(bsa_valid) > 0L) 100 * length(common) / length(bsa_valid) else NA_real_,
      bsg_match_percentage = if (length(bsg_valid) > 0L) 100 * length(common) / length(bsg_valid) else NA_real_,
      many_to_many_key_count = length(many_to_many_keys),
      school_id_comparable_common_keys = school_comparable,
      school_id_mismatch_common_keys = school_mismatch,
      possible_many_to_many = length(many_to_many_keys) > 0L,
      audit_status = status,
      audit_note = note,
      stringsAsFactors = FALSE
    )
  }

  occurrence_schema_parts <- list()
  value_label_parts <- list()
  distribution_parts <- list()
  pv_design_parts <- list()
  file_status_parts <- list()
  key_audit_parts <- list()

  for (system_index in seq_along(systems)) {
    system_code <- systems[[system_index]]
    message(sprintf(
      "Auditing system %d/%d: %s",
      system_index, length(systems), system_code
    ))
    bsa_row <- manifest_main[
      manifest_main$country_system_code == system_code &
        manifest_main$file_type == "BSA",
      ,
      drop = FALSE
    ]
    bsg_row <- manifest_main[
      manifest_main$country_system_code == system_code &
        manifest_main$file_type == "BSG",
      ,
      drop = FALSE
    ]

    missing_result <- function(file_type) {
      list(
        file_status = data.frame(
          country_system_code = system_code, file_type = file_type,
          source_file = NA_character_, source_object = NA_character_,
          read_success = FALSE, warning_count = 0L,
          warning_message = NA_character_, error_message = "Manifest file missing",
          rows = NA_integer_, columns = NA_integer_, stringsAsFactors = FALSE
        ),
        schema = empty_occurrence_schema(), value_labels = empty_value_labels(),
        distribution = empty_distribution(), pv_design = empty_pv_design(),
        keys = list(), school_id = NULL
      )
    }

    bsa_result <- if (nrow(bsa_row) == 1L) {
      message("  Reading BSA: ", bsa_row$file_name[[1]])
      inspect_one_file(bsa_row[1, , drop = FALSE], candidate_names)
    } else {
      missing_result("BSA")
    }
    bsg_result <- if (nrow(bsg_row) == 1L) {
      message("  Reading BSG: ", bsg_row$file_name[[1]])
      inspect_one_file(bsg_row[1, , drop = FALSE], candidate_names)
    } else {
      missing_result("BSG")
    }

    file_status_parts[[length(file_status_parts) + 1L]] <- bsa_result$file_status
    file_status_parts[[length(file_status_parts) + 1L]] <- bsg_result$file_status
    for (result in list(bsa_result, bsg_result)) {
      if (nrow(result$schema) > 0L) {
        occurrence_schema_parts[[length(occurrence_schema_parts) + 1L]] <- result$schema
      }
      if (nrow(result$value_labels) > 0L) {
        value_label_parts[[length(value_label_parts) + 1L]] <- result$value_labels
      }
      if (nrow(result$distribution) > 0L) {
        distribution_parts[[length(distribution_parts) + 1L]] <- result$distribution
      }
      if (nrow(result$pv_design) > 0L) {
        pv_design_parts[[length(pv_design_parts) + 1L]] <- result$pv_design
      }
    }

    key_audit_parts[[length(key_audit_parts) + 1L]] <- key_summary_row(
      system_code,
      "IDSTUD",
      "IDSTUD",
      bsa_result,
      bsg_result
    )
    key_audit_parts[[length(key_audit_parts) + 1L]] <- key_summary_row(
      system_code,
      "IDCNTRY_IDSTUD",
      c("IDCNTRY", "IDSTUD"),
      bsa_result,
      bsg_result
    )

    rm(bsa_result, bsg_result)
    gc(verbose = FALSE)
  }

  occurrence_schema <- if (length(occurrence_schema_parts) > 0L) {
    do.call(rbind, occurrence_schema_parts)
  } else {
    empty_occurrence_schema()
  }
  value_label_audit <- if (length(value_label_parts) > 0L) {
    do.call(rbind, value_label_parts)
  } else {
    empty_value_labels()
  }
  candidate_missingness_distribution <- if (length(distribution_parts) > 0L) {
    do.call(rbind, distribution_parts)
  } else {
    empty_distribution()
  }
  pv_weight_design_audit <- if (length(pv_design_parts) > 0L) {
    do.call(rbind, pv_design_parts)
  } else {
    empty_pv_design()
  }
  file_read_status <- if (length(file_status_parts) > 0L) {
    do.call(rbind, file_status_parts)
  } else {
    empty_file_status()
  }
  premerge_key_audit <- do.call(rbind, key_audit_parts)

  row.names(occurrence_schema) <- NULL
  row.names(value_label_audit) <- NULL
  row.names(candidate_missingness_distribution) <- NULL
  row.names(pv_weight_design_audit) <- NULL
  row.names(file_read_status) <- NULL
  row.names(premerge_key_audit) <- NULL

  mode_character <- function(x) {
    values <- blank_if_na(x)
    counts <- table(values, useNA = "no")
    if (length(counts) == 0L) return("")
    winners <- names(counts)[counts == max(counts)]
    sort(winners)[[1]]
  }

  occurrence_schema$modal_schema_signature <- NA_character_
  occurrence_schema$modal_normalized_label <- NA_character_
  occurrence_schema$modal_value_label_signature <- NA_character_
  occurrence_schema$modal_value_label_order_signature <- NA_character_
  occurrence_schema$modal_missing_attribute_signature <- NA_character_

  comparison_groups <- split(
    seq_len(nrow(occurrence_schema)),
    paste(
      occurrence_schema$file_type,
      occurrence_schema$variable_name,
      sep = "\r"
    )
  )
  for (indices in comparison_groups) {
    occurrence_schema$modal_schema_signature[indices] <- mode_character(
      occurrence_schema$schema_signature[indices]
    )
    occurrence_schema$modal_normalized_label[indices] <- mode_character(
      occurrence_schema$variable_label_normalized[indices]
    )
    occurrence_schema$modal_value_label_signature[indices] <- mode_character(
      occurrence_schema$value_label_signature[indices]
    )
    occurrence_schema$modal_value_label_order_signature[indices] <- mode_character(
      occurrence_schema$value_label_order_signature[indices]
    )
    occurrence_schema$modal_missing_attribute_signature[indices] <- mode_character(
      occurrence_schema$missing_attribute_signature[indices]
    )
  }

  equal_blank_safe <- function(x, reference) {
    blank_if_na(x) == blank_if_na(reference)
  }
  occurrence_schema$schema_matches_modal <- equal_blank_safe(
    occurrence_schema$schema_signature,
    occurrence_schema$modal_schema_signature
  )
  occurrence_schema$normalized_label_matches_modal <- equal_blank_safe(
    occurrence_schema$variable_label_normalized,
    occurrence_schema$modal_normalized_label
  )
  occurrence_schema$value_label_mapping_matches_modal <- equal_blank_safe(
    occurrence_schema$value_label_signature,
    occurrence_schema$modal_value_label_signature
  )
  occurrence_schema$value_label_order_matches_modal <- equal_blank_safe(
    occurrence_schema$value_label_order_signature,
    occurrence_schema$modal_value_label_order_signature
  )
  occurrence_schema$missing_attributes_match_modal <- equal_blank_safe(
    occurrence_schema$missing_attribute_signature,
    occurrence_schema$modal_missing_attribute_signature
  )

  occurrence_schema$cross_system_status <- ifelse(
    nzchar(blank_if_na(occurrence_schema$variable_error)),
    "UNRESOLVED",
    ifelse(
      occurrence_schema$quality_flag != "OK",
      "DATA_QUALITY_WARNING",
      ifelse(
        !occurrence_schema$schema_matches_modal |
          !occurrence_schema$value_label_order_matches_modal,
        "HARMONISE",
        ifelse(
          !occurrence_schema$normalized_label_matches_modal |
            !occurrence_schema$value_label_mapping_matches_modal |
            !occurrence_schema$missing_attributes_match_modal |
            occurrence_schema$possible_special_missing_n > 0L,
          "REVIEW_DOCUMENTATION",
          "READY"
        )
      )
    )
  )

  if (nrow(value_label_audit) > 0L) {
    code_group <- paste(
      value_label_audit$file_type,
      value_label_audit$variable_name,
      value_label_audit$category_code,
      sep = "\r"
    )
    label_group <- paste(
      value_label_audit$file_type,
      value_label_audit$variable_name,
      value_label_audit$category_label_normalized,
      sep = "\r"
    )
    system_code_group <- paste(
      value_label_audit$country_system_code,
      code_group,
      sep = "\r"
    )
    system_label_group <- paste(
      value_label_audit$country_system_code,
      label_group,
      sep = "\r"
    )
    # Compare each system's complete mapping signature with other systems.
    # This avoids treating several codes with the same generic label inside
    # every system (for example, multiple item-response error codes) as a
    # cross-system conflict.
    system_code_label_signature <- ave(
      blank_if_na(value_label_audit$category_label_normalized),
      system_code_group,
      FUN = function(x) paste(sort(unique(x)), collapse = " | ")
    )
    system_label_code_signature <- ave(
      blank_if_na(value_label_audit$category_code),
      system_label_group,
      FUN = function(x) paste(sort(unique(x)), collapse = " | ")
    )
    code_conflict_map <- tapply(
      system_code_label_signature,
      code_group,
      function(x) length(unique(x)) > 1L
    )
    label_conflict_map <- tapply(
      system_label_code_signature,
      label_group,
      function(x) length(unique(x)) > 1L
    )
    value_label_audit$code_same_label_differs <- as.logical(as.vector(
      code_conflict_map[match(code_group, names(code_conflict_map))]
    ))
    value_label_audit$label_same_code_differs <- as.logical(as.vector(
      label_conflict_map[match(label_group, names(label_conflict_map))]
    ))
    value_label_audit$requires_documentation <- with(
      value_label_audit,
      code_same_label_differs | label_same_code_differs |
        possible_missing_label | is_declared_missing
    )
  } else {
    value_label_audit$code_same_label_differs <- logical()
    value_label_audit$label_same_code_differs <- logical()
    value_label_audit$requires_documentation <- logical()
  }

  coverage_grid <- merge(
    expand.grid(
      country_system_code = systems,
      file_type = c("BSA", "BSG"),
      registry_row = seq_len(nrow(candidate_registry)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      registry_row = seq_len(nrow(candidate_registry)),
      candidate_registry,
      stringsAsFactors = FALSE
    ),
    by = "registry_row",
    all.x = TRUE,
    sort = FALSE
  )
  occurrence_key <- paste(
    occurrence_schema$country_system_code,
    occurrence_schema$file_type,
    occurrence_schema$variable_name,
    sep = "\r"
  )
  coverage_key <- paste(
    coverage_grid$country_system_code,
    coverage_grid$file_type,
    coverage_grid$variable_name,
    sep = "\r"
  )
  occurrence_match <- match(coverage_key, occurrence_key)
  coverage_grid$present <- !is.na(occurrence_match)

  occurrence_fields <- c(
    "source_file", "source_object", "r_class", "storage_type", "is_numeric",
    "is_factor", "is_ordered", "is_labelled", "value_label_count",
    "missing_attribute_type", "unweighted_n", "valid_n", "missing_n",
    "missing_percentage", "possible_special_missing_n", "unique_nonmissing_n",
    "quality_flag", "schema_matches_modal", "normalized_label_matches_modal",
    "value_label_mapping_matches_modal", "value_label_order_matches_modal",
    "missing_attributes_match_modal", "cross_system_status", "variable_error"
  )
  for (field in occurrence_fields) {
    coverage_grid[[field]] <- occurrence_schema[[field]][occurrence_match]
  }
  not_applicable <- !coverage_grid$present &
    coverage_grid$file_type == "BSA" &
    coverage_grid$candidate_category %in% c("home_resources", "ICT")
  coverage_grid$cross_system_status[!coverage_grid$present] <- "UNAVAILABLE"
  coverage_grid$cross_system_status[not_applicable] <- "NOT_APPLICABLE"
  coverage_grid$evidence_status <- ifelse(
    !coverage_grid$present,
    ifelse(
      coverage_grid$registry_status %in% c(
        "official_document_confirmed",
        "official_document_confirmed_candidate"
      ),
      "official_document_confirmed_but_not_inventory_confirmed_here",
      "unavailable_or_unresolved"
    ),
    ifelse(
      coverage_grid$registry_status %in% c(
        "official_document_confirmed",
        "official_document_confirmed_candidate"
      ),
      "official_document_confirmed_and_inventory_confirmed",
      ifelse(
        coverage_grid$registry_status %in% c(
          "keyword_only_candidate",
          "unresolved_multiple_keyword_categories"
        ),
        "label_or_keyword_candidate_and_inventory_confirmed",
        "inventory_confirmed_but_meaning_unresolved"
      )
    )
  )
  coverage_grid$documentation_review_needed <- coverage_grid$cross_system_status ==
    "REVIEW_DOCUMENTATION"
  coverage_grid$status_reason <- ifelse(
    not_applicable,
    "Student-context candidate is not expected in the BSA achievement file.",
    ifelse(
      !coverage_grid$present,
      "Variable not present in this system/file type.",
      ifelse(
        coverage_grid$cross_system_status == "READY",
        "Present; schema and documented attributes match the cross-system modal structure.",
        ifelse(
          coverage_grid$cross_system_status == "HARMONISE",
          "Present, but schema or category order differs from the cross-system modal structure.",
          ifelse(
            coverage_grid$cross_system_status == "REVIEW_DOCUMENTATION",
            "Present, but labels, code mappings, missing attributes, or possible special missing labels require documentation review.",
            ifelse(
              coverage_grid$cross_system_status == "DATA_QUALITY_WARNING",
              "Present, but valid n is zero, below 30, or below 10 percent of records.",
              "Current audit could not resolve the variable structure."
            )
          )
        )
      )
    )
  )

  variable_schema_audit <- coverage_grid[, c(
    "country_system_code", "file_type", "candidate_category",
    "candidate_subcategory", "variable_name", "registry_status", "gating_role",
    "official_source", "evidence_status", "present", "source_file",
    "source_object", "r_class",
    "storage_type", "is_numeric", "is_factor", "is_ordered", "is_labelled",
    "value_label_count", "missing_attribute_type", "unweighted_n", "valid_n",
    "missing_n", "missing_percentage", "possible_special_missing_n",
    "unique_nonmissing_n", "quality_flag", "schema_matches_modal",
    "normalized_label_matches_modal", "value_label_mapping_matches_modal",
    "value_label_order_matches_modal", "missing_attributes_match_modal",
    "cross_system_status", "documentation_review_needed", "variable_error",
    "status_reason"
  )]
  label_fields <- occurrence_schema[, c(
    "country_system_code", "file_type", "variable_name",
    "variable_label_exact", "variable_label_normalized",
    "value_label_signature", "value_label_order_signature",
    "factor_levels_signature", "missing_attribute_signature",
    "modal_schema_signature", "modal_normalized_label",
    "modal_value_label_signature", "modal_value_label_order_signature",
    "modal_missing_attribute_signature"
  )]
  variable_schema_audit <- merge(
    variable_schema_audit,
    label_fields,
    by = c("country_system_code", "file_type", "variable_name"),
    all.x = TRUE,
    sort = FALSE
  )

  candidate_variable_system_coverage <- coverage_grid[, c(
    "country_system_code", "file_type", "candidate_category",
    "candidate_subcategory", "variable_name", "registry_status", "gating_role",
    "evidence_status", "present", "r_class", "storage_type", "is_numeric", "is_factor",
    "is_labelled", "value_label_count", "missing_attribute_type",
    "quality_flag", "cross_system_status", "documentation_review_needed",
    "status_reason"
  )]

  occurrence_schema_map <- setNames(
    occurrence_schema$schema_matches_modal,
    occurrence_key
  )
  pv_weight_design_audit$schema_consistent_across_systems <- vapply(
    seq_len(nrow(pv_weight_design_audit)),
    function(i) {
      row <- pv_weight_design_audit[i, , drop = FALSE]
      variables <- if (row$audit_category == "plausible_values") {
        pv_groups[[row$audit_group]]
      } else {
        row$variable_name
      }
      keys <- paste(
        row$country_system_code,
        row$file_type,
        variables,
        sep = "\r"
      )
      values <- occurrence_schema_map[keys]
      length(values) == length(variables) && all(values %in% TRUE)
    },
    logical(1)
  )
  pv_weight_design_audit$audit_status <- ifelse(
    pv_weight_design_audit$audit_status == "PASS" &
      !pv_weight_design_audit$schema_consistent_across_systems,
    "WARNING",
    pv_weight_design_audit$audit_status
  )

  get_file_status <- function(system_code, file_type) {
    rows <- file_read_status[
      file_read_status$country_system_code == system_code &
        file_read_status$file_type == file_type,
      ,
      drop = FALSE
    ]
    if (nrow(rows) == 1L) rows else empty_file_status()
  }

  system_pairing_rows <- lapply(systems, function(system_code) {
    bsa_manifest <- manifest_main[
      manifest_main$country_system_code == system_code &
        manifest_main$file_type == "BSA",
      ,
      drop = FALSE
    ]
    bsg_manifest <- manifest_main[
      manifest_main$country_system_code == system_code &
        manifest_main$file_type == "BSG",
      ,
      drop = FALSE
    ]
    bsa_status <- get_file_status(system_code, "BSA")
    bsg_status <- get_file_status(system_code, "BSG")
    has_bsa <- nrow(bsa_manifest) == 1L
    has_bsg <- nrow(bsg_manifest) == 1L
    bsa_read <- nrow(bsa_status) == 1L && isTRUE(bsa_status$read_success[[1]])
    bsg_read <- nrow(bsg_status) == 1L && isTRUE(bsg_status$read_success[[1]])
    bsa_rows <- if (bsa_read) bsa_status$rows[[1]] else NA_integer_
    bsg_rows <- if (bsg_read) bsg_status$rows[[1]] else NA_integer_
    status <- if (!has_bsa || !has_bsg || !bsa_read || !bsg_read) {
      "FAIL"
    } else if (!identical(as.integer(bsa_rows), as.integer(bsg_rows))) {
      "WARNING"
    } else {
      "PASS"
    }
    data.frame(
      country_system_code = system_code,
      has_BSA = has_bsa,
      has_BSG = has_bsg,
      bsa_file = if (has_bsa) bsa_manifest$file_name[[1]] else NA_character_,
      bsg_file = if (has_bsg) bsg_manifest$file_name[[1]] else NA_character_,
      bsa_source_object = if (has_bsa) bsa_manifest$object_name[[1]] else NA_character_,
      bsg_source_object = if (has_bsg) bsg_manifest$object_name[[1]] else NA_character_,
      bsa_read_success = bsa_read,
      bsg_read_success = bsg_read,
      bsa_rows = bsa_rows,
      bsg_rows = bsg_rows,
      row_difference_BSA_minus_BSG = if (!is.na(bsa_rows) && !is.na(bsg_rows)) {
        bsa_rows - bsg_rows
      } else {
        NA_integer_
      },
      row_count_equal = if (!is.na(bsa_rows) && !is.na(bsg_rows)) {
        bsa_rows == bsg_rows
      } else {
        NA
      },
      file_warning_count = sum(c(
        if (nrow(bsa_status) == 1L) bsa_status$warning_count[[1]] else 0L,
        if (nrow(bsg_status) == 1L) bsg_status$warning_count[[1]] else 0L
      )),
      pairing_status = status,
      stringsAsFactors = FALSE
    )
  })
  system_file_pairing <- do.call(rbind, system_pairing_rows)

  official_home <- unique(candidate_registry$variable_name[
    candidate_registry$candidate_category == "home_resources" &
      candidate_registry$registry_status == "official_document_confirmed_candidate"
  ])
  official_ict <- unique(candidate_registry$variable_name[
    candidate_registry$candidate_category == "ICT" &
      candidate_registry$registry_status == "official_document_confirmed_candidate"
  ])

  system_eligibility_rows <- lapply(systems, function(system_code) {
    pair_row <- system_file_pairing[
      system_file_pairing$country_system_code == system_code,
      ,
      drop = FALSE
    ]
    key_row <- premerge_key_audit[
      premerge_key_audit$country_system_code == system_code &
        premerge_key_audit$key_definition == "IDCNTRY + IDSTUD",
      ,
      drop = FALSE
    ]
    technical_rows <- pv_weight_design_audit[
      pv_weight_design_audit$country_system_code == system_code,
      ,
      drop = FALSE
    ]
    required_id_names_local <- c("IDCNTRY", "IDSCHOOL", "IDSTUD")
    id_occurrence <- occurrence_schema[
      occurrence_schema$country_system_code == system_code &
        occurrence_schema$file_type %in% c("BSA", "BSG") &
        occurrence_schema$variable_name %in% required_id_names_local,
      ,
      drop = FALSE
    ]
    required_ids_present <- nrow(id_occurrence) ==
      2L * length(required_id_names_local)
    core_technical_failure_rows <- technical_rows[
      technical_rows$audit_status == "FAIL" &
        (
          (
            technical_rows$audit_category == "plausible_values" &
              technical_rows$audit_group != "overall_mathematics"
          ) |
            technical_rows$analysis_role %in% c(
              "required_student_weight_candidate", "required_replication_candidate"
            )
        ),
      ,
      drop = FALSE
    ]
    core_technical_fail <- nrow(core_technical_failure_rows) > 0L
    core_failure_labels <- unique(ifelse(
      is.na(core_technical_failure_rows$variable_name) |
        !nzchar(blank_if_na(core_technical_failure_rows$variable_name)),
      core_technical_failure_rows$audit_group,
      core_technical_failure_rows$variable_name
    ))
    bsg_occurrence <- occurrence_schema[
      occurrence_schema$country_system_code == system_code &
        occurrence_schema$file_type == "BSG",
      ,
      drop = FALSE
    ]
    usable_home <- bsg_occurrence$variable_name %in% official_home &
      !is.na(bsg_occurrence$valid_n) & bsg_occurrence$valid_n > 0L
    usable_ict <- bsg_occurrence$variable_name %in% official_ict &
      !is.na(bsg_occurrence$valid_n) & bsg_occurrence$valid_n > 0L
    relevant_names <- unique(c(
      unlist(pv_groups[c(
        "data_probability", "number", "algebra", "geometry_measurement"
      )]),
      "IDCNTRY", "IDSCHOOL", "IDSTUD", "TOTWGT", "JKZONE", "JKREP",
      official_home, official_ict
    ))
    relevant_schema <- occurrence_schema[
      occurrence_schema$country_system_code == system_code &
        occurrence_schema$variable_name %in% relevant_names,
      ,
      drop = FALSE
    ]
    needs_harmonisation <- any(
      relevant_schema$cross_system_status == "HARMONISE"
    )
    needs_documentation <- any(
      relevant_schema$cross_system_status == "REVIEW_DOCUMENTATION"
    )
    has_unresolved_relevant <- any(
      relevant_schema$cross_system_status == "UNRESOLVED"
    )
    data_quality_count <- sum(
      relevant_schema$cross_system_status == "DATA_QUALITY_WARNING"
    )
    reasons <- character()
    status <- "READY_FOR_REVIEW"
    if (nrow(pair_row) != 1L || pair_row$pairing_status[[1]] == "FAIL") {
      status <- "UNRESOLVED"
      reasons <- c(reasons, "BSA/BSG file pair missing or unreadable")
    } else if (!required_ids_present) {
      status <- "KEY_PROBLEM"
      reasons <- c(reasons, "IDCNTRY, IDSCHOOL, or IDSTUD is absent in BSA/BSG")
    } else if (nrow(key_row) != 1L || key_row$audit_status[[1]] != "PASS") {
      status <- "KEY_PROBLEM"
      reasons <- c(
        reasons,
        "IDCNTRY + IDSTUD key is absent, incomplete, non-unique, or not fully compatible"
      )
    } else if (core_technical_fail) {
      status <- "PV_OR_WEIGHT_PROBLEM"
      reasons <- c(
        reasons,
        paste0(
          "Failed core audit group(s): ",
          paste(sort(core_failure_labels), collapse = ", ")
        )
      )
    } else if (!any(usable_home) || !any(usable_ict)) {
      status <- "INSUFFICIENT_DATA"
      if (!any(usable_home)) reasons <- c(reasons, "No usable official home-resources candidate")
      if (!any(usable_ict)) reasons <- c(reasons, "No usable official ICT candidate")
    } else if (has_unresolved_relevant) {
      status <- "UNRESOLVED"
      reasons <- c(reasons, "At least one core/research candidate could not be audited")
    } else if (needs_harmonisation) {
      status <- "NEEDS_HARMONISATION"
      reasons <- c(reasons, "At least one core/research candidate has schema or category-order differences")
    } else if (needs_documentation) {
      status <- "NEEDS_DOCUMENTATION"
      reasons <- c(reasons, "At least one core/research candidate requires label or missing-code documentation review")
    }
    if (length(reasons) == 0L) {
      reasons <- "No automatic exclusion decision; proceed to substantive review."
    }
    data.frame(
      country_system_code = system_code,
      preliminary_system_status = status,
      bsa_bsg_pairing_pass = nrow(pair_row) == 1L && pair_row$pairing_status[[1]] == "PASS",
      required_id_variables_present = required_ids_present,
      composite_key_pass = nrow(key_row) == 1L && key_row$audit_status[[1]] == "PASS",
      core_pv_weight_design_fail = core_technical_fail,
      usable_official_home_candidate_count = sum(usable_home),
      usable_official_ICT_candidate_count = sum(usable_ict),
      relevant_harmonisation_issue_count = sum(
        relevant_schema$cross_system_status == "HARMONISE"
      ),
      relevant_documentation_issue_count = sum(
        relevant_schema$cross_system_status == "REVIEW_DOCUMENTATION"
      ),
      relevant_data_quality_warning_count = data_quality_count,
      preliminary_reason = paste(reasons, collapse = " | "),
      final_inclusion_decision_made = FALSE,
      stringsAsFactors = FALSE
    )
  })
  system_eligibility_premerge <- do.call(rbind, system_eligibility_rows)

  issue_rows <- list()
  add_issue <- function(
      system_code, file_type, variable_name, category,
      issue_type, severity, details, suggested_status) {
    issue_rows[[length(issue_rows) + 1L]] <<- data.frame(
      country_system_code = system_code,
      file_type = file_type,
      variable_name = variable_name,
      candidate_category = category,
      issue_type = issue_type,
      severity = severity,
      details = details,
      suggested_status = suggested_status,
      stringsAsFactors = FALSE
    )
  }

  problem_schema <- variable_schema_audit[
    variable_schema_audit$present %in% TRUE &
      variable_schema_audit$cross_system_status != "READY",
    ,
    drop = FALSE
  ]
  if (nrow(problem_schema) > 0L) {
    for (i in seq_len(nrow(problem_schema))) {
      row <- problem_schema[i, , drop = FALSE]
      add_issue(
        row$country_system_code,
        row$file_type,
        row$variable_name,
        row$candidate_category,
        paste0("VARIABLE_", row$cross_system_status),
        if (row$gating_role == "CORE_TECHNICAL") "CORE" else "REVIEW",
        row$status_reason,
        row$cross_system_status
      )
    }
  }
  bad_keys <- premerge_key_audit[
    premerge_key_audit$audit_status != "PASS",
    ,
    drop = FALSE
  ]
  if (nrow(bad_keys) > 0L) {
    for (i in seq_len(nrow(bad_keys))) {
      row <- bad_keys[i, , drop = FALSE]
      add_issue(
        row$country_system_code,
        "BSA + BSG",
        row$key_definition,
        "identifiers",
        "PREMERGE_KEY",
        if (row$audit_status == "FAIL") "CORE" else "REVIEW",
        row$audit_note,
        if (row$audit_status == "FAIL") "KEY_PROBLEM" else "REVIEW_DOCUMENTATION"
      )
    }
  }
  bad_technical <- pv_weight_design_audit[
    pv_weight_design_audit$audit_status != "PASS",
    ,
    drop = FALSE
  ]
  if (nrow(bad_technical) > 0L) {
    for (i in seq_len(nrow(bad_technical))) {
      row <- bad_technical[i, , drop = FALSE]
      add_issue(
        row$country_system_code,
        row$file_type,
        ifelse(is.na(row$variable_name), row$audit_group, row$variable_name),
        row$audit_category,
        "PV_WEIGHT_DESIGN",
        if (row$audit_status == "FAIL") "CORE" else "REVIEW",
        paste0(
          "Status=", row$audit_status,
          "; missing=", blank_if_na(row$missing_variables),
          "; schema_consistent=", row$schema_consistent_across_systems
        ),
        if (row$audit_status == "FAIL") "PV_OR_WEIGHT_PROBLEM" else "HARMONISE"
      )
    }
  }
  failed_files <- file_read_status[
    !file_read_status$read_success |
      nzchar(blank_if_na(file_read_status$error_message)) |
      file_read_status$warning_count > 0L,
    ,
    drop = FALSE
  ]
  if (nrow(failed_files) > 0L) {
    for (i in seq_len(nrow(failed_files))) {
      row <- failed_files[i, , drop = FALSE]
      add_issue(
        row$country_system_code,
        row$file_type,
        NA_character_,
        "file_read",
        "FILE_WARNING_OR_ERROR",
        if (!row$read_success) "CORE" else "REVIEW",
        paste(
          blank_if_na(row$error_message),
          blank_if_na(row$warning_message),
          sep = " | "
        ),
        "UNRESOLVED"
      )
    }
  }

  harmonisation_issue_log <- if (length(issue_rows) > 0L) {
    do.call(rbind, issue_rows)
  } else {
    data.frame(
      country_system_code = character(), file_type = character(),
      variable_name = character(), candidate_category = character(),
      issue_type = character(), severity = character(), details = character(),
      suggested_status = character(), stringsAsFactors = FALSE
    )
  }

  validation <- data.frame(
    check_id = integer(), check = character(), status = character(),
    details = character(), stringsAsFactors = FALSE
  )
  add_validation <- function(check_id, check, status, details) {
    validation <<- rbind(
      validation,
      data.frame(
        check_id = check_id, check = check, status = status,
        details = details, stringsAsFactors = FALSE
      )
    )
  }

  add_validation(1L, "First-round metadata read", "PASS", paste(
    "Six required first-round files read; first-round final validation PASS."
  ))
  add_validation(
    2L,
    "Actual system count",
    if (actual_system_count == 47L) "PASS" else "WARNING",
    paste0("Actual systems=", actual_system_count, "; expected reference count=47")
  )
  add_validation(
    3L,
    "BSA and BSG available for every system",
    if (all(system_file_pairing$has_BSA & system_file_pairing$has_BSG)) "PASS" else "FAIL",
    paste0("Complete pairs=", sum(system_file_pairing$has_BSA & system_file_pairing$has_BSG), "/", actual_system_count)
  )
  add_validation(
    4L,
    "Main object identified and read for every system/file",
    if (all(system_file_pairing$bsa_read_success & system_file_pairing$bsg_read_success)) "PASS" else "FAIL",
    paste0("Successfully reread files=", sum(file_read_status$read_success), "/", nrow(file_read_status))
  )
  composite_key_rows <- premerge_key_audit[
    premerge_key_audit$key_definition == "IDCNTRY + IDSTUD",
    ,
    drop = FALSE
  ]
  required_id_names <- c("IDCNTRY", "IDSCHOOL", "IDSTUD")
  required_id_grid <- expand.grid(
    country_system_code = systems,
    file_type = c("BSA", "BSG"),
    variable_name = required_id_names,
    stringsAsFactors = FALSE
  )
  required_id_key <- paste(
    required_id_grid$country_system_code,
    required_id_grid$file_type,
    required_id_grid$variable_name,
    sep = "\r"
  )
  occurrence_id_key <- paste(
    occurrence_schema$country_system_code,
    occurrence_schema$file_type,
    occurrence_schema$variable_name,
    sep = "\r"
  )
  all_required_ids_present <- all(required_id_key %in% occurrence_id_key)
  add_validation(
    5L,
    "Required identifier candidates present",
    if (all_required_ids_present) "PASS" else "FAIL",
    paste0(
      "IDCNTRY, IDSCHOOL, and IDSTUD present file-system combinations=",
      sum(required_id_key %in% occurrence_id_key), "/", length(required_id_key),
      "; candidate composite key=IDCNTRY + IDSTUD."
    )
  )
  unique_keys_ok <- all(
    composite_key_rows$bsa_duplicate_key_count == 0L &
      composite_key_rows$bsg_duplicate_key_count == 0L
  )
  add_validation(
    6L,
    "Identifier key uniqueness",
    if (unique_keys_ok) "PASS" else "FAIL",
    paste0("Systems with non-unique composite key=", sum(
      composite_key_rows$bsa_duplicate_key_count > 0L |
        composite_key_rows$bsg_duplicate_key_count > 0L,
      na.rm = TRUE
    ))
  )
  many_to_many_count <- sum(
    composite_key_rows$possible_many_to_many %in% TRUE,
    na.rm = TRUE
  )
  add_validation(
    7L,
    "Many-to-many risk",
    if (many_to_many_count == 0L) "PASS" else "FAIL",
    paste0("Systems with possible many-to-many=", many_to_many_count)
  )
  core_pv_rows <- pv_weight_design_audit[
    pv_weight_design_audit$audit_category == "plausible_values" &
      pv_weight_design_audit$audit_group != "overall_mathematics",
    ,
    drop = FALSE
  ]
  add_validation(
    8L,
    "Four content-domain PV groups complete and nonempty",
    if (all(
      core_pv_rows$all_present & core_pv_rows$all_numeric &
        core_pv_rows$all_missing_variable_count == 0L
    )) "PASS" else "FAIL",
    paste0(
      "Complete, numeric, nonempty content-domain groups=",
      sum(
        core_pv_rows$all_present & core_pv_rows$all_numeric &
          core_pv_rows$all_missing_variable_count == 0L
      ),
      "/", nrow(core_pv_rows)
    )
  )
  add_validation(
    9L,
    "PV counts consistent",
    if (all(core_pv_rows$present_count == 5L)) "PASS" else "FAIL",
    paste0("Groups with exactly five PVs=", sum(core_pv_rows$present_count == 5L), "/", nrow(core_pv_rows))
  )
  totwgt_rows <- pv_weight_design_audit[
    pv_weight_design_audit$variable_name %in% "TOTWGT",
    ,
    drop = FALSE
  ]
  add_validation(
    10L,
    "Student weight present",
    if (
      nrow(totwgt_rows) != 2L * actual_system_count ||
        any(totwgt_rows$audit_status == "FAIL")
    ) {
      "FAIL"
    } else if (any(totwgt_rows$audit_status == "WARNING")) {
      "WARNING"
    } else {
      "PASS"
    },
    paste0(
      "TOTWGT PASS/WARNING/FAIL=",
      sum(totwgt_rows$audit_status == "PASS"), "/",
      sum(totwgt_rows$audit_status == "WARNING"), "/",
      sum(totwgt_rows$audit_status == "FAIL"),
      "; expected file rows=", 2L * actual_system_count
    )
  )
  jk_rows <- pv_weight_design_audit[
    pv_weight_design_audit$variable_name %in% c("JKZONE", "JKREP"),
    ,
    drop = FALSE
  ]
  add_validation(
    11L,
    "Replication variables present",
    if (
      nrow(jk_rows) != 4L * actual_system_count ||
        any(jk_rows$audit_status == "FAIL")
    ) {
      "FAIL"
    } else if (any(jk_rows$audit_status == "WARNING")) {
      "WARNING"
    } else {
      "PASS"
    },
    paste0(
      "JKZONE/JKREP PASS/WARNING/FAIL=",
      sum(jk_rows$audit_status == "PASS"), "/",
      sum(jk_rows$audit_status == "WARNING"), "/",
      sum(jk_rows$audit_status == "FAIL"),
      "; expected file-variable rows=", 4L * actual_system_count
    )
  )
  home_counts <- vapply(systems, function(system_code) {
    sum(
      occurrence_schema$country_system_code == system_code &
        occurrence_schema$file_type == "BSG" &
        occurrence_schema$variable_name %in% official_home &
        !is.na(occurrence_schema$valid_n) & occurrence_schema$valid_n > 0L,
      na.rm = TRUE
    )
  }, integer(1))
  add_validation(
    12L,
    "Home-resources candidate coverage",
    if (all(home_counts > 0L)) "PASS" else "WARNING",
    paste0("Systems with at least one usable official candidate=", sum(home_counts > 0L), "/", actual_system_count)
  )
  ict_counts <- vapply(systems, function(system_code) {
    sum(
      occurrence_schema$country_system_code == system_code &
        occurrence_schema$file_type == "BSG" &
        occurrence_schema$variable_name %in% official_ict &
        !is.na(occurrence_schema$valid_n) & occurrence_schema$valid_n > 0L,
      na.rm = TRUE
    )
  }, integer(1))
  add_validation(
    13L,
    "ICT candidate coverage",
    if (all(ict_counts > 0L)) "PASS" else "WARNING",
    paste0("Systems with at least one usable official candidate=", sum(ict_counts > 0L), "/", actual_system_count)
  )
  official_occurrence <- occurrence_schema[
    grepl("official_document_confirmed", occurrence_schema$registry_statuses),
    ,
    drop = FALSE
  ]
  schema_difference_count <- sum(
    !official_occurrence$schema_matches_modal |
      !official_occurrence$value_label_order_matches_modal,
    na.rm = TRUE
  )
  noncore_variable_error_count <- sum(
    nzchar(blank_if_na(occurrence_schema$variable_error)) &
      !grepl(
        "official_document_confirmed",
        occurrence_schema$registry_statuses
      )
  )
  add_validation(
    14L,
    "Schema consistency across systems",
    if (
      schema_difference_count == 0L && noncore_variable_error_count == 0L
    ) "PASS" else "WARNING",
    paste0(
      "Official candidate occurrences with structural/order difference=",
      schema_difference_count,
      "; non-core candidate processing errors=", noncore_variable_error_count
    )
  )
  label_conflict_count <- if (nrow(value_label_audit) > 0L) {
    sum(
      value_label_audit$code_same_label_differs |
        value_label_audit$label_same_code_differs,
      na.rm = TRUE
    )
  } else {
    0L
  }
  add_validation(
    15L,
    "Value-label conflicts",
    if (label_conflict_count == 0L) "PASS" else "WARNING",
    paste0("Value-label mapping rows involved in a cross-system conflict=", label_conflict_count)
  )
  missing_difference_count <- sum(
    !official_occurrence$missing_attributes_match_modal,
    na.rm = TRUE
  )
  add_validation(
    16L,
    "Missing-code attribute differences",
    if (missing_difference_count == 0L) "PASS" else "WARNING",
    paste0("Official candidate occurrences with missing-attribute difference=", missing_difference_count)
  )
  core_entirely_missing <- variable_schema_audit[
    variable_schema_audit$gating_role == "CORE_TECHNICAL" &
      variable_schema_audit$present %in% TRUE &
      variable_schema_audit$valid_n == 0L,
    ,
    drop = FALSE
  ]
  core_variable_errors <- variable_schema_audit[
    variable_schema_audit$gating_role == "CORE_TECHNICAL" &
      variable_schema_audit$present %in% TRUE &
      nzchar(blank_if_na(variable_schema_audit$variable_error)),
    ,
    drop = FALSE
  ]
  add_validation(
    17L,
    "Entirely missing or unreadable core candidates",
    if (
      nrow(core_entirely_missing) == 0L && nrow(core_variable_errors) == 0L
    ) "PASS" else "FAIL",
    paste0(
      "Present-but-entirely-missing core rows=", nrow(core_entirely_missing),
      "; core candidate processing errors=", nrow(core_variable_errors)
    )
  )

  # Stable ordering before export.
  system_file_pairing <- system_file_pairing[order(
    system_file_pairing$country_system_code
  ), , drop = FALSE]
  candidate_variable_system_coverage <- candidate_variable_system_coverage[order(
    candidate_variable_system_coverage$country_system_code,
    candidate_variable_system_coverage$file_type,
    candidate_variable_system_coverage$candidate_category,
    candidate_variable_system_coverage$variable_name
  ), , drop = FALSE]
  premerge_key_audit <- premerge_key_audit[order(
    premerge_key_audit$country_system_code,
    premerge_key_audit$key_definition
  ), , drop = FALSE]
  pv_weight_design_audit <- pv_weight_design_audit[order(
    pv_weight_design_audit$country_system_code,
    pv_weight_design_audit$file_type,
    pv_weight_design_audit$audit_category,
    pv_weight_design_audit$audit_group,
    pv_weight_design_audit$variable_name
  ), , drop = FALSE]
  system_eligibility_premerge <- system_eligibility_premerge[order(
    system_eligibility_premerge$country_system_code
  ), , drop = FALSE]
  variable_schema_audit <- variable_schema_audit[order(
    variable_schema_audit$country_system_code,
    variable_schema_audit$file_type,
    variable_schema_audit$candidate_category,
    variable_schema_audit$variable_name
  ), , drop = FALSE]
  value_label_audit <- value_label_audit[order(
    value_label_audit$country_system_code,
    value_label_audit$file_type,
    value_label_audit$variable_name,
    value_label_audit$category_order
  ), , drop = FALSE]
  candidate_missingness_distribution <- candidate_missingness_distribution[order(
    candidate_missingness_distribution$country_system_code,
    candidate_missingness_distribution$file_type,
    candidate_missingness_distribution$variable_name,
    candidate_missingness_distribution$summary_type,
    candidate_missingness_distribution$category_code
  ), , drop = FALSE]
  harmonisation_issue_log <- harmonisation_issue_log[order(
    harmonisation_issue_log$country_system_code,
    harmonisation_issue_log$file_type,
    harmonisation_issue_log$candidate_category,
    harmonisation_issue_log$variable_name
  ), , drop = FALSE]

  if (!dir.exists(paths$archive_dir)) {
    ok <- dir.create(paths$archive_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok || !dir.exists(paths$archive_dir)) {
      stop("Could not create the Round 2A archive directory.", call. = FALSE)
    }
  }
  run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_existing <- function(path) {
    if (!file.exists(path)) return(invisible(NULL))
    destination <- file.path(
      paths$archive_dir,
      paste0(run_stamp, "_", basename(path))
    )
    ok <- file.rename(path, destination)
    if (!ok) {
      stop(
        "Existing Round 2A output could not be archived safely: ",
        to_relative_path(path),
        call. = FALSE
      )
    }
    message("Archived existing Round 2A output: ", to_relative_path(destination))
    invisible(destination)
  }
  for (path in output_paths) backup_existing(path)

  write_csv_checked <- function(data, path) {
    tryCatch({
      for (column_index in seq_along(data)) {
        column_value <- data[[column_index]]
        if (!is.null(dim(column_value))) {
          if (length(column_value) != nrow(data)) {
            stop(
              "Column ", names(data)[column_index],
              " has dimensions that cannot be converted safely."
            )
          }
          column_value <- as.vector(column_value)
        }
        if (is.list(column_value)) {
          stop("Column ", names(data)[column_index], " is a list column.")
        }
        data[[column_index]] <- column_value
      }
      readr::write_excel_csv(data, file = path, na = "")
      if (
        !file.exists(path) || is.na(file.info(path)$size) ||
          file.info(path)$size == 0L
      ) {
        stop("The file does not exist or has zero size.")
      }
    }, error = function(e) {
      stop(
        "Round 2A output could not be written: ", to_relative_path(path), "; ",
        conditionMessage(e), call. = FALSE
      )
    })
    invisible(TRUE)
  }

  csv_output_data <- list(
    system_file_pairing = system_file_pairing,
    candidate_variable_system_coverage = candidate_variable_system_coverage,
    premerge_key_audit = premerge_key_audit,
    pv_weight_design_audit = pv_weight_design_audit,
    system_eligibility_premerge = system_eligibility_premerge,
    variable_schema_audit = variable_schema_audit,
    value_label_audit = value_label_audit,
    candidate_missingness_distribution = candidate_missingness_distribution,
    harmonisation_issue_log = harmonisation_issue_log
  )
  for (name in names(csv_output_data)) {
    write_csv_checked(csv_output_data[[name]], output_paths[[name]])
  }

  loaded_package_lines <- vapply(
    required_packages,
    function(package) paste0(
      package, " ", as.character(utils::packageVersion(package))
    ),
    character(1)
  )
  session_lines <- c(
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Project root: ", project_root),
    paste0("R version: ", R.version.string),
    paste0("Operating system: ", Sys.info()[["sysname"]], " ", Sys.info()[["release"]]),
    paste0("Platform: ", R.version$platform),
    paste0("Locale: ", paste(Sys.getlocale(), collapse = "; ")),
    paste0("Actual system count: ", actual_system_count),
    paste0("Low-valid-n screening threshold: ", low_valid_n_threshold),
    paste0("Low-valid-proportion screening threshold: ", low_valid_proportion_threshold),
    "Loaded/required packages:",
    paste0("  - ", loaded_package_lines),
    "",
    "Complete sessionInfo():",
    capture.output(sessionInfo())
  )
  tryCatch(
    writeLines(
      session_lines,
      output_paths$session_info_harmonisation,
      useBytes = TRUE
    ),
    error = function(e) stop(
      "Could not write session_info_harmonisation.txt: ",
      conditionMessage(e),
      call. = FALSE
    )
  )

  expected_except_summary <- output_paths[
    names(output_paths) != "harmonisation_summary"
  ]
  other_outputs_ok <- all(vapply(
    expected_except_summary,
    function(path) {
      file.exists(path) && !is.na(file.info(path)$size) && file.info(path)$size > 0L
    },
    logical(1)
  ))
  add_validation(
    18L,
    "All required outputs generated",
    if (other_outputs_ok) "PASS" else "FAIL",
    paste0(
      paste(
        names(expected_except_summary),
        ifelse(vapply(expected_except_summary, file.exists, logical(1)), "present", "missing"),
        sep = "=",
        collapse = "; "
      ),
      "; harmonisation_summary=being_written"
    )
  )

  overall_validation <- if (any(validation$status == "FAIL")) {
    "FAIL"
  } else if (any(validation$status == "WARNING")) {
    "WARNING"
  } else {
    "PASS"
  }
  eligibility_table <- table(
    system_eligibility_premerge$preliminary_system_status,
    useNA = "ifany"
  )
  issue_type_table <- table(
    harmonisation_issue_log$issue_type,
    useNA = "ifany"
  )
  validation_output <- capture.output(print(validation, row.names = FALSE))
  eligibility_output <- capture.output(print(eligibility_table))
  issue_output <- if (length(issue_type_table) > 0L) {
    capture.output(print(issue_type_table))
  } else {
    "none"
  }

  summary_lines <- c(
    "TIMSS 2023 Grade 8 System Harmonisation and Pre-merge Audit",
    "===========================================================",
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Project root (local-only log): ", project_root),
    paste0("First-round metadata validation: PASS"),
    paste0("Actual country/system count: ", actual_system_count),
    paste0("BSA files reread successfully: ", sum(
      file_read_status$file_type == "BSA" & file_read_status$read_success
    ), "/", actual_system_count),
    paste0("BSG files reread successfully: ", sum(
      file_read_status$file_type == "BSG" & file_read_status$read_success
    ), "/", actual_system_count),
    paste0("File warnings captured: ", sum(file_read_status$warning_count)),
    paste0("Candidate definitions audited: ", nrow(candidate_registry)),
    paste0("Distinct candidate variable names audited: ", length(candidate_names)),
    paste0("Non-gating keyword definitions: ", sum(
      candidate_registry$gating_role == "NON_GATING_KEYWORD"
    )),
    paste0("Composite-key PASS systems: ", sum(
      composite_key_rows$audit_status == "PASS"
    ), "/", actual_system_count),
    paste0("Possible many-to-many systems: ", many_to_many_count),
    paste0("Complete content-domain PV groups: ", sum(
      core_pv_rows$all_present
    ), "/", nrow(core_pv_rows)),
    paste0("Value-label conflict rows: ", label_conflict_count),
    paste0("Missing-attribute difference occurrences: ", missing_difference_count),
    paste0("Structural/order difference occurrences: ", schema_difference_count),
    paste0("Low valid n threshold: ", low_valid_n_threshold),
    paste0("Low valid proportion threshold: ", low_valid_proportion_threshold),
    "",
    "Preliminary system statuses (not final inclusion decisions):",
    eligibility_output,
    "",
    "Harmonisation issue types:",
    issue_output,
    "",
    "Automatic validation checks:",
    validation_output,
    "",
    paste0("Final overall validation: ", overall_validation),
    "",
    "Interpretation safeguards:",
    "- READY_FOR_REVIEW is not a final inclusion decision.",
    "- Keyword-only candidates never determine system eligibility.",
    "- Possible special missing labels were not automatically recoded.",
    "- No student records or actual identifiers were written.",
    "- No full BSA/BSG join, descriptive outcome analysis, PV averaging, pooling, or model was run.",
    "",
    "Output files:",
    paste0("  - ", vapply(output_paths, to_relative_path, character(1)))
  )
  tryCatch(
    writeLines(
      summary_lines,
      output_paths$harmonisation_summary,
      useBytes = TRUE
    ),
    error = function(e) stop(
      "Could not write harmonisation_summary.txt: ", conditionMessage(e),
      call. = FALSE
    )
  )

  all_outputs_ok <- all(vapply(
    output_paths,
    function(path) {
      file.exists(path) && !is.na(file.info(path)$size) && file.info(path)$size > 0L
    },
    logical(1)
  ))
  if (!all_outputs_ok) {
    stop("At least one required Round 2A output is missing or empty.", call. = FALSE)
  }

  message("Round 2A audit completed.")
  message("Actual systems: ", actual_system_count)
  message(
    "Successful files: ", sum(file_read_status$read_success),
    "; failed files: ", sum(!file_read_status$read_success),
    "; warnings: ", sum(file_read_status$warning_count)
  )
  message("Final validation: ", overall_validation)
  message("Preliminary system status:")
  message(paste(capture.output(print(eligibility_table)), collapse = "\n"))
  message("Output files:")
  for (path in output_paths) message("  - ", to_relative_path(path))
})
