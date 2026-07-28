#!/usr/bin/env Rscript

source(file.path("config", "analysis_config.R"))
source(file.path("R", "timss_helpers.R"))

require_packages(c(
  "haven", "dplyr", "readr", "stringr", "tibble", "tidyr"
))

raw_dir <- analysis_config$input$raw_dir
output_dir <- analysis_config$output$dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

student_files <- list_student_files(
  raw_dir = raw_dir,
  file_regex = analysis_config$input$student_file_regex
)

message("Found ", length(student_files), " TIMSS Grade 8 student file(s).")

first_data <- read_iea_data(student_files[[1L]])
first_catalogue <- catalogue_variables(first_data, student_files[[1L]])

required_resolution <- resolve_required_variables(first_data, analysis_config)
resource_resolution <- resolve_named_group(
  first_data,
  analysis_config$variables$resources,
  "resource"
)
control_resolution <- resolve_named_group(
  first_data,
  analysis_config$variables$controls,
  "control"
)
resolution <- rbind(
  required_resolution,
  resource_resolution,
  control_resolution
)

safe_write_csv(
  first_catalogue,
  file.path(output_dir, "variable_inventory_first_file.csv")
)
safe_write_csv(
  resolution,
  file.path(output_dir, "variable_resolution.csv")
)

required_identifier_constructs <- c("country", "school", "student")
required_design_constructs <- c("total_weight", "jk_zone", "jk_replicate")

missing_required <- resolution[
  (
    resolution$group == "identifier" &
      resolution$construct %in% required_identifier_constructs &
      is.na(resolution$variable)
  ) |
    (
      resolution$group == "survey_design" &
        resolution$construct %in% required_design_constructs &
        is.na(resolution$variable)
    ),
  ,
  drop = FALSE
]

outcome_failures <- resolution[
  resolution$group == "outcome" &
    resolution$method != "all five plausible values found",
  ,
  drop = FALSE
]

if (nrow(missing_required) > 0L || nrow(outcome_failures) > 0L) {
  stop(
    paste0(
      "Required variables were not resolved. Inspect ",
      file.path(output_dir, "variable_resolution.csv"),
      " before continuing."
    ),
    call. = FALSE
  )
}

resolved_lookup <- setNames(
  resolution$variable[
    resolution$group %in% c("resource", "control") &
      !is.na(resolution$variable)
  ],
  resolution$construct[
    resolution$group %in% c("resource", "control") &
      !is.na(resolution$variable)
  ]
)

get_resolved <- function(data, construct) {
  variable <- resolved_lookup[[construct]] %||% NA_character_
  if (is.na(variable) || !variable %in% names(data)) {
    return(rep(NA, nrow(data)))
  }
  data[[variable]]
}

identifier_names <- vapply(
  analysis_config$variables$identifiers,
  function(candidates) {
    resolve_exact(names(first_data), candidates) %||% NA_character_
  },
  character(1)
)
survey_names <- vapply(
  analysis_config$variables$survey_design,
  function(candidates) {
    resolve_exact(names(first_data), candidates) %||% NA_character_
  },
  character(1)
)

extract_one_file <- function(path) {
  message("Reading ", basename(path))
  raw <- read_iea_data(path)

  required_names <- c(
    unname(identifier_names[c("country", "school", "student")]),
    unname(survey_names[c("total_weight", "jk_zone", "jk_replicate")]),
    unlist(analysis_config$variables$outcomes, use.names = FALSE)
  )
  absent <- required_names[!normalise_name(required_names) %in% normalise_name(names(raw))]
  if (length(absent) > 0L) {
    stop(
      "Required variables are absent from ", basename(path), ": ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }

  exact_name <- function(expected) {
    resolved <- resolve_exact(names(raw), expected)
    if (length(resolved) != 1L) {
      stop("Variable not found in ", basename(path), ": ", expected, call. = FALSE)
    }
    resolved
  }

  out <- tibble::tibble(
    source_file = basename(path),
    idcntry = as.integer(as_numeric_clean(raw[[exact_name(identifier_names[["country"]])]])),
    country_name = as_character_clean(
      raw[[exact_name(identifier_names[["country"]])]]
    ),
    idschool = as_character_clean(raw[[exact_name(identifier_names[["school"]])]]),
    idstudent = as_character_clean(raw[[exact_name(identifier_names[["student"]])]]),
    totwgt = as_numeric_clean(raw[[exact_name(survey_names[["total_weight"]])]]),
    jkzone = as.integer(as_numeric_clean(raw[[exact_name(survey_names[["jk_zone"]])]])),
    jkrep = as.integer(as_numeric_clean(raw[[exact_name(survey_names[["jk_replicate"]])]]))
  )

  if (!is.na(identifier_names[["class"]]) &&
      identifier_names[["class"]] %in% names(raw)) {
    out$idclass <- as_character_clean(raw[[identifier_names[["class"]]]])
  } else {
    out$idclass <- NA_character_
  }

  if (!is.na(survey_names[["senate_weight"]]) &&
      survey_names[["senate_weight"]] %in% names(raw)) {
    out$senwgt <- as_numeric_clean(raw[[survey_names[["senate_weight"]]]])
  } else {
    out$senwgt <- NA_real_
  }

  if (!is.na(survey_names[["house_weight"]]) &&
      survey_names[["house_weight"]] %in% names(raw)) {
    out$houwgt <- as_numeric_clean(raw[[survey_names[["house_weight"]]]])
  } else {
    out$houwgt <- NA_real_
  }

  for (domain in names(analysis_config$variables$outcomes)) {
    plausible_values <- analysis_config$variables$outcomes[[domain]]
    for (index in seq_along(plausible_values)) {
      source_name <- exact_name(plausible_values[[index]])
      target_name <- paste0("pv_", domain, "_", index)
      out[[target_name]] <- as_numeric_clean(raw[[source_name]])
    }
  }

  out$her_scale <- as_numeric_clean(
    get_resolved(raw, "home_educational_resources_scale")
  )
  out$her_category <- as_character_clean(
    get_resolved(raw, "home_educational_resources_category")
  )

  access_constructs <- c(
    "own_computer_or_tablet",
    "shared_computer_or_tablet",
    "smartphone",
    "internet_access"
  )
  for (construct in access_constructs) {
    out[[construct]] <- valid_binary_yes(get_resolved(raw, construct))
  }

  use_constructs <- c(
    "schoolwork_textbook_materials",
    "schoolwork_assignments",
    "schoolwork_collaboration",
    "schoolwork_teacher_questions",
    "schoolwork_information_tutorials",
    "schoolwork_learning_games"
  )
  for (construct in use_constructs) {
    out[[construct]] <- reverse_frequency_three(get_resolved(raw, construct))
  }

  out$ict_home_access_index <- row_mean_minimum(
    out,
    c(
      "own_computer_or_tablet",
      "shared_computer_or_tablet",
      "internet_access"
    ),
    minimum_non_missing = 2L
  )
  out$ict_schoolwork_use_index <- row_mean_minimum(
    out,
    use_constructs,
    minimum_non_missing = 4L
  )

  out$sex <- as_character_clean(get_resolved(raw, "sex"))
  out$age <- as_numeric_clean(get_resolved(raw, "age"))
  out$language_of_test_at_home <- as_character_clean(
    get_resolved(raw, "language_of_test_at_home")
  )
  out$books_at_home <- ordered_numeric_without_unknown(
    get_resolved(raw, "books_at_home")
  )
  out$parent_a_education <- ordered_numeric_without_unknown(
    get_resolved(raw, "parent_a_education")
  )
  out$parent_b_education <- ordered_numeric_without_unknown(
    get_resolved(raw, "parent_b_education")
  )
  out$parent_highest_education <- pmax(
    out$parent_a_education,
    out$parent_b_education,
    na.rm = TRUE
  )
  out$parent_highest_education[
    is.na(out$parent_a_education) & is.na(out$parent_b_education)
  ] <- NA_real_
  out$student_born_in_country <- valid_binary_yes(
    get_resolved(raw, "student_born_in_country")
  )

  out
}

data_parts <- lapply(student_files, extract_one_file)
analytic <- dplyr::bind_rows(data_parts)
rm(data_parts, first_data)
invisible(gc())

sample_flow <- data.frame(
  stage = "All Grade 8 student records read",
  students = nrow(analytic),
  schools = dplyr::n_distinct(
    paste(analytic$idcntry, analytic$idschool, sep = "::")
  ),
  education_systems = dplyr::n_distinct(analytic$idcntry),
  stringsAsFactors = FALSE
)

selected_jurisdictions <- analysis_config$sample$jurisdictions
if (!is.null(selected_jurisdictions)) {
  analytic <- analytic[
    analytic$idcntry %in% as.integer(selected_jurisdictions),
    ,
    drop = FALSE
  ]
  sample_flow <- rbind(
    sample_flow,
    data.frame(
      stage = "After jurisdiction restriction",
      students = nrow(analytic),
      schools = dplyr::n_distinct(
        paste(analytic$idcntry, analytic$idschool, sep = "::")
      ),
      education_systems = dplyr::n_distinct(analytic$idcntry),
      stringsAsFactors = FALSE
    )
  )
}

valid_design <- with(
  analytic,
  is.finite(totwgt) & totwgt > 0 &
    !is.na(jkzone) & jkzone > 0 &
    jkrep %in% c(0L, 1L) &
    !is.na(idcntry) &
    nzchar(idschool)
)
analytic <- analytic[valid_design, , drop = FALSE]

sample_flow <- rbind(
  sample_flow,
  data.frame(
    stage = "Valid weight, JRR variables, country, and school ID",
    students = nrow(analytic),
    schools = dplyr::n_distinct(
      paste(analytic$idcntry, analytic$idschool, sep = "::")
    ),
    education_systems = dplyr::n_distinct(analytic$idcntry),
    stringsAsFactors = FALSE
  )
)

country_counts <- analytic |>
  dplyr::count(idcntry, name = "student_n")
retained_countries <- country_counts$idcntry[
  country_counts$student_n >= analysis_config$sample$minimum_country_n
]
analytic <- analytic[
  analytic$idcntry %in% retained_countries,
  ,
  drop = FALSE
]

sample_flow <- rbind(
  sample_flow,
  data.frame(
    stage = paste0(
      "Education systems with at least ",
      analysis_config$sample$minimum_country_n,
      " students"
    ),
    students = nrow(analytic),
    schools = dplyr::n_distinct(
      paste(analytic$idcntry, analytic$idschool, sep = "::")
    ),
    education_systems = dplyr::n_distinct(analytic$idcntry),
    stringsAsFactors = FALSE
  )
)

analytic <- analytic |>
  dplyr::group_by(idcntry) |>
  dplyr::mutate(
    weight_equal_country = totwgt / sum(totwgt, na.rm = TRUE),
    school_id_global = paste(idcntry, idschool, sep = "::"),
    student_id_global = paste(idcntry, idschool, idstudent, sep = "::")
  ) |>
  dplyr::ungroup()

analysis_variables <- c(
  "her_scale",
  "her_category",
  "ict_home_access_index",
  "ict_schoolwork_use_index",
  "own_computer_or_tablet",
  "shared_computer_or_tablet",
  "smartphone",
  "internet_access",
  "sex",
  "age",
  "language_of_test_at_home",
  "books_at_home",
  "parent_highest_education",
  "student_born_in_country"
)

missingness <- analytic |>
  dplyr::group_by(idcntry, country_name) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(analysis_variables),
      ~ mean(is.na(.x)),
      .names = "missing__{.col}"
    ),
    student_n = dplyr::n(),
    school_n = dplyr::n_distinct(school_id_global),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    dplyr::starts_with("missing__"),
    names_to = "variable",
    names_prefix = "missing__",
    values_to = "missing_proportion"
  )

file_summary <- analytic |>
  dplyr::group_by(source_file, idcntry, country_name) |>
  dplyr::summarise(
    student_n = dplyr::n(),
    school_n = dplyr::n_distinct(school_id_global),
    sum_totwgt = sum(totwgt, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(
  analytic,
  file.path(output_dir, analysis_config$output$analytic_file),
  compress = "xz"
)
safe_write_csv(sample_flow, file.path(output_dir, "sample_flow.csv"))
safe_write_csv(missingness, file.path(output_dir, "missingness_by_country.csv"))
safe_write_csv(file_summary, file.path(output_dir, "file_summary.csv"))

message(
  "Analytic file written to ",
  file.path(output_dir, analysis_config$output$analytic_file)
)
