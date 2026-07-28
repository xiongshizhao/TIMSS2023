#!/usr/bin/env Rscript

source(file.path("config", "analysis_config.R"))
source(file.path("R", "timss_helpers.R"))

require_packages(c(
  "dplyr", "purrr", "readr", "survey", "tibble", "tidyr"
))

output_dir <- analysis_config$output$dir
analytic_path <- file.path(
  output_dir,
  analysis_config$output$analytic_file
)
if (!file.exists(analytic_path)) {
  stop(
    "Analytic file not found. Run 01_build_analytic_data.R first.",
    call. = FALSE
  )
}

analytic <- readRDS(analytic_path)
confidence_level <- analysis_config$analysis$confidence_level
missingness_limit <- analysis_config$sample$maximum_predictor_missingness

weighted_standardise <- function(x, weight) {
  valid <- is.finite(x) & is.finite(weight) & weight > 0
  result <- rep(NA_real_, length(x))
  if (sum(valid) < 2L) {
    return(result)
  }
  centre <- stats::weighted.mean(x[valid], weight[valid])
  variance <- stats::weighted.mean((x[valid] - centre)^2, weight[valid])
  if (!is.finite(variance) || variance <= 0) {
    return(result)
  }
  result[valid] <- (x[valid] - centre) / sqrt(variance)
  result
}

analytic <- analytic |>
  dplyr::group_by(idcntry) |>
  dplyr::mutate(
    her_z = weighted_standardise(her_scale, totwgt),
    ict_home_access_z = weighted_standardise(
      ict_home_access_index,
      totwgt
    ),
    ict_schoolwork_use_z = weighted_standardise(
      ict_schoolwork_use_index,
      totwgt
    ),
    age_c = age - stats::weighted.mean(
      age,
      totwgt,
      na.rm = TRUE
    ),
    books_z = weighted_standardise(books_at_home, totwgt),
    parent_education_z = weighted_standardise(
      parent_highest_education,
      totwgt
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    idcntry_factor = factor(idcntry),
    sex_factor = factor(sex),
    language_factor = factor(language_of_test_at_home),
    her_category_factor = factor(her_category)
  )

domain_pvs <- lapply(
  names(analysis_config$variables$outcomes),
  function(domain) paste0("pv_", domain, "_", 1:5)
)
names(domain_pvs) <- names(analysis_config$variables$outcomes)

model_specs <- list(
  her = list(
    predictor = "her_z",
    controls = c("sex_factor", "age_c", "language_factor"),
    note = paste(
      "HER model. ICT is not entered simultaneously because the TIMSS 2023",
      "HER scale includes home study supports."
    )
  ),
  ict_home_access = list(
    predictor = "ict_home_access_z",
    controls = c(
      "sex_factor",
      "age_c",
      "language_factor",
      "books_z",
      "parent_education_z"
    ),
    note = paste(
      "Home ICT access model. Books and parental education are used as",
      "background controls instead of BSBGHER to avoid direct item overlap."
    )
  ),
  ict_schoolwork_use = list(
    predictor = "ict_schoolwork_use_z",
    controls = c(
      "sex_factor",
      "age_c",
      "language_factor",
      "books_z",
      "parent_education_z"
    ),
    note = paste(
      "Internet use for mathematics/science schoolwork model. A higher score",
      "means more frequent use across BSBG14A-F."
    )
  )
)

eligible_terms <- function(data, terms, missingness_limit) {
  terms[vapply(terms, function(term) {
    x <- data[[term]]
    non_missing <- !is.na(x)
    mean(!non_missing) <= missingness_limit &&
      length(unique(x[non_missing])) >= 2L
  }, logical(1))]
}

make_formula <- function(outcome, predictor, controls, country_fixed_effects) {
  right_hand_side <- c(
    predictor,
    controls,
    if (country_fixed_effects) "idcntry_factor" else character(0)
  )
  stats::as.formula(
    paste(outcome, "~", paste(right_hand_side, collapse = " + "))
  )
}

fit_pv_models <- function(
    design,
    data_for_terms,
    pv_variables,
    specification,
    country_fixed_effects) {
  controls <- eligible_terms(
    data_for_terms,
    specification$controls,
    missingness_limit
  )

  if (!specification$predictor %in% eligible_terms(
    data_for_terms,
    specification$predictor,
    missingness_limit
  )) {
    stop("Primary predictor is unavailable or has inadequate variation.")
  }

  model_tables <- lapply(pv_variables, function(pv) {
    formula <- make_formula(
      outcome = pv,
      predictor = specification$predictor,
      controls = controls,
      country_fixed_effects = country_fixed_effects
    )
    model <- survey::svyglm(
      formula,
      design = design,
      na.action = stats::na.omit
    )
    table <- coefficient_table(model)
    attr(table, "complete_case_n") <- stats::nobs(model)
    table
  })

  result <- pool_model_tables(
    model_tables,
    confidence_level = confidence_level
  )
  result$controls_used <- paste(controls, collapse = " | ")
  result$complete_case_n_min <- min(vapply(
    model_tables,
    function(table) attr(table, "complete_case_n"),
    numeric(1)
  ))
  result$complete_case_n_max <- max(vapply(
    model_tables,
    function(table) attr(table, "complete_case_n"),
    numeric(1)
  ))
  result
}

model_log <- list()
append_log <- function(
    scope,
    country,
    domain,
    model_name,
    status,
    detail) {
  model_log[[length(model_log) + 1L]] <<- data.frame(
    scope = scope,
    idcntry = country,
    domain = domain,
    model = model_name,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

domain_descriptive_rows <- list()
resource_descriptive_rows <- list()
country_model_rows <- list()

country_ids <- sort(unique(analytic$idcntry))
for (country_id in country_ids) {
  message("Country-specific descriptives: ", country_id)
  country_data <- analytic[analytic$idcntry == country_id, , drop = FALSE]
  country_name <- unique(country_data$country_name)
  country_name <- country_name[!is.na(country_name)]
  country_name <- if (length(country_name) == 0L) {
    as.character(country_id)
  } else {
    country_name[[1L]]
  }
  country_design <- tryCatch(
    make_jrr_design(
      country_data,
      weight = "totwgt",
      zone = "jkzone",
      replicate = "jkrep",
      number_zones = analysis_config$analysis$jrr_variance_zones
    ),
    error = function(error) {
      append_log(
        "country",
        country_id,
        NA_character_,
        "design",
        "failed",
        conditionMessage(error)
      )
      NULL
    }
  )
  if (is.null(country_design)) {
    next
  }

  for (domain in names(domain_pvs)) {
    pv_results <- lapply(domain_pvs[[domain]], function(pv) {
      estimate <- survey::svymean(
        stats::as.formula(paste0("~", pv)),
        design = country_design,
        na.rm = TRUE
      )
      c(
        estimate = unname(stats::coef(estimate)[[1L]]),
        variance = unname(stats::vcov(estimate)[1L, 1L])
      )
    })
    pooled <- pool_plausible_values(
      estimates = vapply(pv_results, `[[`, numeric(1), "estimate"),
      variances = vapply(pv_results, `[[`, numeric(1), "variance")
    )
    domain_descriptive_rows[[length(domain_descriptive_rows) + 1L]] <-
      data.frame(
        idcntry = country_id,
        country_name = country_name,
        domain = domain,
        estimate = unname(pooled[["estimate"]]),
        std_error = unname(pooled[["std_error"]]),
        student_n = nrow(country_data),
        school_n = dplyr::n_distinct(country_data$school_id_global),
        stringsAsFactors = FALSE
      )
  }

  resource_variables <- c(
    her_scale = "her_scale",
    ict_home_access_index = "ict_home_access_index",
    ict_schoolwork_use_index = "ict_schoolwork_use_index",
    own_computer_or_tablet = "own_computer_or_tablet",
    shared_computer_or_tablet = "shared_computer_or_tablet",
    smartphone = "smartphone",
    internet_access = "internet_access"
  )
  for (resource_name in names(resource_variables)) {
    variable <- resource_variables[[resource_name]]
    result <- tryCatch(
      survey::svymean(
        stats::as.formula(paste0("~", variable)),
        design = country_design,
        na.rm = TRUE
      ),
      error = function(error) NULL
    )
    if (!is.null(result)) {
      resource_descriptive_rows[[length(resource_descriptive_rows) + 1L]] <-
      data.frame(
        idcntry = country_id,
        country_name = country_name,
        resource = resource_name,
        estimate = unname(stats::coef(result)[[1L]]),
        std_error = sqrt(unname(stats::vcov(result)[1L, 1L])),
        non_missing_n = sum(!is.na(country_data[[variable]])),
        missing_proportion = mean(is.na(country_data[[variable]])),
        stringsAsFactors = FALSE
      )
    }
  }

  if (isTRUE(analysis_config$analysis$run_country_specific_models)) {
    for (domain in names(domain_pvs)) {
      for (model_name in names(model_specs)) {
        result <- tryCatch(
          fit_pv_models(
            design = country_design,
            data_for_terms = country_data,
            pv_variables = domain_pvs[[domain]],
            specification = model_specs[[model_name]],
            country_fixed_effects = FALSE
          ),
          error = function(error) {
            append_log(
              "country",
              country_id,
              domain,
              model_name,
              "failed",
              conditionMessage(error)
            )
            NULL
          }
        )
        if (!is.null(result)) {
          result$scope <- "country"
          result$idcntry <- country_id
          result$country_name <- country_name
          result$domain <- domain
          result$model <- model_name
          result$model_note <- model_specs[[model_name]]$note
          country_model_rows[[length(country_model_rows) + 1L]] <- result
          append_log(
            "country",
            country_id,
            domain,
            model_name,
            "succeeded",
            ""
          )
        }
      }
    }
  }
}

domain_descriptives <- dplyr::bind_rows(domain_descriptive_rows)
resource_descriptives <- dplyr::bind_rows(resource_descriptive_rows)
country_models <- dplyr::bind_rows(country_model_rows)

safe_write_csv(
  domain_descriptives,
  file.path(output_dir, "domain_descriptives_by_country.csv")
)
safe_write_csv(
  resource_descriptives,
  file.path(output_dir, "resource_descriptives_by_country.csv")
)
if (nrow(country_models) > 0L) {
  safe_write_csv(
    country_models,
    file.path(output_dir, "preliminary_regressions_by_country.csv")
  )
}

pooled_model_rows <- list()
interaction_model_rows <- list()

if (isTRUE(analysis_config$analysis$run_pooled_fixed_effect_models)) {
  message("Building pooled equal-country JRR design.")
  pooled_design <- make_jrr_design(
    analytic,
    weight = "weight_equal_country",
    zone = "jkzone",
    replicate = "jkrep",
    number_zones = analysis_config$analysis$jrr_variance_zones
  )

  for (domain in names(domain_pvs)) {
    for (model_name in names(model_specs)) {
      result <- tryCatch(
        fit_pv_models(
          design = pooled_design,
          data_for_terms = analytic,
          pv_variables = domain_pvs[[domain]],
          specification = model_specs[[model_name]],
          country_fixed_effects = TRUE
        ),
        error = function(error) {
          append_log(
            "pooled",
            NA_integer_,
            domain,
            model_name,
            "failed",
            conditionMessage(error)
          )
          NULL
        }
      )
      if (!is.null(result)) {
        result <- result[
          !grepl("^idcntry_factor", result$term),
          ,
          drop = FALSE
        ]
        result$scope <- "pooled_equal_country"
        result$idcntry <- NA_integer_
        result$country_name <- "Equal-country pooled estimate"
        result$domain <- domain
        result$model <- model_name
        result$model_note <- model_specs[[model_name]]$note
        pooled_model_rows[[length(pooled_model_rows) + 1L]] <- result
        append_log(
          "pooled",
          NA_integer_,
          domain,
          model_name,
          "succeeded",
          ""
        )
      }
    }
  }

  for (model_name in names(model_specs)) {
    specification <- model_specs[[model_name]]
    controls <- eligible_terms(
      analytic,
      specification$controls,
      missingness_limit
    )

    pv_tables <- list()
    interaction_error <- NULL
    for (pv_index in 1:5) {
      long_data <- dplyr::bind_rows(lapply(
        names(domain_pvs),
        function(domain) {
          current <- analytic
          current$domain <- domain
          current$achievement <-
            current[[domain_pvs[[domain]][[pv_index]]]]
          current
        }
      ))
      long_data$domain <- stats::relevel(
        factor(long_data$domain),
        ref = "data_probability"
      )

      long_design <- tryCatch(
        make_jrr_design(
          long_data,
          weight = "weight_equal_country",
          zone = "jkzone",
          replicate = "jkrep",
          number_zones = analysis_config$analysis$jrr_variance_zones
        ),
        error = function(error) {
          interaction_error <<- conditionMessage(error)
          NULL
        }
      )
      if (is.null(long_design)) {
        break
      }

      formula <- stats::as.formula(paste(
        "achievement ~ domain *",
        specification$predictor,
        if (length(controls) > 0L) {
          paste("+", paste(controls, collapse = " + "))
        } else {
          ""
        },
        "+ idcntry_factor"
      ))
      fitted <- tryCatch(
        survey::svyglm(
          formula,
          design = long_design,
          na.action = stats::na.omit
        ),
        error = function(error) {
          interaction_error <<- conditionMessage(error)
          NULL
        }
      )
      if (is.null(fitted)) {
        break
      }
      pv_tables[[pv_index]] <- coefficient_table(fitted)
      attr(pv_tables[[pv_index]], "complete_case_n") <- stats::nobs(fitted)
    }

    if (length(pv_tables) == 5L) {
      result <- pool_model_tables(
        pv_tables,
        confidence_level = confidence_level
      )
      result <- result[
        result$term == specification$predictor |
          grepl(
            paste0(":", specification$predictor, "$"),
            result$term
          ),
        ,
        drop = FALSE
      ]
      result$reference_domain <- "data_probability"
      result$interpretation <- ifelse(
        result$term == specification$predictor,
        "Resource association in Data and Probability",
        paste(
          "Difference from Data and Probability;",
          "negative means the association is stronger in Data and Probability"
        )
      )
      result$model <- model_name
      result$controls_used <- paste(controls, collapse = " | ")
      result$model_note <- specification$note
      result$complete_case_n_min <- min(vapply(
        pv_tables,
        function(table) attr(table, "complete_case_n"),
        numeric(1)
      ))
      result$complete_case_n_max <- max(vapply(
        pv_tables,
        function(table) attr(table, "complete_case_n"),
        numeric(1)
      ))
      interaction_model_rows[[length(interaction_model_rows) + 1L]] <- result
      append_log(
        "pooled_interaction",
        NA_integer_,
        "all",
        model_name,
        "succeeded",
        ""
      )
    } else {
      append_log(
        "pooled_interaction",
        NA_integer_,
        "all",
        model_name,
        "failed",
        interaction_error %||% "Unknown interaction-model failure."
      )
    }
  }
}

pooled_models <- dplyr::bind_rows(pooled_model_rows)
interaction_models <- dplyr::bind_rows(interaction_model_rows)
if (nrow(pooled_models) > 0L) {
  safe_write_csv(
    pooled_models,
    file.path(output_dir, "preliminary_regressions_pooled.csv")
  )
}
if (nrow(interaction_models) > 0L) {
  safe_write_csv(
    interaction_models,
    file.path(output_dir, "preliminary_domain_interactions.csv")
  )
}

model_log_table <- dplyr::bind_rows(model_log)
safe_write_csv(
  model_log_table,
  file.path(output_dir, "model_run_log.csv")
)

message("Preliminary outputs written to ", output_dir)
