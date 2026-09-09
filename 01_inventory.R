# TIMSS 2023 Grade 8: local BSA/BSG metadata inventory
#
# Scope:
#   - inventory files, RData objects, variable metadata, candidate variables,
#     country/system coverage, and preliminary BSA/BSG pairing;
#   - never merge data, export record-level values, or run an analysis.
#
# Official references used to define exact candidate names:
#   - TIMSS 2023 International Database User Guide, pp. 52, 63, 69-74
#   - T23_Codebook_G8.xlsx, worksheets BSAM8 and BSGM8
#   - T23_G8_Student Questionnaire Variables.xlsx, worksheet SQ
#   - T23_G8_Student Derived Variables.pdf, pp. 1-2

required_packages <- c("here", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "缺少所需R packages: ", paste(missing_packages, collapse = ", "), ".\n",
      "请先在R Console运行：\n",
      "install.packages(c(",
      paste(sprintf("\"%s\"", missing_packages), collapse = ", "),
      "))\n",
      "安装完成后重启R session，再重新Source本脚本。"
    ),
    call. = FALSE
  )
}

project_root <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)

relative_paths <- list(
  bsa_dir = "data_raw/local_only/BSA_achievement_grade8",
  bsg_dir = "data_raw/local_only/BSG_student_questionnaire_grade8",
  metadata_dir = "data_metadata",
  local_metadata_dir = "data_metadata/local_only"
)

paths <- lapply(relative_paths, function(x) here::here(x))

message("Project root: ", project_root)
message("本脚本仅盘点metadata；不会修改RData或导出学生记录。")

for (input_name in c("bsa_dir", "bsg_dir")) {
  if (!dir.exists(paths[[input_name]])) {
    stop(
      paste0(
        "未找到输入目录：", relative_paths[[input_name]], "\n",
        "项目根目录为：", project_root, "\n",
        "请在RStudio Files面板检查 data_raw/local_only/ 下的文件夹名称，",
        "并确认已打开TIMSS_2023的RStudio Project。"
      ),
      call. = FALSE
    )
  }
}

for (output_name in c("metadata_dir", "local_metadata_dir")) {
  if (!dir.exists(paths[[output_name]])) {
    ok <- dir.create(paths[[output_name]], recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(paths[[output_name]])) {
      stop(
        "无法创建输出目录：", relative_paths[[output_name]],
        call. = FALSE
      )
    }
    message("已创建输出目录：", relative_paths[[output_name]])
  }
}

check_directory_writable <- function(path, relative_path) {
  probe <- tempfile(pattern = "inventory_write_test_", tmpdir = path)
  ok <- tryCatch(
    {
      writeLines("write test", probe, useBytes = TRUE)
      file.exists(probe)
    },
    error = function(e) FALSE
  )
  if (file.exists(probe)) {
    unlink(probe)
  }
  if (!isTRUE(ok)) {
    stop(
      "输出目录不可写：", relative_path,
      "。请检查Mac文件权限或RStudio Project位置。",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_directory_writable(paths$metadata_dir, relative_paths$metadata_dir)
check_directory_writable(paths$local_metadata_dir, relative_paths$local_metadata_dir)

bsa_regex <- "^BSA[A-Z0-9]{3}M8\\.RData$"
bsg_regex <- "^BSG[A-Z0-9]{3}M8\\.RData$"

find_rdata_files <- function(directory, regex, file_type) {
  matched <- list.files(
    path = directory,
    pattern = regex,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  matched <- sort(matched)

  if (length(matched) == 0L) {
    visible_files <- list.files(
      path = directory,
      full.names = FALSE,
      recursive = FALSE,
      all.files = FALSE
    )
    sample_files <- if (length(visible_files) == 0L) {
      "<目录为空>"
    } else {
      paste(utils::head(visible_files, 20L), collapse = ", ")
    }
    stop(
      paste0(
        "未找到任何", file_type, "文件。\n",
        "实际搜索目录：", normalizePath(directory, winslash = "/", mustWork = TRUE), "\n",
        "搜索方式：recursive = FALSE（只搜索该目录，不搜索子目录）\n",
        "使用的regular expression：", regex, "\n",
        "目录中发现的部分文件名：", sample_files, "\n",
        "请检查文件是否放错文件夹、是否仍在ZIP中、文件名大小写或扩展名是否不同。"
      ),
      call. = FALSE
    )
  }
  matched
}

bsa_files <- find_rdata_files(paths$bsa_dir, bsa_regex, "BSA")
bsg_files <- find_rdata_files(paths$bsg_dir, bsg_regex, "BSG")

message("找到BSA文件：", length(bsa_files))
message("找到BSG文件：", length(bsg_files))

extract_system_code <- function(file_name, file_type) {
  regex <- if (identical(file_type, "BSA")) {
    "^BSA([A-Z0-9]{3})M8\\.RData$"
  } else {
    "^BSG([A-Z0-9]{3})M8\\.RData$"
  }
  
  if (!grepl(regex, file_name, ignore.case = TRUE)) {
    stop(
      paste0(
        "无法从文件名提取国家/系统代码：",
        file_name
      ),
      call. = FALSE
    )
  }
  
  toupper(
    sub(
      regex,
      "\\1",
      file_name,
      ignore.case = TRUE
    )
  )
}

make_file_index <- function(files, file_type) {
  file_names <- basename(files)
  data.frame(
    full_path = files,
    file_name = file_names,
    file_type = file_type,
    country_system_code = vapply(
      file_names,
      extract_system_code,
      character(1),
      file_type = file_type
    ),
    file_size_bytes = as.numeric(file.info(files)$size),
    stringsAsFactors = FALSE
  )
}

file_index <- rbind(
  make_file_index(bsa_files, "BSA"),
  make_file_index(bsg_files, "BSG")
)
row.names(file_index) <- NULL

to_relative_path <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root_prefix <- paste0(project_root, "/")
  if (startsWith(normalized, root_prefix)) {
    substring(normalized, nchar(root_prefix) + 1L)
  } else if (identical(normalized, project_root)) {
    "."
  } else {
    basename(normalized)
  }
}

sanitize_message <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return("")
  x <- paste(x[!is.na(x)], collapse = " | ")
  gsub(project_root, "<PROJECT_ROOT>", x, fixed = TRUE)
}

collapse_attribute <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  out <- tryCatch(
    paste(as.character(unlist(x, recursive = TRUE, use.names = FALSE)), collapse = " | "),
    error = function(e) paste0("<unprintable: ", conditionMessage(e), ">")
  )
  if (!nzchar(out)) NA_character_ else out
}

first_non_null_attribute <- function(x, candidates) {
  for (nm in candidates) {
    value <- attr(x, nm, exact = TRUE)
    if (!is.null(value)) return(value)
  }
  NULL
}

classify_object <- function(x) {
  if (inherits(x, "tbl_df")) return("tibble")
  if (is.data.frame(x)) return("data.frame")
  if (is.matrix(x)) return("matrix")
  "other"
}

is_tabular_object <- function(x) {
  is.data.frame(x) || is.matrix(x)
}

empty_manifest <- function() {
  data.frame(
    relative_path = character(), file_name = character(), file_type = character(),
    country_system_code = character(), file_size_bytes = numeric(),
    object_name = character(), object_class = character(), object_kind = character(),
    rows = integer(), columns = integer(), main_candidate_status = character(),
    read_success = logical(), read_status = character(), warning_count = integer(),
    warning_message = character(), error_message = character(),
    stringsAsFactors = FALSE
  )
}

empty_variable_inventory <- function() {
  data.frame(
    file_type = character(), country_system_code = character(),
    source_file = character(), relative_path = character(), source_object = character(),
    variable_name = character(), variable_label = character(), r_class = character(),
    storage_type = character(), is_factor = logical(), is_labelled = logical(),
    has_value_labels = logical(), value_label_count = integer(),
    has_missing_attribute = logical(), missing_attribute_type = character(),
    file_type_presence = character(),
    stringsAsFactors = FALSE
  )
}

inspect_variable <- function(x, file_row, object_name, variable_name) {
  variable_label <- first_non_null_attribute(
    x,
    c("label", "variable.label", "var.label")
  )
  value_labels <- first_non_null_attribute(x, c("labels", "value.labels"))
  factor_levels <- if (is.factor(x)) levels(x) else NULL
  value_label_count <- if (!is.null(value_labels)) {
    length(value_labels)
  } else if (!is.null(factor_levels)) {
    length(factor_levels)
  } else {
    0L
  }

  attribute_names <- names(attributes(x))
  if (is.null(attribute_names)) attribute_names <- character()
  missing_names <- unique(c(
    intersect(
      attribute_names,
      c("na_values", "na_range", "missing.values", "missing.range", "missings")
    ),
    grep("^(na_|missing)", attribute_names, value = TRUE, ignore.case = TRUE)
  ))

  data.frame(
    file_type = file_row$file_type,
    country_system_code = file_row$country_system_code,
    source_file = file_row$file_name,
    relative_path = to_relative_path(file_row$full_path),
    source_object = object_name,
    variable_name = variable_name,
    variable_label = collapse_attribute(variable_label),
    r_class = paste(class(x), collapse = " | "),
    storage_type = typeof(x),
    is_factor = is.factor(x),
    is_labelled = inherits(x, c("haven_labelled", "haven_labelled_spss", "labelled")) ||
      !is.null(attr(x, "labels", exact = TRUE)),
    has_value_labels = value_label_count > 0L,
    value_label_count = as.integer(value_label_count),
    has_missing_attribute = length(missing_names) > 0L,
    missing_attribute_type = if (length(missing_names) > 0L) {
      paste(sort(missing_names), collapse = " | ")
    } else {
      NA_character_
    },
    file_type_presence = NA_character_,
    stringsAsFactors = FALSE
  )
}

inspect_one_file <- function(file_row, index, total) {
  message(
    sprintf(
      "正在处理第%d个/共%d个文件：%s",
      index, total, file_row$file_name
    )
  )

  warnings_seen <- character()
  error_seen <- NULL
  metadata_errors <- character()
  data_env <- new.env(parent = emptyenv())

  loaded_names <- tryCatch(
    withCallingHandlers(
      load(file_row$full_path, envir = data_env),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_seen <<- conditionMessage(e)
      character()
    }
  )

  if (!is.null(error_seen)) {
    manifest <- data.frame(
      relative_path = to_relative_path(file_row$full_path),
      file_name = file_row$file_name,
      file_type = file_row$file_type,
      country_system_code = file_row$country_system_code,
      file_size_bytes = file_row$file_size_bytes,
      object_name = NA_character_, object_class = NA_character_, object_kind = NA_character_,
      rows = NA_integer_, columns = NA_integer_,
      main_candidate_status = "load_failed",
      read_success = FALSE, read_status = "failed",
      warning_count = length(warnings_seen),
      warning_message = sanitize_message(warnings_seen),
      error_message = sanitize_message(error_seen),
      stringsAsFactors = FALSE
    )
    rm(data_env)
    invisible(gc(FALSE))
    return(list(
      manifest = manifest,
      variables = empty_variable_inventory(),
      file_status = data.frame(
        file_name = file_row$file_name, file_type = file_row$file_type,
        country_system_code = file_row$country_system_code,
        read_success = FALSE, tabular_count = 0L, multiple_tabular = FALSE,
        warning_count = length(warnings_seen),
        warning_message = sanitize_message(warnings_seen),
        error_message = sanitize_message(error_seen),
        stringsAsFactors = FALSE
      )
    ))
  }

  object_names <- sort(unique(c(loaded_names, ls(data_env, all.names = TRUE))))
  object_details <- vector("list", length(object_names))
  tabular_flags <- logical(length(object_names))

  for (j in seq_along(object_names)) {
    nm <- object_names[[j]]
    object_result <- tryCatch(
      withCallingHandlers(
        {
          obj <- get(nm, envir = data_env, inherits = FALSE)
          tabular <- is_tabular_object(obj)
          list(
            object_name = nm,
            object_class = paste(class(obj), collapse = " | "),
            object_kind = classify_object(obj),
            tabular = tabular,
            rows = if (tabular) as.integer(nrow(obj)) else NA_integer_,
            columns = if (tabular) as.integer(ncol(obj)) else NA_integer_
          )
        },
        warning = function(w) {
          warnings_seen <<- c(warnings_seen, paste0("对象 ", nm, "：", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        metadata_errors <<- c(
          metadata_errors,
          paste0("对象 ", nm, " 无法检查：", conditionMessage(e))
        )
        list(
          object_name = nm, object_class = NA_character_, object_kind = "unrecognized",
          tabular = FALSE, rows = NA_integer_, columns = NA_integer_
        )
      }
    )
    object_details[[j]] <- object_result
    tabular_flags[[j]] <- isTRUE(object_result$tabular)
  }

  tabular_count <- sum(tabular_flags)
  main_status <- if (tabular_count == 1L) {
    ifelse(tabular_flags, "single_tabular_candidate", "not_main_candidate")
  } else if (tabular_count > 1L) {
    ifelse(tabular_flags, "needs_review_multiple_tabular", "not_main_candidate")
  } else {
    rep("no_tabular_object", length(object_names))
  }

  variable_rows <- list()
  variable_counter <- 0L

  if (tabular_count > 0L) {
    for (j in which(tabular_flags)) {
      nm <- object_names[[j]]
      tryCatch(
        withCallingHandlers(
          {
            obj <- get(nm, envir = data_env, inherits = FALSE)
            column_names <- colnames(obj)
            if (is.null(column_names)) {
              column_names <- paste0("V", seq_len(ncol(obj)))
              metadata_errors <<- c(
                metadata_errors,
                paste0("对象 ", nm, " 没有列名；inventory使用V1...Vn作为临时名称。")
              )
            }
            blank_names <- is.na(column_names) | !nzchar(column_names)
            if (any(blank_names)) {
              column_names[blank_names] <- paste0("<blank_column_", which(blank_names), ">")
              metadata_errors <<- c(metadata_errors, paste0("对象 ", nm, " 含空列名。"))
            }

            for (k in seq_len(ncol(obj))) {
              column_vector <- if (is.matrix(obj)) obj[, k, drop = TRUE] else obj[[k]]
              variable_counter <- variable_counter + 1L
              variable_rows[[variable_counter]] <- inspect_variable(
                column_vector,
                file_row = file_row,
                object_name = nm,
                variable_name = column_names[[k]]
              )
            }
          },
          warning = function(w) {
            warnings_seen <<- c(warnings_seen, paste0("对象 ", nm, "：", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          metadata_errors <<- c(
            metadata_errors,
            paste0("对象 ", nm, " 的变量metadata提取失败：", conditionMessage(e))
          )
          NULL
        }
      )
    }
  }

  warnings_all <- c(warnings_seen, metadata_errors)
  read_status <- if (length(metadata_errors) > 0L) {
    "success_with_metadata_warnings"
  } else {
    "success"
  }

  if (length(object_names) == 0L) {
    manifest <- data.frame(
      relative_path = to_relative_path(file_row$full_path),
      file_name = file_row$file_name, file_type = file_row$file_type,
      country_system_code = file_row$country_system_code,
      file_size_bytes = file_row$file_size_bytes,
      object_name = NA_character_, object_class = NA_character_, object_kind = "no_objects",
      rows = NA_integer_, columns = NA_integer_, main_candidate_status = "no_objects",
      read_success = TRUE, read_status = read_status,
      warning_count = length(warnings_all),
      warning_message = sanitize_message(warnings_all), error_message = "",
      stringsAsFactors = FALSE
    )
  } else {
    manifest <- do.call(
      rbind,
      lapply(seq_along(object_details), function(j) {
        detail <- object_details[[j]]
        data.frame(
          relative_path = to_relative_path(file_row$full_path),
          file_name = file_row$file_name, file_type = file_row$file_type,
          country_system_code = file_row$country_system_code,
          file_size_bytes = file_row$file_size_bytes,
          object_name = detail$object_name,
          object_class = detail$object_class,
          object_kind = detail$object_kind,
          rows = detail$rows, columns = detail$columns,
          main_candidate_status = main_status[[j]],
          read_success = TRUE, read_status = read_status,
          warning_count = length(warnings_all),
          warning_message = sanitize_message(warnings_all), error_message = "",
          stringsAsFactors = FALSE
        )
      })
    )
  }

  variables <- if (length(variable_rows) > 0L) {
    do.call(rbind, variable_rows)
  } else {
    empty_variable_inventory()
  }

  rm(list = ls(data_env, all.names = TRUE), envir = data_env)
  rm(data_env)
  invisible(gc(FALSE))

  list(
    manifest = manifest,
    variables = variables,
    file_status = data.frame(
      file_name = file_row$file_name, file_type = file_row$file_type,
      country_system_code = file_row$country_system_code,
      read_success = TRUE, tabular_count = tabular_count,
      multiple_tabular = tabular_count > 1L,
      warning_count = length(warnings_all),
      warning_message = sanitize_message(warnings_all), error_message = "",
      stringsAsFactors = FALSE
    )
  )
}

inspection_results <- vector("list", nrow(file_index))
for (i in seq_len(nrow(file_index))) {
  inspection_results[[i]] <- inspect_one_file(
    file_index[i, , drop = FALSE],
    index = i,
    total = nrow(file_index)
  )
}

raw_file_manifest <- do.call(rbind, lapply(inspection_results, `[[`, "manifest"))
variable_inventory <- do.call(rbind, lapply(inspection_results, `[[`, "variables"))
file_run_status <- do.call(rbind, lapply(inspection_results, `[[`, "file_status"))
row.names(raw_file_manifest) <- NULL
row.names(variable_inventory) <- NULL
row.names(file_run_status) <- NULL

if (sum(file_run_status$read_success) == 0L) {
  stop(
    "所有BSA/BSG RData均读取失败。请查看Console中的错误，并检查RData是否损坏或版本不兼容。",
    call. = FALSE
  )
}

if (nrow(variable_inventory) == 0L ||
    !any(raw_file_manifest$object_kind %in% c("data.frame", "tibble", "matrix"))) {
  stop(
    "已读取的文件中完全无法识别任何表格型对象，因此不能生成变量metadata。",
    call. = FALSE
  )
}

# Add whether each variable name appeared in BSA, BSG, or both.
variable_presence_map <- tapply(
  variable_inventory$file_type,
  toupper(variable_inventory$variable_name),
  function(x) paste(sort(unique(x)), collapse = " + ")
)
presence_values <- variable_presence_map[
  match(
    toupper(variable_inventory$variable_name),
    names(variable_presence_map)
  )
]

variable_inventory[["file_type_presence"]] <- as.vector(
  presence_values,
  mode = "character"
)

if (
  is.list(variable_inventory[["file_type_presence"]]) ||
  !is.null(dim(variable_inventory[["file_type_presence"]]))
) {
  stop(
    "内部错误：file_type_presence仍不是普通字符向量。",
    call. = FALSE
  )
}

make_registry_rows <- function(
    variable_name, category, subcategory, description,
    status = "official_document_confirmed",
    source = "T23_Codebook_G8.xlsx: BSAM8/BSGM8") {
  data.frame(
    variable_name = variable_name,
    candidate_category = category,
    candidate_subcategory = subcategory,
    candidate_description = description,
    candidate_status = status,
    official_source = source,
    stringsAsFactors = FALSE
  )
}

pv_rows <- function(prefix, subcategory, description) {
  make_registry_rows(
    sprintf("%s%02d", prefix, 1:5),
    "plausible_values", subcategory, description
  )
}

official_registry <- do.call(
  rbind,
  list(
    make_registry_rows(
      c("CTY", "IDCNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD"),
      "identifiers",
      c("country_alpha3", "country_system", "school", "class", "student"),
      c(
        "Country/system alpha-3 code", "Country/system numeric identifier",
        "School identifier (unique only within country/system)",
        "Class identifier (unique only within country/system)",
        "Student identifier (unique only within country/system)"
      ),
      source = "User Guide pp. 72-74; Codebook BSAM8/BSGM8"
    ),
    pv_rows("BSMMAT", "overall_mathematics", "Overall mathematics PV; keep separate from content-domain PVs"),
    pv_rows("BSMNUM", "number", "Number content-domain PV"),
    pv_rows("BSMALG", "algebra", "Algebra content-domain PV"),
    pv_rows("BSMGEO", "geometry", "Geometry content-domain PV (dissertation wording: Geometry & Measurement)"),
    pv_rows("BSMDAT", "data_and_probability", "Data & Probability content-domain PV"),
    make_registry_rows(
      c("TOTWGT", "SENWGT", "HOUWGT"),
      "weights",
      c("student_sampling_weight", "senate_weight", "house_weight"),
      c(
        "Overall student sampling weight for most student-level analyses",
        "Student weight scaled to sum to 500 per country/system",
        "Student weight scaled to sample size"
      ),
      source = "User Guide pp. 69-71; Codebook BSAM8/BSGM8"
    ),
    make_registry_rows(
      c("JKZONE", "JKREP", "WGTFAC1", "WGTADJ1", "WGTFAC2", "WGTADJ2", "WGTFAC3", "WGTADJ3"),
      "replication",
      c(
        "jackknife_zone", "jackknife_replicate_indicator",
        "weight_factor_1", "weight_adjustment_1", "weight_factor_2", "weight_adjustment_2",
        "weight_factor_3", "weight_adjustment_3"
      ),
      c(
        "Jackknife sampling zone/stratum", "Jackknife replicate/PSU indicator",
        "School-level weight factor", "School-level weight adjustment",
        "Class-level weight factor", "Class-level weight adjustment",
        "Student-level weight factor", "Student-level weight adjustment"
      ),
      source = "User Guide pp. 70-71; Codebook BSAM8/BSGM8"
    ),
    make_registry_rows(
      c("BSBGHER", "BSDGHER", "BSDG05S", "BSDGEDUP", "BSBG04", "BSBG05D", "BSBG05F", "BSBG06A", "BSBG06B"),
      "home_resources",
      c(
        "home_educational_resources_scale", "home_educational_resources_index",
        "home_study_supports_derived", "parents_highest_education_derived",
        "books_at_home", "internet_at_home", "own_room", "guardian_a_education", "guardian_b_education"
      ),
      c(
        "Home Educational Resources scale", "Home Educational Resources index",
        "Number of Home Study Supports", "Parents' Highest Education Level",
        "Books at home", "Internet access at home", "Own room",
        "Parent/Guardian A education", "Parent/Guardian B education"
      ),
      status = "official_document_confirmed_candidate",
      source = "Codebook BSGM8; SQ worksheet SQG-04/05/06; Student Derived Variables pp. 1-2"
    ),
    make_registry_rows(
      c("BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D"),
      "ICT", "ICT_access",
      c(
        "Own computer or tablet", "Shared computer or tablet", "Smartphone", "Internet access at home"
      ),
      status = "official_document_confirmed_candidate",
      source = "Student Questionnaire Variables SQG-05; questionnaire PDF pp. 5-6"
    ),
    make_registry_rows(
      paste0("BSBG12", LETTERS[1:6]),
      "ICT", "ICT_opportunity_or_use",
      c(
        "Internet use: access course materials", "Internet use: access teacher assignments",
        "Internet use: collaborate with classmates", "Internet use: ask teacher questions",
        "Internet use: find mathematics/science information", "Internet use: access learning games"
      ),
      status = "official_document_confirmed_candidate",
      source = "Student Questionnaire Variables SQG-12; questionnaire PDF p. 10"
    ),
    make_registry_rows(
      c(paste0("BSBG13", LETTERS[1:7]), "BSBGSEC", "BSDGSEC"),
      "ICT", "ICT_self_efficacy",
      c(
        "Digital skill item: write/edit text", "Digital skill item: create presentations",
        "Digital skill item: create tables/charts/graphs", "Digital skill item: find information online",
        "Digital skill item: judge website trustworthiness", "Digital skill item: learn new digital tasks",
        "Digital skill item: help others", "Digital Self-Efficacy scale", "Digital Self-Efficacy index"
      ),
      status = "official_document_confirmed_candidate",
      source = "Codebook BSGM8; Student Questionnaire Variables SQG-13; questionnaire PDF p. 11"
    ),
    make_registry_rows(
      c("ITSEX", "BSBG01", "BSDAGE", "BSBG03", "BSBG09A", "BSBG09B"),
      "controls",
      c("sex_tracking", "sex_questionnaire", "age", "language_at_home", "born_in_country", "age_arrived"),
      c(
        "Sex from tracking/calculated field", "Sex from student questionnaire", "Student age",
        "Frequency speaking language of test at home", "Born in country/system", "Age arrived in country/system"
      ),
      status = "official_document_confirmed_candidate",
      source = "Codebook BSAM8/BSGM8; Student Questionnaire Variables SQG-01/03/09"
    )
  )
)
official_registry$variable_name <- toupper(official_registry$variable_name)
row.names(official_registry) <- NULL

keyword_rules <- list(
  identifiers = "(^ID(CNTRY|SCHOOL|CLASS|STUD)$)|(^CTY$)|(country|system|school|student).*(id|identifier)|(id|identifier).*(country|system|school|student)",
  plausible_values = "(^BSM[A-Z]{3}[0-9]{2}$)|(plausible[[:space:]_-]*value)|(mathematics.*(number|algebra|geometry|data|probability))",
  weights = "(^[A-Z0-9_]*(WGT|WEIGHT)[A-Z0-9_]*$)|(sampling[[:space:]_-]*weight)|(student.*weight)",
  replication = "(^JK(ZONE|REP)$)|(jackknife)|(replicat(e|ion))|(sampling.*(zone|stratum|psu))",
  home_resources = "(home.*(resource|support|books|computer|internet|room|desk))|(parent.*education)|(guardian.*education)|(educational.*resource)",
  ICT = "(ICT)|(digital)|(computer)|(tablet)|(smartphone)|(internet)|(online)|(website)",
  controls = "(^ITSEX$)|(^BSDAGE$)|(sex of student)|(student.*age)|(language.*home)|(born.*country)|(age.*arriv)|(immigra)"
)

make_candidate_hits <- function(variable_inventory, registry, keyword_rules) {
  occurrence <- variable_inventory
  occurrence$variable_upper <- toupper(occurrence$variable_name)
  official_match <- match(occurrence$variable_upper, registry$variable_name)

  official_rows <- occurrence[!is.na(official_match), , drop = FALSE]
  official_idx <- official_match[!is.na(official_match)]
  official_hits <- data.frame(
    country_system_code = official_rows$country_system_code,
    file_type = official_rows$file_type,
    source_file = official_rows$source_file,
    source_object = official_rows$source_object,
    variable_name = official_rows$variable_name,
    variable_label = official_rows$variable_label,
    candidate_category = registry$candidate_category[official_idx],
    candidate_subcategory = registry$candidate_subcategory[official_idx],
    expected_category = registry$candidate_category[official_idx],
    hit_basis = paste0("Exact official name: ", registry$official_source[official_idx]),
    confidence = "high",
    candidate_status = registry$candidate_status[official_idx],
    official_source = registry$official_source[official_idx],
    stringsAsFactors = FALSE
  )

  nonofficial <- occurrence[is.na(official_match), , drop = FALSE]
  keyword_hit_rows <- list()
  counter <- 0L

  if (nrow(nonofficial) > 0L) {
    for (i in seq_len(nrow(nonofficial))) {
      label <- nonofficial$variable_label[[i]]
      if (is.na(label)) label <- ""
      haystack <- paste(nonofficial$variable_name[[i]], label)
      matched_categories <- names(keyword_rules)[vapply(
        keyword_rules,
        function(pattern) grepl(pattern, haystack, ignore.case = TRUE, perl = TRUE),
        logical(1)
      )]
      if (length(matched_categories) > 0L) {
        counter <- counter + 1L
        ambiguous <- length(matched_categories) > 1L
        keyword_hit_rows[[counter]] <- data.frame(
          country_system_code = nonofficial$country_system_code[[i]],
          file_type = nonofficial$file_type[[i]],
          source_file = nonofficial$source_file[[i]],
          source_object = nonofficial$source_object[[i]],
          variable_name = nonofficial$variable_name[[i]],
          variable_label = nonofficial$variable_label[[i]],
          candidate_category = if (ambiguous) "unresolved" else matched_categories[[1]],
          candidate_subcategory = if (ambiguous) {
            paste(matched_categories, collapse = " | ")
          } else {
            "keyword_match"
          },
          expected_category = paste(matched_categories, collapse = " | "),
          hit_basis = "Case-insensitive keyword regex on variable name and label",
          confidence = if (ambiguous) "low" else "medium",
          candidate_status = if (ambiguous) {
            "unresolved_multiple_keyword_categories"
          } else {
            "keyword_only_candidate"
          },
          official_source = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  keyword_hits <- if (length(keyword_hit_rows) > 0L) {
    do.call(rbind, keyword_hit_rows)
  } else {
    official_hits[0, , drop = FALSE]
  }

  found_upper <- unique(occurrence$variable_upper)
  missing_registry <- registry[!registry$variable_name %in% found_upper, , drop = FALSE]
  unresolved_hits <- if (nrow(missing_registry) > 0L) {
    data.frame(
      country_system_code = NA_character_, file_type = NA_character_,
      source_file = NA_character_, source_object = NA_character_,
      variable_name = missing_registry$variable_name,
      variable_label = NA_character_, candidate_category = "unresolved",
      candidate_subcategory = "official_candidate_not_found",
      expected_category = missing_registry$candidate_category,
      hit_basis = "Official candidate name was not found anywhere in the local inventory",
      confidence = "unresolved",
      candidate_status = "unresolved_not_found_in_inventory",
      official_source = missing_registry$official_source,
      stringsAsFactors = FALSE
    )
  } else {
    official_hits[0, , drop = FALSE]
  }

  hits <- rbind(official_hits, keyword_hits, unresolved_hits)
  hits <- unique(hits)
  hits <- hits[order(
    hits$candidate_category,
    hits$variable_name,
    hits$country_system_code,
    hits$file_type,
    na.last = TRUE
  ), , drop = FALSE]
  row.names(hits) <- NULL
  hits
}

candidate_variable_hits <- make_candidate_hits(
  variable_inventory,
  official_registry,
  keyword_rules
)

keyword_definitions <- unique(candidate_variable_hits[
  candidate_variable_hits$candidate_status %in%
    c("keyword_only_candidate", "unresolved_multiple_keyword_categories"),
  c("variable_name", "candidate_category", "candidate_subcategory", "candidate_status"),
  drop = FALSE
])
names(keyword_definitions) <- c(
  "variable_name", "candidate_category", "candidate_subcategory", "candidate_status"
)

candidate_definitions <- rbind(
  official_registry[, c(
    "variable_name", "candidate_category", "candidate_subcategory", "candidate_status"
  )],
  keyword_definitions
)
candidate_definitions <- unique(candidate_definitions)

systems <- sort(unique(file_index$country_system_code))
coverage_grid <- expand.grid(
  country_system_code = systems,
  file_type = c("BSA", "BSG"),
  definition_row = seq_len(nrow(candidate_definitions)),
  stringsAsFactors = FALSE
)
coverage_grid$variable_name <- candidate_definitions$variable_name[coverage_grid$definition_row]
coverage_grid$candidate_category <- candidate_definitions$candidate_category[coverage_grid$definition_row]
coverage_grid$candidate_subcategory <- candidate_definitions$candidate_subcategory[coverage_grid$definition_row]
coverage_grid$defined_status <- candidate_definitions$candidate_status[coverage_grid$definition_row]

inventory_key <- paste(
  variable_inventory$country_system_code,
  variable_inventory$file_type,
  toupper(variable_inventory$variable_name),
  sep = "\r"
)
coverage_key <- paste(
  coverage_grid$country_system_code,
  coverage_grid$file_type,
  toupper(coverage_grid$variable_name),
  sep = "\r"
)
coverage_grid$present <- coverage_key %in% inventory_key

source_object_map <- tapply(
  variable_inventory$source_object,
  inventory_key,
  function(x) paste(sort(unique(x)), collapse = " | ")
)
source_object_values <- source_object_map[
  match(coverage_key, names(source_object_map))
]

coverage_grid[["source_object"]] <- as.vector(
  source_object_values,
  mode = "character"
)
coverage_grid$candidate_status <- ifelse(
  coverage_grid$present,
  coverage_grid$defined_status,
  "unresolved_not_found_for_country_file_type"
)

country_variable_coverage <- coverage_grid[, c(
  "country_system_code", "file_type", "candidate_category", "candidate_subcategory",
  "variable_name", "present", "source_object", "candidate_status"
)]
country_variable_coverage <- country_variable_coverage[order(
  country_variable_coverage$country_system_code,
  country_variable_coverage$file_type,
  country_variable_coverage$candidate_category,
  country_variable_coverage$variable_name
), , drop = FALSE]
row.names(country_variable_coverage) <- NULL

pv_expected <- list(
  number = sprintf("BSMNUM%02d", 1:5),
  algebra = sprintf("BSMALG%02d", 1:5),
  geometry = sprintf("BSMGEO%02d", 1:5),
  data_and_probability = sprintf("BSMDAT%02d", 1:5),
  overall_mathematics = sprintf("BSMMAT%02d", 1:5)
)

pv_check_rows <- list()
pv_counter <- 0L
for (system_code in systems) {
  for (file_type in c("BSA", "BSG")) {
    present_names <- unique(toupper(variable_inventory$variable_name[
      variable_inventory$country_system_code == system_code &
        variable_inventory$file_type == file_type
    ]))
    for (group_name in names(pv_expected)) {
      pv_counter <- pv_counter + 1L
      found <- intersect(pv_expected[[group_name]], present_names)
      pv_check_rows[[pv_counter]] <- data.frame(
        country_system_code = system_code,
        file_type = file_type,
        pv_group = group_name,
        pv_count = length(found),
        pv_names = paste(found, collapse = " | "),
        expected_count = length(pv_expected[[group_name]]),
        complete = setequal(found, pv_expected[[group_name]]),
        stringsAsFactors = FALSE
      )
    }
  }
}
pv_check <- do.call(rbind, pv_check_rows)

single_main <- raw_file_manifest[
  raw_file_manifest$main_candidate_status == "single_tabular_candidate" &
    raw_file_manifest$read_success,
  c("country_system_code", "file_type", "rows", "object_name"),
  drop = FALSE
]

get_main_character <- function(system_code, file_type, column_name) {
  rows <- single_main[
    single_main$country_system_code == system_code & single_main$file_type == file_type,
    , drop = FALSE
  ]
  if (nrow(rows) != 1L) return(NA_character_)
  as.character(rows[[column_name]][[1]])
}

get_main_numeric <- function(system_code, file_type, column_name) {
  rows <- single_main[
    single_main$country_system_code == system_code & single_main$file_type == file_type,
    , drop = FALSE
  ]
  if (nrow(rows) != 1L) return(NA_real_)
  as.numeric(rows[[column_name]][[1]])
}

country_pair_check <- data.frame(
  country_system_code = systems,
  has_BSA = systems %in% file_index$country_system_code[file_index$file_type == "BSA"],
  has_BSG = systems %in% file_index$country_system_code[file_index$file_type == "BSG"],
  bsa_main_object = vapply(systems, get_main_character, character(1), file_type = "BSA", column_name = "object_name"),
  bsg_main_object = vapply(systems, get_main_character, character(1), file_type = "BSG", column_name = "object_name"),
  bsa_rows = vapply(systems, get_main_numeric, numeric(1), file_type = "BSA", column_name = "rows"),
  bsg_rows = vapply(systems, get_main_numeric, numeric(1), file_type = "BSG", column_name = "rows"),
  stringsAsFactors = FALSE
)
country_pair_check$row_difference_BSA_minus_BSG <- with(
  country_pair_check,
  ifelse(!is.na(bsa_rows) & !is.na(bsg_rows), bsa_rows - bsg_rows, NA_real_)
)
country_pair_check$row_count_equal <- with(
  country_pair_check,
  ifelse(!is.na(bsa_rows) & !is.na(bsg_rows), bsa_rows == bsg_rows, NA)
)
country_pair_check$pairing_note <- ifelse(
  !country_pair_check$has_BSA | !country_pair_check$has_BSG,
  "one_file_type_missing",
  ifelse(
    is.na(country_pair_check$row_count_equal),
    "main_object_needs_review",
    ifelse(country_pair_check$row_count_equal, "row_counts_equal", "row_counts_differ")
  )
)

both_systems <- country_pair_check$country_system_code[
  country_pair_check$has_BSA & country_pair_check$has_BSG
]
only_bsa <- country_pair_check$country_system_code[
  country_pair_check$has_BSA & !country_pair_check$has_BSG
]
only_bsg <- country_pair_check$country_system_code[
  !country_pair_check$has_BSA & country_pair_check$has_BSG
]

found_variable <- function(name) {
  toupper(name) %in% toupper(variable_inventory$variable_name)
}

expected_file_keys <- unique(paste(
  file_run_status$country_system_code[
    file_run_status$read_success & file_run_status$tabular_count > 0L
  ],
  file_run_status$file_type[
    file_run_status$read_success & file_run_status$tabular_count > 0L
  ],
  sep = "\r"
))

all_expected_files_have <- function(variable_names, file_type = NULL) {
  expected <- expected_file_keys
  if (!is.null(file_type)) {
    expected <- expected[endsWith(expected, paste0("\r", file_type))]
  }
  if (length(expected) == 0L) return(FALSE)
  present_rows <- variable_inventory[
    toupper(variable_inventory$variable_name) %in% toupper(variable_names),
    , drop = FALSE
  ]
  present <- unique(paste(
    present_rows$country_system_code,
    present_rows$file_type,
    sep = "\r"
  ))
  all(expected %in% present)
}

validation <- data.frame(
  check_id = integer(), check = character(), status = character(), details = character(),
  stringsAsFactors = FALSE
)

add_validation <- function(check_id, check, status, details) {
  validation <<- rbind(
    validation,
    data.frame(
      check_id = check_id, check = check, status = status, details = details,
      stringsAsFactors = FALSE
    )
  )
}

format_codes <- function(x) if (length(x) == 0L) "none" else paste(x, collapse = ", ")

add_validation(1, "至少一个BSA文件", "PASS", paste("BSA files:", length(bsa_files)))
add_validation(2, "至少一个BSG文件", "PASS", paste("BSG files:", length(bsg_files)))
add_validation(3, "BSA/BSG文件计数", "PASS", paste0("BSA=", length(bsa_files), "; BSG=", length(bsg_files)))
add_validation(
  4, "同时拥有BSA和BSG的国家/系统",
  if (length(both_systems) > 0L) "PASS" else "WARNING",
  format_codes(both_systems)
)
add_validation(
  5, "只有一种文件的国家/系统",
  if (length(only_bsa) + length(only_bsg) == 0L) "PASS" else "WARNING",
  paste0("only BSA: ", format_codes(only_bsa), "; only BSG: ", format_codes(only_bsg))
)
failed_files <- file_run_status$file_name[!file_run_status$read_success]
add_validation(
  6, "文件读取失败",
  if (length(failed_files) == 0L) "PASS" else "WARNING",
  format_codes(failed_files)
)
multiple_files <- file_run_status$file_name[file_run_status$multiple_tabular]
add_validation(
  7, "多个疑似主要数据对象",
  if (length(multiple_files) == 0L) "PASS" else "WARNING",
  format_codes(multiple_files)
)
row_problem <- country_pair_check[
  country_pair_check$has_BSA & country_pair_check$has_BSG &
    (is.na(country_pair_check$row_count_equal) | !country_pair_check$row_count_equal),
  , drop = FALSE
]
add_validation(
  8, "每国BSA与BSG主要候选对象行数",
  if (nrow(row_problem) == 0L) "PASS" else "WARNING",
  if (nrow(row_problem) == 0L) "all available pairs have equal row counts" else
    paste(
      paste0(
        row_problem$country_system_code, "(BSA=", row_problem$bsa_rows,
        ", BSG=", row_problem$bsg_rows, ", diff=", row_problem$row_difference_BSA_minus_BSG, ")"
      ),
      collapse = "; "
    )
)
add_validation(9, "country/system identifier候选", if (all_expected_files_have("IDCNTRY")) "PASS" else "WARNING", "Expected in every readable tabular file: IDCNTRY; CTY is an additional alpha-3 candidate")
add_validation(10, "school identifier候选", if (all_expected_files_have("IDSCHOOL")) "PASS" else "WARNING", "Expected in every readable tabular file: IDSCHOOL")
add_validation(11, "student identifier候选", if (all_expected_files_have("IDSTUD")) "PASS" else "WARNING", "Expected in every readable tabular file: IDSTUD")
domain_pv_names <- unlist(pv_expected[c("number", "algebra", "geometry", "data_and_probability")], use.names = FALSE)
add_validation(
  12, "数学内容领域PV候选",
  if (any(vapply(domain_pv_names, found_variable, logical(1)))) "PASS" else "WARNING",
  paste(sort(unique(variable_inventory$variable_name[toupper(variable_inventory$variable_name) %in% domain_pv_names])), collapse = ", ")
)
pv_relevant <- pv_check[pv_check$file_type %in% unique(file_index$file_type), , drop = FALSE]
add_validation(
  13, "每组PV数量与名称结构",
  if (nrow(pv_relevant) > 0L && all(pv_relevant$complete)) "PASS" else "WARNING",
  paste0("complete groups=", sum(pv_relevant$complete), "/", nrow(pv_relevant), "; expected 5 PVs per group")
)
add_validation(14, "student weight候选", if (all_expected_files_have("TOTWGT")) "PASS" else "WARNING", "Expected in every readable tabular file: TOTWGT")
add_validation(
  15, "jackknife/replication候选",
  if (all_expected_files_have("JKZONE") && all_expected_files_have("JKREP")) "PASS" else "WARNING",
  paste0(
    "JKZONE all readable files=", all_expected_files_have("JKZONE"),
    "; JKREP all readable files=", all_expected_files_have("JKREP")
  )
)
home_names <- official_registry$variable_name[official_registry$candidate_category == "home_resources"]
add_validation(
  16, "home resources候选",
  if (all_expected_files_have(home_names, file_type = "BSG")) "PASS" else "WARNING",
  paste(sort(intersect(home_names, toupper(variable_inventory$variable_name))), collapse = ", ")
)
ict_names <- official_registry$variable_name[official_registry$candidate_category == "ICT"]
add_validation(
  17, "ICT候选",
  if (all_expected_files_have(ict_names, file_type = "BSG")) "PASS" else "WARNING",
  paste(sort(intersect(ict_names, toupper(variable_inventory$variable_name))), collapse = ", ")
)

output_paths <- list(
  raw_file_manifest = here::here("data_metadata", "raw_file_manifest.csv"),
  variable_inventory = here::here("data_metadata", "local_only", "variable_inventory.csv"),
  country_variable_coverage = here::here("data_metadata", "country_variable_coverage.csv"),
  candidate_variable_hits = here::here("data_metadata", "candidate_variable_hits.csv"),
  inventory_summary = here::here("data_metadata", "local_only", "inventory_summary.txt"),
  session_info = here::here("data_metadata", "local_only", "session_info.txt")
)

archive_dir <- here::here("data_metadata", "local_only", "archive")
if (!dir.exists(archive_dir)) {
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
}
run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

backup_existing <- function(path) {
  if (!file.exists(path)) return(invisible(NULL))
  archive_name <- paste0(run_stamp, "_", basename(path))
  destination <- file.path(archive_dir, archive_name)
  ok <- file.rename(path, destination)
  if (!ok) {
    stop(
      "现有输出无法安全移入archive，因此停止以避免静默覆盖：", to_relative_path(path),
      call. = FALSE
    )
  }
  message("现有输出已归档：", to_relative_path(destination))
  invisible(destination)
}

for (path in output_paths) backup_existing(path)

write_csv_checked <- function(data, path) {
  tryCatch(
    {
      # Convert any one-dimensional array or one-column matrix into
      # a normal atomic vector before CSV export.
      for (column_index in seq_along(data)) {
        column_value <- data[[column_index]]
        
        if (!is.null(dim(column_value))) {
          if (length(column_value) != nrow(data)) {
            stop(
              paste0(
                "列“", names(data)[column_index],
                "”具有无法安全转换的维度：",
                paste(dim(column_value), collapse = " × ")
              )
            )
          }
          
          column_value <- as.vector(column_value)
        }
        
        if (is.list(column_value)) {
          stop(
            paste0(
              "列“", names(data)[column_index],
              "”是无法直接写入CSV的list column。"
            )
          )
        }
        
        data[[column_index]] <- column_value
      }
      
      # UTF-8 BOM improves compatibility with Excel on macOS.
      readr::write_excel_csv(data, file = path, na = "")
      
      if (
        !file.exists(path) ||
        is.na(file.info(path)$size) ||
        file.info(path)$size == 0
      ) {
        stop("文件不存在或大小为0")
      }
    },
    error = function(e) {
      stop(
        "metadata输出无法写入：",
        to_relative_path(path),
        "；",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  invisible(TRUE)
}

write_csv_checked(raw_file_manifest, output_paths$raw_file_manifest)
write_csv_checked(variable_inventory, output_paths$variable_inventory)
write_csv_checked(country_variable_coverage, output_paths$country_variable_coverage)
write_csv_checked(candidate_variable_hits, output_paths$candidate_variable_hits)

loaded_package_lines <- vapply(
  required_packages,
  function(pkg) paste0(pkg, " ", as.character(utils::packageVersion(pkg))),
  character(1)
)
session_lines <- c(
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Project root: ", project_root),
  paste0("R version: ", R.version.string),
  paste0("Operating system: ", Sys.info()[["sysname"]], " ", Sys.info()[["release"]]),
  paste0("Platform: ", R.version$platform),
  paste0("Locale: ", paste(Sys.getlocale(), collapse = "; ")),
  "Loaded/required packages:",
  paste0("  - ", loaded_package_lines),
  "",
  "Complete sessionInfo():",
  capture.output(sessionInfo())
)
tryCatch(
  writeLines(session_lines, output_paths$session_info, useBytes = TRUE),
  error = function(e) stop("无法写入session_info.txt：", conditionMessage(e), call. = FALSE)
)

successful_files <- sum(file_run_status$read_success)
warning_total <- sum(file_run_status$warning_count)
failed_count <- sum(!file_run_status$read_success)
pv_group_summary <- aggregate(
  pv_count ~ file_type + pv_group,
  data = pv_check,
  FUN = function(x) paste(sort(unique(x)), collapse = ",")
)
pv_incomplete <- pv_check[!pv_check$complete, c(
  "country_system_code", "file_type", "pv_group", "pv_count", "pv_names"
), drop = FALSE]

pair_output <- capture.output(print(country_pair_check, row.names = FALSE))
pv_output <- capture.output(print(pv_group_summary, row.names = FALSE))
pv_incomplete_output <- if (nrow(pv_incomplete) == 0L) {
  "none"
} else {
  capture.output(print(pv_incomplete, row.names = FALSE))
}
validation_without_output <- validation
preliminary_overall <- if (any(validation_without_output$status == "FAIL")) {
  "FAIL"
} else if (any(validation_without_output$status == "WARNING")) {
  "WARNING"
} else {
  "PASS"
}

summary_lines <- c(
  "TIMSS 2023 Grade 8 BSA/BSG Inventory Summary",
  "================================================",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Project root (local-only log): ", project_root),
  paste0("BSA file count: ", length(bsa_files)),
  paste0("BSG file count: ", length(bsg_files)),
  paste0("Systems with both BSA and BSG: ", length(both_systems), " [", format_codes(both_systems), "]"),
  paste0("Only BSA: ", length(only_bsa), " [", format_codes(only_bsa), "]"),
  paste0("Only BSG: ", length(only_bsg), " [", format_codes(only_bsg), "]"),
  paste0("Successfully read files: ", successful_files),
  paste0("Failed files: ", failed_count, " [", format_codes(failed_files), "]"),
  paste0("Warning count: ", warning_total),
  paste0("Files with multiple tabular candidates: ", length(multiple_files), " [", format_codes(multiple_files), "]"),
  "",
  "BSA/BSG preliminary pair check (row counts do not prove mergeability):",
  pair_output,
  "",
  "PV group preliminary check (distinct counts observed across systems):",
  pv_output,
  "Incomplete PV groups by country/system and file type:",
  pv_incomplete_output,
  "Important: overall mathematics PVs (BSMMAT01-05) are kept separate from content-domain PVs.",
  "No PV calculation, averaging, or pooling was performed.",
  "",
  paste0(
    "Weight candidates found: ",
    format_codes(sort(intersect(c("TOTWGT", "SENWGT", "HOUWGT"), toupper(variable_inventory$variable_name))))
  ),
  paste0(
    "Replication/design candidates found: ",
    format_codes(sort(intersect(
      c("JKZONE", "JKREP", "WGTFAC1", "WGTADJ1", "WGTFAC2", "WGTADJ2", "WGTFAC3", "WGTADJ3"),
      toupper(variable_inventory$variable_name)
    )))
  ),
  "No replicate weights were constructed and no variance formula was selected.",
  "",
  "Output files:",
  paste0("  - ", vapply(output_paths, to_relative_path, character(1))),
  "",
  paste0("Preliminary overall validation: ", preliminary_overall),
  "",
  "Validation checks 1-17:",
  capture.output(print(validation_without_output, row.names = FALSE))
)

tryCatch(
  writeLines(summary_lines, output_paths$inventory_summary, useBytes = TRUE),
  error = function(e) stop("无法写入inventory_summary.txt：", conditionMessage(e), call. = FALSE)
)

all_outputs_exist <- vapply(
  output_paths,
  function(path) file.exists(path) && !is.na(file.info(path)$size) && file.info(path)$size > 0,
  logical(1)
)
add_validation(
  18, "六个要求的输出文件",
  if (all(all_outputs_exist)) "PASS" else "FAIL",
  paste0(
    names(all_outputs_exist), "=", ifelse(all_outputs_exist, "present", "missing"),
    collapse = "; "
  )
)

overall_validation <- if (any(validation$status == "FAIL")) {
  "FAIL"
} else if (any(validation$status == "WARNING")) {
  "WARNING"
} else {
  "PASS"
}

post_write_lines <- c(
  "",
  "Final validation check 18:",
  capture.output(print(validation[validation$check_id == 18, , drop = FALSE], row.names = FALSE)),
  paste0("Final overall validation: ", overall_validation)
)
write(post_write_lines, file = output_paths$inventory_summary, append = TRUE)

message("盘点完成。")
message("成功文件：", successful_files, "；失败文件：", failed_count, "；warnings：", warning_total)
message("最终验证：", overall_validation)
message("输出文件：")
for (path in output_paths) message("  - ", to_relative_path(path))

if (!all(all_outputs_exist)) {
  stop(
    "至少一个核心metadata输出未成功生成；请查看inventory_summary.txt和Console。",
    call. = FALSE
  )
}

if (failed_count > 0L) {
  warning(
    failed_count, "个文件读取失败；其余文件已完成盘点。请查看raw_file_manifest.csv。",
    call. = FALSE
  )
}

if (length(multiple_files) > 0L) {
  warning(
    length(multiple_files), "个文件包含多个表格型对象，需要人工复核主要对象。",
    call. = FALSE
  )
}
