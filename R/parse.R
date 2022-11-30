#' @export
tbl_format_footer.bpsr_tbl <- function(x, setup, ...) {
  metadata <- pillar::style_subtle(str_c(
    "# ", cli::symbol$info, " Read the metadata with `bps_metadata()`"
  ))

  c(NextMethod(), metadata)
}


#' @export
print.bpsr_multiple_datasets <- function(x, ...) {
  cli_text("{.cls {class(x)}}")
  str(x, max.level = 3, vec.len = 1, list.len = 7, give.attr = FALSE)
  invisible(x)
}


parse_table <- function(table) {
  check_table(table)

  table$table |>
    dplyr::bind_rows() |>
    as_tibble() |>
    rename_column() |>
    clean_character()
}


parse_dataset <- function(dataset, keep = "none") {
  check_dataset(dataset)

  keep_value <- c(
    "all",
    "none",
    "vertical_var_id",
    "derived_var_id",
    "year_id",
    "period_id"
  )

  keep <- arg_match(keep, keep_value, multiple = TRUE)

  lookup <- dataset$lookup_table
  metadata <- dataset$metadata

  data <- dataset$dataset |>
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to = "composite_id",
      values_to = "var"
    ) |>
    parse_vertical_var(lookup$vertical_var, metadata$dataset_id) |>
    parse_derived_var(lookup$derived_var, metadata$dataset_id) |>
    parse_year_n_period(lookup$year, lookup$period, metadata$dataset_id)

  data <- data |>
    clean_character() |>
    dplyr::relocate(var, .after = tidyselect::last_col()) |>
    dplyr::select(!composite_id)

  if (!identical(keep, "all")) {
    if (identical(keep, "none")) {
      data <- dplyr::select(data, !dplyr::ends_with("id"))
    } else {
      dont_keep <- setdiff(keep_value[-c(1, 2)], keep)
      data <- dplyr::select(data, !tidyselect::any_of(dont_keep))
    }
  }

  new_bpsr_tbl(data, metadata)
}


parse_datasets <- function(dataset, dataset_id, keep = "none") {
  if (!purrr::every(dataset, is_dataset)) {
    cli_abort("{.arg dataset} must contain {.cls bpsr_dataset} objects.")
  }

  dataset <- dataset |>
    map(parse_dataset, keep = keep) |>
    rlang::set_names(dataset_id)

  class(dataset) <- "bpsr_multiple_datasets"

  dataset
}


parse_dataset_trade <- function(dataset, by, flow, freq) {
  data <- dataset$dataset

  data <- data |>
    dplyr::mutate(
      code = str_extract(kodehs, "^\\[\\d+\\]"),
      code = str_remove_all(code, "\\[|\\]"),
      description = str_remove(kodehs, "^\\[\\d+\\]\\s"),
      tahun = as.integer(tahun)
    ) |>
    clean_character() |>
    dplyr::select(!kodehs) |>
    dplyr::relocate(code, description)

  if (identical(freq, "monthly")) {
    data <- data |>
      dplyr::mutate(
        bulan = readr::parse_number(bulan),
        bulan = as.integer(bulan)
      ) |>
      dplyr::rename(month = bulan) |>
      dplyr::arrange(code, pod, ctr, tahun, month)
  } else {
    data <- dplyr::arrange(data, code, pod, ctr, tahun)
  }

  hs <- if (identical(by, "chapter")) "hs_chapter" else "hs_code"
  hs <- as.symbol(hs)
  flow <- as.symbol(flow)

  data <- data |>
    dplyr::rename(
      {{ hs }} := code,
      port = pod,
      partner = ctr,
      year = tahun,
      net_weight = netweight,
      {{ flow }} := value
    ) |>
    dplyr::relocate(net_weight, {{ flow }}, .after = tidyselect::last_col())

  new_bpsr_tbl(data, dataset$metadata)
}


new_bpsr_tbl <- function(data, metadata) {
  new_tibble(data, metadata = metadata, class = "bpsr_tbl")
}


# Helper ------------------------------------------------------------------

clean_character <- function(data) {
  if (any("vertical_var" == names(data))) {
    # Remove HTML character references to clean the strings
    data <- dplyr::mutate(
      data,
      vertical_var = stringr::str_remove_all(vertical_var, "&lt;/?.&gt;")
    )
  }

  data |>
    dplyr::mutate(
      dplyr::across(\(x) is_character(x), ~ dplyr::na_if(.x, "")),
      dplyr::across(\(x) is_character(x), ~ dplyr::na_if(.x, "Tidak Ada")),
      dplyr::across(
        \(x) is_character(x), ~ dplyr::na_if(.x, "Tidak Ada Satuan")
      ),
      dplyr::across(\(x) is_character(x), str_squish),
      dplyr::across(tidyr::ends_with("id"), as.character)
    )
}


rename_column <- function(data) {
  subject <- c(
    "subcat_id" = "subject_category_id",
    "^subcat$" = "subject_category",
    "sub_id|subj_id" = "subject_id",
    "^subj$|sub_name" = "subject"
  )

  dataset <- c("^var_id$" = "dataset_id")

  verticalVar <- c(
    "kode_ver_id" = "vertical_var_id",
    "vervar_id" = "vertical_var_id",
    "^vervar$" = "vertical_var",
    "item_ver_id" = "vertical_var_item_id",
    "^group_ver_id$|^vertical$" = "vertical_var_group_id",
    "name_group_ver_id" = "vertical_var_group"
  )

  derivedVar <- c(
    "^turvar_id$" = "derived_var_id",
    "^turvar$" = "derived_var",
    "^group_turvar_id$" = "derived_var_group_id",
    "name_group_turvar" = "derived_var_group"
  )

  year <- c("^th_id" = "year_id", "^th$" = "year")

  period <- c(
    "^turth_id$" = "period_id",
    "^turth$" = "period",
    "group_turth_id" = "period_group_id",
    "name_group_turth" = "period_group"
  )

  spreadsheet <- c("^table_id$" = "dataset_id")

  sdg <- c("sdgs_" = "sdg_", "meta_" = "metadata_")

  other <- c(
    "cr_date" = "created",
    "rl_date" = "released",
    "updt_date" = "last_updated",
    "^size$" = "file_size",
    "excel|pdf|^dl$" = "file_url"
  )

  lookup <- c(
    subject,
    dataset,
    verticalVar,
    derivedVar,
    year,
    period,
    spreadsheet,
    sdg,
    other
  )

  dplyr::rename_with(data, ~ str_replace_all(.x, lookup))
}


is_bpsr_tbl <- function(x) {
  inherits(x, "bpsr_tbl")
}


check_bpsr_tbl <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_bpsr_tbl(x)) {
    cli_abort("{.arg {arg}} must be a {.cls bpsr_tbl} object.", call = call)
  }
}


## Dataset ----

create_id_pattern <- function(x, anchor = "start") {
  x <- str_c(x, collapse = "|")

  if (identical(anchor, "end")) {
    return(str_c("(", x, ")$"))
  }

  str_c("^(", x, ")")
}


parse_vertical_var <- function(data, vertical_var, dataset_id) {
  stopifnot(any("composite_id" == names(data)))

  pattern <- str_c(
    str_c(vertical_var$val, collapse = "|"),
    "(?=", dataset_id, ")"
  )

  names(vertical_var) <- c("vertical_var_id", "vertical_var")
  vertical_var$vertical_var_id <- as.character(vertical_var$vertical_var_id)

  data |>
    dplyr::mutate(vertical_var_id = str_extract(composite_id, pattern)) |>
    left_join(vertical_var, by = "vertical_var_id")
}


parse_derived_var <- function(data, derived_var, dataset_id) {
  stopifnot(all(c("composite_id", "vertical_var_id") %in% names(data)))

  pattern <- str_c(
    "(?<=", dataset_id, ")",
    str_c(derived_var$val, collapse = "|")
  )

  names(derived_var) <- c("derived_var_id", "derived_var")
  derived_var$derived_var_id <- as.character(derived_var$derived_var_id)

  data |>
    dplyr::mutate(derived_var_id = str_extract(composite_id, pattern)) |>
    left_join(derived_var, by = "derived_var_id")
}


parse_year_n_period <- function(data, year, period, dataset_id) {
  stopifnot(all(c("vertical_var_id", "derived_var_id") %in% names(data)))

  pattern_year <- create_id_pattern(year$val)
  pattern_period <- create_id_pattern(period$val, anchor = "end")

  names(year) <- c("year_id", "year")
  year$year_id <- as.character(year$year_id)
  year$year <- as.integer(year$year)
  names(period) <- c("period_id", "period")
  period$period_id <- as.character(period$period_id)

  data |>
    dplyr::mutate(
      date_id = str_remove(
        composite_id,
        str_c(vertical_var_id, dataset_id, derived_var_id)
      ),
      year_id = str_extract(date_id, pattern_year),
      period_id = str_extract(composite_id, pattern_period)
    ) |>
    left_join(year, by = "year_id") |>
    left_join(period, by = "period_id") |>
    dplyr::select(!date_id) |>
    dplyr::relocate(period_id, .before = period)
}
