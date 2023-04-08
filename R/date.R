#' Create date column in dataset
#'
#' Adds a `date` variable to a BPS dataset, based on years and period IDs or
#' numeric representations of months. The values are the floor date of the
#' given period. The format is year-month-date (ymd).
#'
#' @param data A tibble with the `bpsr_tbl` subclass.
#'
#' @returns A tibble that has a `date` column.
#'
#' @inherit bps_metadata seealso
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' data <- bps_get_trade_hs_chapter("01", "export", year = 2021)
#' bps_add_date(data)
#'
#' @export
bps_add_date <- function(data) {
  check_bpsr_tbl(data)

  has_period_id <- any("period_id" == names(data))
  has_month <- any("month" == names(data))

  if (!has_period_id && !has_month) {
    cli_abort("{.arg data} must have {.var period_id} or {.var month} column.")
  }

  if (!any("year" == names(data))) {
    cli_abort("{.arg data} must have {.var year} column.")
  }

  if (has_period_id) {
    # Add date using `year` and `period_id` for datasets with those columns
    data_with_date <- make_date_with_period_id(data)

    return(data_with_date)
  }

  # Add date using `year` and `month` for trade datasets
  if (!is_integer(data$year) && !is_integer(data$month)) {
    cli_abort("{.var year} and {.var month} must be integers.")
  }

  data |>
    dplyr::mutate(date = lubridate::make_date(year, month)) |>
    dplyr::relocate(date, .after = month)
}



# Helper ------------------------------------------------------------------

make_date_with_period_id <- function(data, call = caller_env()) {
  if (!exists("period_table")) {
    cli_abort(
      str_c(
        "Can't find the period table in database. ",
        "Unable to make date out of {.var period_id} and {.var year}."
      ),
      call = call
    )
  }

  period_as_month <- parse_period_id_to_month(period_table)

  data |>
    left_join(period_as_month, by = "period_id") |>
    dplyr::mutate(
      date = dplyr::case_when(
        !str_detect(period, "Tahun|Annual") ~ lubridate::make_date(year, month)
      )
    ) |>
    dplyr::select(!month) |>
    dplyr::relocate(date, .after = period)
}


# Parse the month number of the periods, which can facilitate date creation
# using period IDs. This is useful for periods such as quarters and semesters
parse_period_id_to_month <- function(data, call = caller_env()) {
  required_column <- c("period_id", "period", "period_group")

  if (!all(required_column %in% names(data))) {
    cli_abort(
      "{.arg data} must contain {.var {required_column}} columns.",
      call = call
    )
  }

  monthly <- data |>
    dplyr::filter(period_id %in% seq(1, 12)) |>
    dplyr::mutate(month = as.integer(period_id))

  quarterly <- data |>
    dplyr::filter(
      str_detect(period, "Triwulan") | str_detect(period_group, "Triwulan"),
      !str_detect(period, "Tahun"),
      period != "Jumlah"
    ) |>
    dplyr::mutate(
      month = dplyr::case_when(
        str_detect(period, "Triwulan IV|4") ~ 10,
        str_detect(period, "Triwulan I{3}|3") ~ 7,
        str_detect(period, "Triwulan I{2}|2") ~ 4,
        str_detect(period, "Triwulan I{1}|1") ~ 1,
        period == "Maret" ~ 1,
        period == "Juni" ~ 4,
        period == "September" ~ 7,
        period == "Desember" ~ 10
      )
    )

  semester <- data |>
    dplyr::filter(
      str_detect(period_group, "Semester"),
      !str_detect(period, "Tahun")
    ) |>
    dplyr::mutate(
      period = str_remove(period, "Semester\\s\\d\\s"),
      period = str_remove_all(period, "[:punct:]")
    ) |>
    left_join(monthly[, c("period", "month")], by = "period")

  annual <- data |>
    dplyr::filter(str_detect(period, "Tahun") | period == "Jumlah") |>
    dplyr::mutate(month = NA)

  parsed <- dplyr::bind_rows(monthly, quarterly, semester, annual)

  parsed |>
    dplyr::mutate(month = as.integer(month)) |>
    dplyr::select(period_id, month)
}
