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
    return(make_date_with_period_id(data))
  }

  if (!is_integer(data$year) && !is_integer(data$month)) {
    cli_abort("{.var year} and {.var month} must be integers.")
  }

  data |>
    dplyr::mutate(date = lubridate::make_date(year, month)) |>
    dplyr::relocate(date, .after = month)
}



# Helper ------------------------------------------------------------------

make_date_with_period_id <- function(data, call = caller_env()) {
  if (!exists("db_period_month")) {
    cli_alert_warning(
      str_c(
        "Can't find {.var db_period_month}. ",
        "Unable to make date out of {.var period_id} and {.var year}."
      )
    )

    return(data)
  }

  data |>
    left_join(db_period_month, by = "period_id") |>
    dplyr::mutate(
      date = dplyr::case_when(
        !str_detect(period, "Tahun|Annual") ~ lubridate::make_date(year, month)
      )
    ) |>
    dplyr::select(!month) |>
    dplyr::relocate(date, .after = period)
}
