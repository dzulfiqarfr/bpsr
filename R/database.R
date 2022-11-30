update_database <- function(overwrite = TRUE) {
  check_logical(overwrite)

  db_domain <- bps_domain()
  db_subject_category <- bps_subject_category()
  db_subject <- bps_subject(page = Inf)
  db_dataset <- bps_dataset(page = Inf)
  db_vertical_var <- bps_vertical_var(page = Inf)
  db_derived_var <- bps_derived_var(page = Inf)
  db_year <- bps_year(page = Inf)
  db_period <- bps_period(page = Inf)
  db_spreadsheet <- bps_dataset_spreadsheet(page = Inf)
  db_hs <- scrape_bps_hs_code()

  db_period_month <- parse_period_id_to_month(db_period)

  usethis::use_data(
    db_domain,
    db_subject_category,
    db_subject,
    db_dataset,
    db_vertical_var,
    db_derived_var,
    db_year,
    db_period,
    db_spreadsheet,
    db_hs,
    db_period_month,
    internal = TRUE,
    overwrite = overwrite
  )
}


get_database <- function(x) {
  check_string(x)

  switch (x,
    domain = db_domain,
    subject_category = db_subject_category,
    subject = db_subject,
    dataset = db_dataset,
    vertical_var = db_vertical_var,
    derived_var = db_derived_var,
    year = db_year,
    period = db_period,
    hs = db_hs,
    cli_abort("Can't find a database for {.val {x}}.")
  )
}


# Helper ------------------------------------------------------------------

scrape_bps_hs_code <- function(year = 2022, parse = TRUE) {
  check_number(year)
  check_logical(parse)

  if (!any(year %in% c(2008, 2009, 2012, 2017, 2022))) {
    abort("`year` must be either 2008, 2009, 2012, 2017 or 2022.")
  }

  html <- rvest::read_html(
    "https://www.bps.go.id/exim/masterhs.html#subjekViewTab5"
  )

  table <- html |>
    rvest::html_element(str_c("table#hs", year)) |>
    rvest::html_table()

  if (!parse) {
    return(table)
  }

  table |>
    dplyr::rename(description = DESKRIPSI) |>
    dplyr::mutate(
      year = year,
      hs_code = str_extract(description, "\\[\\d*\\]"),
      hs_code = str_remove_all(hs_code, "[:punct:]"),
      description = str_remove(description, "\\[\\d*\\]\\s"),
      hs_chapter = str_sub(hs_code, 1, 2)
    ) |>
    dplyr::select(year, hs_chapter, hs_code, description)
}


# Parse the month number of the periods, which can facilitate date creation
# using period IDs. This is useful for periods such as quarters and semesters
parse_period_id_to_month <- function(data) {
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
