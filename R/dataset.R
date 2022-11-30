#' Look up and request dataset
#'
#' @description
#' Search and get datasets that BPS distributes in JSONs. The API calls them
#' dynamic tables.
#'
#' * `bps_dataset()` requests the dataset table. This table contains dataset
#'   IDs, which are needed to request datasets
#' * `bps_get_dataset()` requests a dataset
#' * `bps_get_datasets()` requests multiple datasets
#'
#' # Request filter
#'
#' Filter a request by multiple values of `vertical_var_id`,
#' `derived_var_id`, `year_id` or `period_id` by supplying a vector of the IDs,
#' to `bps_get_dataset()`. For example, `vertical_var_id = c("1100", "1200")`.
#'
#' For `year_id` and `period_id`, filter by a range of values by concatenating
#' the start and the end of the range using a colon, e.g.
#' `year_id = "118:121"` for observations from 2018 to 2021.
#'
#' @inheritParams bps_list
#' @param subject_id The subject ID. Use [bps_subject()] to see the list of
#'   subject IDs.
#' @param vertical_var_group_id The vertical variable group ID. Use
#'   [bps_vertical_var()] to see the list of vertical variable group IDs.
#'
#' @returns
#' * `bps_dataset()` returns a tibble containing dataset IDs and titles.
#' * `bps_get_dataset()` returns a tibble with the `bpsr_tbl` subclass, which
#'   has a `metadata` attribute. Use [bps_metadata()] to read it.
#' * `bps_get_datasets()` returns a list with the `bpsr_multiple_tbl` class,
#'   which contains tibbles with the `bpsr_tbl` subclass.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Get the dataset on consumer price index of food category
#' bps_get_dataset("1905", lang = "eng")
#'
#' # Get some Human Development Index datasets
#' bps_get_datasets(c("414", "415", "416", "417"), lang = "eng")
#'
#' @export
bps_dataset <- function(subject_id = NULL,
                        vertical_var_group_id = NULL,
                        domain_id = "0000",
                        page = NULL,
                        lang = c("ind", "eng")) {
  if (!is_null(subject_id)) {
    check_subject_id(subject_id)
  }

  if (!is_null(vertical_var_group_id)) {
    check_vertical_var_group_id(vertical_var_group_id)
  }

  table <- bps_list(
    "var",
    subject = subject_id,
    vervar = vertical_var_group_id,
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}


#' @rdname bps_dataset
#'
#' @param dataset_id The dataset ID. To request multiple datasets, supply a
#'   vector of the IDs to `bps_get_datasets()`. Use `bps_dataset()` to see
#'   the list of dataset IDs.
#' @param vertical_var_id The vertical variable ID. Use [bps_vertical_var()]
#'   to see the list of vertical variable IDs.
#' @param derived_var_id The derived variable ID. Use [bps_derived_var()] to
#'   see the list of derived variable IDs.
#' @param year_id The year ID. Use [bps_year()] to see the list of year IDs.
#' @param period_id The period ID. Use [bps_period()] to see the list of
#'   period IDs.
#' @param keep ID columns to keep. Defaults to "none", which drops all of them.
#'   Set to "all" keep all ID columns. Otherwise, must be "vertical_var_id",
#'   "derived_var_id", "year_id" or "period_id". To keep multiple ID columns,
#'   supply a vector of them.
#'
#' @export
bps_get_dataset <- function(dataset_id,
                            vertical_var_id = NULL,
                            derived_var_id = NULL,
                            year_id = NULL,
                            period_id = NULL,
                            domain_id = "0000",
                            lang = c("ind", "eng"),
                            keep = "none") {
  check_dataset_id(dataset_id)

  if (!is_null(vertical_var_id)) {
    check_vertical_var_id(vertical_var_id, .dataset_id = dataset_id)
    vertical_var_id <- concat(vertical_var_id)
  }

  if (!is_null(derived_var_id)) {
    check_derived_var_id(derived_var_id)
    derived_var_id <- concat(derived_var_id)
  }

  if (!is_null(year_id)) {
    check_year_id(year_id)
    if (!is_range(year_id)) year_id <- concat(year_id)
  }

  if (!is_null(period_id)) {
    check_period_id(period_id)
    if (!is_range(period_id)) period_id <- concat(period_id)
  }

  check_domain_id(domain_id)

  lang <- arg_match(lang)

  resp <- bps_request(
    "list",
    model = "data",
    var = dataset_id,
    vervar = vertical_var_id,
    turvar = derived_var_id,
    th = year_id,
    turth = period_id,
    domain = domain_id,
    lang = lang
  )

  if (has_no_data(resp)) {
    return()
  }

  dataset <- as_dataset(resp)

  parse_dataset(dataset, keep = keep)
}


#' @rdname bps_dataset
#' @export
bps_get_datasets <- function(dataset_id,
                             domain_id = "0000",
                             lang = c("ind", "eng"),
                             keep = "none") {
  if (length(dataset_id) == 1) {
    cli_abort(c(
      "{.arg dataset_id} must be a vector of dataset IDs.",
      "i" = "Did you mean to use {.fun bpsr::bps_get_dataset}?"
    ))
  }

  check_dataset_id(dataset_id, length_one = FALSE)
  check_domain_id(domain_id)

  lang <- arg_match(lang)

  resp <- bps_request_multiple(
    "list",
    list(
      model = "data",
      var = dataset_id,
      domain = domain_id,
      lang = lang
    )
  )

  dataset <- map(resp, as_dataset)

  parse_datasets(dataset, dataset_id, keep = keep)
}


#' @export
print.bpsr_dataset <- function(x, ...) {
  cli_text("{.cls {class(x)}}")
  str(x, max.level = 2, vec.len = 1, give.attr = FALSE)
  invisible(x)
}


new_dataset <- function(dataset = tibble::tibble(),
                        lookup_table = list(),
                        metadata = list()) {
  stopifnot(tibble::is_tibble(dataset))
  check_list(lookup_table)
  check_list(metadata)

  structure(
    list(
      dataset = dataset,
      lookup_table = lookup_table,
      metadata = metadata
    ),
    class = "bpsr_dataset"
  )
}


dataset <- function(dataset = tibble::tibble(),
                    lookup_table = list(),
                    metadata = list()) {
  dataset <- as_tibble(dataset)
  lookup_table <- as.list(lookup_table)
  metadata <- as.list(metadata)

  new_dataset(dataset, lookup_table, metadata)
}


as_dataset <- function(resp) {
  dataset(
    pluck(resp, "datacontent"),
    extract_dataset_lookup_table(resp),
    extract_dataset_metadata(resp)
  )
}


# Helper ------------------------------------------------------------------

check_subject_id <- function(x, arg = caller_arg(x), call = caller_env()) {
  database <- get_database("subject")

  check_id(
    x,
    database$subject_id,
    "subject",
    length_one = TRUE,
    arg = arg,
    call = call
  )
}



check_dataset_id <- function(x,
                             length_one = TRUE,
                             arg = caller_arg(x),
                             call = caller_env()) {
  database <- get_database("dataset")

  check_id(
    x,
    database$dataset_id,
    "dataset",
    length_one = length_one,
    arg = arg,
    call = call
  )
}


check_vertical_var_id <- function(x,
                                  .dataset_id = NULL,
                                  length_one = FALSE,
                                  arg = caller_arg(x),
                                  call = caller_env()) {
  database <- get_database("vertical_var")

  if (!is_null(.dataset_id)) {
    dataset <- get_database("dataset")

    id <- dataset |>
      dplyr::filter(dataset_id == .dataset_id) |>
      dplyr::pull(vertical_var_group_id)

    database <- dplyr::filter(database, vertical_var_group_id == id)
  }

  check_id(
    x,
    database$vertical_var_id,
    "vertical variable",
    length_one = length_one,
    arg = arg,
    call = call
  )
}


check_vertical_var_group_id <- function(x,
                                        arg = caller_arg(x),
                                        call = caller_env()) {
  database <- get_database("vertical_var")

  check_id(
    x,
    database$vertical_var_group_id,
    "vertical variable group",
    length_one = TRUE,
    arg = arg,
    call = call
  )
}


check_derived_var_id <- function(x,
                                 length_one = FALSE,
                                 arg = caller_arg(x),
                                 call = caller_env()) {
  database <- get_database("derived_var")

  check_id(
    x,
    database$derived_var_id,
    "derived variable",
    length_one = length_one,
    arg = arg,
    call = call
  )
}


is_range <- function(x) {
  length(x) == 1 && str_detect(x, ":")
}


check_year_id <- function(x,
                          length_one = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (is_range(x)) x <- unlist(str_split(x, ":"))

  database <- get_database("year")

  check_id(
    x,
    database$year_id,
    "year",
    length_one = length_one,
    arg = arg,
    call = call
  )
}


check_period_id <- function(x,
                            length_one = FALSE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  if (is_range(x)) x <- unlist(str_split(x, ":"))

  database <- get_database("period")

  check_id(
    x,
    database$period_id,
    "period",
    length_one = length_one,
    arg = arg,
    call = call
  )
}


extract_dataset_lookup_table <- function(resp) {
  list(
    vertical_var = resp$vervar,
    derived_var = resp$turvar,
    year = resp$tahun,
    period = resp$turtahun
  )
}


extract_dataset_metadata <- function(resp) {
  metadata <- resp$var
  methodology <- resp$metadata

  list(
    dataset_id = as.character(metadata$val),
    dataset = metadata$label,
    vertical_var = resp$labelvervar,
    subject = metadata$subj,
    methodology = methodology$variable,
    activity = methodology$activity,
    note = metadata$note,
    def = metadata$def,
    decimal = metadata$decimal,
    var = metadata$unit
  )
}


is_dataset <- function(x) {
  inherits(x, "bpsr_dataset")
}


check_dataset <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_dataset(x)) {
    cli_abort("{.arg {arg}} must be a {.cls bpsr_dataset} object.")
  }
}
