#' Request trade data
#'
#' @description
#' Get data on export from and import to Indonesia.
#'
#' * `bps_hs_code()` retrieves the Harmonized System (HS) code table. This table
#'   contains the HS chapters and codes, which are needed to request trade
#'   data
#' * `bps_get_trade_hs_chapter()` requests export or import data by the
#'   HS chapter or the first two digits of the HS code
#' * `bps_get_trade_hs_full()` requests export or import data by the HS code
#' * `bps_guess_latest_trade_year()` gives the estimated year in which the most
#'   recent data are available
#'
#' # Scope
#'
#' The API provides export and import data only as far back as 2014.
#'
#' @param hs_chapter,hs_code The HS chapter or code. To request by multiple
#'   chapters or codes, supply a vector of them.
#' @param flow The trade flow or direction. Must be either "export" or "import".
#' @param year The year of observation. Defaults to
#'   `bps_guess_latest_trade_year()`, which will try to guess the year in which
#'   the latest observations are available.
#'
#'   Set to `Inf` to request data across all years.
#'
#'   To request by multiple years, supply a vector of the years.
#'   See the Scope section below for more details.
#' @param freq The frequency of observation. Must be either "monthly" or
#'   "annual". Defaults to "monthly".
#'
#' @returns
#' * `bps_hs_code()` returns a tibble containing the HS chapters and codes, as
#'   well as their description.
#' * `bps_get_trade_hs_chapter()` and `bps_get_trade_hs_full()` return a tibble
#'   with the `bpsr_tbl` subclass, which has a `metadata` attribtue. Use
#'   [bps_metadata()] to read it.
#' * `bps_guess_latest_trade_year()` returns an integer.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Request by the HS chapter
#' bps_get_trade_hs_chapter("03", "export", year = 2018)
#'
#' @export
bps_get_trade_hs_chapter <- function(hs_chapter,
                                     flow,
                                     year = bps_guess_latest_trade_year(),
                                     freq = c("monthly", "annual")) {
  bps_dataexim(
    "chapter",
    hs = hs_chapter,
    flow = flow,
    year = year,
    freq = freq
  )
}


#' @rdname bps_get_trade_hs_chapter
#' @export
bps_get_trade_hs_full <- function(hs_code,
                                  flow,
                                  year = bps_guess_latest_trade_year(),
                                  freq = c("monthly", "annual")) {
  bps_dataexim(
    "full",
    hs = hs_code,
    flow = flow,
    year = year,
    freq = freq
  )
}


#' @rdname bps_get_trade_hs_chapter
#' @export
bps_hs_code <- function() {
  get_database("hs")
}


#' @rdname bps_get_trade_hs_chapter
#' @export
bps_guess_latest_trade_year <- function() {
  today <-  lubridate::today()
  year <- lubridate::year(today)

  # If the current month is January or February, lag the year to prevent
  # requesting data that may not be available, yet
  if (any(lubridate::month(today) == c(1, 2))) year <- year - 1

  as.integer(year)
}


# The low-level function to send a request to the dataexim endpoint
bps_dataexim <- function(by,
                         hs,
                         flow,
                         year = bps_guess_latest_trade_year(),
                         freq = c("monthly", "annual"),
                         call = caller_env()) {
  by <- arg_match(by, c("chapter", "full"), error_call = call)

  arg_hs <- if (identical(by, "chapter")) "hs_chapter" else "hs_code"

  check_hs(by, hs, arg = arg_hs, call = call)

  flow <- arg_match(flow, c("export", "import"), error_call = call)

  check_trade_year(year, call = call)

  freq <- arg_match(freq, error_call = call)

  if (length(hs) > 1) hs <- concat(hs)

  resp <- bps_request(
    "dataexim",
    jenishs = as_trade_api_value(by, "jenishs"),
    kodehs = hs,
    sumber = as_trade_api_value(flow, "sumber"),
    tahun = as_trade_api_year(year),
    periode = as_trade_api_value(freq, "periode")
  )

  if (has_no_data(resp)) {
    return()
  }

  dataset <- trade_dataset(
    pluck(resp, "data"),
    extract_trade_metadata(resp, arg_hs, flow)
  )

  parse_dataset_trade(dataset, by, flow, freq)
}


#' @export
print.bpsr_trade_dataset <- function(x, ...) {
  cli_text("{.cls {class(x)}}")
  str(x, max.level = 2, vec.len = 1, give.attr = FALSE)
  invisible(x)
}


new_trade_dataset <- function(dataset = data.frame(), metadata = list()) {
  stopifnot(is.data.frame(dataset))
  check_list(metadata)

  structure(
    list(
      dataset = dataset,
      metadata = metadata
    ),
    class = "bpsr_trade_dataset"
  )
}


trade_dataset <- function(dataset = data.frame(), metadata = list()) {
  dataset <- as.data.frame(dataset)
  metadata <- as.list(metadata)

  new_trade_dataset(dataset, metadata)
}


# Helper ------------------------------------------------------------------

extract_trade_metadata <- function(resp, by, flow) {
  metadata <- resp$metadata

  metadata <- list(
    note = metadata$source,
    hs = metadata$kodehs,
    port = metadata$pod,
    country = metadata$ctr,
    year = metadata$tahun,
    net_weight = metadata$netweight,
    value = metadata$value
  )

  names(metadata)[2] <- by
  names(metadata)[7] <- flow

  metadata
}


check_hs <- function(by,
                     x,
                     arg = caller_arg(x),
                     call = caller_env()) {
  by <- arg_match(by, c("chapter", "full"))

  check_character(x, arg = arg, call = call)

  database <- get_database("hs")

  if (identical(by, "chapter")) {
    database_sub <- database$hs_chapter
  } else {
    database_sub <- database$hs_code
  }

  if (!is_valid(x, database_sub)) {
    if (length(x) > 1) x <- subset_invalid(x, database_sub)

    if (identical(by, "chapter")) {
      desc <- "a chapter or the first two digits of an HS code"
    } else {
      desc <- "an HS code"
    }

    cli_abort(
      c(
        "{.arg {arg}} must be {desc}.",
        "x" = "There {?is/are} no {.val {x}} in BPS's HS code database."
      ),
      call = call
    )
  }
}


check_trade_year <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (length(x) == 1 && is.infinite(x)) {
    return()
  }

  check_number(x, arg = arg, call = call)

  latest <- bps_guess_latest_trade_year()
  database <- seq(2014, latest)

  if (!is_valid(x, database)) {
    if (length(x) != 1) x <- subset_invalid(x, database)

    cli_abort("{.arg {arg}} must be between 2014 and {latest}.", call = call)
  }
}


as_trade_api_value <- function(x, query_param) {
  switch (query_param,
    jenishs = if (identical(x, "chapter")) "1" else "2",
    sumber = if (identical(x, "export")) "1" else "2",
    periode =  if (identical(x, "monthly")) "1" else "2",
    cli_abort("Can't find a trade API value for {.val {query_param}}.")
  )
}


as_trade_api_year <- function(x) {
  if (length(x) == 1 && is.infinite(x)) {
    x <- seq(2014, bps_guess_latest_trade_year())
  }
  concat(x)
}


is_trade_dataset <- function(x) {
  inherits(x, "bpsr_trade_dataset")
}


check_trade_dataset <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_trade_dataset(x)) {
    cli_abort(
      "{.arg {arg}} must be a {.cls bpsr_trade_dataset} object",
      call = call
    )
  }
}
