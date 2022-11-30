#' Perform request to list endpoint
#'
#' The low-level function to send one or more requests, depending on `page`,
#' to the list endpoint.
#'
#' # Model
#'
#' `model` must be one of the following:
#' * "subject";
#' * "subcat";
#' * "var";
#' * "vervar";
#' * "turvar";
#' * "turth";
#' * "th";
#' * "unit";
#' * "news";
#' * "newscategory";
#' * "pressrelease";
#' * "publication";
#' * "statictable";
#' * "subcatcsa";
#' * "subjectcsa";
#' * "tablestatistic";
#' * "indicators";
#' * "infographic";
#' * "glosarium";
#' * "sdgs";
#' * "kbli2009";
#' * "kbli2015";
#' * "kbli2017";
#' * "kbli2020"; or
#' * "kbki2015".
#'
#' The API actually also accepts "data" for `model`, but bpsr puts it in a
#' separate family of functions due to differences in the response structure.
#' See [bps_get_dataset()] and [bps_get_datasets()] for more
#' details.
#'
#' @inheritParams bps_request
#' @param model The result model, e.g. "publication" to request a list of
#'   publications. See the Model section below for more details.
#' @param domain_id The domain ID of a BPS bureau. Defaults to "0000", which
#'   is the domain ID of the central bureau. Use [bps_domain()] to see the
#'   list of domain IDs.
#' @param lang The result's language. Must be either "ind" for Indonesian or
#'   "eng" for English. Defaults to Indonesian.
#'
#' @returns A list with the `bpsr_table` class.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Get a list of some publications
#' bps_list("publication", page = c(1, 2, 3), lang = "eng")
#'
#' @export
bps_list <- function(model,
                     ...,
                     domain_id = "0000",
                     page = NULL,
                     lang = c("ind", "eng")) {
  check_model(model, "list")
  check_domain_id(domain_id)

  lang <- arg_match(lang)

  if (!is_null(page)) {
    resp <- bps_request_paginated(
      "list",
      ...,
      model = model,
      domain = domain_id,
      lang = lang,
      page = page
    )
  } else {
    resp <- bps_request(
      "list",
      ...,
      model = model,
      domain = domain_id,
      lang = lang
    )
  }

  # Put a single `resp` inside a list to prevent errors when `map()`-ing
  if (is_response(resp)) resp <- list(resp)

  resp <- map(resp, parse_empty_resp)

  table(
    map(resp, pluck, "data", 2),
    map(resp, pluck, "data", 1),
    model
  )
}


#' @export
print.bpsr_table <- function(x, ...) {
  cli_text("{.cls {class(x)}}")

  # Translate `model` to provide a helpful table name
  model <- attr(x, "model")

  cli_text("{.strong Table}: {as_table_name(model)}")
  cli_text("{.strong Content}:")
  str(x, max.level = 3, vec.len = 1, list.len = 5, give.attr = FALSE)

  invisible(x)
}


new_table <- function(table = list(),
                      result_info = list(),
                      model = character()) {
  check_list(table)
  check_list(result_info)
  check_character(model)

  structure(
    list(
      table = table,
      result_info = result_info
    ),
    model = model,
    class = "bpsr_table"
  )
}


table <- function(table = list(),
                  result_info = list(),
                  model = character()) {
  table <- as.list(table)
  result_info <- as.list(result_info)
  model <- as.character(model)

  new_table(table, result_info, model)
}


# Helper ------------------------------------------------------------------

check_model <- function(x, path, arg = caller_arg(x), call = caller_env()) {
  check_string(x, arg = arg, call = call)

  path <- arg_match(path, c("list", "view"))

  if (identical(path, "list")) {
    if (identical(x, "data")) {
      cli_abort(
        c(
          "{.arg {arg}} must be other than {.val {x}}.",
          "i" = "Did you mean to use {.fun bpsr::bps_get_dataset}?"
        ),
        call = call
      )
    }

    model <- c(
      "subject",
      "subcat",
      "var",
      "vervar",
      "turvar",
      "turth",
      "th",
      "unit",
      "news",
      "newscategory",
      "pressrelease",
      "publication",
      "statictable",
      "subcatcsa",
      "subjectcsa",
      "tablestatistic",
      "indicators",
      "infographic",
      "glosarium",
      "sdgs",
      str_c("kbli", c(2009, 2015, 2017, 2020)),
      "kbki2015"
    )
  } else {
    model <- c(
      "glosarium",
      "news",
      "publication",
      "pressrelease",
      "statictable",
      "tablestatistic",
      str_c("kbli", c(2009, 2015, 2017, 2020)),
      "kbki2015"
    )
  }

  if (!any(x == model)) {
    cli_abort(
      c(
        "{.arg {arg}} must be accepted by the endpoint.",
        "i" = "See the Model section of {.fun bpsr::bps_{path}} documentation."
      ),
      call = call
    )
  }
}


check_domain_id <- function(x, arg = caller_arg(x), call = caller_env()) {
  database <- get_database("domain")

  check_id(
    x,
    database$domain_id,
    "domain",
    length_one = TRUE,
    arg = arg,
    call = call
  )
}


# Ensure that empty elements are translated to `NULL`s, instead of empty strings
parse_empty_resp <- function(resp) {
  if (!has_no_data(resp)) {
    return(resp)
  }

  list(data = list(list(), data.frame()))
}


as_table_name <- function(x) {
  if (length(x) == 0) {
    return(x)
  }

  lookup <- c(
    subcat = "subject category",
    var = "dataset",
    vervar = "vertical variable",
    turvar = "derived variable",
    th = "year",
    turth = "period",
    newscategory = "news category",
    pressrelease = "press release",
    statictable = "spreadsheet dataset",
    sdgs = "Sustainable Development Goals (SDG)",
    kbli2009 = "2009 industrial classification (KBLI)",
    kbli2015 = "2015 industrial classification (KBLI)",
    kbli2017 = "2017 industrial classification (KBLI)",
    kbli2020 = "2020 industrial classification (KBLI)",
    kbki2015 = "2015 commodity classification (KBKI)"
  )

  if (!(x %in% names(lookup))) {
    return(x)
  }

  lookup[[x]]
}


is_table <- function(x) {
  inherits(x, "bpsr_table")
}


check_table <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_table(x)) {
    cli_abort("{.arg {arg}} must be a {.cls bpsr_table} object.", call = call)
  }
}
