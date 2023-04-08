#' Perform single, multiple or paginated request to BPS API
#'
#' @description
#' Perform both a single request and multiple requests to BPS API.
#'
#' * `bps_request()` sends a single request
#' * `bps_request_multiple()` sends multiple requests with multiple query
#'   parameters to one endpoint at a time
#' * `bps_request_paginated()` sends multiple requests to multiple pages to
#'   the list endpoint
#'
#' @param url_path The path component of the URL. Must be either "domain",
#'   "list", "view", "dataexim" or "interoperabilitas".
#' @param ... Name-value pairs that provide query parameters. See
#'   [httr2::req_url_query()] for more details.
#'
#' @returns
#' * `bps_request()` returns a list with the `bpsr_response` class.
#' * `bps_request_multiple()` and `bps_request_paginated()` return a list with
#'   the `bpsr_multiple_responses` class.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Request a list of domains of every BPS bureau
#' bps_request("domain", type = "all")
#'
#' # Request a list of dataset subjects on each domain on different pages
#' bps_request_multiple(
#'   "list",
#'   list(
#'     model = "subject",
#'     domain = c("0000", "1100", "1500"),
#'     page = c(1, 2, 1),
#'     lang = "eng"
#'   )
#' )
#'
#' # Request the complete list of dataset subjects on the central bureau's
#' # domain
#' bps_request_paginated(
#'   "list",
#'   model = "subject",
#'   domain = "0000",
#'   lang = "eng",
#'   page = Inf
#' )
#'
#' @export
bps_request <- function(url_path, ...) {
  resp <- build_request(url_path, ...) |>
    httr2::req_perform()

  # Use a safer wrapped function to handle the error when the response is
  # actually an HTML file
  body <- safe_resp_body_json(resp, simplifyVector = TRUE)

  check_parsing_error(body)

  body <- body$result

  # Override the error status of responses from the dataexim endpoint when
  # there are no data despite correct inputs. This prevents a misleading error
  if (identical(url_path, "dataexim")) {
    body <- override_status_empty_resp(body, "OK")
  }

  if (is_status_error(body)) {
    abort(parse_error_message(body))
  }

  if (has_no_data(body)) cli_alert_info("The request yields no data.")

  new_response(body)
}


#' @rdname bps_request
#'
#' @param query_params A list of name-value pairs that provide query parameters.
#'
#' @export
bps_request_multiple <- function(url_path, query_params) {
  check_query_params(query_params)

  resp <- pmap(
    query_params,
    build_request,
    url_path = url_path
  ) |>
    httr2::multi_req_perform()

  body <- map(resp, safe_resp_body_json, simplifyVector = TRUE)

  check_parsing_error(body)

  body <- map(body, pluck, "result")

  if (identical(url_path, "dataexim")) {
    body <- map(body, override_status_empty_resp, "OK")
  }

  check_status_error(body)

  info_no_data(body)

  new_multiple_responses(body)
}


#' @rdname bps_request
#'
#' @param page The page number of the result. Defaults to page one. Set to `Inf`
#'   to request all pages.
#'
#' @export
bps_request_paginated <- function(url_path, ..., page = 1) {
  path <- arg_match(url_path, "list")

  page_is_inf <- length(page) == 1 && is.infinite(page)

  if (!page_is_inf) {
    check_number(page)
  }

  if (page_is_inf) {
    first_page <- bps_request("list", page = 1, ...)

    if (has_no_data(first_page)) {
      return(first_page)
    }

    total_page <- pluck(first_page, "data", 1, "pages")

    if (total_page == 1) {
      return(first_page)
    }

    query_param <- list(page = seq(2, total_page))
  } else {
    query_param <- list(page = page)
  }

  resp <- pmap(
    query_param,
    build_request,
    url_path = "list",
    ...
  ) |>
    httr2::multi_req_perform()

  body <- map(resp, safe_resp_body_json, simplifyVector = TRUE)

  check_parsing_error(body)

  body <- map(body, pluck, "result")

  if (!page_is_inf) {
    check_status_error(body)
  }

  if (page_is_inf) body <- c(list(first_page), body)

  info_no_data(body)

  new_multiple_responses(body)
}


#' @export
print.bpsr_response <- function(x, ...) {
  cli_text("{.cls {class(x)}}")
  cli_text("{.strong Content}:")
  str(x, max.level = 3, vec.len = 1, give.attr = FALSE)
  invisible(x)
}


#' @export
print.bpsr_multiple_responses <- function(x, ...) {
  cli_text("{.cls {class(x)}}")

  status <- purrr::map_chr(x, pluck, "status")

  cli_text("{.strong Status}:")

  ul_status <- cli::cli_ul()

  cli_li("OK: {sum(status == 'OK')}")
  cli_li("Error: {sum(status == 'Error')}")
  cli_end(ul_status)

  data <- purrr::map_lgl(x, has_no_data)

  cli_text("{.strong Data availability}:")

  ul_data <- cli_ul()

  cli_li("Available: {sum(!data)}")
  cli_li("Not available: {sum(data)}")
  cli_end(ul_data)

  cli_text("{.strong Contents}:")
  str(x, max.level = 3, vec.len = 1, list.len = 5, give.attr = FALSE)

  invisible(x)
}


build_request <- function(url_path, ..., key = get_bps_api_key()) {
  url_path <- arg_match(
    url_path,
    c("domain", "list", "view", "dataexim", "interoperabilitas")
  )

  httr2::request("https://webapi.bps.go.id/v1/api/") |>
    httr2::req_url_path_append(url_path) |>
    httr2::req_url_query(..., key = key) |>
    httr2::req_user_agent("bpsr (https://github.com/dzulfiqarfr/bpsr)") |>
    httr2::req_error(body = parse_error_body)
}


new_response <- function(resp = list()) {
  check_list(resp)
  structure(resp, class = "bpsr_response")
}


new_multiple_responses <- function(resp = list()) {
  check_list(resp)
  structure(resp, class = "bpsr_multiple_responses")
}


# Helper ------------------------------------------------------------------

parse_error_body <- function(resp) {
  body <- httr2::resp_body_json(resp)

  if ("message2" %in% names(body)) {
    return(body[["message2"]])
  }

  body[["message"]]
}


safe_resp_body_json <- purrr::safely(httr2::resp_body_json)


is_parsing_error <- function(resp) {
  !is_null(resp$error)
}


check_parsing_error <- function(resp, call = caller_env()) {
  if (length(resp) > 1) {
    if (purrr::none(resp, is_parsing_error)) {
      return()
    }

    abort(
      "Can't parse the body of some responses.",
      parsing_error = map(resp, pluck, "error"),
      call = call
    )
  } else {
    if (!is_parsing_error(resp)) {
      return()
    }

    abort(
      c(
        "Can't parse the response body.",
        "i" = "Please check again the values supplied to the query parameters."
      ),
      parsing_error = resp$error,
      call = call
    )
  }
}


override_status_empty_resp <- function(resp, new_status) {
  new_status <- arg_match(new_status, c("OK", "Error"))

  if (is_status_error(resp) && has_no_data(resp)) {
    if (identical(resp$message, "")) {
      resp$status <- new_status
    }
  }

  resp
}


is_status_error <- function(resp) {
  identical(resp$status, "Error")
}


parse_error_message <- function(resp) {
  message <- resp$message

  if (str_detect(message, "key")) {
    return("Can't authenticate the request.")
  }

  if (str_detect(message, "Parameter.+\\sMissing\\.")) {
    param <- message |>
      str_extract("\\s\\w+") |>
      str_to_lower() |>
      str_squish()

    return(glue("The request must contain `{param}` query parameter."))
  }

  invalid <- str_c(
    "is not implemented for function domain",
    "Model\\s\\w+\\sis not recognized",
    sep = "|"
  )

  if (str_detect(message, invalid)) {
    param <- message |>
      str_extract("\\w+\\s") |>
      str_to_lower() |>
      str_squish()

    value <- message |>
      str_extract("\\s\\w+\\s") |>
      str_squish()

    return(glue('`{param}` can\'t recognize "{value}".'))
  }

  message
}


check_status_error <- function(resp, call = caller_env()) {
  if (purrr::none(resp, is_status_error)) {
    return()
  }

  location <- which(purrr::map_lgl(resp, is_status_error))
  error <- purrr::keep(resp, is_status_error)
  message <- purrr::map_chr(error, parse_error_message)
  message <- str_c("Request ", location, ": ", message) |>
    rlang::set_names("x")

  cli_abort(
    c("Can't perform {length(location)} request{?s} successfully.", message),
    resp = resp,
    call = call
  )
}


has_no_data <- function(resp) {
  str_detect(resp$`data-availability`, "not")
}


info_no_data <- function(resp) {
  if (purrr::none(resp, has_no_data)) {
    return()
  }

  location <- which(purrr::map_lgl(resp, has_no_data))

  cli_alert_info(str_c(
    "{length(location)} request{?s} at {?this/these} location{?s} ",
    "{?yields/yield} no data: {location}."
  ))
}


check_query_params <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_list(x, arg = arg, call = call)

  if (!is_null(names(x))) {
    return()
  }

  cli_abort("{.arg {arg}} must be a named list.", call = call)
}


is_response <- function(x) {
  inherits(x, "bpsr_response")
}


check_response <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_response(x)) {
    return()
  }

  cli_abort(
    "{.arg {arg}} must be a {.cls bpsr_response} object.",
    call = call
  )
}


is_multiple_responses <- function(x) {
  inherits(x, "bpsr_multiple_responses")
}
