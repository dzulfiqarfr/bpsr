#' Perform request to view endpoint
#'
#' The low-level function to send a request to the view endpoint.
#'
#' # Model
#'
#' `model` must be one of the following:
#' * "glosarium";
#' * "news";
#' * "pressrelease";
#' * "publication";
#' * "statictable";
#' * "tablestatistic";
#' * "kbli2009";
#' * "kbli2015";
#' * "kbli2017";
#' * "kbli2020"; or
#' * "kbki2015".
#'
#' @inheritParams bps_request
#' @inheritParams bps_list
#' @param model The result model, e.g. "publication" to view the details of
#'   a particular publication. See the Model section below for more details.
#' @param id An ID, e.g. a publication ID.
#'
#' @returns A list with the `bpsr_file` class.
#'
#' @seealso
#' * [bps_download()] to download resources by URL.
#' * [bps_download_spreadsheet()] to download a spreadsheet dataset by ID.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Get information and the download URL of a spreadsheet dataset on annual
#' # inflation
#' bps_view("statictable", "915", domain = "0000", lang = "eng")
#'
#' @export
bps_view <- function(model,
                     id,
                     ...,
                     lang = c("ind", "eng")) {
  check_model(model, "view")
  check_character(id)
  check_length_one(id)

  lang <- arg_match(lang)

  resp <-  bps_request(
    "view",
    ...,
    model = model,
    id = id,
    lang = lang
  )

  if (identical(model, "glosarium") || str_detect(model, "kb[kl]i")) {
    file <- pluck(resp, "data", 2)
  } else {
    file <- resp$data
  }

  file(file)
}


#' @export
print.bpsr_file <- function(x, ...) {
  cli_text("{.cls {class(x)}}")
  str(x, give.attr = FALSE)
  invisible(x)
}


new_file <- function(file = list()) {
  check_list(file)
  structure(file, class = "bpsr_file")
}


file <- function(file = list()) {
  file <- as.list(file)
  new_file(file)
}
