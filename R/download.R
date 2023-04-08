#' Save resource from BPS API
#'
#' Download resources from BPS API, such as spreadsheet datasets, publications,
#' press releases or infographics.
#'
#' @param url A BPS API download URL. Supply a vector of URLs to download
#'   multiple resources.
#' @param file_path The path to save the resource. Must have a ".xls", ".pdf" or
#'   ".png" extension. Supply a vector of the same length as `url` to
#'   save multiple resources.
#'
#' @returns A character vector of `file_path`.
#'
#' @seealso
#' * [bps_view()] to get information such as the download URL of a resource.
#' * [bps_search()] to find resources and their download URLs.
#' * [bps_dataset_spreadsheet()] to look up spreadsheet datasets and their
#'   download URLs.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Download an infographic
#' infographics <- bps_search("infographic")
#'
#' \dontrun{
#' bps_download(infographics$file_url[[1]], "infographics.png")
#' }
#'
#' @export
bps_download <- function(url, file_path) {
  check_url_file_path_length(url, file_path)
  check_download_url(url)
  check_file_extension(file_path)

  nfile <- length(file_path)

  cli_progress_step(
    "Downloading {nfile} file{?s}...",
    msg_done = "Successfully downloaded {nfile} file{?s}."
  )

  if (length(url) > 1) {
    map(url, httr2::request) |>
      httr2::multi_req_perform(paths = file_path)
  } else {
    httr2::request(url) |>
      httr2::req_perform(path = file_path)
  }

  invisible(file_path)
}


# Helper ------------------------------------------------------------------

check_url_file_path_length <- function(url, file_path, call = caller_env()) {
  if (length(url) != length(file_path)) {
    cli_abort(
      "{.arg url} and {.arg file_path} must have the same length.",
      call = call
    )
  }
}


is_download_url <- function(x) {
  str_detect(x, "https://www\\.bps\\.go\\.id/.+/(downloadapi|download)\\.html")
}


check_download_url <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (purrr::every(x, is_download_url)) {
    return()
  }

  cli_abort("{.arg {arg}} must be a BPS API download URL.", call = call)
}


is_file_extension_ok <- function(x) {
  str_detect(x, "\\.(xls|pdf|png)$")
}


check_file_extension <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_character(x, arg = arg, call = call)

  if (purrr::every(x, is_file_extension_ok)) {
    return()
  }

  cli_abort(
    str_c(
      '{.arg {arg}} must have either ',
      '{.or {.val {c(".xls", ".pdf", ".png")}}} extension.'
    ),
    call = call
  )
}
