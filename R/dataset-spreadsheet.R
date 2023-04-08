#' Look up and request spreadsheet dataset
#'
#' @description
#' Search and download datasets that BPS distributes in spreadsheets. The API
#' calls them static tables.
#'
#' * `bps_dataset_spreadsheet()` requests the spreadsheet dataset table
#' * `bps_download_spreadsheet()` downloads a spreadsheet
#'
#' # File format
#'
#' The spreadsheet dataset comes in a Microsoft Excel file format with the
#' ".xls" extension.
#'
#' @inheritParams bps_search
#' @param month The month in which a spreadsheet is last updated.
#' @param year The year in which a spreadsheet is last updated.
#'
#' @returns
#' * `bps_dataset_spreadsheet()` returns a tibble containing dataset IDs and
#'   titles, as well as other related details.
#' * `bps_download_spreadsheet()` invisibly returns a string of the file path.
#'
#' @seealso [bps_download()] to download resources by URL.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Search spreadsheet datasets on unemployment
#' spreadsheet <- bps_dataset_spreadsheet(
#'   keyword = "unemployment",
#'   lang = "eng"
#' )
#'
#' \dontrun{
#' bps_download_spreadsheet(spreadsheet$dataset_id[[1]], "spreadsheet.xls")
#' }
#'
#' @export
bps_dataset_spreadsheet <- function(keyword = NULL,
                                    month = NULL,
                                    year = NULL,
                                    domain_id = "0000",
                                    page = NULL,
                                    lang = c("ind", "eng")) {
  if (!is_null(month)) {
    check_month(month)
  }

  if (!is_null(year)) {
    check_number(year)
    check_length_one(year)
  }

  bps_search(
    "statictable",
    month = month,
    year = year,
    keyword = keyword,
    domain_id = domain_id,
    page = page,
    lang = lang
  )
}


#' @rdname bps_dataset_spreadsheet
#'
#' @param dataset_id The dataset ID. Use [bps_dataset_spreadsheet()] to see
#'   the list of dataset IDs.
#' @param file_path The path to save the spreadsheet. Must have a ".xls"
#'   extension.
#'
#' @export
bps_download_spreadsheet <- function(dataset_id,
                                     file_path,
                                     domain_id = "0000",
                                     lang = c("ind", "eng")) {
  check_spreadsheet_dataset_id(dataset_id, domain_id)

  if (!str_detect(file_path, "\\.xls$")) {
    cli_abort('{.arg file_path} must have a {.val ".xls" extension.')
  }

  check_domain_id(domain_id)

  file <- bps_view(
    "statictable",
    id = dataset_id,
    domain = domain_id,
    lang = lang
  )

  bps_download(file$excel, file_path)
}


# Helper ------------------------------------------------------------------

check_month <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_number(x, arg = arg, call = call)
  check_length_one(x, arg = arg, call = call)

  if (any(x == seq(1, 12))) {
    return()
  }

  cli_abort("{.arg {arg}} must be between {.val {c(1, 12)}}.", call = call)
}
