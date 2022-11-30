#' Extract dataset metadata
#'
#' Read the metadata of a BPS dataset.
#'
#' @param data A tibble with the `bpsr_tbl` subclass.
#'
#' @returns A list with the `bpsr_metadata` class.
#'
#' @seealso
#' * [bps_get_dataset()] and [bps_get_datasets()] to request datasets.
#' * [bps_get_trade_hs_chapter()] and [bps_get_trade_hs_full()] to request
#'   trade datasets.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Read the metadata of personal care and other services inflation dataset
#' inflation <- bps_get_dataset("1927", lang = "eng")
#' bps_metadata(inflation)
#'
#' # Read the metadata of a trade dataset
#' trade <- bps_get_trade_hs_chapter("01", "export")
#' bps_metadata(trade)
#'
#' @export
bps_metadata <- function(data) {
  check_bpsr_tbl(data)

  metadata <- attr(data, "metadata")

  class(metadata) <- "bpsr_metadata"

  metadata
}


#' @export
print.bpsr_metadata <- function(x, ...) {
  cli_text("{.cls {class(x)}}")
  str(x, give.attr = FALSE)
  invisible(x)
}
