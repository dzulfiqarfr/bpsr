#' Set BPS API key
#'
#' @description
#' Provide the API key to bpsr by storing it in an environment variable called
#' `BPSR_KEY`.
#'
#' @param key The API key. Defaults to `NULL`, which prompts the user to add it
#'   using a dialog box.
#'
#' @examples
#' \dontrun{
#' bps_set_key()
#' }
#'
#' @export
bps_set_key <- function(key = NULL) {
  if (is_null(key)) {
    key <- askpass::askpass("Please enter your API key:")
  }

  Sys.setenv(BPSR_KEY = key)
}


get_bps_api_key <- function() {
  key <- Sys.getenv("BPSR_KEY")

  if (testthat::is_testing()) {
    key <- httr2::secret_decrypt(
      "ktsDYcMd10lccwO21N-8AlJ9G9PHkj3AL9xqC3Mvu6lZW-JIGbIh8JdQXTY4J-vG",
      "BPSR_KEY"
    )
  }

  if (identical(key, "")) {
    cli_abort("Can't find API key. Set it with {.fun bpsr::bps_set_key}.")
  }

  key
}
