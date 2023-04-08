#' Look up SDG dataset
#'
#' Request the table of datasets related to the Sustainable Development Goals
#' (SDG). This table contains dataset IDs, which are needed to request datasets
#' using [bps_get_dataset()] or [bps_get_datasets()].
#'
#' @inheritParams bps_list
#' @param goal The goal number.
#'
#' @returns A tibble containing dataset IDs and titles, among other things.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Get a list of some datasets related to goal number one
#' bps_sdg(goal = 1, lang = "eng")
#'
#' @export
bps_sdg <- function(goal = NULL, page = NULL, lang = c("ind", "eng")) {
  if (!is_null(goal)) {
    check_goal(goal)
  }

  table <- bps_list(
    "sdgs",
    goal = goal,
    page = page,
    lang = lang
  )

  parse_table(table)
}


# Helper ------------------------------------------------------------------

check_goal <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_number(x, arg = arg, call = call)
  check_length_one(x, arg = arg, call = call)

  if (any(x == seq(1, 17))) {
    return()
  }

  cli_abort("{.arg {arg}} must be between {.val {c(1, 17)}}.", call = call)
}
