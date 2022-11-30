#' Look up domain of BPS bureau
#'
#' Request the table of domains of BPS bureaus at the city or regency level,
#' province level or both levels.
#'
#' @param level The level of BPS bureaus. Must be either "all", "province" or
#'   "city". Defaults to "all".
#'
#' @returns A tibble containing domain IDs, names and URLs of every BPS bureau
#'   at the given `level`.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # See the list of domains of BPS bureaus at the province level
#' bps_domain(level = "province")
#'
#' @export
bps_domain <- function(level = c("all", "province", "city")) {
  level <- arg_match(level)

  if (!identical(level, "all")) {
    level <- if (identical(level, "province")) "prov" else "kab"
  }

  resp <- bps_request("domain", type = level)

  table <- table(
    pluck(resp, "data", 2),
    pluck(resp, "data", 1),
    "domain"
  )

  parse_table(table)
}
