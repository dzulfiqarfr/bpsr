#' Find resource on list endpoint
#'
#' Search resources on the list endpoint, such as press releases, news and
#' publications.
#'
#' # Model
#'
#' `model` must be one of the following:
#' * "statictable";
#' * "pressrelease";
#' * "publication";
#' * "infographic"; or
#' * "news".
#'
#' @inheritParams bps_list
#' @param keyword The keyword to filter the search result.
#'
#' @returns A tibble.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' bps_search("news", keyword = "inflation", lang = "eng")
#'
#' @export
bps_search <- function(model,
                       ...,
                       keyword = NULL,
                       domain_id = "0000",
                       page = NULL,
                       lang = c("ind", "eng")) {
  model <- arg_match(
    model,
    c(
      "statictable",
      "pressrelease",
      "publication",
      "infographic",
      "news"
    )
  )

  if (!is_null(keyword)) {
    check_keyword(keyword)
  }

  table <- bps_list(
    model,
    ...,
    keyword = replace_whitespace(keyword),
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}


# Helper ------------------------------------------------------------------

replace_whitespace <- function(x) {
  str_replace_all(x, "\\s", "+")
}


check_keyword <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_length_one(x, arg = arg, call = call)
  check_string(x, arg = arg, call = call)
}
