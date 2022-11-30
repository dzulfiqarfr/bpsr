#' Look up subject category and subject of dataset
#'
#' @description
#' Get tables of dataset subject category and subject. The subject table
#' contains subject IDs, which are useful to request specific records in the
#' dataset table using [bps_dataset()].
#'
#' * `bps_subject_category()` requests the subject category table
#' * `bps_subject()` requests the subject table
#'
#' @inheritParams bps_list
#'
#' @returns
#' * `bps_subject_category()` returns a tibble containing subject category IDs
#'   and titles.
#' * `bps_subject()` returns a tibble containing subject IDs and titles, as well
#'   as subject categories.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Look up the subject category
#' bps_subject_category(lang = "eng")
#'
#' # Look up the subjects in the economic and trade category
#' bps_subject(subject_category_id = "2", lang = "eng")
#'
#' @export
bps_subject_category <- function(domain_id = "0000",
                                 page = NULL,
                                 lang = c("ind", "eng")) {
  table <- bps_list(
    "subcat",
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}


#' @rdname bps_subject_category
#'
#' @param subject_category_id The subject category ID. Use
#'   [bps_subject_category()] to see the list of subject category IDs.
#'
#' @export
bps_subject <- function(subject_category_id = NULL,
                        domain_id = "0000",
                        page = NULL,
                        lang = c("ind", "eng")) {
  if (!is_null(subject_category_id)) {
    check_subject_category_id(subject_category_id)
  }

  table <- bps_list(
    "subject",
    subcat = subject_category_id,
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}


# Helper ------------------------------------------------------------------

check_subject_category_id <- function(x,
                                      arg = caller_arg(x),
                                      call = caller_env()) {
  database <- get_database("subject_category")

  check_id(
    x,
    database$subject_category_id,
    "subject category",
    length_one = TRUE,
    arg = arg,
    call = call
  )
}
