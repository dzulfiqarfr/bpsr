#' Look up dataset variable
#'
#' @description
#' Get tables of dataset variables. Each table contains the variable IDs, which
#' are useful to request specific observations in a dataset using
#' [bps_get_dataset()].
#'
#' * `bps_vertical_var()` requests the vertical variable table
#' * `bps_derived_var()` requests the derived variable table
#' * `bps_year()` requests the year table
#' * `bps_period()` requests the period table
#'
#' @inheritParams bps_dataset
#' @param dataset_id The dataset ID. Use [bps_dataset()] to see the list of
#'   dataset IDs.
#'
#' @returns
#' * `bps_vertical_var()` returns a tibble containing vertical variable IDs,
#'   names and groups.
#' * `bps_derived_var()` returns a tibble containing derived variable IDs,
#'   names and groups.
#' * `bps_year()` returns a tibble containing year IDs and years.
#' * `bps_period()` returns a tibble containing period IDs and periods.
#'
#' @examplesIf httr2::secret_has_key("BPSR_KEY")
#' # Look up the vertical variables in the gross domestic product dataset
#' bps_vertical_var(dataset_id = "1955", lang = "eng")
#'
#' @export
bps_vertical_var <- function(dataset_id = NULL,
                             domain_id = "0000",
                             page = NULL,
                             lang = c("ind", "eng")) {
  if (!is_null(dataset_id)) {
    check_dataset_id(dataset_id, domain_id)
  }

  table <- bps_list(
    "vervar",
    var = dataset_id,
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}


#' @rdname bps_vertical_var
#'
#' @param derived_var_group_id The derived variable group ID. Use
#'   [bps_derived_var()] to see the list of derived variable group IDs.
#'
#' @export
bps_derived_var <- function(dataset_id = NULL,
                            derived_var_group_id = NULL,
                            domain_id = "0000",
                            page = NULL,
                            lang = c("ind", "eng")) {
  if (!is_null(dataset_id)) {
    check_dataset_id(dataset_id, domain_id)
  }

  if (!is_null(derived_var_group_id)) {
    check_id(derived_var_group_id)
  }

  table <- bps_list(
    "turvar",
    var = dataset_id,
    group = derived_var_group_id,
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}


#' @rdname bps_vertical_var
#' @export
bps_year <- function(dataset_id = NULL,
                     domain_id = "0000",
                     page = NULL,
                     lang = c("ind", "eng")) {
  if (!is_null(dataset_id)) {
    check_dataset_id(dataset_id, domain_id)
  }

  table <- bps_list(
    "th",
    var = dataset_id,
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}


#' @rdname bps_vertical_var
#' @export
bps_period <- function(dataset_id = NULL,
                       domain_id = "0000",
                       page = NULL,
                       lang = c("ind", "eng")) {
  if (!is_null(dataset_id)) {
    check_dataset_id(dataset_id, domain_id)
  }

  table <- bps_list(
    "turth",
    var = dataset_id,
    domain_id = domain_id,
    page = page,
    lang = lang
  )

  parse_table(table)
}
