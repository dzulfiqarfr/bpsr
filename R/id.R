check_by_id_list <- function(x,
                             id_list,
                             description,
                             domain_id = NULL,
                             arg = caller_arg(x),
                             call = caller_env()) {
  # If `id_list` is `NULL` due to the absence of ID list, exit to prevent
  # false errors
  if (is_null(id_list)) {
    return()
  }

  if (is_valid(x, id_list)) {
    return()
  }

  if (length(x) > 1) x <- subset_invalid(x, id_list)

  abort_invalid_id(
    x,
    description,
    domain_id = domain_id,
    arg = arg,
    call = call
  )
}


check_domain_id <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_character(x, arg = arg, call = call)
  check_length_one(x, arg = arg, call = call)

  check_by_id_list(
    x,
    domain_lookup,
    "domain",
    arg = arg,
    call = call
  )
}


check_dataset_id <- function(x,
                             domain_id,
                             length_one = TRUE,
                             arg = caller_arg(x),
                             call = caller_env()) {
  check_character(x, arg = arg, call = call)

  if (length_one) {
    check_length_one(x, arg = arg, call = call)
  }

  check_by_id_list(
    x,
    dataset_id_list[[domain_id]],
    "dataset",
    domain_id = domain_id,
    arg = arg,
    call = call
  )
}


check_spreadsheet_dataset_id <- function(x,
                                         domain_id,
                                         length_one = TRUE,
                                         arg = caller_arg(x),
                                         call = caller_env()) {
  check_character(x, arg = arg, call = call)

  if (length_one) {
    check_length_one(x, arg = arg, call = call)
  }

  check_by_id_list(
    x,
    spreadsheet_id_list[[domain_id]],
    "spreadsheet dataset",
    domain_id = domain_id,
    arg = arg,
    call = call
  )
}


# Basic check for IDs of dataset variables, etc
check_id <- function(x,
                     length_one = TRUE,
                     arg = caller_arg(x),
                     call = caller_env()) {
  check_character(x, arg = arg, call = call)

  if (length_one) {
    check_length_one(x, arg = arg, call = call)
  }

  check_digit(x, arg = arg, call = call)
}


check_year_period_id <- function(x,
                                 arg = caller_arg(x),
                                 call = caller_env()) {
  check_character(x, arg = arg, call = call)

  if (is_range(x)) x <- unlist(str_split(x, ":"))

  check_digit(x, arg = arg, call = call)
}


# Helper ------------------------------------------------------------------

is_valid <- function(x, id_list) {
  if (length(x) == 1) {
    return(any(x == id_list))
  }

  all(x %in% id_list)
}


subset_invalid <- function(x, id_list) {
  x[!(x %in% id_list)]
}


abort_invalid_id <- function(x,
                             description,
                             domain_id = NULL,
                             arg = caller_arg(x),
                             call = caller_env()) {
  article <- if (str_detect(description, "^[AaIiUuEeOo]")) "an" else "a"

  if (!is_null(domain_id)) {
    domain_lookup_inverted <- domain_lookup |>
      tibble::enframe() |>
      dplyr::select(value, name) |>
      tibble::deframe()

    domain_name <- domain_lookup_inverted[[domain_id]]

    statement <- c(
      str_c(
        "{.arg {arg}} must be an ID of {article} {description} ",
        "that exists in the given domain."
      ),
      "i" = "Domain: {domain_name}."
    )
  } else {
    statement <- "{.arg {arg}} must be {article} {description} ID."
  }

  cli_abort(
    c(
      statement,
      "x" = "{?This/These} ID{?s} {?doesn't/don't} exist: {.val {x}}."
    ),
    call = call
  )
}


is_range <- function(x) {
  length(x) == 1 && str_detect(x, ":")
}


check_digit <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (purrr::none(x, \(x) str_detect(x, "\\D+"))) {
    return()
  }

  cli_abort("{.arg {arg}} must contain only digits.", call = call)
}
