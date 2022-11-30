check_logical <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!rlang::is_logical(x)) {
    cli_abort("{.arg {arg}} must be a logical, not {.type {x}}.", call = call)
  }
}


check_string <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!rlang::is_string(x)) {
    cli_abort("{.arg {arg}} must be a string, not {.type {x}}.", call = call)
  }
}


check_character <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_character(x)) {
    cli_abort("{.arg {arg}} must be a character, not {.type {x}}.", call = call)
  }
}


check_number <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!rlang::is_double(x) && !is_integer(x)) {
    cli_abort("{.arg {arg}} must be a number.", call = call)
  }
}


check_list <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!rlang::is_list(x)) {
    cli_abort("{.arg {arg}} must be a list.", call = call)
  }
}


check_length_one <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (length(x) != 1) {
    cli_abort("{.arg {arg}} must be a length-one vector.", call = call)
  }
}


is_valid <- function(x, database) {
  if (length(x) == 1) {
    return(any(x == database))
  }

  all(x %in% database)
}


subset_invalid <- function(x, database) {
  x[!(x %in% database)]
}


abort_invalid_id <- function(x,
                             param,
                             arg = caller_arg(x),
                             call = caller_env()) {
  article <- if (str_detect(param, "^[AaIiUuEeOo]")) "an" else "a"

  cli_abort(
    c(
      "{.arg {arg}} must be {article} {param} ID.",
      "x" = "{?This/These} ID{?s} {?doesn't/don't} exist: {.val {x}}."
    ),
    call = call
  )
}


check_id <- function(x,
                     database,
                     param,
                     length_one = FALSE,
                     arg = caller_arg(x),
                     call = caller_env()) {
  check_character(x, arg = arg, call = call)

  if (length_one) {
    check_length_one(x, arg = arg, call = call)
  }

  if (!is_valid(x, database)) {
    if (!length_one) x <- subset_invalid(x, database)

    abort_invalid_id(x, param, arg = arg, call = call)
  }
}


# Concatenate multiple values into a string, mostly separated by semi-colons,
# so they follow the BPS API syntax
concat <- function(x, collapse = ";") {
  str_c(x, collapse = collapse)
}
