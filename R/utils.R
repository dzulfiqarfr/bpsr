check_logical <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (rlang::is_logical(x)) {
    return()
  }

  cli_abort("{.arg {arg}} must be a logical, not {.type {x}}.", call = call)
}


check_character <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_character(x)) {
    return()
  }

  cli_abort("{.arg {arg}} must be a character, not {.type {x}}.", call = call)
}


check_number <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (rlang::is_double(x) || is_integer(x)) {
    return()
  }

  cli_abort("{.arg {arg}} must be a number, not {.type {x}}.", call = call)
}


check_list <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (rlang::is_list(x)) {
    return()
  }

  cli_abort("{.arg {arg}} must be a list.", call = call)
}


check_length_one <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (length(x) == 1) {
    return()
  }

  cli_abort("{.arg {arg}} must be a length-one vector.", call = call)
}

# Concatenate multiple values into a string, mostly separated by semi-colons,
# so they follow the BPS API syntax
concat <- function(x, collapse = ";") {
  str_c(x, collapse = collapse)
}
