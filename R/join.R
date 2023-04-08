#' @export
inner_join.bpsr_tbl <- function(x,
                                y,
                                by = NULL,
                                copy = FALSE,
                                suffix = c(".x", ".y"),
                                ...,
                                keep = FALSE,
                                na_matches = c("na", "never")) {
  x <- unclass_bpsr_tbl(x)
  y <- unclass_bpsr_tbl(y)
  NextMethod()
}


#' @export
left_join.bpsr_tbl <- function(x,
                               y,
                               by = NULL,
                               copy = FALSE,
                               suffix = c(".x", ".y"),
                               ...,
                               keep = FALSE,
                               na_matches = c("na", "never")) {
  x <- unclass_bpsr_tbl(x)
  y <- unclass_bpsr_tbl(y)
  NextMethod()
}


#' @export
right_join.bpsr_tbl <- function(x,
                                y,
                                by = NULL,
                                copy = FALSE,
                                suffix = c(".x", ".y"),
                                ...,
                                keep = FALSE,
                                na_matches = c("na", "never")) {
  x <- unclass_bpsr_tbl(x)
  y <- unclass_bpsr_tbl(y)
  NextMethod()
}


#' @export
full_join.bpsr_tbl <- function(x,
                               y,
                               by = NULL,
                               copy = FALSE,
                               suffix = c(".x", ".y"),
                               ...,
                               keep = FALSE,
                               na_matches = c("na", "never")) {
  x <- unclass_bpsr_tbl(x)
  y <- unclass_bpsr_tbl(y)
  NextMethod()
}


# Helper ------------------------------------------------------------------

unclass_bpsr_tbl <- function(data) {
  attr(data, "metadata") <- NULL
  as_tibble(data)
}
