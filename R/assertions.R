is_number <- function(x, null_ok = FALSE) {
  if (null_ok && is.null(x)) {
    return(TRUE)
  }
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

is_count <- function(x, null_ok = FALSE) {
  if (null_ok && is.null(x)) {
    return(TRUE)
  }
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == trunc(x) && x > 0
}
