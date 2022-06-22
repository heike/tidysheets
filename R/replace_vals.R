#' Replace values in vector
#'
#' The `replace_vals` function allows re-coding a set of values in vector `x`
#' (in most cases a vector of integers) by their corresponding `labels`.
#' @param x vector of values to be replaced
#' @param values vector of values in x to be replaced.
#' @param labels vector of the same length as `values`. It is assumed that values
#' are replaced 1-1, i.e. the ith value in values is replaced by the ith value in labels
#' @export
replace_vals <- function(x, values, labels) {
  stopifnot(length(values) == length(labels))
# classes of values and x should be compatible:
  stopifnot(any(values %in% unique(x)))

  for (i in 1:length(values)) {
    x <- replace(x, which(x == values[i]), labels[i])
  }
  x
}

