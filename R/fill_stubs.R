#' Fill Stubs
#'
#' Variable x is assumed to consist of stubs (assuming the data stems from converting a table with stubs, such as pivot tables in Excel).
#' @param x variable - missing values will be filled with the last non-NA value
#' @return vector of the same type as x with missing values filled in
#' @export
fill_stubs <- function(x) {
  n <- length(x)
  idx <- which(!is.na(x))
  diff_idx <- c(diff(idx), n-max(idx)+1)

  rep(x[idx], diff_idx)
}

