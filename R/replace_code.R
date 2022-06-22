#' Re-code vector x based on codebook
#' @param x vector of values
#' @param translate data frame containing a code book. It is assumed that it has
#'   columns called `Variable`, `Value`, and `Label`
#' @param xname name of the variable of `x` used in the codebook. By default,
#'   the name of the variable is used for a search, regular expressions in xname
#'   can be used to resolve disambiguity.
#' @param as.factor binary variable - should the result be returned as factor?
#'   In that case, factor levels are ordered according to the original values in x.
#' @return vector of the same length as x with re-coded values
#' @importFrom stats reorder
#' @export
replace_code <- function(x, translate, xname=NULL, as.factor = TRUE) {
  call <- as.list(match.call())
  if (is.null(xname)) xname = call$x
  stopifnot(is.vector(x), is.data.frame(translate),
            "Variable" %in% names(translate),
            "Value" %in% names(translate),
            "Label" %in% names(translate))

  idx <- grep(pattern=xname, ignore.case = TRUE, translate$Variable)
  variables <- unique(translate$Variable[idx])
  if (length(variables) != 1)
    stop(paste0("Can't find the right variable for:", xname, ", listed as <", paste0(variables, collapse=",") ,">"))

  dx <- replace_vals(x, translate$Value[idx], translate$Label[idx])
  if (as.factor) {
    dx <- factor(dx)
    dx <- reorder(dx, x)
  }
  dx
}
