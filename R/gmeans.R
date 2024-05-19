#' G-means Clustering
#'
#' @param x numeric matrix of data, or an object that can be coerced to such a matrix
#'   (such as a numeric vector or a data frame with all numeric columns).
#' @references
#' `r format_bib("hamerly2003learning")`
#' @export
gmeans <- function(x) {
  structure(list(), class = "gmeans")
}

#' @export
predict.gmeans <- function(object, newdata, ...) {
  stop("predict.gmeans is not yet implemented")
}
