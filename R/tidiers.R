#' Tidy a G-means Clustering Object
#'
#' @description
#' Summarize a G-means clustering model as data frames, following the conventions of
#' the broom package. The generics come from the generics package, so load it or broom
#' to call the methods.
#'
#' @param x (`gmeans()`)\cr
#'   An object of class `"gmeans"`.
#' @param col.names (`character()`)\cr
#'   Column names for the centers. Defaults to the column names of the centers, or
#'   `x1`, `x2`, ... if they are unnamed.
#' @param data (`matrix()`)\cr
#'   The data used to fit the model, a numeric matrix or a data frame.
#' @param ... (`any`)\cr
#'   Additional arguments. Currently unused.
#' @returns
#' A `data.frame()`:
#' * `tidy()`: one row per cluster with the centers, `size`, `withinss`, and `cluster`.
#' * `augment()`: `data` with a `.cluster` factor giving the cluster assignments.
#' * `glance()`: one row with `k`, `k_init`, `k_max`, `level`, `totss`, `tot.withinss`,
#'   `betweenss`, and `iter`.
#' @seealso [gmeans()]
#' @name gmeans_tidiers
#' @examplesIf requireNamespace("generics", quietly = TRUE)
#' library(generics)
#' set.seed(123)
#' x <- as.matrix(iris[, -5])
#' cl <- gmeans(x)
#'
#' tidy(cl)
#' glance(cl)
#' head(augment(cl, x))
NULL

#' @rdname gmeans_tidiers
#' @exportS3Method generics::tidy
tidy.gmeans <- function(x, col.names = colnames(x$centers), ...) {
  centers <- x$centers
  if (is.null(col.names)) {
    col.names <- paste0("x", seq_len(ncol(centers)))
  }
  stopifnot(is.character(col.names), length(col.names) == ncol(centers))
  tab <- as.data.frame(centers)
  names(tab) <- col.names
  tab$size <- x$size
  tab$withinss <- x$withinss
  tab$cluster <- factor(seq_len(nrow(tab)))
  row.names(tab) <- NULL
  tab
}

#' @rdname gmeans_tidiers
#' @exportS3Method generics::augment
augment.gmeans <- function(x, data, ...) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("`data` must be a matrix or data frame", call. = FALSE)
  }
  if (nrow(data) != length(x$cluster)) {
    stop("`data` must have one row per observation used to fit the model", call. = FALSE)
  }
  if (is.matrix(data) && is.null(colnames(data))) {
    colnames(data) <- paste0("X", seq_len(ncol(data)))
  }
  tab <- as.data.frame(data)
  tab$.cluster <- factor(x$cluster)
  tab
}

#' @rdname gmeans_tidiers
#' @exportS3Method generics::glance
glance.gmeans <- function(x, ...) {
  data.frame(
    k = nrow(x$centers),
    k_init = x$k_init,
    k_max = x$k_max,
    level = x$level,
    totss = x$totss,
    tot.withinss = x$tot.withinss,
    betweenss = x$betweenss,
    iter = x$iter,
    check.names = FALSE
  )
}
