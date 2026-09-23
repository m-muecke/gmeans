#' Summarize a G-means Clustering Object
#'
#' @description
#' Summarize the result of a G-means clustering: the number of clusters found, the settings used to
#' fit the model, a per-cluster table of sizes, within-cluster sums of squares and centers, and the
#' overall sums of squares.
#'
#' @param object (`gmeans()`)\cr
#'   An object of class `"gmeans"`.
#' @param x (`summary.gmeans()`)\cr
#'   An object of class `"summary.gmeans"`.
#' @param digits (`integer(1)`)\cr
#'   Number of significant digits to print.
#' @param ... (`any`)\cr
#'   Additional arguments. Currently unused.
#' @returns
#' `summary()` returns an object of class `"summary.gmeans"`, a list with components:
#' * `k`, `k_init`, `k_max`, `level`: the number of clusters found and the settings used to fit the
#'   model.
#' * `clusters`: a `data.frame()` with one row per cluster and the columns `cluster`, `size`,
#'   `withinss`, followed by the cluster centers.
#' * `totss`, `tot.withinss`, `betweenss`, `iter`: as in [stats::kmeans()].
#'
#' `print()` returns `x` invisibly.
#' @seealso [gmeans()]
#' @export
#' @examples
#' set.seed(123)
#' x <- as.matrix(iris[, -5])
#' cl <- gmeans(x)
#' summary(cl)
summary.gmeans <- function(object, ...) {
  centers <- object$centers
  k <- nrow(centers)
  if (is.null(colnames(centers))) {
    colnames(centers) <- paste0("x", seq_len(ncol(centers)))
  }
  clusters <- data.frame(
    cluster = factor(seq_len(k)),
    size = object$size,
    withinss = object$withinss,
    centers,
    check.names = FALSE
  )
  row.names(clusters) <- NULL
  structure(
    list(
      k = k,
      k_init = object$k_init,
      k_max = object$k_max,
      level = object$level,
      clusters = clusters,
      totss = object$totss,
      tot.withinss = object$tot.withinss,
      betweenss = object$betweenss,
      iter = object$iter
    ),
    class = "summary.gmeans"
  )
}

#' @rdname summary.gmeans
#' @export
print.summary.gmeans <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat(sprintf(
    "G-means clustering with %d cluster%s (k_init = %d, k_max = %d, level = %s)\n\n",
    x$k,
    if (x$k == 1L) "" else "s",
    x$k_init,
    x$k_max,
    format(x$level, digits = digits)
  ))
  print(x$clusters, digits = digits, row.names = FALSE)
  ratio <- if (x$totss > 0) {
    sprintf(" (%s%% of total)", format(100 * x$betweenss / x$totss, digits = digits))
  } else {
    ""
  }
  cat(sprintf(
    "\nTotal SS: %s, within SS: %s, between SS: %s%s\n",
    format(x$totss, digits = digits),
    format(x$tot.withinss, digits = digits),
    format(x$betweenss, digits = digits),
    ratio
  ))
  invisible(x)
}
