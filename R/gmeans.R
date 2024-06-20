#' G-means Clustering
#'
#' @description
#' Perform G-means clustering on a data matrix.
#'
#' @param x numeric matrix of data, or an object that can be coerced to such a matrix
#'   (such as a numeric vector or a data frame with all numeric columns).
#' @param k_init `integer(1)` initial amount of centers. Default is `1L`.
#' @param k_max `integer(1)` maximum amount of centers. Default is `Inf`.
#' @param level `numeric(1)` significance level for the Anderson-Darling test.
#'   Default is `0.05`. See [ad.test()] for more information.
#' @param ... additional arguments passed to [stats::kmeans()].
#' @references
#' `r format_bib("hamerly2003learning")`
#' @export
#' @examples
#' set.seed(123)
#' x <- rbind(
#'   matrix(rnorm(100, sd = 0.3), ncol = 2),
#'   matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
#' )
#' colnames(x) <- c("x", "y")
#' cl <- gmeans(x)
gmeans <- function(x, k_init = 1L, k_max = Inf, level = 0.05, ...) {
  if (inherits(x, "data.frame")) {
    x <- as.matrix(x)
  }
  stopifnot(
    is.matrix(x),
    is_count(k_init),
    is_count(k_max),
    is_number(level), level > 0, level < 1
  )
  km <- stats::kmeans(x, k_init, ...)
  repeat {
    new_centers <- suggest_centers(x, km, k_max, level, ...)
    # no more centers added
    if (nrow(km$centers) == nrow(new_centers)) {
      break
    }
    km <- stats::kmeans(x, nrow(new_centers))
  }
  class(km) <- c("gmeans", class(km))
  km
}

suggest_centers <- function(data, km, k_max, level, ...) {
  centers <- NULL
  k <- nrow(km$centers)
  for (i in seq_len(k)) {
    cluster <- which(km$cluster == i)
    new_centers <- split_and_search(data, cluster, level, ...)
    if (is.null(new_centers) || k >= k_max) {
      centers <- rbind(centers, km$centers[i, ])
    } else {
      centers <- rbind(centers, new_centers)
      k <- k + 1L
    }
  }
  centers
}

split_and_search <- function(data, cluster, level, ...) {
  if (length(cluster) == 1L) {
    return(NULL)
  }
  points <- data[cluster, ]
  km <- stats::kmeans(points, 2L, ...)
  new_centers <- km$centers
  if (nrow(new_centers) > 1L && !is_null_hypothesis(points, new_centers)) {
    new_centers
  } else {
    NULL
  }
}

#' Null Hypothesis Test
#'
#  @description
#  Simplifies the test for Gaussian fit by projecting the data to one dimension using
#  the following formula, as described in `r cite_bib("hamerly2003learning")`:
#' \deqn{
#'   x_{i}^{*}=\frac{\left \langle x_{i}, v \right \rangle}{\left \| v \right \|^{2}}
#' }
#' @noRd
is_null_hypothesis <- function(data, centers, level = 0.05) {
  v <- centers[1L, ] - centers[2L, ]
  points <- as.vector(data %*% v / sum(v^2))
  ad.test(points)$p.value > level
}

#' Predict Method for G-means Clustering
#'
#' Predicted values based on the G-means clustering model.
#'
#' @param object of class inheriting from `"gmeans"`.
#' @param newdata `matrix()` new data to predict on.
#' @param method `character(1)` distance metric to use.
#'   Either `"euclidean"`, `"manhattan"`, or `"minkowski"`. Default is `"euclidean"`.
#' @param p `numeric(1)` power of the Minkowski distance. Default is `2`.
#' @param ... additional arguments.
#' @source Adapted from \CRANpkg{clue}
#' @export
#' @examples
#' set.seed(123)
#' x <- as.matrix(iris[, -5])
#' cl <- gmeans(x)
#'
#' newdata <- x[1:10, ]
#' predict(cl, newdata)
predict.gmeans <- function(object,
                           newdata,
                           method = c("euclidean", "manhatten", "minkowski"),
                           p = 2,
                           ...) {
  stopifnot(is_number(p))
  method <- match.arg(method)
  newdata <- as.matrix(newdata)
  centers <- object$centers
  if (!all(colnames(centers) %in% colnames(newdata))) {
    stop("`newdata` must have the same columns as the centers", call. = FALSE)
  }

  distance <- switch(method,
    euclidean = function(data, x) rowSums(sweep(data, 2L, x)^2),
    manhattan = function(data, x) rowSums(abs(sweep(data, 2L, x))),
    minkowski = function(data, x) (rowSums(abs(sweep(data, 2L, x))^p))^(1 / p)
  )

  data_nms <- colnames(newdata)
  center_nms <- colnames(centers)
  if (!is.null(data_nms) && !is.null(center_nms) && !identical(data_nms, center_nms)) {
    newdata <- newdata[, center_nms, drop = FALSE]
  }
  d <- apply(centers, 1L, function(x) distance(newdata, x))
  cl <- max.col(-d)
  cl
}

#' Anderson-Darling Normality Test
#'
#' @description
#' Perform the Anderson-Darling normality test.
#'
#' @details
#' The Anderson-Darling test is an EDF omnibus test for the composite hypothesis of
#' normality. The test statistic is \deqn{
#'   A^2 = -n -\frac{1}{n} \sum_{i=1}^{n} (2i - 1) [\ln(z_{i}) + \ln(1 - z_{n + 1 - i})]
#' }
#' where \eqn{z_{i} = \Phi(\frac{x_{i} - \bar{x}}{s})}. Here,
#' \eqn{\Phi} is the cumulative distribution function of the standard normal
#' distribution, and \eqn{\bar{x}} and \eqn{s} are mean and standard deviation of
#' the data values. The p-value is computed from the modified statistic
#' \eqn{A^2_*=A^2 (1.0 + 0.75/n + 2.25/n^{2})} according to Table 4.9 in
#' Stephens (1986).
#'
#' @param x `numeric()` vector of data values. Missing values are allowed, but the
#'   number of non-missing values must be greater than 7.
#'
#' @returns
#' A list inheriting from classes `"htest"` containing the following components:
#'   * statistic: the value of the statistic.
#'   * p.value: the p-value of the test.
#'   * method: the character string `"Anderson-Darling normality test"`.
#'   * data.name: a character string giving the name(s) of the data.
#' @seealso [stats::shapiro.test()] for performing the Shapiro-Wilk test for normality.
#'   [nortest::cvm.test()], [nortest::lillie.test()], [nortest::pearson.test()],
#'   [nortest::sf.test()] for performing further tests for normality.
#'   [stats::qqnorm()] for producing a normal quantile-quantile plot.
#' @source Adapted from [nortest::ad.test()]
#' @references
#' `r format_bib("d2017goodness", "thode2002testing")`
#' @export
#' @examples
#' set.seed(123)
#' ad.test(rnorm(100, mean = 5, sd = 3))
#' ad.test(runif(100, min = 2, max = 4))
ad.test <- function(x) {
  stopifnot(is.numeric(x), length(x) > 7L)
  dname <- deparse1(substitute(x))
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 8L) {
    stop("sample size must be greater than 7", call. = FALSE)
  }

  scaled <- (x - mean(x)) / stats::sd(x)
  logp1 <- stats::pnorm(scaled, log.p = TRUE)
  logp2 <- stats::pnorm(-scaled, log.p = TRUE)
  h <- (2 * 1:n - 1) * (logp1 + rev(logp2))

  A <- -n - mean(h)
  AA <- (1 + 0.75 / n + 2.25 / n^2) * A
  if (AA < 0.2) {
    pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  } else if (AA < 0.34) {
    pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  } else if (AA < 0.6) {
    pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  } else if (AA < 10) {
    pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
  } else {
    pval <- 3.7e-24
  }
  structure(
    list(
      statistic = c(A = A),
      p.value = pval,
      method = "Anderson-Darling normality test",
      data.name = dname
    ),
    class = "htest"
  )
}
