#' G-means Clustering
#'
#' Perform G-means clustering on a data matrix.
#'
#' @param x numeric matrix of data, or an object that can be coerced to such a matrix
#'   (such as a numeric vector or a data frame with all numeric columns).
#' @param k_init `integer(1)` initial amount of centers. Default is `1L`.
#' @param k_max `integer(1)` maximum amount of centers. Default is `Inf`.
#' @param alpha `numeric(1)` significance level for the Anderson-Darling test.
#'   Default is `0.05`.
#' @references
#' `r format_bib("hamerly2003learning")`
#' @export
gmeans <- function(x, k_init = 1L, k_max = Inf, alpha = 0.05) {
  stopifnot(
    is.matrix(x),
    is.numeric(k_init), length(k_init) == 1L, k_init >= 1L,
    is.numeric(k_max), length(k_max) == 1L, k_max >= 1L
  )
  km <- stats::kmeans(x, k_init)
  repeat {
    new_centers <- statistical_optimization(km, k_max, alpha)
    # no more centers added
    if (nrow(km$centers) == nrow(new_centers)) {
      break
    }
    km <- stats::kmeans(x, nrow(new_centers))
  }
  km
}

statistical_optimization <- function(km, k_max, alpha) {
  centers <- NULL
  k <- nrow(km$centers)
  for (i in seq_len(k)) {
    cluster <- which(km$cluster == i)
    new_centers <- split_and_search(data, cluster, alpha)
    if (is.null(new_centers) || k >= k_max) {
      centers <- rbind(centers, km$centers[i, ])
    } else {
      centers <- rbind(centers, new_centers)
      k <- k + 1L
    }
  }
  centers
}

split_and_search <- function(data, cluster, alpha) {
  if (length(cluster) == 1L) {
    return(NULL)
  }
  points <- data[cluster, ]
  km <- stats::kmeans(points, 2L)
  new_centers <- km$centers
  if (nrow(new_centers) > 1L && !is_null_hypothesis(points, new_centers)) {
    new_centers
  } else {
    NULL
  }
}

#' @details
#' \deqn{
#'   x_{i}^{*}=\frac{\left \langle x_{i}, v \right \rangle}{\left \| v \right \|^{2}}
#' }
is_null_hypothesis <- function(data, centers, alpha = 0.05) {
  v <- centers[1, ] - centers[2, ]
  points <- as.vector(data %*% v / sum(v^2L))
  ad.test(points)$p.value > alpha
}

#' @export
print.gmeans <- function(x, ...) {
  cat("G-means clustering with", nrow(x$centers), "centers\n")
  invisible(x)
}

#' Predict Method for G-means Clustering
#'
#' @param object of class inheriting from `"gmeans"`.
#' @param newdata `matrix()` new data to predict on.
#' @param ... additional arguments.
#' @source Adapted from \CRANpkg{clue}
#' @export
predict.gmeans <- function(object, newdata, ...) {
  stopifnot(is.matrix(newdata))
  d <- rxdist(newdata, object$centers)
  cl <- max.col(-d)
  cl
}

rxdist <- function(data,
                   centers,
                   method = c("euclidean", "manhattan", "minkowski"),
                   p = 2) {
  method <- match.arg(method)
  distance <- switch(method,
    euclidean = function(data, x) sqrt(rowSums(sweep(data, 2L, x)^2)),
    manhattan = function(data, x) rowSums(abs(sweep(data, 2L, x))),
    minkowski = function(data, x) (rowSums(abs(sweep(data, 2L, x))^p))^(1 / p)
  )
  data_nms <- colnames(data)
  center_nms <- colnames(centers)
  if (!is.null(data_nms) && !is.null(center_nms) && !identical(data_nms, center_nms)) {
    data <- data[, center_nms, drop = FALSE]
  }
  apply(centers, 1L, function(x) distance(data, x))
}

#' Anderson-Darling Normality Test
#'
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
#' ad.test(rnorm(100, mean = 5, sd = 3))
#' ad.test(runif(100, min = 2, max = 4))
ad.test <- function(x) {
  stopifnot(is.numeric(x), length(x) > 7L)
  dname <- deparse(substitute(x))
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
