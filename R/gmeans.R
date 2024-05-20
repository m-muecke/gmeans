#' G-means Clustering
#'
#' Perform G-means clustering on a data matrix.
#'
#' @param x numeric matrix of data, or an object that can be coerced to such a matrix
#'   (such as a numeric vector or a data frame with all numeric columns).
#' @references
#' `r format_bib("hamerly2003learning")`
#' @export
gmeans <- function(x) {
  structure(list(cluster = NULL, centers = NULL), class = "gmeans")
}

# steps: can initialize with just k = 1, or larger if we have some prioer knowledge
# 1. start with a small number of k-means clusters and grows the no. of clusters
# 2. each iteration splits into two those clusters those centers whose data appear not to come from a Gaussian distribution
# 3. beween each round of splitting run k-means on the entire dataset and all the centers to refine the current solution

#' @export
predict.gmeans <- function(object, newdata, ...) {
  stop("predict.gmeans is not yet implemented")
}

#' Anderson-Darling Normality Test
#'
#' Perform the Anderson-Darling normality test.
#'
#' @param x `numeric()` vector of data values. Missing values are allowed, but the
#'   number of non-missing values must be greater than 7.
#'
#' @seealso [stats::shapiro.test()] and [stats::ks.test()] for other normality tests.
#' @returns
#' A list inheriting from classes `"htest"` containing the following components:
#'   * statistic: the value of the statistic.
#'   * p.value: the p-value of the test.
#'   * method: the character string `"Anderson-Darling normality test"`.
#'   * data.name: a character string giving the name(s) of the data.
#' @export
ad.test <- function(x) {
  dname <- deparse(substitute(x))
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 8L) {
    stop("sample size must be greater than 7")
  }

  scaled <- (x - mean(x)) / sd(x)
  logp1 <- pnorm(scaled, log.p = TRUE)
  logp2 <- pnorm(-scaled, log.p = TRUE)
  h <- (2 * 1:n - 1) * (logp1 + rev(logp2))

  A <- -n - mean(h)
  # D'Agostino (1986) adjustment
  AA <- (1 + 0.75 / n + 2.25 / n^2) * A
  # Stephens (1974) adjustment
  # AA <- (1 + 4 / n - 25 / n^2) * A
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