test_that("gmeans works", {
  withr::local_seed(1234L)
  # x needs to be a matrix
  expect_error(gmeans(NULL))
  expect_error(gmeans(letters))
  expect_error(gmeans(numeric()))
  expect_error(gmeans(rnorm(5L)))
  # k_init and k_max need to be integers
  x <- matrix(rnorm(100L, sd = 0.3), ncol = 2L)
  colnames(x) <- c("x", "y")
  expect_error(gmeans(x, k_init = NA_integer_))
  expect_error(gmeans(x, k_init = 1:10))
  expect_error(gmeans(x, k_init = 1.5))
  expect_error(gmeans(x, k_max = NA_integer_))
  expect_error(gmeans(x, k_max = 1:10))
  expect_error(gmeans(x, k_max = 1.5))
  # level needs to be a number between 0 and 1
  expect_error(gmeans(x, level = NULL))
  expect_error(gmeans(x, level = NA_real_))
  expect_error(gmeans(x, level = 0))
  expect_error(gmeans(x, level = 1))
  expect_error(gmeans(x, level = c(0.5, 0.7)))
})

test_that("gmeans works with a single column", {
  withr::local_seed(1234L)
  x <- matrix(c(rnorm(60L, sd = 0.3), rnorm(60L, mean = 8, sd = 0.3)), ncol = 1L)
  colnames(x) <- "v"
  cl <- withr::with_seed(1L, gmeans(x))
  expect_s3_class(cl, "gmeans")
  expect_identical(ncol(cl$centers), 1L)
  expect_gt(nrow(cl$centers), 1L)
  expect_identical(colnames(cl$centers), "v")
  expect_identical(cl$centers, withr::with_seed(1L, gmeans(as.data.frame(x)))$centers)
})

test_that("gmeans works with a single initial center", {
  withr::local_seed(1234L)
  x <- rbind(
    matrix(rnorm(100L, sd = 0.3), ncol = 2L),
    matrix(rnorm(100L, mean = 3, sd = 0.3), ncol = 2L)
  )
  expect_gt(nrow(gmeans(x, k_init = 1L)$centers), 1L)
  expect_gt(nrow(gmeans(x[, 1L, drop = FALSE], k_init = 1L)$centers), 1L)
})

test_that("kmeans_plusplus works", {
  withr::local_seed(1234L)
  x <- matrix(rnorm(100L, sd = 0.3), ncol = 2L)
  for (i in 1:5) {
    res <- kmeans_plusplus(x, i)
    expect_identical(dim(res), c(i, 2L))
    expect_true(is.matrix(res))
  }

  # check distances are non-negative
  centroids <- matrix(NA_real_, nrow = 2, ncol = ncol(x))
  centroids[1L, ] <- x[sample.int(nrow(x), 1L), ]
  dists <- apply(x, 1L, \(xi) min(colSums((t(centroids[1L, ]) - xi)^2)))
  expect_true(all(dists >= 0))
  # probabilities sum to 1
  prob <- dists / sum(dists)
  expect_identical(sum(prob), 1)
})

test_that("predict works", {
  withr::local_seed(1234L)
  x <- matrix(rnorm(100L, sd = 0.3), ncol = 2L)
  colnames(x) <- c("x", "y")
  cl <- gmeans(x)
  # should return an integer vector
  expect_vector(predict(cl, x), ptype = integer())
  # should raise an error with no new data provided
  expect_error(predict(cl))
  # newdata should work with a single row
  newdata <- matrix(rnorm(2L), ncol = 2L, dimnames = list(NULL, c("x", "y")))
  expect_no_error(predict(cl, newdata))
  # error when required cols are missing
  expect_error(predict(cl, x[, "x", drop = FALSE]))
  # allow more than required cols
  newdata <- cbind(x, z = 1:50)
  expect_no_error(predict(cl, newdata))
})

test_that("ad.test works", {
  withr::local_seed(1234L)
  x <- rnorm(100L, mean = 5, sd = 3)
  res <- ad.test(x)
  expect_s3_class(res, "htest")
  expect_named(res, c("statistic", "p.value", "method", "data.name"))
  expect_vector(res$statistic, ptype = numeric(), size = 1L)
  expect_vector(res$p.value, ptype = numeric(), size = 1L)
  expect_gte(res$p.value, 0)
  expect_gte(res$p.value, 0)
  expect_lte(res$p.value, 1)
  expect_identical(res$method, "Anderson-Darling normality test")
  expect_identical(res$data.name, "x")
  expect_snapshot(res)
  # input validation
  expect_error(ad.test(NULL))
  expect_error(ad.test(letters))
  expect_error(ad.test(numeric()))
  expect_error(ad.test(rnorm(7L)))
})

test_that("compute_wss works", {
  km <- kmeans(mtcars, 5)
  expect_equal(compute_wss(km), compute_wss(km, mtcars)) # nolint
})
