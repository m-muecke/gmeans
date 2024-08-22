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
