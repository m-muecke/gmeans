test_that("gmeans works", {
  # x needs to be a matrix
  expect_error(gmeans(NULL))
  expect_error(gmeans(letters))
  expect_error(gmeans(numeric()))
  expect_error(gmeans(rnorm(5L)))
  # k_init and k_max need to be integers
  x <- matrix(rnorm(100, sd = 0.3), ncol = 2)
  colnames(x) <- c("x", "y")
  expect_error(gmeans(x, k_init = NA_integer_))
  expect_error(gmeans(x, k_init = 1:10))
  expect_error(gmeans(x, k_init = 1.5))
  expect_error(gmeans(x, k_max = NA_integer_))
  expect_error(gmeans(x, k_max = 1:10))
  expect_error(gmeans(x, k_max = 1.5))
  # alpha needs to be a number between 0 and 1
  expect_error(gmeans(x, alpha = NULL))
  expect_error(gmeans(x, alpha = NA_real_))
  expect_error(gmeans(x, alpha = 0))
  expect_error(gmeans(x, alpha = 1))
  expect_error(gmeans(x, alpha = c(0.5, 0.7)))
})

test_that("ad.test works", {
  expect_error(ad.test(NULL))
  expect_error(ad.test(letters))
  expect_error(ad.test(numeric()))
  expect_error(ad.test(rnorm(5L)))
})
