test_that("tidy returns one row per cluster", {
  skip_if_not_installed("generics")
  withr::local_seed(123L)
  x <- as.matrix(iris[, -5L])
  cl <- gmeans(x)
  res <- generics::tidy(cl)
  expect_s3_class(res, "data.frame")
  expect_named(res, c(colnames(x), "size", "withinss", "cluster"))
  expect_identical(nrow(res), nrow(cl$centers))
  expect_identical(res$size, cl$size)
  expect_identical(res$withinss, cl$withinss)
  expect_identical(res$cluster, factor(seq_len(nrow(cl$centers))))
  expect_identical(unname(as.matrix(res[, colnames(x)])), unname(cl$centers))
})

test_that("tidy names unnamed centers and accepts col.names", {
  skip_if_not_installed("generics")
  withr::local_seed(123L)
  x <- unname(as.matrix(iris[, -5L]))
  cl <- gmeans(x)
  expect_named(generics::tidy(cl), c("x1", "x2", "x3", "x4", "size", "withinss", "cluster"))
  expect_named(
    generics::tidy(cl, col.names = letters[1:4]),
    c("a", "b", "c", "d", "size", "withinss", "cluster")
  )
  expect_error(
    generics::tidy(cl, col.names = "a"),
    "length(col.names) == ncol(centers)",
    fixed = TRUE
  )
})

test_that("augment adds cluster assignments", {
  skip_if_not_installed("generics")
  withr::local_seed(123L)
  x <- as.matrix(iris[, -5L])
  cl <- gmeans(x)
  res <- generics::augment(cl, x)
  expect_s3_class(res, "data.frame")
  expect_named(res, c(colnames(x), ".cluster"))
  expect_identical(res$.cluster, factor(cl$cluster))
  expect_identical(generics::augment(cl, iris[, -5L])$.cluster, factor(cl$cluster))
  expect_named(generics::augment(cl, unname(x)), c("X1", "X2", "X3", "X4", ".cluster"))
  expect_error(generics::augment(cl, x[1:10, ]), "one row per observation", fixed = TRUE)
  expect_error(generics::augment(cl, as.vector(x)), "matrix or data frame", fixed = TRUE)
})

test_that("glance returns a single row summary", {
  skip_if_not_installed("generics")
  withr::local_seed(123L)
  cl <- gmeans(as.matrix(iris[, -5L]), k_init = 1L, k_max = 5L, level = 0.01)
  res <- generics::glance(cl)
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 1L)
  expect_named(
    res,
    c("k", "k_init", "k_max", "level", "totss", "tot.withinss", "betweenss", "iter")
  )
  expect_identical(res$k, nrow(cl$centers))
  expect_identical(res$k_init, 1L)
  expect_identical(res$k_max, 5L)
  expect_identical(res$level, 0.01)
  expect_identical(res$totss, cl$totss)
})

test_that("methods dispatch when broom is loaded", {
  skip_if_not_installed("broom")
  skip_if_not_installed("generics")
  withr::local_seed(123L)
  x <- as.matrix(iris[, -5L])
  cl <- gmeans(x)
  expect_identical(broom::tidy(cl), generics::tidy(cl))
  expect_identical(broom::glance(cl), generics::glance(cl))
  expect_identical(broom::augment(cl, x), generics::augment(cl, x))
})
