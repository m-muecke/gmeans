test_that("ad.test works", {
  expect_error(ad.test(NULL))
  expect_error(ad.test(letters))
  expect_error(ad.test(numeric()))
  expect_error(ad.test(numeric(rnorm(5L))))
})
