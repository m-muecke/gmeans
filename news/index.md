# Changelog

## gmeans (development version)

- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now splits a cluster by starting the two new centers along its main
  principal component, as described in Hamerly and Elkan (2003), instead
  of at random points. The split step no longer depends on the random
  seed and the number of clusters is found more reliably, so results may
  differ from earlier versions.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now uses `k_init = 1` by default, as in Hamerly and Elkan (2003). The
  previous default of `2` could never return a single cluster. With one
  initial center the result no longer depends on the random seed.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now uses `level = 0.0001` by default, the significance level used by
  Hamerly and Elkan (2003). The previous default of `0.05` often split
  clusters that are Gaussian, especially in higher dimensions.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now stores `k_init`, `k_max`, and `level` in the returned object.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now errors clearly when `centers` is passed, pointing to `k_init`
  instead.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now errors clearly when `k_init` is not less than the number of rows
  in `x`.
- New [`summary()`](https://rdrr.io/r/base/summary.html) method for
  `gmeans` objects.
- New [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`augment()`](https://generics.r-lib.org/reference/augment.html), and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) methods
  for `gmeans` objects.

## gmeans 0.2.0

CRAN release: 2026-09-11

- Removed the mlr3 integration vignette since the learner now ships in
  mlr3cluster as `lrn("clust.gmeans")`.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now errors when `k_init` exceeds `k_max`.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now requires finite numeric input (logical is coerced to 0/1) and
  errors clearly when there are fewer distinct points than centers.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now errors clearly on input with zero rows or zero columns.
- [`predict.gmeans()`](https://m-muecke.github.io/gmeans/reference/predict.gmeans.md)
  now errors when the Minkowski power `p` is not positive.
- [`predict.gmeans()`](https://m-muecke.github.io/gmeans/reference/predict.gmeans.md)
  and
  [`compute_wss()`](https://m-muecke.github.io/gmeans/reference/compute_wss.md)
  now accept data frames with unused non-numeric columns and error
  clearly when `newdata` is not a matrix or data frame.
- [`predict.gmeans()`](https://m-muecke.github.io/gmeans/reference/predict.gmeans.md)
  and
  [`compute_wss()`](https://m-muecke.github.io/gmeans/reference/compute_wss.md)
  now error on a column mismatch between `newdata` and unnamed centers
  instead of returning wrong distances.

## gmeans 0.1.0

CRAN release: 2026-08-05

- Initial CRAN submission.
