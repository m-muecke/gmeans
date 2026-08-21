# Changelog

## gmeans (development version)

- Removed the mlr3 integration vignette since the learner now ships in
  mlr3cluster as `lrn("clust.gmeans")`.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now errors when `k_init` exceeds `k_max`.
- [`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)
  now requires finite numeric input (logical is coerced to 0/1) and
  errors clearly when there are fewer distinct points than centers.
- [`predict.gmeans()`](https://m-muecke.github.io/gmeans/reference/predict.gmeans.md)
  and
  [`compute_wss()`](https://m-muecke.github.io/gmeans/reference/compute_wss.md)
  now error on a column mismatch between `newdata` and unnamed centers
  instead of returning wrong distances.

## gmeans 0.1.0

CRAN release: 2026-08-05

- Initial CRAN submission.
