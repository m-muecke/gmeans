# gmeans (development version)

* Removed the mlr3 integration vignette since the learner now ships in mlr3cluster as `lrn("clust.gmeans")`.
* `gmeans()` now errors when `k_init` exceeds `k_max`.
* `gmeans()` now requires finite numeric input (logical is coerced to 0/1) and errors clearly when there are fewer distinct points than centers.
* `predict.gmeans()` and `compute_wss()` now error on a column mismatch between `newdata` and unnamed centers instead of returning wrong distances.

# gmeans 0.1.0

* Initial CRAN submission.
