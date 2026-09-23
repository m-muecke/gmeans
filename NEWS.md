# gmeans (development version)

* `gmeans()` now splits a cluster by starting the two new centers along its main principal component, as described in Hamerly and Elkan (2003), instead of at random points. The split step no longer depends on the random seed and the number of clusters is found more reliably, so results may differ from earlier versions.
* `gmeans()` now uses `k_init = 1` by default, as in Hamerly and Elkan (2003). The previous default of `2` could never return a single cluster. With one initial center the result no longer depends on the random seed.
* `gmeans()` now uses `level = 0.0001` by default, the significance level used by Hamerly and Elkan (2003). The previous default of `0.05` often split clusters that are Gaussian, especially in higher dimensions.
* `gmeans()` now stores `k_init`, `k_max`, and `level` in the returned object.
* `gmeans()` now errors clearly when `centers` is passed, pointing to `k_init` instead.
* New `summary()` method for `gmeans` objects.
* New `tidy()`, `augment()`, and `glance()` methods for `gmeans` objects.

# gmeans 0.2.0

* Removed the mlr3 integration vignette since the learner now ships in mlr3cluster as `lrn("clust.gmeans")`.
* `gmeans()` now errors when `k_init` exceeds `k_max`.
* `gmeans()` now requires finite numeric input (logical is coerced to 0/1) and errors clearly when there are fewer distinct points than centers.
* `gmeans()` now errors clearly on input with zero rows or zero columns.
* `predict.gmeans()` now errors when the Minkowski power `p` is not positive.
* `predict.gmeans()` and `compute_wss()` now accept data frames with unused non-numeric columns and error clearly when `newdata` is not a matrix or data frame.
* `predict.gmeans()` and `compute_wss()` now error on a column mismatch between `newdata` and unnamed centers instead of returning wrong distances.

# gmeans 0.1.0

* Initial CRAN submission.
