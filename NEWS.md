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
