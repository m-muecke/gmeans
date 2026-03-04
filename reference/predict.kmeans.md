# Predict Method for G-means Clustering

Predicted values based on the G-means clustering model.

## Usage

``` r
# S3 method for class 'kmeans'
predict(
  object,
  newdata,
  method = c("euclidean", "manhattan", "minkowski"),
  p = 2,
  ...
)
```

## Source

Adapted from [clue](https://CRAN.R-project.org/package=clue)

## Arguments

- object:

  (`any`)  
  Class inheriting from `"kmeans"`.

- newdata:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  New data to predict on.

- method:

  (`character(1)`)  
  Distance metric to use. Either `"euclidean"`, `"manhattan"`, or
  `"minkowski"`. Default is `"euclidean"`.

- p:

  (`numeric(1)`)  
  Power of the Minkowski distance. Default is `2`.

- ...:

  (`any`)  
  Additional arguments.

## Details

The `predict` method for G-means clustering assigns new data points to
the nearest cluster center identified by the G-means algorithm. The
method uses the specified distance metric to calculate the distance
between each new data point and all cluster centers, and then assigns
each point to the cluster with the closest center.

The `method` argument specifies the distance metric to use. The
following options:

- `"euclidean"`: The Euclidean distance is the default metric used in
  the k-means and is defined as \$\$ d(x, y) = \sqrt{\sum\_{i=1}^{n}
  (x_i - y_i)^2} \$\$

- `"manhattan"`: The Manhattan distance is defined as \$\$ d(x, y) =
  \sum\_{i=1}^{n} \|x_i - y_i\| \$\$

- `"minkowski"`: The Minkowski distance is defined as \$\$ d(x, y) =
  \left( \sum\_{i=1}^{n} \|x_i - y_i\|^p \right)^{1/p}, \$\$ where \\p\\
  is a parameter that defines the distance type (e.g., \\p=2\\ for
  Euclidean, \\p=1\\ for Manhattan).

## Examples

``` r
set.seed(123)
x <- as.matrix(iris[, -5])
cl <- gmeans(x)

newdata <- x[1:10, ]
predict(cl, newdata)
#>  [1] 4 4 4 4 4 4 4 4 4 4
```
