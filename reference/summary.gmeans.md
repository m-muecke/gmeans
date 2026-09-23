# Summarize a G-means Clustering Object

Summarize the result of a G-means clustering: the number of clusters
found, the settings used to fit the model, a per-cluster table of sizes,
within-cluster sums of squares and centers, and the overall sums of
squares.

## Usage

``` r
# S3 method for class 'gmeans'
summary(object, ...)

# S3 method for class 'summary.gmeans'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- object:

  ([`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md))  
  An object of class `"gmeans"`.

- ...:

  (`any`)  
  Additional arguments. Currently unused.

- x:

  (`summary.gmeans()`)  
  An object of class `"summary.gmeans"`.

- digits:

  (`integer(1)`)  
  Number of significant digits to print.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `"summary.gmeans"`, a list with components:

- `k`, `k_init`, `k_max`, `level`: the number of clusters found and the
  settings used to fit the model.

- `clusters`: a [`data.frame()`](https://rdrr.io/r/base/data.frame.html)
  with one row per cluster and the columns `cluster`, `size`,
  `withinss`, followed by the cluster centers.

- `totss`, `tot.withinss`, `betweenss`, `iter`: as in
  [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html).

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.

## See also

[`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)

## Examples

``` r
set.seed(123)
x <- as.matrix(iris[, -5])
cl <- gmeans(x)
summary(cl)
#> G-means clustering with 2 clusters (k_init = 1, k_max = 10, level = 1e-04)
#> 
#>  cluster size withinss Sepal.Length Sepal.Width Petal.Length Petal.Width
#>        1   97   123.80        6.301       2.887        4.959      1.6959
#>        2   53    28.55        5.006       3.370        1.560      0.2906
#> 
#> Total SS: 681.4, within SS: 152.3, between SS: 529 (77.64% of total)
```
