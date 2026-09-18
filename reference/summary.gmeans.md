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
#> G-means clustering with 7 clusters (k_init = 2, k_max = 10, level = 0.05)
#> 
#>  cluster size withinss Sepal.Length Sepal.Width Petal.Length Petal.Width
#>        1   36   11.454        6.247       2.847        4.775       1.575
#>        2    7    1.263        5.243       2.371        3.443       1.029
#>        3   21    4.177        5.629       2.724        4.133       1.295
#>        4   50   15.151        5.006       3.428        1.462       0.246
#>        5   12    4.655        7.475       3.125        6.300       2.050
#>        6   18    3.052        6.611       3.000        5.506       2.083
#>        7    6    1.230        6.283       3.233        5.517       2.400
#> 
#> Total SS: 681.4, within SS: 40.98, between SS: 640.4 (93.99% of total)
```
