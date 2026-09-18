# Tidy a G-means Clustering Object

Summarize a G-means clustering model as data frames, following the
conventions of the broom package. The generics come from the generics
package, so load it or broom to call the methods.

## Usage

``` r
# S3 method for class 'gmeans'
tidy(x, col.names = colnames(x$centers), ...)

# S3 method for class 'gmeans'
augment(x, data, ...)

# S3 method for class 'gmeans'
glance(x, ...)
```

## Arguments

- x:

  ([`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md))  
  An object of class `"gmeans"`.

- col.names:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Column names for the centers. Defaults to the column names of the
  centers, or `x1`, `x2`, ... if they are unnamed.

- ...:

  (`any`)  
  Additional arguments. Currently unused.

- data:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  The data used to fit the model, a numeric matrix or a data frame.

## Value

A [`data.frame()`](https://rdrr.io/r/base/data.frame.html):

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html): one row
  per cluster with the centers, `size`, `withinss`, and `cluster`.

- [`augment()`](https://generics.r-lib.org/reference/augment.html):
  `data` with a `.cluster` factor giving the cluster assignments.

- [`glance()`](https://generics.r-lib.org/reference/glance.html): one
  row with `k`, `k_init`, `k_max`, `level`, `totss`, `tot.withinss`,
  `betweenss`, and `iter`.

## See also

[`gmeans()`](https://m-muecke.github.io/gmeans/reference/gmeans.md)

## Examples

``` r
library(generics)
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
set.seed(123)
x <- as.matrix(iris[, -5])
cl <- gmeans(x)

tidy(cl)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width size  withinss cluster
#> 1     6.247222    2.847222     4.775000    1.575000   36 11.454444       1
#> 2     5.242857    2.371429     3.442857    1.028571    7  1.262857       2
#> 3     5.628571    2.723810     4.133333    1.295238   21  4.177143       3
#> 4     5.006000    3.428000     1.462000    0.246000   50 15.151000       4
#> 5     7.475000    3.125000     6.300000    2.050000   12  4.655000       5
#> 6     6.611111    3.000000     5.505556    2.083333   18  3.052222       6
#> 7     6.283333    3.233333     5.516667    2.400000    6  1.230000       7
glance(cl)
#>   k k_init k_max level    totss tot.withinss betweenss iter
#> 1 7      2    10  0.05 681.3706     40.98267  640.3879    1
head(augment(cl, x))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width .cluster
#> 1          5.1         3.5          1.4         0.2        4
#> 2          4.9         3.0          1.4         0.2        4
#> 3          4.7         3.2          1.3         0.2        4
#> 4          4.6         3.1          1.5         0.2        4
#> 5          5.0         3.6          1.4         0.2        4
#> 6          5.4         3.9          1.7         0.4        4
```
