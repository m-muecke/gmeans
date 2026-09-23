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
x <- as.matrix(iris[, -5])
cl <- gmeans(x)

tidy(cl)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width size  withinss cluster
#> 1     6.301031    2.886598     4.958763    1.695876   97 123.79588       1
#> 2     5.005660    3.369811     1.560377    0.290566   53  28.55208       2
glance(cl)
#>   k k_init k_max level    totss tot.withinss betweenss iter
#> 1 2      1    10 1e-04 681.3706      152.348  529.0226    1
head(augment(cl, x))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width .cluster
#> 1          5.1         3.5          1.4         0.2        2
#> 2          4.9         3.0          1.4         0.2        2
#> 3          4.7         3.2          1.3         0.2        2
#> 4          4.6         3.1          1.5         0.2        2
#> 5          5.0         3.6          1.4         0.2        2
#> 6          5.4         3.9          1.7         0.4        2
```
