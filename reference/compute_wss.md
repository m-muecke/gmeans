# Compute Within-Cluster Sum of Squares

Compute Within-Cluster Sum of Squares

## Usage

``` r
compute_wss(object, newdata = NULL)
```

## Arguments

- object:

  of class inheriting from `"kmeans"`.

- newdata:

  [`matrix()`](https://rdrr.io/r/base/matrix.html) new data to predict
  on.

## Details

WSS is defined as \$\$ \sum\_{i=1}^{n} \left\\x\_{i} -
\mu\_{j(i)}\right\\^2 \$\$, where \\x\_{i}\\ is a data point and
\\\mu\_{j(i)}\\ is the centroid of the cluster to which \\x\_{i}\\ is
assigned. When new data is provided, the function predicts the nearest
cluster for each new observation and computes the WSS for these points
based on their predicted clusters.

## Examples

``` r
km <- kmeans(mtcars, 5)
compute_wss(km)
#> [1]  4665.041 10247.471  8808.032 11474.702  7654.146
# or with new data
compute_wss(km, mtcars)
#> [1]  4665.041 10247.471  8808.032 11474.702  7654.146
```
