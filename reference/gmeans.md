# G-means Clustering

Perform G-means clustering on a data matrix.

## Usage

``` r
gmeans(x, k_init = 2L, k_max = 10L, level = 0.05, ...)
```

## Arguments

- x:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Numeric matrix of data, or an object that can be coerced to such a
  matrix (such as a numeric vector or a data frame with all numeric
  columns).

- k_init:

  (`integer(1)`)  
  Initial amount of centers. Default is `2L`.

- k_max:

  (`integer(1)`)  
  Maximum amount of centers. Default is `10L`.

- level:

  (`numeric(1)`)  
  Significance level for the Anderson-Darling test. Default is `0.05`.
  See
  [`ad.test()`](https://m-muecke.github.io/gmeans/reference/ad.test.md)
  for more information.

- ...:

  (`any`)  
  Additional arguments passed to
  [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html).

## Details

The G-means clustering algorithm is an extension of the traditional
k-means algorithm that automatically determines the number of clusters
by iteratively testing the Gaussianity of data within clusters. The
process begins with a specified initial number of clusters (`k_init`)
and iteratively increases the number of clusters until it reaches the
specified maximum (`k_max`) or the data within clusters is determined to
be Gaussian at the specified significance level (`level`).

The algorithm is outlined as follows:

1.  Let \\C\\ be the initial set of centers (usually \\C \leftarrow
    \\\bar{x}\\\\).

2.  Perform k-means clustering on the dataset \\X\\ using the current
    set of centers \\C\\, i.e., \\C \leftarrow \text{kmeans}(C, X)\\.

3.  For each center \\c_j\\, identify the set of data points \\\\x_i
    \mid \text{class}(x_i) = j\\\\ that are assigned to \\c_j\\.

4.  Use the Anderson-Darling test to check if the set of data points
    \\\\x_i \mid \text{class}(x_i) = j\\\\ follows a Gaussian
    distribution

5.  If the data points appear Gaussian, keep \\c_j\\. at the confidence
    level \\\alpha\\. Otherwise, replace \\c_j\\ with two new centers.

6.  Repeat from step 2 until no more centers are added.

## References

Hamerly, Greg, Elkan, Charles (2003). “Learning the k in k-means.” In
Thrun S, Saul L, Schölkopf B (eds.), *Advances in Neural Information
Processing Systems*, volume 16.
<https://proceedings.neurips.cc/paper_files/paper/2003/file/234833147b97bb6aed53a8f4f1c7a7d8-Paper.pdf>.

## Examples

``` r
set.seed(123)
x <- rbind(
  matrix(rnorm(100, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
)
colnames(x) <- c("x", "y")
cl <- gmeans(x)
```
