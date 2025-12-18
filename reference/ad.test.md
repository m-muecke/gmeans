# Anderson-Darling Normality Test

Perform the Anderson-Darling normality test.

## Usage

``` r
ad.test(x)
```

## Source

Adapted from `nortest::ad.test()`

## Arguments

- x:

  [`numeric()`](https://rdrr.io/r/base/numeric.html) vector of data
  values. Missing values are allowed, but the number of non-missing
  values must be greater than 7.

## Value

A list inheriting from classes `"htest"` containing the following
components:

- statistic: the value of the statistic.

- p.value: the p-value of the test.

- method: the character string `"Anderson-Darling normality test"`.

- data.name: a character string giving the name(s) of the data.

## Details

The Anderson-Darling test is an EDF omnibus test for the composite
hypothesis of normality. The test statistic is \$\$ A^2 = -n
-\frac{1}{n} \sum\_{i=1}^{n} (2i - 1) \[\ln(z\_{i}) + \ln(1 - z\_{n +
1 - i})\] \$\$ where \\z\_{i} = \Phi(\frac{x\_{i} - \bar{x}}{s})\\.
Here, \\\Phi\\ is the cumulative distribution function of the standard
normal distribution, and \\\bar{x}\\ and \\s\\ are mean and standard
deviation of the data values. The p-value is computed from the modified
statistic \\A^2\_\*=A^2 (1.0 + 0.75/n + 2.25/n^{2})\\ according to Table
4.9 in Stephens (1986).

## References

Stephens, A. M (1986). “Goodness-of-Fit-Techniques.” In D'Agostino, B. R
(eds.), chapter Tests based on EDF statistics. CRC Press.

Thode, C. H (2002). *Testing for normality*, 1 edition. CRC Press.
[doi:10.1201/9780203910894](https://doi.org/10.1201/9780203910894) .

## See also

[`stats::shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) for
performing the Shapiro-Wilk test for normality. `nortest::cvm.test()`,
`nortest::lillie.test()`, `nortest::pearson.test()`,
`nortest::sf.test()` for performing further tests for normality.
[`stats::qqnorm()`](https://rdrr.io/r/stats/qqnorm.html) for producing a
normal quantile-quantile plot.

## Examples

``` r
set.seed(123)
ad.test(rnorm(100, mean = 5, sd = 3))
#> 
#>  Anderson-Darling normality test
#> 
#> data:  rnorm(100, mean = 5, sd = 3)
#> A = 0.182, p-value = 0.9104
#> 
ad.test(runif(100, min = 2, max = 4))
#> 
#>  Anderson-Darling normality test
#> 
#> data:  runif(100, min = 2, max = 4)
#> A = 1.3941, p-value = 0.001244
#> 
```
