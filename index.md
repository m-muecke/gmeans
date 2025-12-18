# gmeans

The aim of the package is to provide an implementation of the G-means
algorithm in R. The G-means algorithm is a clustering algorithm that
extends the k-means algorithm by automatically determining the number of
clusters. The algorithm was introduced by [Hamerly and Elkan
(2003)](https://proceedings.neurips.cc/paper_files/paper/2003/file/234833147b97bb6aed53a8f4f1c7a7d8-Paper.pdf).

## Installation

You can install the development version of gmeans from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("m-muecke/gmeans")
```

## Usage

``` r
library(gmeans)

km <- gmeans(mtcars)
km
#> K-means clustering with 2 clusters of sizes 18, 14
#> 
#> Cluster means:
#>        mpg      cyl     disp        hp     drat       wt     qsec        vs
#> 1 23.97222 4.777778 135.5389  98.05556 3.882222 2.609056 18.68611 0.7777778
#> 2 15.10000 8.000000 353.1000 209.21429 3.229286 3.999214 16.77214 0.0000000
#>          am     gear     carb
#> 1 0.6111111 4.000000 2.277778
#> 2 0.1428571 3.285714 3.500000
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   1                   1                   1                   1 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   2                   1                   2                   1 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   1                   1                   1                   2 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   2                   2                   2                   2 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   2                   1                   1                   1 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   1                   2                   2                   2 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   2                   1                   1                   1 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   2                   1                   2                   1 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 58920.54 93643.90
#>  (between_SS / total_SS =  75.5 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#> [6] "betweenss"    "size"         "iter"         "ifault"
```

## Related work

- [nortest](https://CRAN.R-project.org/package=nortest): R package for
  testing the composite hypothesis of normality.
