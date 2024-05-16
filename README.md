
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actsims

<!-- badges: start -->
<!-- badges: end -->

The goal of actsims is to …

## Installation

You can install the development version of actsims like so:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/actsims")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
library(actsims)


## create an InteRact class
act <- interact(dictionary = "usfullsurveyor2015", equations = "us2010")
act # custom print
#> <Dictionary>:  usfullsurveyor2015 
#>     group   :  all 
#> <Equations> :  us2010

# calculate deflection for specific ABOs in dictionary
act$deflection(list(A = "deadbeat", B = "kill", O = "god"))
#> [1] 136.564
act$deflection(list(A = "deadbeat", B = "kill", O = "deadbeat"))
#> [1] 97.81486

# create a grid of specific AB0s
events <- tidyr::crossing(
  A = act$dictionary |> dplyr::filter(component == "identity") |> dplyr::pull(term),
  B = act$dictionary |> dplyr::filter(component == "behavior") |> dplyr::pull(term) |> sample(3),
  O = act$dictionary |> dplyr::filter(component == "identity") |> dplyr::pull(term)
) 


# calculate deflection for grid
events$d <- act$deflection(events)

str(events)
#> tibble [2,589,123 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ A: chr [1:2589123] "abortionist" "abortionist" "abortionist" "abortionist" ...
#>  $ B: chr [1:2589123] "delay" "delay" "delay" "delay" ...
#>  $ O: chr [1:2589123] "abortionist" "academic" "accomplice" "accountant" ...
#>  $ d: num [1:2589123] 5.21 16.47 4.02 7.87 6.98 ...

# act$optimal_behavior()
# act$reidentify_actor()
# act$reidentify_object()
# act$nearest_neighbors()
# act$transient_impressions()
```
