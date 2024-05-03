
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
#> Dictionary:  usfullsurveyor2015 
#>     group :  all 
#> Equations :  us2010

# calculate deflection for specific ABOs in dictionary
act$deflection(list(A = "abortionist", B = "kill", O = "abortionist"))
#>        1 
#> 28.84482

# create a grid of specific AB0s
events <- crossing(
  A = act$dictionary |> filter(component == "identity") |> pull(term),
  B = c("kill", "aid"),
  O = act$dictionary |> filter(component == "identity") |> pull(term)
) 

# calculate deflection for grid
events$d <- act$deflection(events)

# low deflection
events |> 
  arrange(d) |> 
  head()
#> # A tibble: 6 × 4
#>   A            B     O              d
#>   <chr>        <chr> <chr>      <dbl>
#> 1 grown_up     aid   sports_fan  2.46
#> 2 adult        aid   sports_fan  2.53
#> 3 bank_manager aid   sports_fan  2.58
#> 4 commissioner aid   sports_fan  2.59
#> 5 editor       aid   sports_fan  2.59
#> 6 white_man    aid   sports_fan  2.62

# high deflection
events |> 
  arrange(desc(d)) |> 
  head()
#> # A tibble: 6 × 4
#>   A           B     O               d
#>   <chr>       <chr> <chr>       <dbl>
#> 1 hero        kill  hero         299.
#> 2 best_friend kill  hero         297.
#> 3 hero        kill  best_friend  295.
#> 4 best_friend kill  best_friend  294.
#> 5 hero        kill  brain        289.
#> 6 hero        kill  wife_abuser  289.

# high deflection?
act$deflection(list(A = "hero", B = "kill", O = "wife_abuser"))
#>        1 
#> 288.6055
```
