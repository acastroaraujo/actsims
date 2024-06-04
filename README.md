
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actsims

<!-- badges: start -->
<!-- badges: end -->

`actsims` is an ACT package used for internal development of
`interactShiny`

It is not meant for public consumption.

## Installation

You can install the development version of actsims like so:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/actsims")
```

## Performance

- `actsims` is easy to use and it is meant to be fast.
- It uses the R6 OOP system.
- It is integrated with the `acdata` package.

Create an inteRact object.

``` r
library(actsims)
suppressMessages(library(tidyverse))

act <- interact(dictionary = "usfullsurveyor2015", equation = "us2010")
class(act)
#> [1] "InteRact" "R6"
act # custom print
#> <Dictionary>:  usfullsurveyor2015 
#>     group   :  all 
#> <Equation>  :  us2010
```

This object comes with built in functions.

Deflection scores.

``` r
act$deflection(list(A = "deadbeat", B = "kill", O = "god"))
#> Deflection Scores 
#> [1] 137
act$deflection(list(A = "deadbeat", B = "kill", O = "deadbeat"))
#> Deflection Scores 
#> [1] 97.8
```

You can also extract useful metadata from these scores.

``` r
d <- act$deflection(list(A = "ceo", B = "advise", O = "benefactor"))
d
#> Deflection Scores 
#> [1] 6.95
get_fundamentals(d)
#>        Ae   Ap   Aa   Be   Bp   Ba   Oe   Op  Oa
#> [1,] 0.71 3.22 1.48 2.57 2.28 0.28 1.97 1.98 0.1
get_transients(d)
#>         Ae     Ap     Aa       Be     Bp     Ba       Oe        Op       Oa
#> 1 1.919359 2.0477 0.3866 1.801596 2.7161 0.3874 2.456031 0.8600632 0.899438
get_element_wise_deflection(d)
#>         Ae       Ap       Aa        Be        Bp         Ba        Oe       Op
#> 1 1.462549 1.374287 1.195524 0.5904447 0.1901832 0.01153476 0.2362261 1.254258
#>          Oa
#> 1 0.6391011
```

You can also do other stuff.

``` r
act$reidentify_object(list(A = "ceo", B = "advise", O = "benefactor"))
#>             Oe        Op        Oa
#> [1,] 0.7631745 0.7457585 0.3578638
act$optimal_behavior(list(A = "ceo", B = "advise", O = "benefactor"), who = "actor")
#>          Be      Bp       Ba
#> 1 0.5051057 1.54558 1.021906
```

And you can use a grid of events to estimate multiple deflection scores
simultaneously.

``` r
# create a grid of specific AB0s
events <- tidyr::crossing(
  A = act$dictionary |> dplyr::filter(component == "identity") |> dplyr::pull(term),
  B = act$dictionary |> dplyr::filter(component == "behavior") |> dplyr::pull(term) |> sample(3),
  O = act$dictionary |> dplyr::filter(component == "identity") |> dplyr::pull(term)
) 

glimpse(events)
#> Rows: 2,589,123
#> Columns: 3
#> $ A <chr> "abortionist", "abortionist", "abortionist", "abortionist", "abortio…
#> $ B <chr> "bequeath_to", "bequeath_to", "bequeath_to", "bequeath_to", "bequeat…
#> $ O <chr> "abortionist", "academic", "accomplice", "accountant", "accounting_c…

# calculate deflection for grid
events$d <- act$deflection(events)

events
#> # A tibble: 2,589,123 × 4
#>    A           B           O                d         
#>    <chr>       <chr>       <chr>            <deflectn>
#>  1 abortionist bequeath_to abortionist      2.055058  
#>  2 abortionist bequeath_to academic         4.496345  
#>  3 abortionist bequeath_to accomplice       1.831423  
#>  4 abortionist bequeath_to accountant       2.389145  
#>  5 abortionist bequeath_to accounting_clerk 1.705365  
#>  6 abortionist bequeath_to accused          1.637489  
#>  7 abortionist bequeath_to acquaintance     1.765211  
#>  8 abortionist bequeath_to actor            4.402076  
#>  9 abortionist bequeath_to addict           2.691712  
#> 10 abortionist bequeath_to adolescent       4.019005  
#> # ℹ 2,589,113 more rows
```
