
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actsims

<!-- badges: start -->

[![R-CMD-check](https://github.com/acastroaraujo/actsims/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/acastroaraujo/actsims/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

- `actsims` is an ACT package used for internal development of
  `interactShiny`

- It is meant to be *fast* and *easy* to use.

- It is integrated with the `actdata` package.

- It uses the R6 OOP system to keep better track of EPA ratings and
  transient impression equations.

## Installation

You can install the development version of actsims like so:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/actsims")
```

## Usage

First, create an “`InteRactModel`” R6 object.

``` r
library(actsims)
#> Loading required package: actdata
```

``` r
suppressMessages(library(tidyverse))

act <- interact(dictionary = "usfullsurveyor2015", equations = "us2010")
#> → equations = list(key = "us2010", group = "all")
#> → dictionary = list(dataset = "usfullsurveyor2015", group = "all")
```

``` r
act
#> 
#> ── Interact Analysis ───────────────────────────────────────────────────────────
#> ℹ Dictionary: usfullsurveyor2015
#>   group: all
#> ℹ Equations: us2010
#>   group: all
#>   type: impressionabo
```

*Dictionary*

``` r
act$dictionary
#> # A tibble: 2,403 × 5
#>    term          component ratings   n         sd       
#>    <chr>         <chr>     <list>    <list>    <list>   
#>  1 abandon       behavior  <dbl [3]> <dbl [3]> <dbl [3]>
#>  2 abandoned     modifier  <dbl [3]> <dbl [3]> <dbl [3]>
#>  3 abduct        behavior  <dbl [3]> <dbl [3]> <dbl [3]>
#>  4 abet          behavior  <dbl [3]> <dbl [3]> <dbl [3]>
#>  5 abhor         behavior  <dbl [3]> <dbl [3]> <dbl [3]>
#>  6 able_bodied   modifier  <dbl [3]> <dbl [3]> <dbl [3]>
#>  7 abort         behavior  <dbl [3]> <dbl [3]> <dbl [3]>
#>  8 abortionist   identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  9 absent_minded modifier  <dbl [3]> <dbl [3]> <dbl [3]>
#> 10 abuse         behavior  <dbl [3]> <dbl [3]> <dbl [3]>
#> # ℹ 2,393 more rows
```

*Equations*

``` r
act$equations
#>               Ae'   Ap'   Aa'   Be'   Bp'   Ba'   Oe'   Op'   Oa'
#> (Intercept) -0.16 -0.06 -0.05 -0.26  0.09  0.21 -0.18  0.00 -0.14
#> Ae           0.62  0.00  0.00  0.52  0.00  0.00  0.00  0.00  0.00
#> Ap           0.00  0.57  0.00  0.00  0.53  0.00  0.00  0.00  0.00
#> Aa           0.00  0.00  0.26  0.00  0.00  0.15  0.00  0.00  0.00
#> Be           0.46 -0.29 -0.18  0.50 -0.21 -0.12  0.26  0.51  0.33
#> Bp           0.00  0.62  0.12  0.00  0.64  0.00  0.00 -0.57 -0.41
#> Ba           0.00  0.00  0.86  0.00  0.00  0.91  0.00  0.00  0.31
#> Oe           0.00  0.00  0.00  0.00  0.00  0.00  0.91  0.00  0.00
#> Op           0.00 -0.20  0.00  0.00  0.00  0.00  0.00  0.47  0.00
#> Oa           0.00  0.00  0.00  0.00  0.00  0.09  0.00  0.00  0.54
#> Ae:Be        0.00  0.00  0.00  0.00  0.00  0.00  0.16  0.00  0.54
#> Be:Oe        0.29  0.00  0.00  0.32  0.00  0.00  0.11  0.00  0.00
#> Bp:Oe       -0.22  0.00  0.00 -0.27  0.00  0.00 -0.15  0.00  0.00
#> Be:Oa       -0.09  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
#> Aa:Ba:Oe     0.00  0.00  0.00  0.00  0.00  0.00  0.00 -0.10  0.00
```

`interact()` uses `group = all` by default for both equations and
dictionaries. If this option does not exist in the `actdata` package,
then you will see an error.

``` r
interact(dictionary = "indiana2003", equations = "nc1978")
#> → equations = list(key = "nc1978", group = "all")
#> ! equations groups: male and female
#> Error:
#> ! `all` not found in `nc1978` equations in `actdata` package
```

You can change the defaults by specifying a second element in either
argument.

For example:

``` r
interact(dictionary = "indiana2003", equations = list("nc1978", "male"))
#> → dictionary = list(dataset = "indiana2003", group = "all")
#> 
#> ── Interact Analysis ───────────────────────────────────────────────────────────
#> ℹ Dictionary: indiana2003
#>   group: all
#> ℹ Equations: nc1978
#>   group: male
#>   type: impressionabo
```

## Methods

`InteRactModel` objects come with built in methods which you can access
via the `$` operator.

**Fundamentals.**

``` r
act$fundamentals("mother")
#> # Source: usfullsurveyor2015 (all)
#> # A data frame: 2 × 5
#>   term   component     e     p     a
#> * <chr>  <chr>     <dbl> <dbl> <dbl>
#> 1 mother behavior   2.79  2.84  0.18
#> 2 mother identity   3.05  2.66  0.76
```

*Note. This is just a simple function that looks inside
`act$dictionary`*

**Deflection scores.**

``` r
d <- act$deflection(data.frame(A = "god", B = "kill", O = "deadbeat"))
d
#> # Event deflection
#> # A data frame: 1 × 4
#>   A     B     O        deflection
#> * <chr> <chr> <chr>         <dbl>
#> 1 god   kill  deadbeat       173.
```

You can also extract useful metadata from these scores.

``` r
get_fundamentals(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  3.19  3.45 -0.46 -4.26  1.95 -0.11  -2.8 -2.57 -1.45
```

``` r
get_transients(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  3.96  4.86 0.737  4.56  4.06 0.422 -3.88 -4.48 -10.5
```

``` r
get_element_wise_deflection(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 0.597  2.00  1.43  77.8  4.46 0.283  1.16  3.64  81.9
```

**Behaviors and Reidentification**

Optimal behaviors and/or reidentification uses the deflection scores
estimated by `$deflection()` and stored in `d`. They require you specify
the perspective of the one doing the behaving or the reidentifying.

Behaviors

``` r
act$optimal_behavior(d, who = "actor")
#> # A tibble: 1 × 3
#>      Be    Bp     Ba
#>   <dbl> <dbl>  <dbl>
#> 1  1.90  1.18 -0.637
```

``` r
act$optimal_behavior(d, who = "object")
#> # A tibble: 1 × 3
#>      Be     Bp    Ba
#>   <dbl>  <dbl> <dbl>
#> 1 0.111 -0.525 0.233
```

Reidentification

``` r
act$reidentify(d, who = "actor")
#> # A tibble: 1 × 3
#>       Ae    Ap    Aa
#>    <dbl> <dbl> <dbl>
#> 1 -0.999  2.35 0.848
```

``` r
act$reidentify(d, who = "object")
#> # A tibble: 1 × 3
#>      Oe    Op    Oa
#>   <dbl> <dbl> <dbl>
#> 1 -1.26 -4.85 -11.1
```

**Solve For…**

``` r
act$max_confirm(
  events = tibble(A = "god", O = "deadbeat"), 
  solve_for = "behavior"
) 
#> # A tibble: 1 × 3
#>      Be    Bp     Ba
#>   <dbl> <dbl>  <dbl>
#> 1 0.545  1.54 -0.634
```

``` r

act$max_confirm(
  events = list(A = "god", B = "kill"), 
  solve_for = "object"
) 
#> # A tibble: 1 × 3
#>      Oe    Op    Oa
#>   <dbl> <dbl> <dbl>
#> 1 -1.26 -4.85 -11.1
```

``` r

act$max_confirm(
  events = data.frame(B = "kill", O = "deadbeat"), 
  solve_for = "actor"
) 
#> # A tibble: 1 × 3
#>       Ae    Ap    Aa
#>    <dbl> <dbl> <dbl>
#> 1 -0.999  2.35 0.848
```

**Closest Terms**

``` r
act$closest_terms(list(e = 1, p = 0, a = -1), component = "behavior", max_dist = 0.5)
#>        obey      bow_to      nuzzle comply_with  curtsey_to     gaze_at 
#>      0.0581      0.1419      0.2141      0.2822      0.2948      0.4301 
#>      stroke 
#>      0.4806
```

``` r

deadbeat <- act$fundamentals("deadbeat")
deadbeat
#> # Source: usfullsurveyor2015 (all)
#> # A data frame: 1 × 5
#>   term     component     e     p     a
#> * <chr>    <chr>     <dbl> <dbl> <dbl>
#> 1 deadbeat identity   -2.8 -2.57 -1.45
```

``` r

act$closest_terms(deadbeat, component = "modifier", max_dist = 0.5)
#>  uneducated incompetent    helpless    cowardly        poor  unemployed 
#>      0.1581      0.2489      0.3126      0.3507      0.3901      0.4555
```

## In Bulk…

You can use a grid of events to estimate multiple deflection scores
simultaneously.

For example, the following `events` object contains 4 million ABO events
randomly created from the `usfullsurveyor2015` dictionary.

``` r
# create a grid of specific AB0s
events <- tidyr::crossing(
  A = dplyr::filter(act$dictionary, component == "identity")[["term"]] |> sample(200),
  B = dplyr::filter(act$dictionary, component == "behavior")[["term"]] |> sample(100),
  O = dplyr::filter(act$dictionary, component == "identity")[["term"]] |> sample(200)
) 

glimpse(events)
#> Rows: 4,000,000
#> Columns: 3
#> $ A <chr> "abortionist", "abortionist", "abortionist", "abortionist", "abortio…
#> $ B <chr> "abhor", "abhor", "abhor", "abhor", "abhor", "abhor", "abhor", "abho…
#> $ O <chr> "abortionist", "accused", "addict", "adolescent", "adopted_son", "ad…
```

Every method is designed to work in bulk. However, only the
`$deflection()` method will work fast with millions of observations.

``` r
d <- act$deflection(events)
d
#> # Event deflection
#> # A data frame: 4,000,000 × 4
#>    A           B     O             deflection
#>  * <chr>       <chr> <chr>              <dbl>
#>  1 abortionist abhor abortionist         8.12
#>  2 abortionist abhor accused             7.06
#>  3 abortionist abhor addict             11.1 
#>  4 abortionist abhor adolescent          7.39
#>  5 abortionist abhor adopted_son        13.2 
#>  6 abortionist abhor adult              12.8 
#>  7 abortionist abhor agnostic            6.52
#>  8 abortionist abhor apprentice         11.1 
#>  9 abortionist abhor architect          15.3 
#> 10 abortionist abhor army_chaplain      19.4 
#> # ℹ 3,999,990 more rows
```

``` r
get_fundamentals(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 -0.69     1 -0.21 -2.13   0.9  0.62 -0.69  1    -0.21
#>  2 -0.69     1 -0.21 -2.13   0.9  0.62 -1.46 -1.38 -0.09
#>  3 -0.69     1 -0.21 -2.13   0.9  0.62 -2.23 -2.36  0.19
#>  4 -0.69     1 -0.21 -2.13   0.9  0.62  0.66 -0.81  2.17
#>  5 -0.69     1 -0.21 -2.13   0.9  0.62  1.65  0.41  0.28
#>  6 -0.69     1 -0.21 -2.13   0.9  0.62  1.06  1.81  0.29
#>  7 -0.69     1 -0.21 -2.13   0.9  0.62  0.22 -0.08 -0.28
#>  8 -0.69     1 -0.21 -2.13   0.9  0.62  1.46 -0.29  0.03
#>  9 -0.69     1 -0.21 -2.13   0.9  0.62  1.52  1.7  -0.28
#> 10 -0.69     1 -0.21 -2.13   0.9  0.62  2.16  1.59  0.23
#> # ℹ 3,999,990 more rows
```

``` r
get_transients(d)
#> # A tibble: 4,000,000 × 9
#>        Ae    Ap    Aa     Be    Bp    Ba      Oe     Op      Oa
#>     <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl>   <dbl>
#>  1 -1.05   1.49  0.92 -1.05   1.64 0.979 -0.872  -1.14  -0.339 
#>  2 -0.394  1.96  0.92 -0.334  1.64 0.990 -1.29   -2.27  -0.275 
#>  3  0.288  2.16  0.92  0.378  1.64 1.02  -1.70   -2.74  -0.123 
#>  4 -1.69   1.85  0.92 -2.29   1.64 1.19  -0.142  -1.97   0.946 
#>  5 -2.86   1.60  0.92 -3.21   1.64 1.02   0.394  -1.39  -0.0749
#>  6 -2.38   1.32  0.92 -2.66   1.64 1.02   0.0745 -0.735 -0.0695
#>  7 -1.80   1.70  0.92 -1.89   1.64 0.973 -0.380  -1.63  -0.377 
#>  8 -2.75   1.74  0.92 -3.03   1.64 1.00   0.291  -1.72  -0.210 
#>  9 -2.86   1.35  0.92 -3.09   1.64 0.973  0.323  -0.781 -0.377 
#> 10 -3.29   1.37  0.92 -3.68   1.64 1.02   0.669  -0.824 -0.102 
#> # ℹ 3,999,990 more rows
```

``` r
get_element_wise_deflection(d)
#> # A tibble: 4,000,000 × 9
#>        Ae    Ap    Aa     Be    Bp    Ba     Oe    Op      Oa
#>     <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>   <dbl>
#>  1 0.126  0.236  1.28 1.18   0.552 0.129 0.0330 4.57  0.0168 
#>  2 0.0877 0.925  1.28 3.23   0.552 0.137 0.0296 0.787 0.0341 
#>  3 0.956  1.34   1.28 6.29   0.552 0.156 0.276  0.143 0.0983 
#>  4 1.00   0.719  1.28 0.0269 0.552 0.329 0.643  1.35  1.50   
#>  5 4.71   0.364  1.28 1.17   0.552 0.163 1.58   3.22  0.126  
#>  6 2.84   0.105  1.28 0.285  0.552 0.164 0.971  6.48  0.129  
#>  7 1.23   0.492  1.28 0.0589 0.552 0.125 0.360  2.42  0.00946
#>  8 4.26   0.553  1.28 0.817  0.552 0.145 1.37   2.04  0.0575 
#>  9 4.71   0.120  1.28 0.920  0.552 0.125 1.43   6.15  0.00946
#> 10 6.74   0.135  1.28 2.41   0.552 0.159 2.22   5.83  0.110  
#> # ℹ 3,999,990 more rows
```

Other methods might require a minute or so…

``` r
d_sub <- dplyr::slice_sample(d, n = 10000) # sample 10000 event deflections
d_sub
#> # Event deflection
#> # A data frame: 10,000 × 4
#>    A                       B          O                    deflection
#>  * <chr>                   <chr>      <chr>                     <dbl>
#>  1 sweetheart              remember   partner                   11.7 
#>  2 boss                    stroke     craftsman                  7.14
#>  3 army_officer            endure     bridesmaid                 7.85
#>  4 quack                   brush_off  geologist                  8.95
#>  5 geologist               disappoint optical_engineer          30.0 
#>  6 call_girl               kick       uncle                     18.6 
#>  7 rancher                 toast      nut                        5.00
#>  8 co_worker               reward     nut                        6.82
#>  9 bonehead                treat      bank_teller               20.7 
#> 10 manager_of_branch_store mortify    assembly_line_worker      28.0 
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>        Ae     Ap      Aa
#>     <dbl>  <dbl>   <dbl>
#>  1  0.670  1.28  -0.389 
#>  2  0.772  0.258 -1.28  
#>  3  1.06   0.391 -1.11  
#>  4 -1.08   0.582  0.0427
#>  5 -1.13  -0.506 -0.189 
#>  6 -1.08   0.265  1.04  
#>  7  0.330  1.31   1.31  
#>  8  0.442  1.89   0.449 
#>  9  0.300  1.74   0.0165
#> 10 -1.39   1.60   1.79  
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>         Be     Bp      Ba
#>      <dbl>  <dbl>   <dbl>
#>  1 -0.165   0.429  0.0701
#>  2  0.713   1.90   1.38  
#>  3  1.65    1.91   2.17  
#>  4  0.818  -1.17   1.57  
#>  5  0.240  -0.828 -0.195 
#>  6  0.536  -2.26   0.796 
#>  7  0.113   0.847 -0.341 
#>  8 -0.191   0.664  0.328 
#>  9 -0.921  -1.85  -0.306 
#> 10  0.0517  0.450  1.72  
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>        Oe      Op      Oa
#>     <dbl>   <dbl>   <dbl>
#>  1  3.94   0.0395  6.28  
#>  2  0.580 -0.245   0.481 
#>  3 -1.58  -1.23   -0.986 
#>  4 -0.413 -1.27   -0.0872
#>  5 -0.983 -1.71   -4.45  
#>  6 -0.163 -1.77    1.82  
#>  7  2.10   0.454   3.62  
#>  8  2.94   0.272   4.55  
#>  9 -0.184  1.01   -2.57  
#> 10 -0.609 -2.84   -4.16  
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>         Be      Bp       Ba
#>      <dbl>   <dbl>    <dbl>
#>  1  0.817   1.88   -1.23   
#>  2  1.25    0.444  -0.0724 
#>  3  1.56    0.239   1.29   
#>  4  1.12    1.85   -0.272  
#>  5  0.677   2.65   -0.383  
#>  6  1.25    2.79    0.432  
#>  7 -0.284  -0.260  -0.133  
#>  8 -0.0343 -0.0815  0.0911 
#>  9 -0.372   1.07   -0.00708
#> 10 -0.914   0.529   0.904  
#> # ℹ 9,990 more rows
```

``` r

act$max_confirm(d_sub[c("A", "O")], solve_for = "behavior")
#> # A tibble: 10,000 × 3
#>        Be     Bp      Ba
#>     <dbl>  <dbl>   <dbl>
#>  1  0.888  1.29  -0.171 
#>  2  1.32   1.93   0.555 
#>  3  1.36   1.73   1.02  
#>  4 -0.144 -0.533  0.869 
#>  5  0.739  0.576 -0.741 
#>  6 -0.302 -0.832  0.782 
#>  7  0.630  0.730  0.0735
#>  8  0.577  0.751  0.465 
#>  9 -0.575 -1.18  -0.829 
#> 10  0.909  1.43   1.10  
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("A", "B")], solve_for = "object")
#> # A tibble: 10,000 × 3
#>        Oe      Op      Oa
#>     <dbl>   <dbl>   <dbl>
#>  1  3.94   0.0395  6.28  
#>  2  0.580 -0.245   0.481 
#>  3 -1.58  -1.23   -0.986 
#>  4 -0.413 -1.27   -0.0872
#>  5 -0.983 -1.71   -4.45  
#>  6 -0.163 -1.77    1.82  
#>  7  2.10   0.454   3.62  
#>  8  2.94   0.272   4.55  
#>  9 -0.184  1.01   -2.57  
#> 10 -0.609 -2.84   -4.16  
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("B", "O")], solve_for = "actor")
#> # A tibble: 10,000 × 3
#>        Ae     Ap      Aa
#>     <dbl>  <dbl>   <dbl>
#>  1  0.670  1.28  -0.389 
#>  2  0.772  0.258 -1.28  
#>  3  1.06   0.391 -1.11  
#>  4 -1.08   0.582  0.0427
#>  5 -1.13  -0.506 -0.189 
#>  6 -1.08   0.265  1.04  
#>  7  0.330  1.31   1.31  
#>  8  0.442  1.89   0.449 
#>  9  0.300  1.74   0.0165
#> 10 -1.39   1.60   1.79  
#> # ℹ 9,990 more rows
```

## Deference Score

You can also create the deference scores discussed by Freeland & Hoey
(2018).

But this requires to modify the existing dictionary so we get the right
combination of identities and behaviors.

This seems to be the easiest way to do this:

``` r
occ <- interact(dictionary = "occs2019")
#> → dictionary = list(dataset = "occs2019", group = "all")
```

``` r
occ$dictionary
#> # A tibble: 650 × 5
#>    term                             component ratings   n         sd       
#>    <chr>                            <chr>     <list>    <list>    <list>   
#>  1 911_dispatcher                   identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  2 accountant                       identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  3 actor                            identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  4 actress                          identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  5 actuary_for_an_insurance_company identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  6 acupuncturist                    identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  7 administrative_assistant         identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  8 advertising_executive            identity  <dbl [3]> <dbl [3]> <dbl [3]>
#>  9 advertising_salesman             identity  <dbl [3]> <dbl [3]> <dbl [3]>
#> 10 aerobics_instructor              identity  <dbl [3]> <dbl [3]> <dbl [3]>
#> # ℹ 640 more rows
```

``` r

occupations <- occ$dictionary$term ## save for later use

defer_to <- act$dictionary |> 
  dplyr::filter(term == "defer_to")

## replace original dictionary
occ$dictionary <- dplyr::bind_rows(defer_to, occ$dictionary)
#> ✔ added new dictionary
```

Note that a message appeared signaling that the replacement was
successful.

``` r
occ
#> 
#> ── Interact Analysis ───────────────────────────────────────────────────────────
#> ℹ Dictionary: External [!]
#>   group: ?
#> ℹ Equations: us2010
#>   group: all
#>   type: impressionabo
```

Now you just create another grid of events, calculate the deflection
scores, and average over the As.

``` r
events <- tidyr::crossing(
  A = occupations,
  B = "defer_to",
  O = occupations
)

output <- occ$deflection(events)

output |> 
  group_by(A) |> 
  summarize(avg = mean(deflection)) |> 
  arrange(desc(avg)) 
#> # A tibble: 650 × 2
#>    A                            avg
#>    <chr>                      <dbl>
#>  1 firefighter                 16.7
#>  2 fireman                     15.0
#>  3 paramedic                   13.9
#>  4 professional_athlete        12.4
#>  5 fire_department_lieutenant  12.3
#>  6 ambulance_driver            11.4
#>  7 auctioneer                  11.1
#>  8 dynamite_blaster            11.0
#>  9 911_dispatcher              10.9
#> 10 surgeon                     10.6
#> # ℹ 640 more rows
```
