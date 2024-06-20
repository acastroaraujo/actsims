
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actsims

<!-- badges: start -->

[![R-CMD-check](https://github.com/acastroaraujo/actsims/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/acastroaraujo/actsims/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

- `actsims` is an ACT package used for internal development of
  [**`interactShiny`**](https://acastroaraujo.shinyapps.io/interactShiny/)

- It is meant to be *fast* and *easy* to use.

- It is integrated with the
  [**`actdata`**](https://github.com/ahcombs/actdata) package.

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
#> $ B <chr> "abort", "abort", "abort", "abort", "abort", "abort", "abort", "abor…
#> $ O <chr> "accounting_clerk", "accused", "acquaintance", "adopted_child", "ado…
```

Every method is designed to work in bulk. However, only the
`$deflection()` method will work fast with millions of observations.

``` r
d <- act$deflection(events)
d
#> # Event deflection
#> # A data frame: 4,000,000 × 4
#>    A           B     O                       deflection
#>  * <chr>       <chr> <chr>                        <dbl>
#>  1 abortionist abort accounting_clerk              9.65
#>  2 abortionist abort accused                       6.41
#>  3 abortionist abort acquaintance                  9.12
#>  4 abortionist abort adopted_child                14.8 
#>  5 abortionist abort adopted_son                  13.1 
#>  6 abortionist abort advertising_copy_writer       8.29
#>  7 abortionist abort airline_pilot                17.9 
#>  8 abortionist abort architect                    15.0 
#>  9 abortionist abort army_officer                 16.1 
#> 10 abortionist abort ass                          10.0 
#> # ℹ 3,999,990 more rows
```

``` r
get_fundamentals(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 -0.69     1 -0.21 -1.85  1.33 -0.21  1.06  0.46 -0.87
#>  2 -0.69     1 -0.21 -1.85  1.33 -0.21 -1.46 -1.38 -0.09
#>  3 -0.69     1 -0.21 -1.85  1.33 -0.21  1.11  0.02 -0.16
#>  4 -0.69     1 -0.21 -1.85  1.33 -0.21  1.97 -0.1   0.18
#>  5 -0.69     1 -0.21 -1.85  1.33 -0.21  1.65  0.41  0.28
#>  6 -0.69     1 -0.21 -1.85  1.33 -0.21  0.65  0.55  0.66
#>  7 -0.69     1 -0.21 -1.85  1.33 -0.21  1.66  2.41  0.33
#>  8 -0.69     1 -0.21 -1.85  1.33 -0.21  1.52  1.7  -0.28
#>  9 -0.69     1 -0.21 -1.85  1.33 -0.21  1.28  2.36  1.64
#> 10 -0.69     1 -0.21 -1.85  1.33 -0.21 -1.48  0.29  1.38
#> # ℹ 3,999,990 more rows
```

``` r
get_transients(d)
#> # A tibble: 4,000,000 × 9
#>         Ae    Ap    Aa     Be    Bp    Ba      Oe     Op      Oa
#>      <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl>   <dbl>
#>  1 -2.46    1.78 0.207 -2.55   1.86 0.131  0.0807 -1.49  -1.14  
#>  2 -0.243   2.15 0.207 -0.155  1.86 0.201 -1.20   -2.34  -0.720 
#>  3 -2.39    1.87 0.207 -2.60   1.86 0.195  0.106  -1.70  -0.758 
#>  4 -3.04    1.89 0.207 -3.42   1.86 0.226  0.542  -1.76  -0.574 
#>  5 -2.76    1.79 0.207 -3.11   1.86 0.235  0.380  -1.52  -0.520 
#>  6 -1.87    1.76 0.207 -2.16   1.86 0.269 -0.127  -1.45  -0.315 
#>  7 -2.76    1.39 0.207 -3.12   1.86 0.239  0.385  -0.576 -0.493 
#>  8 -2.75    1.53 0.207 -2.99   1.86 0.184  0.314  -0.909 -0.823 
#>  9 -2.23    1.40 0.207 -2.76   1.86 0.357  0.192  -0.598  0.214 
#> 10  0.0180  1.81 0.207 -0.136  1.86 0.334 -1.21   -1.56   0.0736
#> # ℹ 3,999,990 more rows
```

``` r
get_element_wise_deflection(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa     Be    Bp    Ba     Oe    Op     Oa
#>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>
#>  1 3.14  0.607 0.174 0.493  0.281 0.116 0.959  3.80  0.0737
#>  2 0.200 1.32  0.174 2.87   0.281 0.169 0.0692 0.929 0.397 
#>  3 2.88  0.752 0.174 0.562  0.281 0.164 1.01   2.95  0.358 
#>  4 5.53  0.794 0.174 2.46   0.281 0.190 2.04   2.75  0.569 
#>  5 4.29  0.623 0.174 1.60   0.281 0.198 1.61   3.71  0.641 
#>  6 1.39  0.579 0.174 0.0974 0.281 0.229 0.604  3.98  0.951 
#>  7 4.29  0.151 0.174 1.62   0.281 0.202 1.63   8.92  0.678 
#>  8 4.23  0.282 0.174 1.30   0.281 0.155 1.45   6.81  0.295 
#>  9 2.36  0.159 0.174 0.830  0.281 0.321 1.18   8.75  2.03  
#> 10 0.501 0.661 0.174 2.94   0.281 0.296 0.0745 3.42  1.71  
#> # ℹ 3,999,990 more rows
```

Other methods might require a minute or so…

``` r
d_sub <- dplyr::slice_sample(d, n = 10000) # sample 10000 event deflections
d_sub
#> # Event deflection
#> # A data frame: 10,000 × 4
#>    A                  B           O                              deflection
#>  * <chr>              <chr>       <chr>                               <dbl>
#>  1 hostage            anger       radio_and_television_announcer      21.0 
#>  2 street_preacher    touch       bystander                            6.29
#>  3 bonehead           photograph  crane_operator                       9.05
#>  4 chap               guide       advertising_copy_writer              5.61
#>  5 collaborator       chew_out    husband                             35.5 
#>  6 church_deacon      incriminate white_supremacist                   26.0 
#>  7 university_student brutalize   grandchild                          76.7 
#>  8 clod               overlook    rabbi                               11.5 
#>  9 cpa                speak_to    aunt                                 6.10
#> 10 neurotic           disconcert  real_estate_agent                    5.22
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>        Ae     Ap      Aa
#>     <dbl>  <dbl>   <dbl>
#>  1 -1.63   0.620  1.74  
#>  2  0.644  1.23  -0.521 
#>  3  0.920  0.593 -0.282 
#>  4  0.766  2.02  -0.749 
#>  5 -1.05   0.778  0.772 
#>  6 -1.79   1.19   1.83  
#>  7 -1.39   0.870  0.888 
#>  8 -1.12  -0.292  0.0573
#>  9  0.275  1.15   1.30  
#> 10 -1.53   0.123  0.212 
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>         Be      Bp      Ba
#>      <dbl>   <dbl>   <dbl>
#>  1 -0.483  -3.48   -1.48  
#>  2 -0.485   0.0241  1.98  
#>  3 -1.09   -2.20   -0.504 
#>  4  0.0216  0.275   0.696 
#>  5  0.839  -0.899   1.12  
#>  6  1.12    0.269  -0.0772
#>  7 -1.59    0.0153  1.83  
#>  8  0.729  -1.92    0.0436
#>  9  0.558   1.43   -1.07  
#> 10  0.0209 -1.35    1.77  
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>         Oe      Op      Oa
#>      <dbl>   <dbl>   <dbl>
#>  1  0.0136 -1.25    2.14  
#>  2 -0.301  -0.0369 -1.34  
#>  3 -0.0322  0.621  -2.03  
#>  4  3.51    0.419   3.43  
#>  5 -0.581  -2.30   -3.42  
#>  6 -0.495  -2.06   -3.25  
#>  7 -0.811  -2.78   -4.90  
#>  8 -0.410  -1.27    0.332 
#>  9  1.90    0.515   3.89  
#> 10 -0.541  -1.05    0.0209
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>         Be     Bp     Ba
#>      <dbl>  <dbl>  <dbl>
#>  1  0.118   2.61   0.512
#>  2 -0.183  -0.858 -0.169
#>  3 -0.123   1.39   0.559
#>  4  0.553   0.924  0.428
#>  5  0.890   3.04   0.114
#>  6 -0.0151  0.423  1.57 
#>  7 -0.555   1.84   0.877
#>  8  1.51    2.79  -0.706
#>  9 -0.0513  0.526 -0.538
#> 10  0.595   1.44   1.59 
#> # ℹ 9,990 more rows
```

``` r

act$max_confirm(d_sub[c("A", "O")], solve_for = "behavior")
#> # A tibble: 10,000 × 3
#>        Be     Bp      Ba
#>     <dbl>  <dbl>   <dbl>
#>  1 -1.50  -2.52  -1.02  
#>  2 -0.314  0.267  1.45  
#>  3 -0.687 -1.39  -0.711 
#>  4  0.697  0.476  0.479 
#>  5  1.03   1.17   0.329 
#>  6  0.889  1.09   0.0543
#>  7  1.10   0.985  1.30  
#>  8 -0.127 -0.676 -0.289 
#>  9  1.25   1.23  -0.209 
#> 10 -0.377 -0.797  1.32  
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("A", "B")], solve_for = "object")
#> # A tibble: 10,000 × 3
#>         Oe      Op      Oa
#>      <dbl>   <dbl>   <dbl>
#>  1  0.0136 -1.25    2.14  
#>  2 -0.301  -0.0369 -1.34  
#>  3 -0.0322  0.621  -2.03  
#>  4  3.51    0.419   3.43  
#>  5 -0.581  -2.30   -3.42  
#>  6 -0.495  -2.06   -3.25  
#>  7 -0.811  -2.78   -4.90  
#>  8 -0.410  -1.27    0.332 
#>  9  1.90    0.515   3.89  
#> 10 -0.541  -1.05    0.0209
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("B", "O")], solve_for = "actor")
#> # A tibble: 10,000 × 3
#>        Ae     Ap      Aa
#>     <dbl>  <dbl>   <dbl>
#>  1 -1.63   0.620  1.74  
#>  2  0.644  1.23  -0.521 
#>  3  0.920  0.593 -0.282 
#>  4  0.766  2.02  -0.749 
#>  5 -1.05   0.778  0.772 
#>  6 -1.79   1.19   1.83  
#>  7 -1.39   0.870  0.888 
#>  8 -1.12  -0.292  0.0573
#>  9  0.275  1.15   1.30  
#> 10 -1.53   0.123  0.212 
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
