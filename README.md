
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
#> → dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> → equations = list(key = "us2010", group = "all")
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
#> → dictionary = list(dataset = "indiana2003", group = "all")
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
#> $ A <chr> "accused", "accused", "accused", "accused", "accused", "accused", "a…
#> $ B <chr> "admit", "admit", "admit", "admit", "admit", "admit", "admit", "admi…
#> $ O <chr> "accused", "adopted_son", "adulteress", "ambassador", "applicant", "…
```

Every method is designed to work in bulk. However, only the
`$deflection()` method will work fast with millions of observations.

``` r
d <- act$deflection(events)
d
#> # Event deflection
#> # A data frame: 4,000,000 × 4
#>    A       B     O                    deflection
#>  * <chr>   <chr> <chr>                     <dbl>
#>  1 accused admit accused                    7.13
#>  2 accused admit adopted_son                6.94
#>  3 accused admit adulteress                 7.25
#>  4 accused admit ambassador                 8.18
#>  5 accused admit applicant                  6.82
#>  6 accused admit arsonist                   7.47
#>  7 accused admit artist                     6.61
#>  8 accused admit asian_woman                6.42
#>  9 accused admit ass                        7.81
#> 10 accused admit assembly_line_worker       7.81
#> # ℹ 3,999,990 more rows
```

``` r
get_fundamentals(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 -1.46 -1.38 -0.09  0.92  1.16  0.18 -1.46 -1.38 -0.09
#>  2 -1.46 -1.38 -0.09  0.92  1.16  0.18  1.65  0.41  0.28
#>  3 -1.46 -1.38 -0.09  0.92  1.16  0.18 -2.43  0.45  0.96
#>  4 -1.46 -1.38 -0.09  0.92  1.16  0.18  1.46  2.28  0.31
#>  5 -1.46 -1.38 -0.09  0.92  1.16  0.18  1.12 -0.63 -0.06
#>  6 -1.46 -1.38 -0.09  0.92  1.16  0.18 -3.31  1.55  0.69
#>  7 -1.46 -1.38 -0.09  0.92  1.16  0.18  1.74  0.92 -0.33
#>  8 -1.46 -1.38 -0.09  0.92  1.16  0.18  1.25  0.32 -0.27
#>  9 -1.46 -1.38 -0.09  0.92  1.16  0.18 -1.48  0.29  1.38
#> 10 -1.46 -1.38 -0.09  0.92  1.16  0.18  0.77 -1.02  0.76
#> # ℹ 3,999,990 more rows
```

``` r
get_transients(d)
#> # A tibble: 4,000,000 × 9
#>        Ae     Ap    Aa     Be      Bp    Ba     Oe       Op     Oa
#>     <dbl>  <dbl> <dbl>  <dbl>   <dbl> <dbl>  <dbl>    <dbl>  <dbl>
#>  1 -0.651 -0.118 0.055 -0.532 -0.0922 0.242 -1.38  -0.843   -1.03 
#>  2 -0.646 -0.476 0.055 -0.590 -0.0922 0.275  1.23   0.00337 -0.830
#>  3 -0.750 -0.484 0.055 -0.514 -0.0922 0.336 -2.19   0.0156  -0.463
#>  4 -0.651 -0.850 0.055 -0.587 -0.0922 0.278  1.07   0.882   -0.814
#>  5 -0.624 -0.268 0.055 -0.580 -0.0922 0.244  0.782 -0.486   -1.01 
#>  6 -0.738 -0.704 0.055 -0.497 -0.0922 0.312 -2.93   0.531   -0.609
#>  7 -0.594 -0.578 0.055 -0.592 -0.0922 0.220  1.30   0.243   -1.16 
#>  8 -0.605 -0.458 0.055 -0.583 -0.0922 0.226  0.891 -0.0396  -1.13 
#>  9 -0.773 -0.452 0.055 -0.531 -0.0922 0.374 -1.39  -0.0581  -0.236
#> 10 -0.696 -0.190 0.055 -0.574 -0.0922 0.318  0.489 -0.670   -0.571
#> # ℹ 3,999,990 more rows
```

``` r
get_element_wise_deflection(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap     Aa    Be    Bp      Ba      Oe     Op    Oa
#>    <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl> <dbl>
#>  1 0.654 1.59  0.0210  2.11  1.57 0.00382 0.00672 0.288  0.884
#>  2 0.663 0.817 0.0210  2.28  1.57 0.00904 0.180   0.165  1.23 
#>  3 0.505 0.802 0.0210  2.05  1.57 0.0244  0.0575  0.189  2.03 
#>  4 0.655 0.281 0.0210  2.27  1.57 0.00956 0.155   1.95   1.26 
#>  5 0.699 1.24  0.0210  2.25  1.57 0.00416 0.114   0.0207 0.910
#>  6 0.522 0.457 0.0210  2.01  1.57 0.0174  0.147   1.04   1.69 
#>  7 0.749 0.643 0.0210  2.29  1.57 0.00162 0.193   0.458  0.688
#>  8 0.731 0.850 0.0210  2.26  1.57 0.00208 0.129   0.129  0.735
#>  9 0.471 0.861 0.0210  2.11  1.57 0.0377  0.00726 0.121  2.61 
#> 10 0.584 1.42  0.0210  2.23  1.57 0.0191  0.0790  0.122  1.77 
#> # ℹ 3,999,990 more rows
```

Other methods might require a minute or so…

``` r
d_sub <- dplyr::slice_sample(d, n = 10000) # sample 10000 event deflections
d_sub
#> # Event deflection
#> # A data frame: 10,000 × 4
#>    A                    B          O              deflection
#>  * <chr>                <chr>      <chr>               <dbl>
#>  1 migrant_worker       pamper     in_law               2.41
#>  2 accused              command    skilled_worker      24.3 
#>  3 casual_laborer       forsake    millionaire         19.9 
#>  4 outsider             incite     sidekick             9.25
#>  5 tv_repairman         sleep_with flunky               5.42
#>  6 fisherman            evade      specialist           8.10
#>  7 diplomat             elude      mistress             6.02
#>  8 auctioneer           influence  liar                 8.60
#>  9 construction_foreman wrong      classmate           28.8 
#> 10 funeral_director     scold      plumber             18.5 
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>        Ae      Ap     Aa
#>     <dbl>   <dbl>  <dbl>
#>  1  1.23   0.864  -0.299
#>  2 -0.118  1.65    0.316
#>  3 -1.55   0.0614  0.354
#>  4 -0.542  1.19    0.956
#>  5  0.479  2.31    1.23 
#>  6 -0.952 -0.768  -0.201
#>  7 -0.858  0.215  -0.734
#>  8  0.664  2.31    0.640
#>  9 -1.21   0.576   0.969
#> 10 -1.03   1.26    1.30 
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>         Be      Bp     Ba
#>      <dbl>   <dbl>  <dbl>
#>  1  0.537  -0.720   0.454
#>  2  0.260  -2.80   -0.234
#>  3  0.753  -1.61    0.818
#>  4  0.503  -1.44   -0.460
#>  5  0.0339  0.184  -0.648
#>  6  1.97   -0.232  -0.182
#>  7  1.03    1.52    1.15 
#>  8  0.0101  0.0751  3.05 
#>  9 -0.0557  0.416   2.02 
#> 10  0.828  -0.852  -1.13 
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>         Oe     Op     Oa
#>      <dbl>  <dbl>  <dbl>
#>  1  2.02    0.313  0.481
#>  2 -0.498  -1.20  -0.818
#>  3 -0.620  -1.70  -3.04 
#>  4 -0.447  -0.944 -1.19 
#>  5  0.0705 -0.489  0.658
#>  6 -1.38   -0.514 -2.04 
#>  7 -0.986  -1.20  -1.98 
#>  8 -0.231  -1.19  -1.18 
#>  9 -0.688  -2.67  -3.89 
#> 10 -0.343  -2.02  -2.13 
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>        Be     Bp     Ba
#>     <dbl>  <dbl>  <dbl>
#>  1  0.439  1.16   1.16 
#>  2  0.906  2.87   0.250
#>  3  0.142  2.49   1.45 
#>  4  0.230  1.74   0.243
#>  5 -1.37  -0.666 -1.02 
#>  6  1.30   2.28   0.463
#>  7 -0.699 -1.21   0.669
#>  8 -2.50  -1.24   0.513
#>  9 -0.776  0.829  0.287
#> 10  0.547  2.42  -0.509
#> # ℹ 9,990 more rows
```

``` r

act$max_confirm(d_sub[c("A", "O")], solve_for = "behavior")
#> # A tibble: 10,000 × 3
#>         Be     Bp      Ba
#>      <dbl>  <dbl>   <dbl>
#>  1  0.467  -0.450  0.341 
#>  2 -0.533  -1.23  -0.0500
#>  3  0.777  -0.258  0.196 
#>  4 -0.0692 -0.640 -0.430 
#>  5  0.187   0.497 -0.112 
#>  6  0.915   0.576 -0.439 
#>  7  0.918   1.55   0.689 
#>  8  0.522   1.25   1.54  
#>  9  0.881   1.27   1.06  
#> 10  0.807   0.550 -0.941 
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("A", "B")], solve_for = "object")
#> # A tibble: 10,000 × 3
#>         Oe     Op     Oa
#>      <dbl>  <dbl>  <dbl>
#>  1  2.02    0.313  0.481
#>  2 -0.498  -1.20  -0.818
#>  3 -0.620  -1.70  -3.04 
#>  4 -0.447  -0.944 -1.19 
#>  5  0.0705 -0.489  0.658
#>  6 -1.38   -0.514 -2.04 
#>  7 -0.986  -1.20  -1.98 
#>  8 -0.231  -1.19  -1.18 
#>  9 -0.688  -2.67  -3.89 
#> 10 -0.343  -2.02  -2.13 
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("B", "O")], solve_for = "actor")
#> # A tibble: 10,000 × 3
#>        Ae      Ap     Aa
#>     <dbl>   <dbl>  <dbl>
#>  1  1.23   0.864  -0.299
#>  2 -0.118  1.65    0.316
#>  3 -1.55   0.0614  0.354
#>  4 -0.542  1.19    0.956
#>  5  0.479  2.31    1.23 
#>  6 -0.952 -0.768  -0.201
#>  7 -0.858  0.215  -0.734
#>  8  0.664  2.31    0.640
#>  9 -1.21   0.576   0.969
#> 10 -1.03   1.26    1.30 
#> # ℹ 9,990 more rows
```

# Experimental

The things here are still experimental. The API may change a lot.

## Modifiers

You can create modified identities by combining them with modifier terms
in the ACT dictionaries. These use the `traitid` equations in the
`actdata` package.

``` r
modify <- create_modifier(dictionary = list("usfullsurveyor2015", "all"), equations = list("us2010", "all"))

modify(list(M = "tired", I = "ceo"))
#> # A tibble: 1 × 3
#>   term       component ratings  
#>   <chr>      <chr>     <list>   
#> 1 tired__ceo identity  <dbl [3]>
```

``` r

out <- modify(
  data.frame(
    M = c("tired", "taciturn", "taciturn"), 
    I = c("academic", "academic", "buddhist")
  )
)

out |> tidyr::unnest_wider(ratings)
#> # A tibble: 3 × 5
#>   term               component      e      p      a
#>   <chr>              <chr>      <dbl>  <dbl>  <dbl>
#> 1 tired__academic    identity  -0.233  0.548 -1.53 
#> 2 taciturn__academic identity   0.694  0.931 -0.834
#> 3 taciturn__buddhist identity   0.409 -0.121 -1.91
```

*Note. The `create_modifier` function creates the `modify` function,
which has built-in dictionaries and equations. This setup is still
experimental.*

## Situations

This function is what powers the Analyze Events pane in
[`interactShiny`](https://acastroaraujo.shinyapps.io/interactShiny/)

``` r
situation <- define_situation(
  agent1 = interact(), ## the idea is that each agent can be set up with
  agent2 = interact()  ## different dictionaries and equations.
)
#> → dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> → equations = list(key = "us2010", group = "all")
#> → dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> → equations = list(key = "us2010", group = "all")
```

``` r

situation$history
#> NULL
```

``` r
situation$active  ## one of the two agents will be active at any given time
#> [1] "agent1"
```

``` r

## start anew
situation$start(list(A = "employer", B = "hurt", O = "employee")) ## here `agent1` is `A`
situation$history
#> $deflection
#> # A tibble: 1 × 7
#>    time agent1 agent2 A        B     O        deflection
#>   <int> <chr>  <chr>  <chr>    <chr> <chr>         <dbl>
#> 1     0 A      O      employer hurt  employee       43.5
#> 
#> $fundamentals
#> # A tibble: 1 × 15
#>    time agent1 agent2 A        B     O          Ae    Ap    Aa    Be    Bp    Ba
#>   <int> <chr>  <chr>  <chr>    <chr> <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     0 A      O      employer hurt  employ…  1.57  2.56   0.7 -3.17  1.06  0.92
#> # ℹ 3 more variables: Oe <dbl>, Op <dbl>, Oa <dbl>
#> 
#> $transients
#> # A tibble: 1 × 15
#>    time agent1 agent2 A        B     O          Ae    Ap    Aa    Be    Bp    Ba
#>   <int> <chr>  <chr>  <chr>    <chr> <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     0 A      O      employer hurt  employ… -1.79  3.05  1.62 -2.38  2.79  1.55
#> # ℹ 3 more variables: Oe <dbl>, Op <dbl>, Oa <dbl>
#> 
#> $element_wise_deflection
#> # A tibble: 1 × 15
#>    time agent1 agent2 A        B     O          Ae    Ap    Aa    Be    Bp    Ba
#>   <int> <chr>  <chr>  <chr>    <chr> <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     0 A      O      employer hurt  employ…  11.3 0.240 0.848 0.622  3.00 0.398
#> # ℹ 3 more variables: Oe <dbl>, Op <dbl>, Oa <dbl>
```

``` r

opt_beh <- situation$optimal_behavior("agent2")
opt_beh
#> # A tibble: 1 × 3
#>       Be    Bp     Ba
#>    <dbl> <dbl>  <dbl>
#> 1 -0.972  1.14 -0.576
```

``` r

situation$agent2$closest_terms(opt_beh, "behavior")[1:5]
#> pull_away_from           lure        censure     disbelieve       unfriend 
#>     0.06979871     0.21121502     0.23896718     0.25228467     0.27095720
```

``` r

situation$active <- "agent2" ## one way of changing active agent

situation$new(list(A = "employee", B = "pull_away_from", O = "employer"))
situation$history$deflection
#> # A tibble: 2 × 7
#>    time agent1 agent2 A        B              O        deflection
#>   <int> <chr>  <chr>  <chr>    <chr>          <chr>         <dbl>
#> 1     0 A      O      employer hurt           employee       43.5
#> 2     1 O      A      employee pull_away_from employer       19.2
```

``` r

situation$
  activate("agent1")$ ## another way of changing active agent
  new(list(A = "employer", B = "confront", O = "employee"))

situation$history$deflection
#> # A tibble: 3 × 7
#>    time agent1 agent2 A        B              O        deflection
#>   <int> <chr>  <chr>  <chr>    <chr>          <chr>         <dbl>
#> 1     0 A      O      employer hurt           employee       43.5
#> 2     1 O      A      employee pull_away_from employer       19.2
#> 3     2 A      O      employer confront       employee       13.3
```

This `situation` allows for “method chaining” using the `$` operator.
This type of code is not very R like.

``` r
situation$
  activate("agent1")$
  start(list(A = "employer", B = "hurt", O = "employee"))$ 
  activate("agent2")$
  new(list(A = "employee", B = "pull_away_from", O = "employer"))$
  activate("agent1")$
  new(list(A = "employer", B = "confront", O = "employee"))$
  history
#> $deflection
#> # A tibble: 3 × 7
#>    time agent1 agent2 A        B              O        deflection
#>   <int> <chr>  <chr>  <chr>    <chr>          <chr>         <dbl>
#> 1     0 A      O      employer hurt           employee       43.5
#> 2     1 O      A      employee pull_away_from employer       19.2
#> 3     2 A      O      employer confront       employee       13.3
#> 
#> $fundamentals
#> # A tibble: 3 × 15
#>    time agent1 agent2 A        B       O        Ae    Ap    Aa    Be    Bp    Ba
#>   <int> <chr>  <chr>  <chr>    <chr>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     0 A      O      employer hurt    empl…  1.57  2.56   0.7 -3.17  1.06  0.92
#> 2     1 O      A      employee pull_a… empl…  1.04 -0.37   0.2 -0.81  0.93 -0.6 
#> 3     2 A      O      employer confro… empl…  1.57  2.56   0.7  0.39  1.99  1.28
#> # ℹ 3 more variables: Oe <dbl>, Op <dbl>, Oa <dbl>
#> 
#> $transients
#> # A tibble: 3 × 15
#>    time agent1 agent2 A      B     O         Ae    Ap    Aa     Be     Bp     Ba
#>   <int> <chr>  <chr>  <chr>  <chr> <chr>  <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#> 1     0 A      O      emplo… hurt  empl… -1.79   3.05  1.62 -2.38   2.79   1.55 
#> 2     1 O      A      emplo… pull… empl… -0.486 -1.26 -1.33 -0.472 -0.449 -0.680
#> 3     2 A      O      emplo… conf… empl… -0.662  1.83  1.35 -0.607  1.76   1.28 
#> # ℹ 3 more variables: Oe <dbl>, Op <dbl>, Oa <dbl>
#> 
#> $element_wise_deflection
#> # A tibble: 3 × 15
#>    time agent1 agent2 A       B     O        Ae    Ap    Aa    Be     Bp      Ba
#>   <int> <chr>  <chr>  <chr>   <chr> <chr> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>
#> 1     0 A      O      employ… hurt  empl… 11.3  0.240 0.848 0.622 3.00   3.98e-1
#> 2     1 O      A      employ… pull… empl…  2.33 0.795 2.33  0.114 1.90   6.43e-3
#> 3     2 A      O      employ… conf… empl…  4.98 0.531 0.424 0.995 0.0511 1.94e-5
#> # ℹ 3 more variables: Oe <dbl>, Op <dbl>, Oa <dbl>
```

``` r
  
situation$history$deflection
#> # A tibble: 3 × 7
#>    time agent1 agent2 A        B              O        deflection
#>   <int> <chr>  <chr>  <chr>    <chr>          <chr>         <dbl>
#> 1     0 A      O      employer hurt           employee       43.5
#> 2     1 O      A      employee pull_away_from employer       19.2
#> 3     2 A      O      employer confront       employee       13.3
```

You can also use R’s pipe operator using the `sttn_*` functions.

``` r
df <- situation |> 
  sttn_activate("agent1") |> 
  sttn_start(list(A = "employer", B = "hurt", O = "employee")) |> 
  sttn_activate("agent2") |> 
  sttn_new(list(A = "employee", B = "pull_away_from", O = "employer")) |> 
  sttn_activate("agent1") |> 
  sttn_new(list(A = "employer", B = "confront", O = "employee")) |> 
  sttn_extract()
  
df$deflection
#> # A tibble: 3 × 7
#>    time agent1 agent2 A        B              O        deflection
#>   <int> <chr>  <chr>  <chr>    <chr>          <chr>         <dbl>
#> 1     0 A      O      employer hurt           employee       43.5
#> 2     1 O      A      employee pull_away_from employer       19.2
#> 3     2 A      O      employer confront       employee       13.3
```

## Deference Score

You can also create the deference scores discussed by Freeland & Hoey
(2018).

But this requires to modify the existing dictionary so we get the right
combination of identities and behaviors.

This seems to be the easiest way to do this:

``` r
act <- interact()
#> → dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> → equations = list(key = "us2010", group = "all")
```

``` r

defer_to <- act$dictionary |> ## save for later use
  dplyr::filter(term == "defer_to")

occ <- interact(dictionary = "occs2019")
#> → equations = list(key = "us2010", group = "all")
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
