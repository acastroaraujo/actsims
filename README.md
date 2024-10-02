
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
suppressMessages(library(tidyverse, quietly = TRUE))
library(actsims)
#> Loading required package: actdata
```

``` r

act <- interact(dictionary = "usfullsurveyor2015", equations = "us2010")
#> ✔ dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> ✔ equations = list(key = "us2010", group = "all")
```

``` r
act
#> 
#> ── Interact Analysis ───────────────────────────────────────────────────────────
#> ℹ Dictionary: usfullsurveyor2015 (group: all)
#> ℹ Equations: us2010
#>   impressionabo (group: all)
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
#> ✔ dictionary = list(dataset = "indiana2003", group = "all")
#> ✔ equations = list(key = "nc1978", group = "all")
#> ! equations groups: male and female
#> Error:
#> ! `all` not found in `nc1978` equations in `actdata` package
```

You can change the defaults by specifying a second element in either
argument.

For example:

``` r
interact(dictionary = "indiana2003", equations = list("nc1978", "male"))
#> ✔ dictionary = list(dataset = "indiana2003", group = "all")
#> 
#> ── Interact Analysis ───────────────────────────────────────────────────────────
#> ℹ Dictionary: indiana2003 (group: all)
#> ℹ Equations: nc1978
#>   impressionabo (group: male)
```

## Methods

`InteRactModel` objects come with built in methods which you can access
via the `$` operator.

**Fundamentals.**

``` r
act$fundamentals("mother")
#> # Source:  ()
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
#> # Source:  ()
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
#> $ B <chr> "abuse", "abuse", "abuse", "abuse", "abuse", "abuse", "abuse", "abus…
#> $ O <chr> "addict", "adolescent", "adopted_son", "air_force_enlistee", "air_fo…
```

Every method is designed to work in bulk. However, only the
`$deflection()` method will work fast with millions of observations.

``` r
d <- act$deflection(events)
d
#> # Event deflection
#> # A data frame: 4,000,000 × 4
#>    A           B     O                  deflection
#>  * <chr>       <chr> <chr>                   <dbl>
#>  1 abortionist abuse addict                   31.2
#>  2 abortionist abuse adolescent               16.6
#>  3 abortionist abuse adopted_son              30.7
#>  4 abortionist abuse air_force_enlistee       38.5
#>  5 abortionist abuse air_force_officer        39.8
#>  6 abortionist abuse ally                     48.4
#>  7 abortionist abuse american                 28.7
#>  8 abortionist abuse architect                33.9
#>  9 abortionist abuse asian_man                22.7
#> 10 abortionist abuse asian_woman              26.9
#> # ℹ 3,999,990 more rows
```

``` r
get_fundamentals(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 -0.69     1 -0.21 -3.85  0.61  1.51 -2.23 -2.36  0.19
#>  2 -0.69     1 -0.21 -3.85  0.61  1.51  0.66 -0.81  2.17
#>  3 -0.69     1 -0.21 -3.85  0.61  1.51  1.65  0.41  0.28
#>  4 -0.69     1 -0.21 -3.85  0.61  1.51  2.06  1.56  0.86
#>  5 -0.69     1 -0.21 -3.85  0.61  1.51  2.07  2.42  1.6 
#>  6 -0.69     1 -0.21 -3.85  0.61  1.51  2.34  2.15 -0.16
#>  7 -0.69     1 -0.21 -3.85  0.61  1.51  1.41  1.81  1.77
#>  8 -0.69     1 -0.21 -3.85  0.61  1.51  1.52  1.7  -0.28
#>  9 -0.69     1 -0.21 -3.85  0.61  1.51  0.82  0.2  -0.35
#> 10 -0.69     1 -0.21 -3.85  0.61  1.51  1.25  0.32 -0.27
#> # ℹ 3,999,990 more rows
```

``` r
get_transients(d)
#> # A tibble: 4,000,000 × 9
#>        Ae    Ap    Aa     Be    Bp    Ba      Oe    Op     Oa
#>     <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>
#>  1  0.496  2.48  1.96  0.571  1.82  2.03 -1.64   -3.49 0.345 
#>  2 -2.43   2.17  1.96 -3.47   1.82  2.21 -0.495  -2.67 1.41  
#>  3 -4.33   1.92  1.96 -4.85   1.82  2.04 -0.104  -2.07 0.393 
#>  4 -4.64   1.69  1.96 -5.42   1.82  2.09  0.0577 -1.51 0.706 
#>  5 -4.39   1.52  1.96 -5.43   1.82  2.16  0.0617 -1.11 1.11  
#>  6 -5.34   1.57  1.96 -5.81   1.82  2.00  0.168  -1.23 0.156 
#>  7 -3.51   1.64  1.96 -4.51   1.82  2.17 -0.199  -1.42 1.20  
#>  8 -4.36   1.66  1.96 -4.67   1.82  1.99 -0.156  -1.46 0.0908
#>  9 -3.51   1.96  1.96 -3.69   1.82  1.98 -0.432  -2.19 0.0530
#> 10 -4.02   1.94  1.96 -4.29   1.82  1.99 -0.262  -2.12 0.0962
#> # ℹ 3,999,990 more rows
```

``` r
get_element_wise_deflection(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa      Be    Bp    Ba    Oe    Op     Oa
#>    <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1  1.41 2.18   4.71 19.5     1.46 0.272 0.352  1.28 0.0239
#>  2  3.04 1.36   4.71  0.148   1.46 0.490 1.33   3.46 0.572 
#>  3 13.2  0.851  4.71  0.997   1.46 0.281 3.08   6.13 0.0128
#>  4 15.6  0.480  4.71  2.47    1.46 0.339 4.01   9.44 0.0236
#>  5 13.7  0.271  4.71  2.51    1.46 0.421 4.03  12.4  0.244 
#>  6 21.6  0.330  4.71  3.85    1.46 0.240 4.72  11.4  0.0996
#>  7  7.95 0.413  4.71  0.440   1.46 0.441 2.59  10.4  0.327 
#>  8 13.4  0.442  4.71  0.667   1.46 0.230 2.81  10.0  0.138 
#>  9  7.93 0.931  4.71  0.0259  1.46 0.224 1.57   5.72 0.162 
#> 10 11.1  0.885  4.71  0.193   1.46 0.231 2.29   5.96 0.134 
#> # ℹ 3,999,990 more rows
```

Other methods might require a minute or so…

``` r
d_sub <- dplyr::slice_sample(d, n = 10000) # sample 10000 event deflections
d_sub
#> # Event deflection
#> # A data frame: 10,000 × 4
#>    A              B                        O               deflection
#>  * <chr>          <chr>                    <chr>                <dbl>
#>  1 conformist     disable                  bachelor             14.5 
#>  2 loser          admonish                 creditor              8.46
#>  3 clod           cripple                  capitalist           20.6 
#>  4 philanthropist cut_someone_down_to_size white_man            22.5 
#>  5 traitor        disagree_with            firstborn            11.2 
#>  6 mafioso        haunt                    german               13.9 
#>  7 snot           punish                   bigot                10.1 
#>  8 hothead        join                     student_teacher      15.3 
#>  9 deadbeat_dad   hold_up                  shoplifter           14.3 
#> 10 neighbor       confide_in               educator              5.32
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>        Ae    Ap      Aa
#>     <dbl> <dbl>   <dbl>
#>  1 -1.42  0.525  1.20  
#>  2 -0.536 0.122  0.694 
#>  3 -1.45  1.01   1.49  
#>  4 -1.16  0.691  1.37  
#>  5  0.698 0.761  0.660 
#>  6 -1.81  1.63  -0.0746
#>  7 -1.85  1.34   1.48  
#>  8  0.794 1.36   0.339 
#>  9 -1.29  1.12   1.32  
#> 10  0.966 0.941 -0.876 
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>         Be     Bp     Ba
#>      <dbl>  <dbl>  <dbl>
#>  1 -0.295  -2.46  -0.977
#>  2 -0.781  -2.61  -1.30 
#>  3 -0.0379 -2.22  -0.412
#>  4  2.94    1.43   0.690
#>  5 -0.860  -1.42  -0.956
#>  6 -0.116  -0.778  0.677
#>  7 -0.852  -1.91   0.636
#>  8 -1.07   -1.30   2.75 
#>  9 -0.384  -1.31  -1.36 
#> 10  0.0864  0.217  1.07 
#> # ℹ 9,990 more rows
```

``` r
act$reidentify(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>          Oe      Op      Oa
#>       <dbl>   <dbl>   <dbl>
#>  1 -0.101   -1.75    1.27  
#>  2 -1.08    -0.0351 -0.0601
#>  3 -0.00135 -2.87    1.80  
#>  4 -0.537   -2.03   -4.19  
#>  5 -1.52    -0.835  -0.888 
#>  6 -0.258   -3.19   -0.781 
#>  7 -0.571   -1.14    0.307 
#>  8 -1.81     0.702  -2.95  
#>  9 -0.340   -1.26    2.46  
#> 10  2.53     0.249   2.27  
#> # ℹ 9,990 more rows
```

``` r
act$optimal_behavior(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>        Be     Bp     Ba
#>     <dbl>  <dbl>  <dbl>
#>  1  0.212  2.22   0.534
#>  2 -0.455  1.84   0.139
#>  3  0.657  2.64   0.875
#>  4  1.57   1.49   1.29 
#>  5  0.160  1.21   0.524
#>  6  0.221  1.32   0.932
#>  7 -1.54   0.470  0.815
#>  8  0.875  0.489  1.65 
#>  9 -1.06  -0.118 -1.37 
#> 10  1.06   2.18  -0.408
#> # ℹ 9,990 more rows
```

``` r

act$max_confirm(d_sub[c("A", "O")], solve_for = "behavior")
#> # A tibble: 10,000 × 3
#>         Be     Bp         Ba
#>      <dbl>  <dbl>      <dbl>
#>  1 -1.03   -1.71  -0.650    
#>  2 -1.10   -1.72  -1.19     
#>  3 -0.589  -1.04  -0.128    
#>  4  0.917   1.57   0.0000974
#>  5 -0.456  -0.793 -0.353    
#>  6 -0.0796  0.297  0.466    
#>  7 -0.959  -0.639  0.449    
#>  8 -0.0967 -0.709  1.78     
#>  9 -0.688  -0.152 -0.959    
#> 10  0.827   0.619  0.0187   
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("A", "B")], solve_for = "object")
#> # A tibble: 10,000 × 3
#>          Oe      Op      Oa
#>       <dbl>   <dbl>   <dbl>
#>  1 -0.101   -1.75    1.27  
#>  2 -1.08    -0.0351 -0.0601
#>  3 -0.00135 -2.87    1.80  
#>  4 -0.537   -2.03   -4.19  
#>  5 -1.52    -0.835  -0.888 
#>  6 -0.258   -3.19   -0.781 
#>  7 -0.571   -1.14    0.307 
#>  8 -1.81     0.702  -2.95  
#>  9 -0.340   -1.26    2.46  
#> 10  2.53     0.249   2.27  
#> # ℹ 9,990 more rows
```

``` r
act$max_confirm(d_sub[c("B", "O")], solve_for = "actor")
#> # A tibble: 10,000 × 3
#>        Ae    Ap      Aa
#>     <dbl> <dbl>   <dbl>
#>  1 -1.42  0.525  1.20  
#>  2 -0.536 0.122  0.694 
#>  3 -1.45  1.01   1.49  
#>  4 -1.16  0.691  1.37  
#>  5  0.698 0.761  0.660 
#>  6 -1.81  1.63  -0.0746
#>  7 -1.85  1.34   1.48  
#>  8  0.794 1.36   0.339 
#>  9 -1.29  1.12   1.32  
#> 10  0.966 0.941 -0.876 
#> # ℹ 9,990 more rows
```

# Experimental

The things here are still experimental. The API may change a lot.

## Modify Identity

You can create modified identities by combining them with modifier terms
in the ACT dictionaries. These use the `traitid` equations in the
`actdata` package.

The `$modify_identity` method does not work unless you first specify the
`traitid` equation.

``` r
act$modify_identity(list(M = "angry", I = "doctor"))
#> Error in `act$modify_identity()`:
#> ! must first set up `traitid` equation with the `$add_equation` method
```

``` r
act$add_equation(type = "traitid", group = "all")
#> ✔ traitid = list(key = "us2010", group = "all")
```

``` r
act$modify_identity(list(M = "angry", I = "doctor")) 
#> # A tibble: 1 × 3
#>   term          component ratings  
#>   <chr>         <chr>     <list>   
#> 1 angry__doctor identity  <dbl [3]>
```

``` r

act$fundamentals("academic")
#> # Source:  ()
#> # A data frame: 1 × 5
#>   term     component     e     p     a
#> * <chr>    <chr>     <dbl> <dbl> <dbl>
#> 1 academic identity   2.34  2.26 -0.12
```

``` r

grid <- data.frame(
  M = c("tired", "taciturn", "angry", "happy"), 
  I = c("academic", "academic", "academic", "academic")
)

out <- act$modify_identity(grid) 
out |> tidyr::unnest_wider(ratings)
#> # A tibble: 4 × 5
#>   term               component      e     p      a
#>   <chr>              <chr>      <dbl> <dbl>  <dbl>
#> 1 tired__academic    identity  -0.233 0.548 -1.53 
#> 2 taciturn__academic identity   0.694 0.931 -0.834
#> 3 angry__academic    identity  -1.11  1.83   0.786
#> 4 happy__academic    identity   3.10  2.42   0.276
```

## Characteristic Emotion

``` r
act$add_equation("emotionid", "male")
#> ✔ emotionid = list(key = "us2010", group = "male")
```

``` r
act$characteristic_emotion(list(I = "brute"))
#> # A tibble: 1 × 3
#>      Me    Mp    Ma
#>   <dbl> <dbl> <dbl>
#> 1 -1.40  3.05  1.46
```

``` r

grid <- act$dictionary |> 
  dplyr::filter(component == "identity") |> 
  dplyr::sample_n(size = 10) |> 
  dplyr::select(I = term)

grid
#> # A tibble: 10 × 1
#>    I               
#>    <chr>           
#>  1 governor        
#>  2 blogger         
#>  3 dolt            
#>  4 lecturer        
#>  5 liberal         
#>  6 stranger        
#>  7 adopted_daughter
#>  8 guy             
#>  9 pimp            
#> 10 bookkeeper
```

``` r

act$characteristic_emotion(grid)
#> # A tibble: 10 × 3
#>        Me     Mp     Ma
#>     <dbl>  <dbl>  <dbl>
#>  1  2.13   0.779  0.572
#>  2  1.62   0.971  1.21 
#>  3  1.47   0.685 -0.503
#>  4 -0.692 -1.21  -0.443
#>  5  1.61   3.05   1.07 
#>  6  1.74   2.02   1.20 
#>  7  1.88   2.39   0.770
#>  8  1.82   1.28   1.34 
#>  9 -9.14   2.42   0.129
#> 10  0.740  0.385  0.337
```

## Situations

This function is what powers the Analyze Events pane in
[`interactShiny`](https://acastroaraujo.shinyapps.io/interactShiny/)

``` r
situation <- define_situation(
  agent1 = interact(), ## the idea is that each agent can be set up with
  agent2 = interact()  ## different dictionaries and equations.
)
#> ✔ dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> ✔ equations = list(key = "us2010", group = "all")
#> ✔ dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> ✔ equations = list(key = "us2010", group = "all")
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

situation$activate("agent1") ## another way of changing active agent
situation$new(list(A = "employer", B = "confront", O = "employee"))

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

Instead, you can use R’s pipe operator in tandem with the `sttn_*`
functions.

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
#> ✔ dictionary = list(dataset = "usfullsurveyor2015", group = "all")
#> ✔ equations = list(key = "us2010", group = "all")
```

``` r

defer_to <- act$dictionary |> ## save for later use
  dplyr::filter(term == "defer_to")

occ <- interact(dictionary = "occs2019")
#> ✔ equations = list(key = "us2010", group = "all")
#> ✔ dictionary = list(dataset = "occs2019", group = "all")
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
#> ℹ Dictionary: External [!] (group: ?)
#> ℹ Equations: us2010
#>   impressionabo (group: all)
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
  filter(A != O) |> 
  group_by(A) |> 
  summarize(avg = mean(deflection)) |> 
  arrange(desc(avg)) 
#> # A tibble: 650 × 2
#>    A                            avg
#>    <chr>                      <dbl>
#>  1 firefighter                 16.6
#>  2 fireman                     15.0
#>  3 paramedic                   13.9
#>  4 professional_athlete        12.4
#>  5 fire_department_lieutenant  12.3
#>  6 ambulance_driver            11.4
#>  7 auctioneer                  11.1
#>  8 dynamite_blaster            10.9
#>  9 911_dispatcher              10.9
#> 10 surgeon                     10.6
#> # ℹ 640 more rows
```
