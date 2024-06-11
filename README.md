
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
suppressMessages(library(tidyverse))

act <- interact(dictionary = "usfullsurveyor2015", equations = "us2010")
#> → equations = list(key = "us2010", group = "all")
#> → dictionary = list(dataset = "usfullsurveyor2015", group = "all")
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
get_transients(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  3.96  4.86 0.737  4.56  4.06 0.422 -3.88 -4.48 -10.5
get_element_wise_deflection(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 0.597  2.00  1.43  77.8  4.46 0.283  1.16  3.64  81.9
```

**Behaviors**

``` r
act$optimal_behavior(d, who = "actor")
#> # A tibble: 1 × 3
#>      Be    Bp     Ba
#>   <dbl> <dbl>  <dbl>
#> 1  1.90  1.18 -0.637
act$optimal_behavior(d, who = "object")
#> # A tibble: 1 × 3
#>      Be     Bp    Ba
#>   <dbl>  <dbl> <dbl>
#> 1 0.111 -0.525 0.233
```

**Re-identification**

``` r
act$reidentify(d, who = "actor")
#> # A tibble: 1 × 3
#>       Ae    Ap    Aa
#>    <dbl> <dbl> <dbl>
#> 1 -0.999  2.35 0.848
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

act$max_confirm(
  events = list(A = "god", B = "kill"), 
  solve_for = "object"
) 
#> # A tibble: 1 × 3
#>      Oe    Op    Oa
#>   <dbl> <dbl> <dbl>
#> 1 -1.26 -4.85 -11.1

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

deadbeat <- act$fundamentals("deadbeat")
deadbeat
#> # Source: usfullsurveyor2015 (all)
#> # A data frame: 1 × 5
#>   term     component     e     p     a
#> * <chr>    <chr>     <dbl> <dbl> <dbl>
#> 1 deadbeat identity   -2.8 -2.57 -1.45

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
#> $ A <chr> "academic", "academic", "academic", "academic", "academic", "academi…
#> $ B <chr> "abduct", "abduct", "abduct", "abduct", "abduct", "abduct", "abduct"…
#> $ O <chr> "abortionist", "adolescent", "adulteress", "air_force_officer", "ane…
```

Every method is designed to work in bulk. However, only the
`$deflection()` method will work fast with millions of observations.

``` r
d <- act$deflection(events)
d
#> # Event deflection
#> # A data frame: 4,000,000 × 4
#>    A        B      O                 deflection
#>  * <chr>    <chr>  <chr>                  <dbl>
#>  1 academic abduct abortionist             87.3
#>  2 academic abduct adolescent              95.9
#>  3 academic abduct adulteress             122. 
#>  4 academic abduct air_force_officer      128. 
#>  5 academic abduct anesthetist            106. 
#>  6 academic abduct applicant               93.1
#>  7 academic abduct army_chaplain          124. 
#>  8 academic abduct army_enlistee          134. 
#>  9 academic abduct army_general           129. 
#> 10 academic abduct asian                  100. 
#> # ℹ 3,999,990 more rows
get_fundamentals(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  2.34  2.26 -0.12 -3.81  1.98  1.35 -0.69  1    -0.21
#>  2  2.34  2.26 -0.12 -3.81  1.98  1.35  0.66 -0.81  2.17
#>  3  2.34  2.26 -0.12 -3.81  1.98  1.35 -2.43  0.45  0.96
#>  4  2.34  2.26 -0.12 -3.81  1.98  1.35  2.07  2.42  1.6 
#>  5  2.34  2.26 -0.12 -3.81  1.98  1.35  1.52  1.85 -0.89
#>  6  2.34  2.26 -0.12 -3.81  1.98  1.35  1.12 -0.63 -0.06
#>  7  2.34  2.26 -0.12 -3.81  1.98  1.35  2.16  1.59  0.23
#>  8  2.34  2.26 -0.12 -3.81  1.98  1.35  2.39  1.64  1.3 
#>  9  2.34  2.26 -0.12 -3.81  1.98  1.35  1.81  3.34  2.32
#> 10  2.34  2.26 -0.12 -3.81  1.98  1.35  1.41  0.39 -0.34
#> # ℹ 3,999,990 more rows
get_transients(d)
#> # A tibble: 4,000,000 × 9
#>        Ae    Ap    Aa     Be    Bp    Ba    Oe    Op    Oa
#>     <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  0.529  3.36  2.00  0.262  3.36  1.86 -2.73 -2.61 -6.72
#>  2 -0.734  3.72  2.00 -2.11   3.36  2.07 -2.47 -3.44 -5.43
#>  3  3.61   3.47  2.00  3.31   3.36  1.96 -3.07 -2.90 -6.09
#>  4 -3.10   3.08  2.00 -4.58   3.36  2.02 -2.20 -1.90 -5.74
#>  5 -3.11   3.19  2.00 -3.61   3.36  1.80 -2.30 -2.18 -7.09
#>  6 -2.21   3.69  2.00 -2.91   3.36  1.87 -2.38 -3.35 -6.64
#>  7 -3.71   3.24  2.00 -4.74   3.36  1.90 -2.18 -2.29 -6.48
#>  8 -3.70   3.23  2.00 -5.14   3.36  1.99 -2.13 -2.26 -5.90
#>  9 -2.45   2.89  2.00 -4.12   3.36  2.09 -2.25 -1.47 -5.35
#> 10 -2.75   3.48  2.00 -3.42   3.36  1.85 -2.32 -2.87 -6.79
#> # ℹ 3,999,990 more rows
get_element_wise_deflection(d)
#> # A tibble: 4,000,000 × 9
#>       Ae    Ap    Aa      Be    Bp    Ba     Oe    Op    Oa
#>    <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
#>  1  3.28 1.21   4.51 16.6     1.89 0.259  4.17  13.1   42.4
#>  2  9.45 2.14   4.51  2.90    1.89 0.523  9.79   6.93  57.8
#>  3  1.61 1.47   4.51 50.7     1.89 0.377  0.407 11.2   49.7
#>  4 29.6  0.667  4.51  0.591   1.89 0.451 18.2   18.7   53.9
#>  5 29.7  0.866  4.51  0.0384  1.89 0.200 14.6   16.2   38.4
#>  6 20.7  2.04   4.51  0.806   1.89 0.273 12.2    7.40  43.3
#>  7 36.6  0.966  4.51  0.858   1.89 0.301 18.8   15.0   45.0
#>  8 36.5  0.946  4.51  1.77    1.89 0.416 20.5   15.2   51.9
#>  9 23.0  0.400  4.51  0.0977  1.89 0.542 16.5   23.2   58.9
#> 10 25.9  1.49   4.51  0.151   1.89 0.247 13.9   10.6   41.6
#> # ℹ 3,999,990 more rows
```

Other methods might require a minute or so…

``` r
d_sub <- dplyr::slice_sample(d, n = 10000) # sample 10000 event deflections
d_sub
#> # Event deflection
#> # A data frame: 10,000 × 4
#>    A             B                O                 deflection
#>  * <chr>         <chr>            <chr>                  <dbl>
#>  1 pornographer  assail           subordinate             3.33
#>  2 wallflower    intruder         nursing_assistant      33.1 
#>  3 hairstylist   reform           interrogator            4.29
#>  4 miser         raise_the_pay_of topless_dancer         21.6 
#>  5 vigilante     scream_at        miser                  14.9 
#>  6 social_worker contemplate      referee                 3.83
#>  7 chairperson   jeer             spy                    17.7 
#>  8 waiter        frighten         scholar                32.0 
#>  9 foreman       reform           tease                   1.65
#> 10 church_deacon ban              murderess              41.8 
#> # ℹ 9,990 more rows
act$reidentify(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>        Ae     Ap      Aa
#>     <dbl>  <dbl>   <dbl>
#>  1 -0.764  0.945  1.88  
#>  2 -1.32   0.556  0.589 
#>  3  1.30   1.13  -0.0679
#>  4  0.790  2.11   0.118 
#>  5 -0.548  1.38   3.92  
#>  6  1.61   0.514 -1.36  
#>  7 -0.572 -0.292  2.36  
#>  8 -0.889  0.858  0.224 
#>  9  1.09   1.44  -0.0641
#> 10 -1.73   1.71   2.51  
#> # ℹ 9,990 more rows
act$optimal_behavior(d_sub, who = "actor")
#> # A tibble: 10,000 × 3
#>         Be     Bp     Ba
#>      <dbl>  <dbl>  <dbl>
#>  1  0.0859  0.584  0.365
#>  2 -0.153  -2.81  -1.93 
#>  3  1.00   -0.268  0.911
#>  4 -1.17   -1.89  -0.462
#>  5  0.330   0.789  0.485
#>  6  1.24    0.655  0.512
#>  7  1.31    1.53   1.26 
#>  8  1.73   -2.32   1.72 
#>  9  0.875   0.558  0.675
#> 10  0.713  -0.761  0.133
#> # ℹ 9,990 more rows
act$reidentify(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>        Oe     Op      Oa
#>     <dbl>  <dbl>   <dbl>
#>  1 -0.437 -1.50   0.771 
#>  2 -0.412 -1.85  -1.75  
#>  3  0.173 -0.639 -0.285 
#>  4 -0.548  0.522 -2.58  
#>  5 -0.353 -2.37   0.0396
#>  6  3.25  -0.419  0.675 
#>  7 -0.656 -2.04  -2.33  
#>  8 -0.563 -1.60  -3.05  
#>  9 -0.318 -0.885 -0.887 
#> 10 -0.450 -2.64  -4.21  
#> # ℹ 9,990 more rows
act$optimal_behavior(d_sub, who = "object")
#> # A tibble: 10,000 × 3
#>        Be      Bp     Ba
#>     <dbl>   <dbl>  <dbl>
#>  1 -0.499 -0.254  -0.776
#>  2  1.39   3.68    0.105
#>  3  0.453  1.03    1.34 
#>  4 -0.618  0.652   1.55 
#>  5 -1.00  -0.0762 -1.42 
#>  6  1.11   1.26    1.06 
#>  7 -0.239  0.861  -0.855
#>  8  1.85   4.12   -0.135
#>  9 -0.642 -0.155   0.897
#> 10 -0.199  0.889   0.860
#> # ℹ 9,990 more rows

act$max_confirm(d_sub[c("A", "O")], solve_for = "behavior")
#> # A tibble: 10,000 × 3
#>         Be     Bp      Ba
#>      <dbl>  <dbl>   <dbl>
#>  1 -0.515   0.530  0.545 
#>  2 -0.0114 -0.977 -1.17  
#>  3  0.784   0.439  0.696 
#>  4 -1.51   -0.845 -1.14  
#>  5  0.0776  1.04   1.06  
#>  6  1.11    1.08  -0.0643
#>  7  0.869   1.72   0.453 
#>  8  0.420   0.143  0.487 
#>  9  0.831   1.07   0.517 
#> 10  0.760   0.915 -0.166 
#> # ℹ 9,990 more rows
act$max_confirm(d_sub[c("A", "B")], solve_for = "object")
#> # A tibble: 10,000 × 3
#>        Oe     Op      Oa
#>     <dbl>  <dbl>   <dbl>
#>  1 -0.437 -1.50   0.771 
#>  2 -0.412 -1.85  -1.75  
#>  3  0.173 -0.639 -0.285 
#>  4 -0.548  0.522 -2.58  
#>  5 -0.353 -2.37   0.0396
#>  6  3.25  -0.419  0.675 
#>  7 -0.656 -2.04  -2.33  
#>  8 -0.563 -1.60  -3.05  
#>  9 -0.318 -0.885 -0.887 
#> 10 -0.450 -2.64  -4.21  
#> # ℹ 9,990 more rows
act$max_confirm(d_sub[c("B", "O")], solve_for = "actor")
#> # A tibble: 10,000 × 3
#>        Ae     Ap      Aa
#>     <dbl>  <dbl>   <dbl>
#>  1 -0.764  0.945  1.88  
#>  2 -1.32   0.556  0.589 
#>  3  1.30   1.13  -0.0679
#>  4  0.790  2.11   0.118 
#>  5 -0.548  1.38   3.92  
#>  6  1.61   0.514 -1.36  
#>  7 -0.572 -0.292  2.36  
#>  8 -0.889  0.858  0.224 
#>  9  1.09   1.44  -0.0641
#> 10 -1.73   1.71   2.51  
#> # ℹ 9,990 more rows
```

## Deference Score

You can also create the deference scores discussed by Freeland & Hoey
(2018).

But this requires to set up a new dictionary, which you’ll have to do
with the help of some external packages.

For example:

``` r
occupation_ratings <- actdata::epa_subset(dataset = "occs2019") |> 
  dplyr::select(term, component, E, P, A) |> 
  dplyr::rename_all(tolower) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(ratings = list(c(e = e, p = p, a = a))) |> 
  dplyr::ungroup() |> 
  dplyr::select(term, component, ratings)

defer_to <- act$dictionary |> 
  dplyr::filter(term == "defer_to")

# Then you'll have to replace the original dictionary.

act$dictionary <- dplyr::bind_rows(defer_to, occupation_ratings)
#> ✔ added new dictionary
```

Note that a message appeared signaling that the replacement was
succesful.

``` r
act
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
  A = occupation_ratings$term,
  B = "defer_to",
  O = occupation_ratings$term
)

output <- act$deflection(events)

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
