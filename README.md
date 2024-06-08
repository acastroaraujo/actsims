
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actsims

<!-- badges: start -->

[![R-CMD-check](https://github.com/acastroaraujo/actsims/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/acastroaraujo/actsims/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`actsims` is an ACT package used for internal development of
`interactShiny`

## Installation

You can install the development version of actsims like so:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/actsims")
```

## Usage

- `actsims` is meant to be *fast* and *easy* to use.
- It uses the R6 OOP system to keep better track of EPA ratings and
  transient impression equations.
- It is integrated with the `actdata` package.

Create an “InteRactModel” R6 object.

``` r
library(actsims)
#> Loading required package: actdata
suppressMessages(library(tidyverse))

act <- interact(dictionary = "usfullsurveyor2015", equations = "us2010")
act
#> 
#> ── Interact Analysis ───────────────────────────────────────────────────────────
#> ℹ Dictionary: usfullsurveyor2015
#> ✔ group: all
#> ℹ Equations: us2010
#> ✔ group: all
#> ✔ type: impressionabo
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

This object comes with built in methods which you can access via the `$`
operator.

## Methods

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

## Closest term to deadbeat (stored in d object)
deadbeat <- get_fundamentals(d) |> select(matches("O")) 
deadbeat
#> # A tibble: 1 × 3
#>      Oe    Op    Oa
#>   <dbl> <dbl> <dbl>
#> 1  -2.8 -2.57 -1.45
act$closest_terms(epa = deadbeat, component = "modifier", max_dist = 0.5)
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
#> $ A <chr> "acquaintance", "acquaintance", "acquaintance", "acquaintance", "acq…
#> $ B <chr> "acknowledge", "acknowledge", "acknowledge", "acknowledge", "acknowl…
#> $ O <chr> "adopted_son", "air_force_reservist", "alumnus", "angel", "anti_semi…
```

Every method, with the exception of `$closest_terms()`, is designed to
work in bulk. However, only the `$deflection()` method will work fast
with millions of observations. Other methods might require a minute or
so…

``` r
d <- act$deflection(events)
d
#> # Event deflection
#> # A data frame: 4,000,000 × 4
#>    A            B           O                   deflection
#>  * <chr>        <chr>       <chr>                    <dbl>
#>  1 acquaintance acknowledge adopted_son               5.98
#>  2 acquaintance acknowledge air_force_reservist       5.61
#>  3 acquaintance acknowledge alumnus                   6.04
#>  4 acquaintance acknowledge angel                    11.4 
#>  5 acquaintance acknowledge anti_semite               8.35
#>  6 acquaintance acknowledge apprentice                6.69
#>  7 acquaintance acknowledge army_general              5.47
#>  8 acquaintance acknowledge asian                     7.23
#>  9 acquaintance acknowledge asian_woman               7.04
#> 10 acquaintance acknowledge ass                       5.99
#> # ℹ 3,999,990 more rows
```

## Deference Score

You can also create the deference scores discussed by Freeland & Hoey
(2018). But this requires to set up a new dictionary.

You’ll have to do with the help of some external packages.

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
#> ✔ group: ?
#> ℹ Equations: us2010
#> ✔ group: all
#> ✔ type: impressionabo
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
  summarize(avg = mean(deflection), sd = sd(deflection)) |> 
  arrange(desc(avg)) 
#> # A tibble: 650 × 3
#>    A                            avg    sd
#>    <chr>                      <dbl> <dbl>
#>  1 firefighter                 16.7  1.50
#>  2 fireman                     15.0  1.47
#>  3 paramedic                   13.9  1.45
#>  4 professional_athlete        12.4  1.40
#>  5 fire_department_lieutenant  12.3  1.45
#>  6 ambulance_driver            11.4  1.38
#>  7 auctioneer                  11.1  1.31
#>  8 dynamite_blaster            11.0  1.34
#>  9 911_dispatcher              10.9  1.40
#> 10 surgeon                     10.6  1.51
#> # ℹ 640 more rows
```
