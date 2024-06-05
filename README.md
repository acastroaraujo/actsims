
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actsims

<!-- badges: start -->
<!-- badges: end -->

`actsims` is an ACT package used for internal development of
`interactShiny`

**It is not meant for public consumption.**

## Installation

However, if you must insist…

You can install the development version of actsims like so:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/actsims")
```

## Performance

- `actsims` is meant to be fast and easy to use.
- It uses the R6 OOP system to keep better track of EPA ratings and
  transient impression equations.
- It is integrated with the `actdata` package.

Create an “InteRact” R6 object.

``` r
library(actsims)
#> Loading required package: actdata
suppressMessages(library(tidyverse))

act <- build_interact(dictionary = "usfullsurveyor2015", equation = "us2010")
act
#> 
#> ── Interact Analysis ───────────────────────────────────────────────────────────
#> ℹ Dictionary: usfullsurveyor2015
#> ℹ group: all
#> ℹ Equations: us2010
```

This object comes with built in functions.

Deflection scores.

``` r
act$deflection(list(A = "deadbeat", B = "kill", O = "god"))
#> # Event deflection
#> # A data frame: 1 × 4
#>   A        B     O     deflection
#> * <chr>    <chr> <chr>      <dbl>
#> 1 deadbeat kill  god         137.
act$deflection(list(A = "deadbeat", B = "kill", O = "deadbeat"))
#> # Event deflection
#> # A data frame: 1 × 4
#>   A        B     O        deflection
#> * <chr>    <chr> <chr>         <dbl>
#> 1 deadbeat kill  deadbeat       97.8
```

You can also extract useful metadata from these scores.

``` r
d <- act$deflection(list(A = "ceo", B = "advise", O = "benefactor"))
d
#> # Event deflection
#> # A data frame: 1 × 4
#>   A     B      O          deflection
#> * <chr> <chr>  <chr>           <dbl>
#> 1 ceo   advise benefactor       6.95
get_fundamentals(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  0.71  3.22  1.48  2.57  2.28  0.28  1.97  1.98   0.1
get_transients(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp    Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  1.92  2.05 0.387  1.80  2.72 0.387  2.46 0.860 0.899
get_element_wise_deflection(d)
#> # A tibble: 1 × 9
#>      Ae    Ap    Aa    Be    Bp     Ba    Oe    Op    Oa
#>   <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1  1.46  1.37  1.20 0.590 0.190 0.0115 0.236  1.25 0.639
```

You can also do other stuff (more to come).

``` r
act$reidentify_object(d)
#> # A tibble: 1 × 3
#>      Oe    Op    Oa
#>   <dbl> <dbl> <dbl>
#> 1 0.763 0.746 0.358
```

## Many many

You can use a grid of events to estimate multiple deflection scores
simultaneously.

For example, the following `events` object contains more than 2 million
ABO events randomly created from the `usfullsurveyor2015` dictionary.

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
#> $ B <chr> "beat", "beat", "beat", "beat", "beat", "beat", "beat", "beat", "bea…
#> $ O <chr> "abortionist", "academic", "accomplice", "accountant", "accounting_c…
```

Now you can repeat the earlier steps.

``` r
d <- act$deflection(events)
d
#> # Event deflection
#> # A data frame: 2,589,123 × 4
#>    A           B     O                deflection
#>  * <chr>       <chr> <chr>                 <dbl>
#>  1 abortionist beat  abortionist            12.2
#>  2 abortionist beat  academic               28.4
#>  3 abortionist beat  accomplice             10.4
#>  4 abortionist beat  accountant             15.7
#>  5 abortionist beat  accounting_clerk       14.7
#>  6 abortionist beat  accused                11.3
#>  7 abortionist beat  acquaintance           13.8
#>  8 abortionist beat  actor                  15.1
#>  9 abortionist beat  addict                 15.9
#> 10 abortionist beat  adolescent             11.3
#> # ℹ 2,589,113 more rows
```

## Deference Score

You can also create the deference scores discussed by Freeland & Hoey
(2018). But this requires to set up a new dictionary.

Which you’ll have to do with the help of external packages.

This is just way to do this.

``` r
occupation_ratings <- actdata::epa_subset(dataset = "occs2019") |> 
  dplyr::select(term, component, E, P, A) |> 
  dplyr::rename_all(tolower) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(ratings = list(c(e = e, p = p, a = a))) |> 
  dplyr::ungroup() |> 
  dplyr::select(term, component, ratings)

occupation_ratings
#> # A tibble: 650 × 3
#>    term                             component ratings  
#>    <chr>                            <chr>     <list>   
#>  1 911_dispatcher                   identity  <dbl [3]>
#>  2 accountant                       identity  <dbl [3]>
#>  3 actor                            identity  <dbl [3]>
#>  4 actress                          identity  <dbl [3]>
#>  5 actuary_for_an_insurance_company identity  <dbl [3]>
#>  6 acupuncturist                    identity  <dbl [3]>
#>  7 administrative_assistant         identity  <dbl [3]>
#>  8 advertising_executive            identity  <dbl [3]>
#>  9 advertising_salesman             identity  <dbl [3]>
#> 10 aerobics_instructor              identity  <dbl [3]>
#> # ℹ 640 more rows

defer_to <- act$dictionary |> 
  dplyr::filter(term == "defer_to")
```

Then you’ll have to replace the original dictionary.

``` r
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
#> ℹ group: all
#> ℹ Equations: us2010
```

Now you just create another grid of events, calculate the deflection
scores, and average over the As.

``` r
events <- crossing(
  A = occupation_ratings$term,
  B = "defer_to",
  O = occupation_ratings$term
)

output <- act$deflection(events)

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
