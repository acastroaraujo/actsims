## code to prepare `dicts` dataset goes here


library(tidyverse)

usfullsurveyor2015 <- actdata::epa_subset(dataset = "usfullsurveyor2015") |>
  filter(group == "all") |>
  rename_all(tolower) |>
  rowwise() |>
  mutate(ratings = list(c(e = e, p = p, a = a))) |>
  mutate(n = list(c(e = n_e, p = n_p, a = n_a))) |>
  mutate(sd = list(c(e = sd_e, p = sd_p, a = sd_a))) |>
  select(term, component, ratings, n, sd) |>
  ungroup()

equations <- actdata::equations

dictionaries <- actdata::dataset_keys()

usethis::use_data(usfullsurveyor2015, equations, dictionaries, internal = TRUE, overwrite = TRUE)
