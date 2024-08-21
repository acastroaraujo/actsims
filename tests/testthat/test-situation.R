
test_that("boilerplate situation has corresponding deflection scores", {

  situation <- define_situation(interact(), interact())

  df <- situation |>
    sttn_start(list(A = "employer", B = "hurt", O = "employee")) |>
    sttn_activate("agent2") |>
    sttn_new(list(A = "employee", B = "pull_away_from", O = "employer")) |>
    sttn_activate("agent1") |>
    sttn_new(list(A = "employer", B = "confront", O = "employee")) |>
    sttn_extract()

  expect_equal(situation$time, 2L)
  expect_equal(situation$active, "agent1")

  situation$activate("agent2")

  expect_equal(situation$active, "agent2")

  out <- round(df$deflection$deflection, 2)

  expect_equal(out, c(43.54, 19.23, 13.29))

})
