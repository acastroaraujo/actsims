
test_that("subsetting methods work in attribute datasets", {

  act <- interact(dictionary = "usfullsurveyor2015", equations = "us2010")
  act

  identities <- act$dictionary$term[act$dictionary$component == "identity"]
  behaviors <- act$dictionary$term[act$dictionary$component == "behavior"]

  events <- expand.grid(
    A = sample(identities, 10),
    B = sample(behaviors, 10),
    O = sample(identities, 10)
  )

  d <- act$deflection(events)
  out <- d[1, ]

  expect_equal(nrow(out), nrow(get_fundamentals(out)))
  expect_equal(nrow(out), nrow(get_transients(out)))
  expect_equal(nrow(out), nrow(get_element_wise_deflection(out)))

  out <- d[sample(nrow(d), 1), ]
  sub <- act$fundamentals(out$A) |>
    dplyr::filter(component == "identity")

  expect_equal(
    unname(unlist(sub[c("e", "p", "a")])),
    unname(unlist(get_fundamentals(out)[1:3]))
  )

  sub <- act$fundamentals(out$B) |>
    dplyr::filter(component == "behavior")

  expect_equal(
    unname(unlist(sub[c("e", "p", "a")])),
    unname(unlist(get_fundamentals(out)[4:6]))
  )

  sub <- act$fundamentals(out$O) |>
    dplyr::filter(component == "identity")

  expect_equal(
    unname(unlist(sub[c("e", "p", "a")])),
    unname(unlist(get_fundamentals(out)[7:9]))
  )

  events <- expand.grid(
    A = sample(identities, 10),
    B = sample(behaviors, 10),
    O = sample(identities, 10)
  )

  d <- act$deflection(events)
  out <- d[1, ]

  expect_equal(nrow(out), nrow(get_fundamentals(out)))
  expect_equal(nrow(out), nrow(get_transients(out)))
  expect_equal(nrow(out), nrow(get_element_wise_deflection(out)))

  out <- d[sample(nrow(d), 1), ]
  sub <- act$fundamentals(out$A) |>
    dplyr::filter(component == "identity")

  expect_equal(
    unname(unlist(sub[c("e", "p", "a")])),
    unname(unlist(get_fundamentals(out)[1:3]))
  )

  sub <- act$fundamentals(out$B) |>
    dplyr::filter(component == "behavior")

  expect_equal(
    unname(unlist(sub[c("e", "p", "a")])),
    unname(unlist(get_fundamentals(out)[4:6]))
  )

  sub <- act$fundamentals(out$O) |>
    dplyr::filter(component == "identity")

  expect_equal(
    unname(unlist(sub[c("e", "p", "a")])),
    unname(unlist(get_fundamentals(out)[7:9]))
  )

})
