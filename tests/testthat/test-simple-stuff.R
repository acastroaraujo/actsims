

test_that("ceo abuse god", {

  ## The Java App settings are
  ## - Culture: USA Combined Surveyor 2015
  ## - Mode: Gender Comparison
  ## - Both Person 1 and Person 2 are set to male
  ## - The equations are U.S.A. 2010 (but double check, cause the app is buggy)

  act <- interact(dictionary = "usfullsurveyor2015", equations = "us2010")
  act
  events <- data.frame(A = "ceo", B = "abuse", O = "god")
  d <- act$deflection(events)

  expect_equal(round(d$deflection, 2), 95.82)
  expect_equal(d$deflection, rowSums(get_element_wise_deflection(d)))
  expect_equal(d$deflection, sum((get_fundamentals(d) - get_transients(d))^2))

  expect_equal(
    ## corresponds to Actor Labels in Java app
    object = act$reidentify(d, who = "actor") |> round(3),
    expected = dplyr::tibble(Ae = -0.941, Ap = -0.085, Aa = -0.72)
  )

  expect_equal(
    ## corresponds to Object Labels in Java app
    object = act$reidentify(d, who = "object") |> round(3),
    expected = dplyr::tibble(Oe = -0.491, Op = -3.605, Oa = -2.196)
  )

  expect_equal(
    ## corresponds to Actor Behaviors in Java App
    object = act$optimal_behavior(d, who = "actor") |> round(3),
    expected = dplyr::tibble(Be = -0.193, Bp = -0.175, Ba = 1.592)
  )

  expect_equal(
    ## corresponds to Object Behaviors in Java App
    object = act$optimal_behavior(d, who = "object") |> round(3),
    expected = dplyr::tibble(Be = 2.85, Bp = 5.422, Ba = -1.458)
  )

  out <- act$max_confirm(events = list(A = "army_enlistee", O = "american"), solve_for = "behavior")
  expect_equal(
    object = round(out, 3),
    expected = dplyr::tibble(Be = 1.085, Bp = 1.316, Ba = 0.738)
  )

  out <- act$max_confirm(events = list(A = "army_enlistee", B = "abandon"), solve_for = "object")
  expect_equal(
    object = round(out, 3),
    expected = dplyr::tibble(Oe = -1.255, Op = -2.652, Oa = -7.087)
  )

  out <- act$max_confirm(events = list(O = "american", B = "abandon"), solve_for = "actor")
  expect_equal(
    object = round(out, 3),
    expected = dplyr::tibble(Ae = -1.401, Ap = -0.475, Aa = 0.44)
  )

})

test_that("errors", {
  expect_error(interact(equations = c("nc1978", "all")))
  expect_error(interact(equations = c("us2010", "female")))
})




