
test_that("max confirm replicates interact Indiana", {

  obj <- interact(dictionary = "indiana2003", equations = c("nc1978", "male"))

  events <- data.frame(A = c("abortionist", "adolescent"), O = c("adolescent", "abortionist"))
  out <- obj$max_confirm(events, solve_for = "behavior")

  expect_equal(
    object = round(out[1, ], 3),
    expected = dplyr::tibble(Be = -0.536, Bp = 0.980, Ba = -0.360)
  )

  expect_equal(
    object = round(out[2, ], 3),
    expected = dplyr::tibble(Be = 0.156, Bp = -0.461, Ba = 1.14)
  )


  obj <- interact(list("indiana2003", "male"), list("nc1978", "male"))
  events <- data.frame(A = c("abortionist", "adolescent"), O = c("adolescent", "abortionist"))

  out <- obj$max_confirm(events, solve_for = "behavior")

  expect_equal(
    object = round(out, 2),
    expected = dplyr::tibble(Be = c(-0.54, 0.18), Bp = c(0.31, -0.19), Ba = c(-0.10, 0.67))
  )

})


test_that("max confirm replicates interact Germany", {

  obj <- interact(dictionary = c("germany2007", "male"), equations = c("germany2007","all"))
  obj

  events <- data.frame(A = c("academic", "alcoholic"), O = c("alcoholic", "academic"))
  out <- obj$max_confirm(events, solve_for = "behavior")

  expect_equal(
    object = round(out, 2),
    expected = dplyr::tibble(Be = c(-0.13, -2.15), Bp = c(1.57, -0.92), Ba = c(0.17, -0.10))
  )

})
