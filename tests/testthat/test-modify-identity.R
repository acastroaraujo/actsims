

test_that("simple identity modifications work", {

  benchmark <- dplyr::tibble(
    term = c("tired__ceo", "absent_minded__academic", "taciturn__buddhist", "taciturn__ceo", "taciturn__academic"),
    component = "identity",
    ratings = list(
      c(e = -0.80, p = 1.21, a = -0.49),
      c(e = -0.14, p = 0.31, a = -1.12),
      c(e = 0.41, p = -0.12, a = -1.91),
      c(e = -0.13, p = 1.59, a = 0.20),
      c(e = 0.69, p = 0.93, a = -0.83)
    )
  )

  act <- interact(dictionary = c("usfullsurveyor2015", "all"), equations = list("us2010", "all"))
  act$add_equation(type = "traitid", group = "all")

  d <- data.frame(
    M = c("tired", "absent_minded", "taciturn", "taciturn", "taciturn"),
    I = c("ceo", "academic", "buddhist", "ceo", "academic")
  )

  out <- act$modify_identity(d)

  out$ratings <- lapply(out$ratings, \(x) round(x, digits = 2))

  expect_equal(out, benchmark)

})
