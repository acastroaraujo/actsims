
act <- build_interact(dictionary = "usfullsurveyor2015", equation = "us2010", group = "all")
events <- data.frame(A = "mother", B = "abandon", O = "child")
d <- act$deflection(events)

test_that("mother abandons child", {
  expect_equal(d$deflection, 85.124527)
  expect_equal(d$deflection, rowSums(get_element_wise_deflection(d)))
  expect_equal(d$deflection, sum((get_fundamentals(d) - get_transients(d))^2))
})


