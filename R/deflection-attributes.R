
#' @title Deflection Score Attributes
#' @name deflection_attributes
#' @export
#'
#' @description Fundamentals, transients, and element-wise deflection.
#'
#' @param x an "`event_deflection`" object created by the [`$deflection()`][`method-deflection`] method
#'
#' @return A data frame containing either fundamentals, transients, or element-wise deflection scores

#' @rdname deflection_attributes
#' @export
get_fundamentals <- function(x) {
  stopifnot(inherits(x, "event_deflection"))
  attr(x, "fundamentals", exact = TRUE)
}

#' @rdname deflection_attributes
#' @export
get_transients <- function(x) {
  stopifnot(inherits(x, "event_deflection"))
  attr(x, "transients", exact = TRUE)
}

#' @rdname deflection_attributes
#' @export
get_element_wise_deflection <- function(x) {
  stopifnot(inherits(x, "event_deflection"))
  attr(x, "element_wise_deflection", exact = TRUE)
}

