
#' @title Extract deflection score attributes
#' @name deflection_attributes
#' @export
#'
#' @description Fundamentals, transients, and element-wise deflection.
#'
#' @param x an "`event_deflection`" object created by the [`$deflection()`][`method-deflection`] method
#'
#' @return A data frame containing either fundamentals, transients, or element-wise deflection scores
#'
#' @examples
#' act <- interact("indiana2003", list("nc1978", "female"))
#'
#' grid <- expand.grid(
#'   A = "girlfriend",
#'   B = c("help", "retaliate_against", "torment", "interrogate"),
#'   O = "boyfriend"
#' )
#'
#' d <- act$deflection(grid)
#' d
#' get_fundamentals(d)
#' get_transients(d)
#' get_element_wise_deflection(d)
#' get_long_form(d)
#'

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

#' @rdname deflection_attributes
#' @export
get_long_form <- function(x) {
  stopifnot(inherits(x, "event_deflection"))

  out <- dplyr::tibble(
    A = rep(x[["A"]], each = 9),
    B = rep(x[["B"]], each = 9),
    O = rep(x[["O"]], each = 9)
  )

  out$id <- rep(rep(c("A", "B", "O"), each = 3), times = nrow(x))
  out$dim <- rep(c("e", "p", "a"), times = 3 * nrow(x))
  out$fundamentals <- as.vector(t(get_fundamentals(x)))
  out$transients <- as.vector(t(get_transients(x)))
  out$deflections <- as.vector(t(get_element_wise_deflection(x)))

  out <- out |>
    dplyr::mutate(entity = dplyr::case_when(
      .data$id == "A" ~ .data$A,
      .data$id == "B" ~ .data$B,
      .data$id == "O" ~ .data$O
    ))

  return(out)

}



# Filter and Subset -------------------------------------------------------

## This is experimental.
## I want the attributes of the event_deflection data frame to change
## accordingly each time I subset it.
## But... this requires that I create a bunch of generic methods, that will not
## extend to other packages.
## So what I'm thinking right now is that maybe the attributes should be stored
## in the deflection column... that way, every time I subset I don't have to
## worry about what happens to the attributes. This is probably what I will do.


#' @export
dplyr_row_slice.event_deflection <- function(data, i, ...) {

  out <- dplyr::dplyr_row_slice(dplyr::as_tibble(data), i)
  tra <- dplyr::dplyr_row_slice(get_transients(data), i)
  fun <- dplyr::dplyr_row_slice(get_fundamentals(data), i)
  ewd <- dplyr::dplyr_row_slice(get_element_wise_deflection(data), i)

  ## Parallel filtering output
  structure(
    out,
    class = c("event_deflection", class(out)),
    element_wise_deflection = ewd,
    transients = tra,
    fundamentals = fun
  )
}


#' @export
`[.event_deflection` <- function(data, i, j, ..., drop = TRUE) {

  out <- NextMethod("[")  ## method dispatch to `tbl_df`, `tbl`, or `data.frame` class
  tra <- get_transients(data)[i, ]
  fun <- get_fundamentals(data)[i, ]
  ewd <- get_element_wise_deflection(data)[i, ]

  structure(
    out,
    class = class(out),
    element_wise_deflection = ewd,
    transients = tra,
    fundamentals = fun
  )

}


