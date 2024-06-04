
validate_new_dictionary <- function(dict) {

  ok <- all(c("term", "ratings") %in% colnames(dict))
  if (!ok) stop(call. = FALSE, "New dictionary must be a data frame with <term> and <ratings> columns")

  ok <- all(map_lgl(dict$ratings, \(x) all(names(x) == c("e", "p", "a"))))
  if (!ok) stop(call. = FALSE, "The ratings column must be a named list of epa ratings")

}

stack_epa_ratings <- function(events, dict) {

  stopifnot(all(purrr::map_lgl(c("A", "B", "O"), \(x) class(events[[x]]) == "character")))

  mat <- do.call(rbind, dict[["ratings"]])
  rownames(mat) <- dict[["term"]]

  out <- cbind(
    mat[events[["A"]], , drop = FALSE],
    mat[events[["B"]], , drop = FALSE],
    mat[events[["O"]], , drop = FALSE]
  )

  rownames(out) <- NULL
  colnames(out) <- paste0(rep(c("A", "B", "O"), each = 3), rep(c("e", "p", "a"), times = 3))

  return(out)
}

get_data_matrix <- function(data, eq) {
  terms <- rownames(eq)[-1]
  form <- stats::reformulate(terms)
  X <- stats::model.matrix(form, data = as.data.frame(data))
  return(X)
}

get_actor <- function(events, dict, eq) {

  fundamentals <- stack_epa_ratings(events, dict)
  X <- get_data_matrix(fundamentals, eq)
  transients <- X %*% eq
  fundamentals[grepl("B", colnames(fundamentals))] <- 1
  transients[grepl("B", colnames(transients))] <- 1
  cbind(fundamentals, get_data_matrix(transients, eq))

}

get_object <- function(events, dict, eq) {

  fundamentals <- stack_epa_ratings(events, dict)
  X <- get_data_matrix(fundamentals, eq)
  transients <- X %*% eq
  fundamentals[grepl("O", colnames(fundamentals))] <- 1
  transients[grepl("O", colnames(transients))] <- 1
  cbind(fundamentals, get_data_matrix(transients, eq))

}


# Print Methods -----------------------------------------------------------


#' @export
print.deflection <- function(x, ...) {
  cat("Deflection Scores", "\n")
  print.default(as.vector(x), digits = 3, ...)
}


# Deflection Attributes ---------------------------------------------------

#' Get Deflection Score Attributes
#'
#' Fundamentals, transients, and element-wise deflection.
#'
#' @param x a deflection score
#'
#' @return A matrix object containing either fundamentals, transients, or element-wise deflection scores
#' @export

#' @export
#' @rdname deflection_attributes
get_fundamentals <- purrr::attr_getter("fundamentals")

#' @export
#' @rdname deflection_attributes
get_transients <- purrr::attr_getter("transients")

#' @export
#' @rdname deflection_attributes
get_element_wise_deflection <- purrr::attr_getter("element_wise_deflection")

