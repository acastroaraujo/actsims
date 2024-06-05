
validate_new_dictionary <- function(dict) {

  ok <- all(c("term", "ratings") %in% colnames(dict))
  if (!ok) stop(call. = FALSE, "New dictionary must be a data frame with <term> and <ratings> columns")

  ok <- all(purrr::map_lgl(dict$ratings, \(x) all(names(x) == c("e", "p", "a"))))
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

get_object <- function(d, eq) {
  fundamentals <- get_fundamentals(d)
  transients <- get_transients(d)
  fundamentals[grepl("O", colnames(fundamentals))] <- 1
  transients[grepl("O", colnames(transients))] <- 1
  cbind(fundamentals, get_data_matrix(transients, eq))
}

get_actor <- function(d, dict, eq) {
  fundamentals <- get_fundamentals(d)
  transients <- get_transients(d)
  fundamentals[grepl("B", colnames(fundamentals))] <- 1
  transients[grepl("B", colnames(transients))] <- 1
  cbind(fundamentals, get_data_matrix(transients, eq))
}

## there's still some repetition here... I could wrap the <- 1 assignments
## in a switch statement

# Print Methods -----------------------------------------------------------

#' @export
tbl_format_header.event_deflection <- function(x, setup, ...) {
  c(cli::col_blue("# Event deflection"), cli::col_blue("# A data frame: ", setup$tbl_sum))
}


