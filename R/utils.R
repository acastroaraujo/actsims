
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



