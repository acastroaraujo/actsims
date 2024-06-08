

# Misc --------------------------------------------------------------------

stack_abo_ratings <- function(events, dict) {

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

stack_pair_ratings <- function(events, solve_for, dict) {

  ## stopifnot(all(purrr::map_lgl(c("A", "O"), \(x) class(events[[x]]) == "character")))
  ## add message that warns if B column is present

  mat <- do.call(rbind, dict[["ratings"]])
  rownames(mat) <- dict[["term"]]

  plug <- matrix(1, nrow = nrow(as.data.frame(events)), ncol = 3)

  out <- switch(solve_for,
    "actor" = cbind(plug, mat[events[["B"]], , drop = FALSE], mat[events[["O"]], , drop = FALSE]),
    "behavior" = cbind(mat[events[["A"]], , drop = FALSE], plug, mat[events[["O"]], , drop = FALSE]),
    "object" = cbind(mat[events[["A"]], , drop = FALSE], mat[events[["B"]], , drop = FALSE], plug)
  )

  rownames(out) <- NULL
  colnames(out) <- paste0(rep(c("A", "B", "O"), each = 3), rep(c("e", "p", "a"), times = 3))

  return(out)
}

reverse_ao <- function(x) {
  ## x is either fundamentals or transients
  nms <- colnames(x)
  x <- x[c(7:9, 4:6, 1:3)]
  colnames(x) <- nms
  x
}

# Matrices ----------------------------------------------------------------

get_data_matrix <- function(data, eq) {
  ff <- stats::reformulate(rownames(eq)[-1])
  X <- stats::model.matrix(ff, data = as.data.frame(data))
  return(X)
}

get_selection_matrix <- function(equation) {

  I <- diag(9)

  ## grepl outputs TRUE whenever there is a match between the column name
  ## and rownames(equation). as.integer creates a vector of 1s and 0s.
  ## sapply assembles the resulting vectors (column-wise) into a matrix.

  ## The selection matrix stacks up a 9x9 identity matrix on top of the previous
  ## matrix.

  out <- sapply(colnames(equation), function(x) {
    as.integer(grepl(x, rownames(equation)))
  }, simplify = TRUE)

  return(rbind(I, out))

}

get_h_matrix <- function(X) {
  rbind(diag(ncol(X)), -1*X) %*% cbind(diag(ncol(X)), -1*t(X))
}


# Solutions ---------------------------------------------------------------

solve_equations <- function(Im, S, h) {

  St <- t(S)
  g <- matrix(1 - rowSums(S), ncol = 1)

  if (requireNamespace("pbapply", quietly = TRUE)) {
    loop <- pbapply::pbapply
  } else {
    loop <- base::apply
  }

  out <- loop(Im, MARGIN = 1, function(X) {
    I <- diag(X)
    X <- St %*% I %*% h %*% I
    -1 * solve(X %*% S) %*% X %*% g
  }, simplify = TRUE)

  rownames(out) <- colnames(S)
  return(t(out))

}

