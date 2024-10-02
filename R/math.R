

# Misc --------------------------------------------------------------------

stack_abo_ratings <- function(events, dict) {

  i <- dict[dict[["component"]] == "identity", ]
  b <- dict[dict[["component"]] == "behavior", ]

  identities <- purrr::set_names(i[["ratings"]], nm = i[["term"]]) |>
    do.call(what = "rbind")

  behaviors <- purrr::set_names(b[["ratings"]], nm = b[["term"]]) |>
    do.call(what = "rbind")

  out <- cbind(
    identities[events[["A"]], , drop = FALSE],
    behaviors[events[["B"]], , drop = FALSE],
    identities[events[["O"]], , drop = FALSE]
  )

  rownames(out) <- NULL
  colnames(out) <- paste0(rep(c("A", "B", "O"), each = 3), rep(c("e", "p", "a"), times = 3))

  return(out)

}

stack_pair_ratings <- function(events, solve_for, dict) {

  i <- dict[dict[["component"]] == "identity", ]
  b <- dict[dict[["component"]] == "behavior", ]

  identities <- purrr::set_names(i[["ratings"]], nm = i[["term"]]) |>
    do.call(what = "rbind")

  behaviors <- purrr::set_names(b[["ratings"]], nm = b[["term"]]) |>
    do.call(what = "rbind")

  plug <- matrix(1, nrow = nrow(as.data.frame(events)), ncol = 3)

  out <- switch(solve_for,
    "actor" = cbind(plug, behaviors[events[["B"]], , drop = FALSE], identities[events[["O"]], , drop = FALSE]),
    "behavior" = cbind(identities[events[["A"]], , drop = FALSE], plug, identities[events[["O"]], , drop = FALSE]),
    "object" = cbind(identities[events[["A"]], , drop = FALSE], behaviors[events[["B"]], , drop = FALSE], plug)
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

epa_selector <- function(x = c("A", "B", "O")) {
  x <- match.arg(x)
  paste0(rep(x, 3), c("e", "p", "a"))
}

stack_mi_ratings <- function(events, dict) {

  m <- dict[dict[["component"]] == "modifier", ]
  i <- dict[dict[["component"]] == "identity", ]

  identities <- purrr::set_names(i[["ratings"]], nm = i[["term"]]) |>
    do.call(what = "rbind")

  modifiers <- purrr::set_names(m[["ratings"]], nm = m[["term"]]) |>
    do.call(what = "rbind")

  out <- cbind(
    modifiers[events[["M"]], , drop = FALSE],
    identities[events[["I"]], , drop = FALSE]
  )

  rownames(out) <- NULL
  colnames(out) <- paste0(rep(c("M", "I"), each = 3), rep(c("e", "p", "a"), times = 2))

  return(out)

}

# Matrices ----------------------------------------------------------------

get_data_matrix <- function(data, eq) {
  ff <- stats::reformulate(rownames(eq)[-1])
  X <- stats::model.matrix(ff, data = as.data.frame(data))
  return(X)
}

get_selection_matrix <- function(B) {

  ## `B` is private$.impressionabo

  I <- diag(ncol(B))

  ## The selection matrix stacks up a 9x9 identity matrix (in case of ABO
  ## situation) on top of the next matrix.

  out <- sapply(colnames(B), function(x) {
    ## grepl outputs TRUE whenever there is a match between colnames(B)
    ## and rownames(B). as.integer creates a vector of 1s and 0s.
    as.integer(grepl(x, rownames(B)))
    ## sapply assembles the resulting vectors (column-wise) into a matrix.
  }, simplify = TRUE)

  return(rbind(I, out))

}

get_h_matrix <- function(X) {
  rbind(diag(ncol(X)), -1*X) %*% cbind(diag(ncol(X)), -1*t(X))
}


# Solutions ---------------------------------------------------------------

solve_equations <- function(Im, S, H) {

  # `Im` is a matrix with every row corresponding to the diagonal
  # in the `I` matrix used in Heise's equations.

  if (requireNamespace("pbapply", quietly = TRUE)) {
    loop <- pbapply::pbapply  ## for progress bars
  } else {
    loop <- base::apply
  }

  # Precompute `St` and `g` for later use.

  St <- t(S)
  g <- matrix(1 - rowSums(S), ncol = 1)

  # Solve equations for each row of `Im`

  out <- loop(Im, MARGIN = 1, function(row) {
    I <- diag(row)
    M <- St %*% I %*% H %*% I
    -1 * solve(M %*% S) %*% M %*% g
  }, simplify = TRUE)

  rownames(out) <- colnames(S)

  return(t(out))

}

