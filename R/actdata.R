
#' @import actdata

z_decode <- function(
    x,                   ## Character vector with Z* coded names (Heise 2007, 121-3)
    equation_type = c(
      "impressionabo",   ## This is how the equation types are
      "impressionabos",  ## specified in the actdata package.
      "selfdir",         ##
      "emotionid",       ## See `actdata::equations` for more.
      "traitid")
) {

  equation_type <- match.arg(equation_type)

  terms <- switch(equation_type,
    "impressionabo" = c("A", "B", "O"),
    "impressionabos" = c("A", "B", "O", "S"),
    "selfdir" = c("A", "B"),
    "emotionid" = c("M", "I"),
    "traitid" = c("M", "I")
  )

  index <- lapply(strsplit(sub(".", "", x), ""), \(x) as.logical(as.integer(x)))
  elements <- paste0(rep(terms, each = 3), rep(c("e", "p", "a"), times = length(terms)))

  out <- purrr::map_chr(index, function(i) {
    if (sum(i) == 0) "(Intercept)" else paste(elements[i], collapse = ":")
  })

  return(out)

}

get_dictionary <- function(dataset, group) {

  vars <- c("term", "component", "ratings", "n", "sd")

  actdata::epa_subset(dataset = dataset) |>
    dplyr::filter(.data$group == !!group) |>
    dplyr::rename_all(tolower) |>
    dplyr::rowwise() |>
    dplyr::mutate(ratings = list(c(e = .data$e, p = .data$p, a = .data$a))) |>
    dplyr::mutate(n = list(c(e = .data$n_e, p = .data$n_p, a = .data$n_a))) |>
    dplyr::mutate(sd = list(c(e = .data$sd_e, p = .data$sd_p, a = .data$sd_a))) |>
    dplyr::select(dplyr::any_of(vars)) |>
    dplyr::ungroup()

}

get_impressionabo_equation <- function(key, group) {

  eq <- actdata::equations |>
    dplyr::filter(.data$key == !!key, .data$equation_type == "impressionabo", .data$group == !!group) |>
    dplyr::pull(.data$df) |>
    unlist(recursive = FALSE)

  out <- as.matrix(as.data.frame(eq[-1]))
  colnames(out) <- paste0(rep(c("A", "B", "O"), each = 3), rep(c("e", "p", "a"), times = 3))
  rownames(out) <- z_decode(eq[[1]], equation_type = "impressionabo")

  return(out)

}

get_equation <- function(key, group, equation_type) {

  eq <- actdata::equations |>
    dplyr::filter(.data$key == !!key, .data$equation_type == !!equation_type, .data$group == !!group) |>
    dplyr::pull(.data$df) |>
    unlist(recursive = FALSE)

  terms <- switch(equation_type,
    "impressionabo" = c("A", "B", "O"),
    "impressionabos" = c("A", "B", "O", "S"),
    "selfdir" = c("A", "B"),
    "emotionid" = c("I"),
    "traitid" = c("I")
  )

  out <- as.matrix(as.data.frame(eq[-1]))
  colnames(out) <- paste0(rep(terms, each = 3), rep(c("e", "p", "a"), times = length(terms)))
  rownames(out) <- z_decode(eq[[1]], equation_type)

  return(out)

}


