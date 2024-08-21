

#' Identity Modifier
#'
#' @param dictionary (character) dictionary name and group. The default is set to `list("usfullsurveyor2015", "all")`.
#'
#' See available options using [`actdata::dataset_keys()`]
#'
#' @param equations (character) equations name and group. The default is set to `list("usfullsurveyor2015", "all")`.
#'
#' See available options using [`actdata::equations`]
#'
#' Currently only equations with "`equation_type == traitid`" are valid.
#'
#' @return A function that takes events of type data.frame(M = c(...), I = (...)) and outputs new dictionary entries
#'
#' @export
#'
create_modifier <- function(dictionary = list("usfullsurveyor2015", "all"), equations = list("us2010", "all")) {

  dictionary <- validate_dictionary(dictionary)
  equations <- validate_traitid_equations(equations)

  dict <- do.call(get_dictionary, dictionary)
  EQ <- do.call(get_equation, equations)

  function(events) {

    events <- validate_mi_events(events, dict)
    fundamentals <- stack_mi_ratings(events, dict)
    fundamentals <- as.data.frame(fundamentals)
    M <- get_data_matrix(fundamentals, EQ)
    out <- M %*% EQ

    colnames(out) <- c("e", "p", "a")

    d <- dplyr::tibble(
      term = apply(as.data.frame(events), 1, paste, collapse = "__"),
      component = "identity"
    )

    dplyr::bind_cols(d, out) |>
      dplyr::rowwise() |>
      dplyr::mutate(ratings = list(c(e = .data$e, p = .data$p, a = .data$a))) |>
      dplyr::ungroup() |>
      dplyr::select("term", "component", "ratings")

  }

}






