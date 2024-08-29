
# To do:
# - Add generalized emotion thing for after events.
# - Add the modifier and characteristic emotion to `InteRactModel`


create_characteristic_emotion <- function(dictionary = list("usfullsurveyor2015", "all"), equations = list("us2010", "male")) {

  dictionary <- validate_dictionary(dictionary)
  equations <- validate_emotionid_equations(equations)

  dict <- do.call(get_dictionary, dictionary)
  EQ <- do.call(get_equation, equations)

  function(events) {

    events <- validate_ce_events(events, dict)
    # fundamentals <- stack_mi_ratings(events, dict)
    # fundamentals <- as.data.frame(fundamentals)

    fundamentals <- dict |> dplyr::filter(.data$term == events[["I"]], .data$component == "identity") |> dplyr::pull("ratings")
    fundamentals <- dplyr::bind_rows(fundamentals)
    colnames(fundamentals) <- paste0("I", colnames(fundamentals))

    data <- dplyr::bind_cols(fundamentals, dplyr::tibble(Me = 1, Mp = 1, Ma = 1))
    M <- get_data_matrix(data, EQ)

    r <- fundamentals |> unlist()

    ## Note. The documentation here is very poor.
    ## Chapter 14 of Heise (2006)

    X <- sweep(EQ, 1, M, `*`) |> t()

    S <- sapply(paste0("M", c("e", "p", "a")), function(m) {
      as.integer(grepl(m, colnames(X)))
    }, simplify = TRUE)

    g <- matrix(1 - rowSums(S), nrow = 1)
    out <- solve(X %*% S, r - X %*% t(g))

    return(dplyr::as_tibble(t(out)))

  }
}

# ce <- create_characteristic_emotion()
#
# ce(list(I = "brute"))
# ce(list(I = "brother"))
# ce(list(I = "clod"))

