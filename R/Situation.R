
# Figure out whether the interactors should be allowed to have different dictionaries etc.;
# i.e., this allows for cross-culture and cross-gender comparisons


#' Create a Definition of the Situation
#'
#' @param init_event an ABO, where A is person1 and O is person2
#' @param dictionary (character) dictionary name and group. The default is set to `list("usfullsurveyor2015", "all")`.
#'
#' See available options using [`actdata::dataset_keys()`]
#'
#' @param equations (character) equations name and group. The default is set to `list("usfullsurveyor2015", "all")`.
#'
#' See available options using [`actdata::equations`]
#'
#' Currently only equations with "`equation_type == impressionabo`" are valid.
#'
#' @return A `Situation` R6 object.
#' @export
#'
start_situation <- function(
    init_event = NULL,
    dictionary = list("usfullsurveyor2015", "all"),
    equations = list("us2010", "all")
) {

  Situation$new(init_event, dictionary, equations)

}

# Add documentation when time allows

Situation <- R6::R6Class(
  classname = "Situation",

  public = list(
    init_event = NULL,
    history = NULL,
    engine = NULL,

    initialize = function(init_event, dictionary = list("usfullsurveyor2015", "all"), equations = list("us2010", "all")) {

      engine <- InteRactModel$new(dictionary, equations)
      init <- engine$deflection(init_event)

      start_row <- dplyr::tibble(time = 0L, person1 = "actor", person2 = "object")
      start_row <- dplyr::bind_cols(start_row, init_event)

      deflection <- dplyr::bind_cols(start_row, init["deflection"])

      fundamentals <- dplyr::bind_cols(start_row, get_fundamentals(init))
      transients <- dplyr::bind_cols(start_row, get_transients(init))
      element_wise_deflection <- dplyr::bind_cols(start_row, get_element_wise_deflection(init))

      self$history <- list(deflection = deflection, fundamentals = fundamentals, transients = transients, element_wise_deflection = element_wise_deflection)
      self$engine <- engine

    }
  ),

  private = list(
    .time = 0L,
    .add_time = function() {
      private$.time <- private$.time + 1L
      invisible(self)
    }
  )

)

Situation$set(
  "public", "next_person1",
  function(events) {

    fundamentals <- stack_abo_ratings(events, self$engine$dictionary)
    actor_select <- epa_selector("A")
    behavior_select <- epa_selector("B")
    object_select <- epa_selector("O")

    transients_input <- tail(self$history$transients, n = 1)

    previous <- tail(self$history$transients, n = 1)
    transients_input <- previous[c(actor_select, behavior_select, object_select)]

    ok <- previous$person1 == "actor" && previous$person2 == "object"

    if (!ok) {
      transients_input <- reverse_ao(transients_input)
    }

    new_behavior <- self$engine$fundamentals(events$B) |>
      dplyr::filter(component == "behavior") |>
      dplyr::select(dplyr::all_of(c("e", "p", "a")))

    transients_input[behavior_select] <- new_behavior

    EQ <- self$engine$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)

    M <- get_data_matrix(transients_input, EQ)

    transients_out <- M %*% EQ

    element_wise_deflection <- (transients_out - fundamentals)^2
    deflection <- list(unname(rowSums(element_wise_deflection)))

    names(deflection) <- "deflection"
    deflection <- dplyr::as_tibble(deflection)

    private$.add_time()

    new_row <- dplyr::tibble(time = private$.time, person1 = "actor", person2 = "object")
    new_row <- dplyr::bind_cols(new_row, events)

    out <- list(
      deflection = dplyr::bind_cols(new_row, deflection),
      fundamentals = dplyr::bind_cols(new_row, fundamentals),
      transients = dplyr::bind_cols(new_row, transients_out),
      element_wise_deflection = dplyr::bind_cols(new_row, element_wise_deflection)
    )

    for (x in names(out)) {
      self$history[[x]] <- dplyr::bind_rows(self$history[[x]], out[[x]])
    }

    invisible(self$history)

  }
)

Situation$set(
  "public", "next_person2",
  function(events) {

    fundamentals <- stack_abo_ratings(events, self$engine$dictionary)
    actor_select <- epa_selector("A")
    behavior_select <- epa_selector("B")
    object_select <- epa_selector("O")

    previous <- tail(self$history$transients, n = 1)
    transients_input <- previous[c(actor_select, behavior_select, object_select)]

    ok <- previous$person2 == "actor" && previous$person1 == "object"

    if (!ok) {
      transients_input <- reverse_ao(transients_input)
    }

    new_behavior <- self$engine$fundamentals(events$B) |>
      dplyr::filter(component == "behavior") |>
      dplyr::select(dplyr::all_of(c("e", "p", "a")))

    transients_input[behavior_select] <- new_behavior

    EQ <- self$engine$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)

    M <- get_data_matrix(transients_input, EQ)

    transients_out <- M %*% EQ

    element_wise_deflection <- (transients_out - fundamentals)^2
    deflection <- list(unname(rowSums(element_wise_deflection)))

    names(deflection) <- "deflection"
    deflection <- dplyr::as_tibble(deflection)

    private$.add_time()

    new_row <- dplyr::tibble(time = private$.time, person1 = "object", person2 = "actor")
    new_row <- dplyr::bind_cols(new_row, events)

    out <- list(
      deflection = dplyr::bind_cols(new_row, deflection),
      fundamentals = dplyr::bind_cols(new_row, fundamentals),
      transients = dplyr::bind_cols(new_row, transients_out),
      element_wise_deflection = dplyr::bind_cols(new_row, element_wise_deflection)
    )

    for (x in names(out)) {
      self$history[[x]] <- dplyr::bind_rows(self$history[[x]], out[[x]])
    }

    invisible(self$history)

  }
)

Situation$set(
  "public", "optimal_behavior",
  function(who = c("person1", "person2")) {

    who <- match.arg(who)
    person_role <- tail(self$history$deflection, n = 1)[c("person1", "person2")]

    all_select <- c(epa_selector("A"), epa_selector("B"), epa_selector("O"))

    transients <- tail(self$history$transients, n = 1)[all_select]
    fundamentals <- tail(self$history$fundamentals, n = 1)[all_select]


    if (person_role[[who]] == "object") {
      fundamentals <- reverse_ao(fundamentals)
      transients <- reverse_ao(transients)
    }

    col_select <- epa_selector("B")

    transients[col_select] <- 1
    fundamentals[col_select] <- 1

    EQ <- self$engine$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)
    selection_matrix <- get_selection_matrix(EQ)

    Im <- cbind(fundamentals, get_data_matrix(transients, EQ))
    S <- selection_matrix[, col_select]
    H <- get_h_matrix(EQ)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))

  }
)

Situation$set(
  "public", "reidentify",
  function(who = c("person1", "person2")) {

    who <- match.arg(who)
    person_role <- tail(self$history$deflection, n = 1)[c("person1", "person2")]

    all_select <- c(epa_selector("A"), epa_selector("B"), epa_selector("O"))

    if (private$.time == 0) {
      fundamentals <- tail(self$history$fundamentals, n = 1)[all_select] # transient inputs or fundamentals?
      transients_in <- fundamentals
    }

    if (private$.time > 0) {
      fundamentals <- tail(self$history$fundamentals, n = 1)[all_select]
      transients_in <- tail(self$history$transients, n = 2)[all_select][1, ]
      transients_in[epa_selector("B")] <- fundamentals[epa_selector("B")]
    }

    col_select <- switch(person_role[[who]],
      "actor" = epa_selector("A"),
      "object" = epa_selector("O")
    )

    fundamentals[col_select] <- 1
    transients_in[col_select] <- 1

    EQ <- self$engine$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)
    selection_matrix <- get_selection_matrix(EQ)

    Im <- cbind(fundamentals, get_data_matrix(transients_in, EQ))
    S <- selection_matrix[, col_select]
    H <- get_h_matrix(EQ)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))

  }
)



