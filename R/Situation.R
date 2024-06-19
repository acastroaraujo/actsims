

#definition_of_the_situation <- function(
#    init_event = NULL,
#    dictionary = list("usfullsurveyor2015", "all"),
#    equations = list("us2010", "all")
#) {
#
#  DefinitionSituation$new(init_event, dictionary, equations)
#
#}




# Add situation object for interactShiny

# This should have a counter for the number of rounds
# Should be used with max_confirm method for every new round
# Figure out whether the interactors should be allowed to have different dictionaries etc.;
# i.e., this allows for cross-culture and cross-gender comparisons

Situation <- R6::R6Class(
  classname = "Situation",

  public = list(
    init_event = NULL,
    history = NULL,
    engine = NULL,

    initialize = function(init_event) {

      engine <- InteRactModel$new()
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

    ## change this so that there's no ambiguity with identity/behavior
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

    ## change this so that there's no ambiguity with identity/behavior
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
  "public", "solve_for",
  function(what = c("behavior", "actor", "object")) {

    ## this is all wrong, I have no idea what I did...
    ## check line 298 in InteractModel for clues...

    browser()
    what <- match.arg(what)
    all_select <- c(epa_selector("A"), epa_selector("B"), epa_selector("O"))

    transients <- tail(self$history$transients, n = 1)[all_select]
    fundamentals <- tail(self$history$fundamentals, n = 1)[all_select]

    col_select <- switch(what,
      "actor" = epa_selector("A"),
      "behavior" = epa_selector("B"),
      "object" = epa_selector("O")
    )

    EQ <- self$engine$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)
    selection_matrix <- get_selection_matrix(EQ)

    Im <- cbind(transients, get_data_matrix(transients, EQ)) # change
    S <- selection_matrix[, col_select]
    H <- get_h_matrix(EQ)

    out <- solve_equations(Im, S, H)
    out
    return(dplyr::as_tibble(out))

  }
)



