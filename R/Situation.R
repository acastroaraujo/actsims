
#' Create a Definition of the Situation
#'
#' @param agent1 an `InteRactModel` created by the `interact()` function
#' @param agent2 an `InteRactModel` created by the `interact()` function
#'
#' @return A `Situation` R6 object.
#' @export
#'
define_situation <- function(agent1, agent2) {
  ok <- inherits(agent1, "InteRactModel") & inherits(agent2, "InteRactModel")
  if (!ok) cli::cli_abort("all agents must be of class `InteRactModel`", call = NULL)
  Situation$new(agent1, agent2)
}


# Situation ----------------------------------------------------------------

#' @title Situation Objects
#'
#' @name Situation
#' @description A `Situation` object is an [R6][R6::R6Class] object created
#'   by the [define_situation()] function.
#'
#'   The object stores two InteRactModel objects.
#'
#'   It also provides methods for starting situations (`$start`), adding new
#'   interactions (`$new`), calculating optimal behaviors (`$optimal_behavior`),
#'   and reidentification (`$reidentify`).
#'
#'   You can alternatively use the `sttn_*` family of functions for the same purposes.
#'
Situation <- R6::R6Class(
  classname = "Situation",
  public = list(
    history = NULL,
    agent1 = NULL,
    agent2 = NULL,

    initialize = function(agent1, agent2) {

      self$agent1 <- agent1$clone()
      self$agent2 <- agent2$clone()
      private$.active <- "agent1"

    },

    start = function(init_event) {

      private$.time <- 0L
      actor <- self$active
      obj <- setdiff(paste0("agent", 1:2), actor)
      out <- self[[actor]]$deflection(init_event)
      start_row <- dplyr::tibble(time = 0L, {{actor}} := "A", {{obj}} := "O")
      start_row <- dplyr::bind_cols(start_row, init_event)
      deflection <- dplyr::bind_cols(start_row, out["deflection"])
      fundamentals <- dplyr::bind_cols(start_row, get_fundamentals(out))
      transients <- dplyr::bind_cols(start_row, get_transients(out))
      element_wise_deflection <- dplyr::bind_cols(start_row, get_element_wise_deflection(out))
      self$history <- list(deflection = deflection, fundamentals = fundamentals, transients = transients, element_wise_deflection = element_wise_deflection)
      invisible(self)

    },

    activate = function(who) {
      self$active <- who
      invisible(self)
    }

  ),
   active = list( ## this `active` is for "active bindings" !!
     time = function(value) {
       if (missing(value)) private$.time else cli::cli_abort("`time` cannot be changed", call = NULL)
     },

     active = function(value) {
       if (missing(value)) {
         private$.active
       } else {
         if (!value %in% paste0("agent", 1:2)) cli::cli_abort("must be either `agent1` or `agent2`")
         private$.active <- value
         invisible(self)
       }
     }

   ),
   private = list(
     .active = NULL,
     .time = NULL,
     .add_time = function() {
       private$.time <- private$.time + 1L
       invisible(self)
     }
   )
 )



# New Event ---------------------------------------------------------------

Situation$set(
  "public", "new",
  function(event) {

    if (is.null(self$history)) {
      cli::cli_abort("must `$start` the situation first", call = NULL)
    }

    actor <- self$active
    obj <- setdiff(paste0("agent", 1:2), actor)

    fundamentals <- stack_abo_ratings(event, self[[actor]]$dictionary)
    actor_select <- epa_selector("A")
    behavior_select <- epa_selector("B")
    object_select <- epa_selector("O")

    previous <- tail(self$history$transients, n = 1)
    transients_input <- previous[c(actor_select, behavior_select, object_select)]

    ok <- previous[[actor]] == "A"

    if (!ok) {
      transients_input <- reverse_ao(transients_input)
    }

    new_behavior <- self[[actor]]$fundamentals(event$B) |>
      dplyr::filter(component == "behavior") |>
      dplyr::select(dplyr::all_of(c("e", "p", "a")))

    transients_input[behavior_select] <- new_behavior

    EQ <- self[[actor]]$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)

    M <- get_data_matrix(transients_input, EQ)

    transients_out <- M %*% EQ

    element_wise_deflection <- (transients_out - fundamentals)^2
    deflection <- list(unname(rowSums(element_wise_deflection)))

    names(deflection) <- "deflection"
    deflection <- dplyr::as_tibble(deflection)

    private$.add_time()

    new_row <- dplyr::tibble(time = private$.time, {{actor}} := "A", {{obj}} := "O")
    new_row <- dplyr::bind_cols(new_row, event)

    out <- list(
      deflection = dplyr::bind_cols(new_row, deflection),
      fundamentals = dplyr::bind_cols(new_row, fundamentals),
      transients = dplyr::bind_cols(new_row, transients_out),
      element_wise_deflection = dplyr::bind_cols(new_row, element_wise_deflection)
    )

    for (x in names(out)) {
      self$history[[x]] <- dplyr::bind_rows(self$history[[x]], out[[x]])
    }

    invisible(self)

  }
)

# Optimal Behavior --------------------------------------------------------

Situation$set(
  "public", "optimal_behavior",
  function(who = c("agent1", "agent2")) {

    who <- match.arg(who)
    agent_role <- tail(self$history$deflection, n = 1)[c("agent1", "agent2")]

    all_select <- c(epa_selector("A"), epa_selector("B"), epa_selector("O"))

    transients <- tail(self$history$transients, n = 1)[all_select]
    fundamentals <- tail(self$history$fundamentals, n = 1)[all_select]

    if (agent_role[[who]] == "O") {
      fundamentals <- reverse_ao(fundamentals)
      transients <- reverse_ao(transients)
    }

    col_select <- epa_selector("B")

    transients[col_select] <- 1
    fundamentals[col_select] <- 1

    EQ <- self[[who]]$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)
    selection_matrix <- get_selection_matrix(EQ)

    Im <- cbind(fundamentals, get_data_matrix(transients, EQ))
    S <- selection_matrix[, col_select]
    H <- get_h_matrix(EQ)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))
  }
)

# Reidentify --------------------------------------------------------------

Situation$set(
  "public", "reidentify",
  function(who = c("agent1", "agent2")) {

    who <- match.arg(who)
    agent_role <- tail(self$history$deflection, n = 1)[c("agent1", "agent2")]

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

    col_select <- switch(agent_role[[who]],
      "A" = epa_selector("A"),
      "O" = epa_selector("O")
    )

    fundamentals[col_select] <- 1
    transients_in[col_select] <- 1

    EQ <- self[[who]]$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)
    selection_matrix <- get_selection_matrix(EQ)

    Im <- cbind(fundamentals, get_data_matrix(transients_in, EQ))
    S <- selection_matrix[, col_select]
    H <- get_h_matrix(EQ)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))

  }
)

# Pipe Functions ----------------------------------------------------------


#' @title Situation Functions
#' @name sttn_family
#' @export
#'
#' @description Start, new, activate, and extract.
#'
#' @param x an R6 "`Situation`" object created by the `define_situation()` function.
#'
#' @param which (character) either "agent1" or "agent2."
#'
#' @param event an ABO event.
#'
#' @return A modified "`Situation`" object.
#'
#' @examples
#'
#' situation <- define_situation(agent1 = interact(), agent2 = interact())
#'
#' df <- situation |>
#'   sttn_activate("agent1") |>
#'   sttn_start(list(A = "employer", B = "hurt", O = "employee")) |>
#'   sttn_activate("agent2") |>
#'   sttn_new(list(A = "employee", B = "pull_away_from", O = "employer")) |>
#'   sttn_activate("agent1") |>
#'   sttn_new(list(A = "employer", B = "confront", O = "employee")) |>
#'   sttn_extract()
#'
#'   df$deflection
#'
#'   df$transients
#'
#' @rdname sttn_family
#' @export
sttn_activate <- function(x, which) {
  stopifnot("`x` must be a `Situation` R6 object" = inherits(x, "Situation"))
  x$active <- which
  invisible(x)
}

#' @rdname sttn_family
#' @export
sttn_start <- function(x, event) {
  stopifnot("`x` must be a `Situation` R6 object" = inherits(x, "Situation"))
  x$start(event)
}

#' @rdname sttn_family
#' @export
sttn_new <- function(x, event) {
  stopifnot("`x` must be a `Situation` R6 object" = inherits(x, "Situation"))
  x$new(event)
}

#' @rdname sttn_family
#' @export
sttn_extract <- function(x) {
  x$history
}


