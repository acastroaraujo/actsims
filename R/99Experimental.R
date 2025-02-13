
#' Create a Definition of the Situation
#'
#' @param ... various `InteRactModel` objects created by the `interact()`,
#'   they must be named "agent1", "agent2", "agent3", and so on.
#'
#' @return A `SituationExperimental` R6 object.
#' @export
#'
define_situation_experimental <- function(...) {
  agents <- list(...)

  ok <- all(purrr::map_lgl(agents, \(x) inherits(x, "InteRactModel")))

  if (!ok) {
    cli::cli_abort("all agents must be of class `InteRactModel`", call = NULL)
  }

  ok <- all(grepl("^agent\\d+$", names(list(...))))

  if (!ok) {
    cli::cli_abort("all agents must be named correctly, e.g., agent1, agent2, and so on.`", call = NULL)
  }

  SituationExperimental$new(...)
}


# Situation ----------------------------------------------------------------

#' @title Situation Experimental Objects
#'
#' @name SituationExperimental
#'
SituationExperimental <- R6::R6Class(
  lock_objects = FALSE,
  classname = "SituationExperimental",

  public = list(
    history = NULL,
    fundamentals = NULL,
    transients = NULL,

    initialize = function(...) {

      agents <- list(...)
      field_names <- names(agents)

      for (i in seq_along(agents)) {
        # Use assign to create a new field dynamically
        assign(field_names[[i]], agents[[i]]$clone(), envir = self)
      }

      private$.active <- "agent1"
      private$.n_agents <- length(agents)

      # base::lockEnvironment(self, bindings = TRUE) ## this doesn't really work...

    },

    start = function(...) {

      identities <- list(...)

      fundamentals <- purrr::imap_dfr(
        identities,
        function(x, nm) {
          index <- self[[nm]]$dictionary$term == x & self[[nm]]$dictionary$component == "identity"
          self[[nm]]$dictionary[index, ]$ratings
        }
      )

      self$fundamentals <- dplyr::bind_cols(
        data.frame(id = names(identities)),
        data.frame(identity = as.character(identities)),
        fundamentals
      )

      self$transients <- vector("list", length(identities))
      names(self$transients) <- unlist(identities)

      self$history <- NULL
      private$.time <- NULL

      invisible(self)

    }#,

   # activate = function(who) {
   #   self$active <- who
   #   invisible(self)
   # }

  # ),
  # active = list( ## this `active` is for "active bindings" !!
  #   time = function(value) {
  #     if (missing(value)) private$.time else cli::cli_abort("`time` cannot be changed", call = NULL)
  #   },
  #
  #   active = function(value) {
  #     if (missing(value)) {
  #       private$.active
  #     } else {
  #       if (!value %in% paste0("agent", 1:private$.n_agents)) cli::cli_abort('must be one of {paste0("agent", 1:private$.n_agents)}')
  #       private$.active <- value
  #       invisible(self)
  #     }
  #   }
  #
  ),
  private = list(
    .time = NULL,
    .add_time = function() {
      private$.time <- private$.time + 1L
      invisible(self)
    },
    .n_agents = NULL
  )
)


# New Event ---------------------------------------------------------------

SituationExperimental$set(
  "public", "new",
  function(event) {

    if (is.null(self$fundamentals)) {
      cli::cli_abort("must `$start` the situation first", call = NULL)
    }

    actor_select <- epa_selector("A")
    behavior_select <- epa_selector("B")
    object_select <- epa_selector("O")

    lookup <- self$fundamentals$identity
    names(lookup) <- self$fundamentals$id

    actor <- self$fundamentals |>
      dplyr::filter(identity == event$A) |>
      dplyr::pull(id)

    obj <- self$fundamentals |>
      dplyr::filter(identity == event$O) |>
      dplyr::pull(id)

    ## Case 1, start of history

    if (is.null(self$history)) {

      private$.time <- 0L

      out <- self[[actor]]$deflection(event)
      start_row <- dplyr::tibble(time = 0L)
      start_row <- dplyr::bind_cols(start_row, event)
      deflection <- dplyr::bind_cols(start_row, out["deflection"])
      fundamentals <- dplyr::bind_cols(start_row, get_fundamentals(out))
      transients <- dplyr::bind_cols(start_row, get_transients(out))
      element_wise_deflection <- dplyr::bind_cols(start_row, get_element_wise_deflection(out))
      self$history <- list(deflection = deflection, fundamentals = fundamentals, transients = transients, element_wise_deflection = element_wise_deflection)

      self$transients[[lookup[[actor]]]] <- purrr::set_names(
        x = transients[actor_select],
        nm = c("e", "p", "a")
      )

      self$transients[[lookup[[obj]]]] <- purrr::set_names(
        x = transients[object_select],
        nm = c("e", "p", "a")
      )

      return(invisible(self))

    }

    ## Case 2, else

    fundamentals <- stack_abo_ratings(event, self[[actor]]$dictionary)

    A <- tail(self$transients[[lookup[[actor]]]], n = 1)
    O <- tail(self$transients[[lookup[[obj]]]], n = 1)

    if (is.null(A)) {
      A <- self[[actor]]$dictionary |>
        dplyr::filter(component == "identity", term == lookup[[actor]]) |>
        dplyr::pull(ratings) |>
        dplyr::bind_rows() ## looks weird, but works perfectly fine
    }

    if (is.null(O)) {
      O <- self[[obj]]$dictionary |>
        dplyr::filter(component == "identity", term == lookup[[obj]]) |>
        dplyr::pull(ratings) |>
        dplyr::bind_rows() ## looks weird, but works perfectly fine
    }

    B <- self[[actor]]$fundamentals(event$B) |>
      dplyr::filter(component == "behavior") |>
      dplyr::select(dplyr::all_of(c("e", "p", "a")))

    colnames(A) <- paste0("A", colnames(A))
    colnames(B) <- paste0("B", colnames(B))
    colnames(O) <- paste0("O", colnames(O))

    transients_input <- dplyr::bind_cols(A, B, O)

    EQ <- self[[actor]]$equations
    colnames(EQ) <- substr(colnames(EQ), 1, 2)

    M <- get_data_matrix(transients_input, EQ)

    transients_out <- M %*% EQ

    element_wise_deflection <- (transients_out - fundamentals)^2
    deflection <- list(unname(rowSums(element_wise_deflection)))

    names(deflection) <- "deflection"
    deflection <- dplyr::as_tibble(deflection)

    private$.add_time()

    new_row <- dplyr::tibble(time = private$.time)
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

    self$transients[[lookup[[actor]]]] <- dplyr::bind_rows(
      self$transients[[lookup[[actor]]]],
      purrr::set_names(
        x = out$transients[actor_select],
        nm = c("e", "p", "a")
      )
    )

    self$transients[[lookup[[obj]]]] <- dplyr::bind_rows(
      self$transients[[lookup[[obj]]]],
      purrr::set_names(
        x = out$transients[object_select],
        nm = c("e", "p", "a")
      )
    )

    invisible(self)

  }
)

# Optimal Behavior --------------------------------------------------------

# SituationExperimental$set(
#   "public", "optimal_behavior",
#   function(who = c("agent1", "agent2")) {
#
#     who <- match.arg(who)
#     agent_role <- tail(self$history$deflection, n = 1)[c("agent1", "agent2")]
#
#     all_select <- c(epa_selector("A"), epa_selector("B"), epa_selector("O"))
#
#     transients <- tail(self$history$transients, n = 1)[all_select]
#     fundamentals <- tail(self$history$fundamentals, n = 1)[all_select]
#
#     if (agent_role[[who]] == "O") {
#       fundamentals <- reverse_ao(fundamentals)
#       transients <- reverse_ao(transients)
#     }
#
#     col_select <- epa_selector("B")
#
#     transients[col_select] <- 1
#     fundamentals[col_select] <- 1
#
#     EQ <- self[[who]]$equations
#     colnames(EQ) <- substr(colnames(EQ), 1, 2)
#     selection_matrix <- get_selection_matrix(EQ)
#
#     Im <- cbind(fundamentals, get_data_matrix(transients, EQ))
#     S <- selection_matrix[, col_select]
#     H <- get_h_matrix(EQ)
#
#     out <- solve_equations(Im, S, H)
#     return(dplyr::as_tibble(out))
#   }
# )

# Reidentify --------------------------------------------------------------

# SituationExperimental$set(
#   "public", "reidentify",
#   function(who = c("agent1", "agent2")) {
#
#     who <- match.arg(who)
#     agent_role <- tail(self$history$deflection, n = 1)[c("agent1", "agent2")]
#
#     all_select <- c(epa_selector("A"), epa_selector("B"), epa_selector("O"))
#
#     if (private$.time == 0) {
#       fundamentals <- tail(self$history$fundamentals, n = 1)[all_select] # transient inputs or fundamentals?
#       transients_in <- fundamentals
#     }
#
#     if (private$.time > 0) {
#       fundamentals <- tail(self$history$fundamentals, n = 1)[all_select]
#       transients_in <- tail(self$history$transients, n = 2)[all_select][1, ]
#       transients_in[epa_selector("B")] <- fundamentals[epa_selector("B")]
#     }
#
#     col_select <- switch(agent_role[[who]],
#                          "A" = epa_selector("A"),
#                          "O" = epa_selector("O")
#     )
#
#     fundamentals[col_select] <- 1
#     transients_in[col_select] <- 1
#
#     EQ <- self[[who]]$equations
#     colnames(EQ) <- substr(colnames(EQ), 1, 2)
#     selection_matrix <- get_selection_matrix(EQ)
#
#     Im <- cbind(fundamentals, get_data_matrix(transients_in, EQ))
#     S <- selection_matrix[, col_select]
#     H <- get_h_matrix(EQ)
#
#     out <- solve_equations(Im, S, H)
#     return(dplyr::as_tibble(out))
#
#   }
# )


