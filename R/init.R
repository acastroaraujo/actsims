
# To do:
#
#  Oooorrr, maybe have two separate instances of interact() and keep track of
#  the history of play
#
#  Or maybe figure out a way to create a history of play object.
#
# - More informative errors—e.g., when event is not in dictionary; for both stack functions
#.  ... and for max-confirm when there's only twooo
# - More documentation
# - See if you can change the print from messages to cat
#
# - Ugh, make the closest_terms thing work in bulk, why not.
#
## include modifiers!
## allow for more dictionary options, figure out what to do with the group arguments

#' @title Create a new InteRactModel object
#'
#' @description
#' Create a new [`InteRactModel`] object by specifying dictionary and
#' equation names available from the `actdata` package.
#'
#' @param dictionary (character) dictionary name.
#'
#' See available options using [`actdata::dataset_keys()`]
#'
#' Currently only dictionaries that have "`group == all`" are valid.
#'
#' @param equations (character) equations name.
#'
#' See available options using [`actdata::equations`]
#'
#' Currently only equations with "`equation_type == impressionabo`" are valid.
#'
#' @param group (character) one of "all" (default), "male", or "female." This specifies the equation type.
#'
#' @return An [`InteRactModel`] object.
#'
#' @export
interact <- function(dictionary = "usfullsurveyor2015", equations = "us2010", group = "all") {
  InteRactModel$new(dictionary, equations, group)
}

# InteRactModel ----------------------------------------------------------------

#' @title InteRactModel Objects
#'
#' @name InteRactModel
#' @description An `InteRactModel` object is an [R6][R6::R6Class] object created
#'   by the [interact()] function.
#'
#'   The object stores (1) a dictionary of EPA ratings
#'   or "fundamentals" and (2) ACT equations used to calculate "transient impressions."
#'
#'   It also provides methods for calculating deflection scores, behaviors, reidentification,
#'   among others.
#'
#' @section Methods: `InteRactModel` objects have the following associated
#'   methods, many of which have their own (linked) documentation pages:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$deflection()`][method-deflection] | Return an "Event Deflection" data frame. |
#'  [`$optimal_behavior()`][method-optimal-behavior] | Calculate optimal behavior following an event |
#'  [`$reidentify()`][method-reidentify] | Reidentify either actor or object following an event |
#'  [`$max_confirm()`][method-max-confirm]|  Solve for... from a pairing of "actor," "object," or "behavior"  |
#'  [`$closest_terms()`][method-closest-terms] |  Return closest terms in `$dictionary` to an EPA profile |
#'
#'
InteRactModel <- R6::R6Class(
  classname = "InteRactModel",

  public = list(
    initialize = function(dictionary = "usfullsurveyor2015", equations = "us2010", group = "all") {
      private$.equations <- get_equation(key = equations, group = group)
      private$.dictionary <- get_dictionary(dataset = dictionary)
      private$.selection_matrix <- get_selection_matrix(private$.equations)

      private$.dict <- dictionary  ## this is
      private$.group_eq <- group   ## for
      private$.eq <- equations     ## printing only
      private$.group_dict <- "all"

    },
    print = function() {
      cli::cli_h1("Interact Analysis")
      cli::cli_alert_info("Dictionary: {private$.dict}")
      cli::cli_alert_success(cli::style_italic("group: {private$.group_dict}"))
      cli::cli_alert_info("Equations: {private$.eq}")
      cli::cli_alert_success(cli::style_italic("group: {private$.group_eq}"))
      cli::cli_alert_success(cli::style_italic("type: impressionabo"))
    }
  ),

  active = list(
    dictionary = function(value) {
      if (missing(value)) {
        private$.dictionary
      } else {
        validate_new_dictionary(value)
        cli::cli_alert_success("added new dictionary")
        private$.dictionary <- value
        private$.group_dict <- "?"
        private$.dict <- "External [!]"
      }
    },
    equations = function(value) {
      if (missing(value)) {
        out <- private$.equations
        # The print looks better with the "'" to indicate predictions
        # But the code will break apart if we actually replace the original
        # equation column names.
        colnames(out) <- paste0(colnames(out), "'")
        out
      } else {
        stop("Can't set `$equation`", call. = FALSE)
      }
    }
  ),

  private = list(
    ## for internal use
    .dictionary = NULL,
    .equations = NULL,
    .selection_matrix = NULL,
    ## for printing
    .dict = NULL,
    .group_eq = NULL,
    .group_dict = NULL,
    .eq = NULL
  )

)

## Event Deflection --------------------------------------------------------

#' @title Calculate Event Deflection Scores
#'
#' @name method-deflection
#' @aliases deflection
#' @family InteRactModel methods
#'
#' @description The `$deflection()` method does this and that..
#'
#' @param events a data frame with A, B, and O
#'
#' Each has to exist within the `$dictionary` field
#'
#' @return An "Event deflection" data frame.
#'
#' This data frame has an `event_deflection` S3 class with custom printing that
#' works seamlessly with the family of `get_*` functions.
#'
#' @seealso [get_transients()], [get_fundamentals()], [get_element_wise_deflection()]
#'
#' @examples
#' \dontrun{
#' act <- interact(dictionary = "usfullsurveyor2015", equation = "us2010")
#' d <- act$deflection(events = data.frame(A = "mother", B = "abandon", O = "baby"))
#' d
#'}
InteRactModel$set(
  "public", "deflection",
  function(events) {

    fundamentals <- stack_abo_ratings(events, private$.dictionary)
    M <- get_data_matrix(fundamentals, private$.equations)
    transients <- M %*% private$.equations
    element_wise_deflection <- (transients - fundamentals)^2
    events$deflection <- unname(rowSums(element_wise_deflection))
    events <- dplyr::as_tibble(events)

    # S3 class output
    structure(
      events,
      class = c("event_deflection", class(events)),
      element_wise_deflection = dplyr::as_tibble(element_wise_deflection),
      transients = dplyr::as_tibble(transients),
      fundamentals = dplyr::as_tibble(fundamentals)
    )

})



## Reidentification --------------------------------------------------------

#' @title Reidentification of Actor or Object, following an event
#'
#' @name method-reidentify
#' @aliases reidentify
#' @family InteRactModel methods
#'
#' @description The `$optimal_behavior_actor()` method does this and that..
#'
#' @param x an "Event deflection" object created by the `$deflection()` method
#'
#' @param who (character) either "actor" or "object"
#'
#' @return a data frame of EPA profiles for the optimal reidentification
#'
InteRactModel$set(
  "public", "reidentify",
  function(x, who = c("actor", "object")) {

    who <- match.arg(who)
    if (!inherits(x, "event_deflection")) stop(call. = FALSE, "`x` must be a data frame created by the $deflection() method")

    col_select <- switch(who,
      "actor" = c("Ae", "Ap", "Aa"),
      "object" = c("Oe", "Op", "Oa")
    )

    fundamentals <- get_fundamentals(x)
    transients <- get_transients(x)
    fundamentals[col_select] <- 1
    transients[col_select] <- 1

    Im <- cbind(fundamentals, get_data_matrix(fundamentals, private$.equations))
    S <- private$.selection_matrix[, col_select]
    H <- get_h_matrix(private$.equations)

    out <- solve_equations(Im, S, H)

    dplyr::as_tibble(out)

  })


## Behaviors ---------------------------------------------------------------

#' @title Calculate the Optimal Behavior for the Actor or Object following an Event
#'
#' @name method-optimal-behavior
#' @aliases optimal_behavior
#' @family InteRactModel methods
#'
#' @description The `$optimal_behavior()` method does this and that..
#'
#' @param x an "Event deflection" object created by the `$deflection()` method
#'
#' @param who (character) either "actor" or "object"
#'
#' @return a data frame of EPA profiles for the optimal behavior
#'
InteRactModel$set(
  "public", "optimal_behavior",
  function(x, who = c("actor", "object")) {

    who <- match.arg(who)
    if (!inherits(x, "event_deflection")) stop(call. = FALSE, "`x` must be a data frame created by the $deflection() method")

    col_select <- c("Be", "Bp", "Ba")

    fundamentals <- get_fundamentals(x)
    transients <- get_transients(x)
    fundamentals[col_select] <- 1
    transients[col_select] <- 1

    if (who == "object") {
      fundamentals <- reverse_ao(fundamentals)
      transients <- reverse_ao(transients)
    }

    Im <- cbind(fundamentals, get_data_matrix(transients, private$.equations))
    S <- private$.selection_matrix[, col_select]
    H <- get_h_matrix(private$.equations)

    out <- solve_equations(Im, S, H)
    dplyr::as_tibble(out)

  })


## Max Confirm -------------------------------------------------------------

#' @title Identify the behavior that would maximally confirm the identities of actor
#' in an actor-object pairing
#'
#' @name method-max-confirm
#' @aliases max_confirm
#' @family InteRactModel methods
#'
#' @description The `$max_confirm()` method does this and that..
#'
#' @param events a data frame with only A and O
#'
#' @return a data frame of EPA profiles for the behavior the maximally confirms the
#' identity of the actor
#'
InteRactModel$set(
  "public", "max_confirm",
  function(events, solve_for = c("actor", "behavior", "object")) {

    solve_for <- match.arg(solve_for)

    col_select <- switch(solve_for,
      "actor" = c("Ae", "Ap", "Aa"),
      "behavior" = c("Be", "Bp", "Ba"),
      "object" = c("Oe", "Op", "Oa")
    )

    fundamentals <- stack_pair_ratings(events, solve_for, private$.dictionary)

    Im <- cbind(fundamentals, get_data_matrix(fundamentals, private$.equations))
    S <- private$.selection_matrix[, col_select]
    H <- get_h_matrix(private$.equations)

    out <- solve_equations(Im, S, H)
    dplyr::as_tibble(out)

  }
)

## Closest Term ------------------------------------------------------------

#' @title Get Closest Terms to an EPA profile
#'
#' @name method-closest-terms
#' @aliases closest_terms
#' @family InteRactModel methods
#'
#' @description The `$closet_terms()` method does this and that..
#'
#' @param epa a vector or list of epa ratings
#'
#' It also works with one row data frame with `e`, `p`, and `a` columns
#'
#' @return a list of closest terms found in `$dictionary`, sorted by closeness.
#'
InteRactModel$set(
  "public", "closest_terms",
  function(epa, component = c("identity", "behavior", "modifier"), max_dist = 1) {

    if (length(epa) != 3) stop(call. = FALSE, "`epa` must be three numbers")

    x <- match.arg(component)

    lookup <- private$.dictionary[private$.dictionary$component == x, ]
    fundamentals <- do.call(rbind, lookup$ratings)
    rownames(fundamentals) <- lookup$term

    ssd <- rowSums(sweep(fundamentals, MARGIN = 2, FUN = "-", unlist(epa))^2)
    i <- which(ssd <= max_dist)
    return(sort(ssd[i]))

  })



