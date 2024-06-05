
# To do:
# - see if you can make stack_epa_ratings faster
# - stop reusing stuff for the other stuff (you know what I mean)
# - bulk for maximally likely behaviors
# - see why Event Deflection header doesn't knit with proper color
# - document R6 methods following the cmdstanr convention
#.  https://roxygen2.r-lib.org/articles/rd-other.html#r6
# - Add more informative errors for when people use non event_deflection objects

#' @title Create a new InteRact object
#'
#' @description
#' Create a new [`InteRact`] by specifying dictionary and
#' equation names available from the `actdata` package.
#'
#' @param dictionary (character) dictionary name.
#'
#' See available options using [`actdata::dataset_keys()`]
#'
#' Currently only dictionaries that have "`group == all`" are valid.
#'
#' @param equation (character) equation name.
#'
#' See available options using [`actdata::equations`]
#'
#' Currently only equations that have "`equation_type == impressionabo`" are valid.
#'
#' @param group (character) one of "all" (default), "male", or "female." This specifies the equation type.
#'
#' @return An [`InteRact`] object.
#'
#' @export
build_interact <- function(dictionary = "usfullsurveyor2015", equation = "us2010", group = "all") {
  InteRact$new(dictionary, equation, group)
}

# InteRact ----------------------------------------------------------------

#' @title InteRact Objects
#'
#' @name InteRact
#' @description An `InteRact` object is an [R6][R6::R6Class] object created
#'   by the [build_interact()] function.
#'
#'   The object stores (1) a dictionary of EPA ratings
#'   or "fundamentals" and (2) an ACT equation used to calculate "transient impressions."
#'
#'   It also provides methods for calculating deflection scores, behaviors, reidentification,
#'   among others.
#'
#' @section Methods: `InteRact` objects have the following associated
#'   methods, many of which have their own (linked) documentation pages:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$deflection()`][method-deflection] | Return an "Event Deflection" data frame. |
#'  `$optimal_behavior()` | Return... |
#'  `$reidentify_object()`| Return... |
#'  `$closest_terms()`|  Return... |
#'
#'
InteRact <- R6::R6Class(
  classname = "InteRact",

  public = list(
    initialize = function(dictionary = "usfullsurveyor2015", equation = "us2010", group = "all") {
      private$.dictionary <- get_dictionary(dataset = dictionary)
      self$equation <- get_equation(key = equation, group = group)
      private$.dict <- dictionary  ## this is
      private$.group <- group      ## for
      private$.eq <- equation      ## printing only
    },
    print = function() {
      cli::cli_h1("Interact Analysis")
      cli::cli_alert_info("Dictionary: {private$.dict}")
      cli::cli_alert("group: all")
      cli::cli_alert_info("Equations: {private$.eq}")
      cli::cli_alert("group: {private$.group}")
      cli::cli_alert("type: impressionabo")

    },
    equation = NULL
  ),

  active = list(
    dictionary = function(value) {
      if (missing(value)) {
        private$.dictionary
      } else {
        validate_new_dictionary(value)
        cli::cli_alert_success("added new dictionary")
        private$.dictionary <- value
        private$.dict <- "External [!]"
      }
    }
  ),

  private = list(
    .dictionary = NULL,
    .dict = NULL,
    .group = NULL,
    .eq = NULL
  )

)

# deflection --------------------------------------------------------------

#' @title Calculate Event Deflection Scores
#'
#' @name method-deflection
#' @aliases deflection
#' @family InteRact methods
#'
#' @description The `$deflection()` method does this and that..
#'
#' @param events a data frame with A, B, and O
#'
#' Each has to exist within the `$dictionary` field
#'
#' @seealso [get_transients()], [get_fundamentals()], [get_element_wise_deflection()]
#'
#' @examples
#' \dontrun{
#' act <- interact_obj(dictionary = "usfullsurveyor2015", equation = "us2010")
#' d <- act$deflection(events = data.frame(A = "mother", B = "abandon", O = "baby"))
#' d
#'}
InteRact$set(
  "public", "deflection",
  function(events) {

    fundamentals <- stack_epa_ratings(events, private$.dictionary)
    X <- get_data_matrix(fundamentals, self$equation)
    transients <- X %*% self$equation
    element_wise_deflection <- (transients - fundamentals)^2
    events$deflection <- as.vector(rowSums(element_wise_deflection))
    events <- dplyr::as_tibble(events)

    # output
    structure(
      events,
      class = c("event_deflection", class(events)),
      element_wise_deflection = dplyr::as_tibble(element_wise_deflection),
      transients = dplyr::as_tibble(transients),
      fundamentals = dplyr::as_tibble(fundamentals)
    )
})


# optimal behavior --------------------------------------------------------

InteRact$set(
  "public", "optimal_behavior",
  function(events, who = c("actor", "object")) {
    #who <- match.arg(who) ## FINISH THE ACTOR VS OBJECT
    data <- get_actor(events, private$.dictionary, self$equation)
    Ib <- apply(data, 1, diag, simplify = FALSE)

    identity <- diag(ncol(self$equation))
    h <- rbind(identity, -1*self$equation) %*% cbind(identity, -1*t(self$equation))

    S <- matrix(0, ncol = 3, nrow = ncol(data)) ## selection matrix

    S[which(grepl("Be", colnames(data))), 1] <- 1
    S[which(grepl("Bp", colnames(data))), 2] <- 1
    S[which(grepl("Ba", colnames(data))), 3] <- 1

    colnames(S) <- paste0(rep("B", 3), c("e", "p", "a"))
    g <- matrix(1 - rowSums(S), ncol = 1)

    out <- purrr::map(Ib, function(X) {
      term1 <- t(S) %*% X %*% h %*% X %*% S
      term1 <- -1 * solve(term1)

      term2 <- t(S) %*% X %*% h %*% X %*% g
      t(term1 %*% term2)

    })

    as.data.frame(do.call(rbind, out))

  })

# reidentify object -------------------------------------------------------

InteRact$set(
  "public", "reidentify_object",
  function(events) {

    stopifnot(inherits(events, "event_deflection"))

    data <- get_object(events, self$equation)
    Ib <- apply(data, 1, diag, simplify = FALSE)

    identity <- diag(ncol(self$equation))
    h <- rbind(identity, -1*self$equation) %*% cbind(identity, -1*t(self$equation))

    S <- matrix(0, ncol = 3, nrow = ncol(data)) ## selection matrix

    S[which(!grepl("Ae", colnames(data))), 1] <- 1
    S[which(!grepl("Ap", colnames(data))), 2] <- 1
    S[which(!grepl("Aa", colnames(data))), 3] <- 1

    colnames(S) <- paste0(rep("O", 3), c("e", "p", "a"))
    g <- 1 - rowSums(S)

    out <- purrr::map(Ib, function(X) {
      term1 <- t(S) %*% X %*% h %*% X %*% S
      term1 <- -1 * solve(term1)

      term2 <- t(S) %*% X %*% h %*% X %*% g
      t(term1 %*% term2)

    })

    dplyr::as_tibble(do.call(rbind, out))

  })

# Closest Term ------------------------------------------------------------

InteRact$set(
  "public", "closest_term",
  function(ratings, component = c("identity", "behavior", "modifier"), max_dist = 1) {

    x <- match.arg(component)

    lookup <- private$.dictionary[private$.dictionary$component == x, ]
    dict_epa <- do.call(rbind, lookup$ratings)
    rownames(dict_epa) <- lookup$term

    out <- apply(ratings, 1, function(x) {

      ssd <- rowSums(sweep(dict_epa, MARGIN = 2, unlist(x))^2)
      i <- which(ssd <= max_dist)
      sort(ssd[i])

    }, simplify = FALSE)

    return(out)

})


# Maximally Confirm Behavior ----------------------------------------------



