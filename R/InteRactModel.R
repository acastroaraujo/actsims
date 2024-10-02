
#' @title Create a new InteRactModel object
#'
#' @description
#' Create a new [`InteRactModel`] object by specifying dictionary and
#' equation names available from the `actdata` package.
#'
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
#' @return An [`InteRactModel`] object.
#'
#' @export
interact <- function(dictionary = list("usfullsurveyor2015", "all"), equations = list("us2010", "all")) {

  if (missing(dictionary)) {
    cli::cli_bullets(c("v" = "dictionary = list(dataset = \"usfullsurveyor2015\", group = \"all\")"))
  }

  if (missing(equations)) {
    cli::cli_bullets(c("v" = "equations = list(key = \"us2010\", group = \"all\")"))
  }

  InteRactModel$new(dictionary, equations)
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
    initialize = function(dictionary = list("usfullsurveyor2015", "all"), equations = list("us2010", "all")) {

      dictionary <- validate_dictionary(dictionary)
      equations <- validate_impressionabo_equations(equations)

      private$.impressionabo <- do.call(get_equation, equations)
      private$.dictionary <- do.call(get_dictionary, dictionary)
      private$.selection_matrix <- get_selection_matrix(private$.impressionabo)

      ## for printing
      private$.info <- list(
        dict = dictionary,
        impressionabo = equations
      )

    },
    print = function() {
      cli::cli_h1("Interact Analysis")
      cli::cli_alert_info("Dictionary: {private$.info$dict[[1]]} (group: {private$.info$dict[[2]]})")
      cli::cli_alert_info("Equations: {private$.info$impressionabo$key}")
      cli::cli_bullets(c(" " = "impressionabo (group: {private$.info$impressionabo$group})"))

      if (!is.null(private$.traitid)) {
        cli::cli_bullets(c(" " = "traitid (group: {private$.info$traitid$group})"))
      }

      if (!is.null(private$.emotionid)) {
        cli::cli_bullets(c(" " = "emotionid (group: {private$.info$emotionid$group})"))
      }
      invisible(self)
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
        private$.info$dict$dataset <- "External [!]"
        private$.info$dict$group <- "?"
      }
    },
    equations = function(value) {
      if (missing(value)) {
        out <- private$.impressionabo
        # The `print` looks better with the "'" to indicate predictions
        # But the code breaks apart when we actually replace the original
        # equation column names.
        colnames(out) <- paste0(colnames(out), "'")
        out
      } else {
        stop("Can't set `$equation`", call. = FALSE)
      }
    }
  ),

  private = list(
    # for internal use
    .dictionary = NULL,
    .impressionabo = NULL,
    .selection_matrix = NULL,
    # extra `add_equations` method
    .emotionid = NULL,
    .traitid = NULL,
    # for printing
    .info = NULL
  )

)


#' @title Lookup fundamentals in dictionary
#'
#' @name method-fundamentals
#' @aliases fundamentals
#' @family InteRactModel methods
#'
#' @description The `$fundamentals()` method does this and that..
#'
#' @param x (character) terms contained within the `$dictionary`
#'
#' @return A data frame of EPA profiles
#'
#' @seealso [get_fundamentals()]
#'
fundamentals <- function(x) {

  i <- x %in% private$.dictionary$term
  ok <- all(i)

  if (!ok) {
    cli::cli_abort("`{x[!i]}` not found in `$dictionary`", call = NULL)
  }

  row_select <- private$.dictionary$term %in% x

  out <- dplyr::bind_cols(
    private$.dictionary[row_select, c("term", "component")],
    dplyr::bind_rows(private$.dictionary[["ratings"]][row_select])
  )

  # S3 class output
  structure(
    out,
    class = c("fundamentals", class(out)),
    dictionary = private$.dict,
    group = private$.group_dict
  )

}
InteRactModel$set("public", "fundamentals", fundamentals)


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

    events <- validate_events(events, private$.dictionary)
    validate_deflection(names(events))

    fundamentals <- stack_abo_ratings(events, private$.dictionary)
    M <- get_data_matrix(fundamentals, private$.impressionabo)
    transients <- M %*% private$.impressionabo
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

    col_select <- epa_selector("B")

    fundamentals <- get_fundamentals(x)
    transients <- get_transients(x)
    fundamentals[col_select] <- 1
    transients[col_select] <- 1

    if (who == "object") {
      fundamentals <- reverse_ao(fundamentals)
      transients <- reverse_ao(transients)
    }

    Im <- cbind(fundamentals, get_data_matrix(transients, private$.impressionabo))
    S <- private$.selection_matrix[, col_select]
    H <- get_h_matrix(private$.impressionabo)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))

  })

## Reidentification --------------------------------------------------------

#' @title Reidentification of Actor or Object, following an event
#'
#' @name method-reidentify
#' @aliases reidentify
#' @family InteRactModel methods
#'
#' @description The `$reidentify()` method does this and that..
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
      "actor" = epa_selector("A"),
      "object" = epa_selector("O")
    )

    fundamentals <- get_fundamentals(x)
    fundamentals[col_select] <- 1
    #transients <- get_transients(x)
    #transients[col_select] <- 1 ## the transients are not being used anywhere [??]

    Im <- cbind(fundamentals, get_data_matrix(fundamentals, private$.impressionabo))
    S <- private$.selection_matrix[, col_select]
    H <- get_h_matrix(private$.impressionabo)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))

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
#' @param events a data frame with only only a pair of A, B, or O.
#'
#' @return a data frame with the maximally confirming EPA profiles
#'
InteRactModel$set(
  "public", "max_confirm",
  function(events, solve_for = c("behavior", "actor", "object")) {

    solve_for <- match.arg(solve_for)
    events <- validate_events(events, private$.dictionary)
    validate_max_confirm(names(events), solve_for)

    fundamentals <- stack_pair_ratings(events, solve_for, private$.dictionary)

    col_select <- switch(solve_for,
      "actor" = epa_selector("A"),
      "behavior" = epa_selector("B"),
      "object" = epa_selector("O")
    )

    Im <- cbind(fundamentals, get_data_matrix(fundamentals, private$.impressionabo))
    S <- private$.selection_matrix[, col_select]
    H <- get_h_matrix(private$.impressionabo)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))

  }
)

## Closest Term ------------------------------------------------------------

#' @title Get Closest Terms to an EPA profile
#'
#' @name method-closest-terms
#' @aliases closest_terms
#' @family InteRactModel methods
#'
#' @description The `$closest_terms()` method does this and that..
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

    epa <- validate_epa(epa)
    x <- match.arg(component)

    lookup <- private$.dictionary[private$.dictionary$component == x, ]
    fundamentals <- do.call(rbind, lookup$ratings)
    rownames(fundamentals) <- lookup$term

    out <- apply(epa, MARGIN = 1, function(row) {
      ssd <- rowSums(sweep(fundamentals, MARGIN = 2, FUN = "-", unlist(row))^2)
      i <- which(ssd <= max_dist)
      sort(ssd[i])
    }, simplify = FALSE)

    if (length(out) == 1L) out <- unlist(out)
    return(out)

  })


# Extra Methods -------------------------------------------------------

# To add:
# - Self Direction
# - Settings

## Add Equations ----------------------------------------------------------

#' @title Add Other Transformation Equation
#'
#' @name method-add-equations
#' @aliases add_equation
#' @family InteRactModel methods
#'
#' @description The `$add_equations()` method does this and that..
#'
#' @param type one of "emotionid" or "traitid"
#' @param group one of "all" or "female" or "male"
#'
#' @return adds the respective transformation equation to the InteRactModel object
#'
InteRactModel$set(
  "public", "add_equation",
  function(type = c("emotionid", "traitid"), group = c("all", "female", "male")) {

    equation_type <- match.arg(type)
    group <- match.arg(group)

    if (equation_type == "emotionid") {

      key <- private$.info$impressionabo$key
      equations <- validate_emotionid_equations(c(key, group))
      private$.info$traitid <- list(key = key, group = group, equation_type = equation_type)

      private$.emotionid <- do.call(get_equation, equations)
    }

    if (equation_type == "traitid") {

      key <- private$.info$impressionabo$key
      equations <- validate_traitid_equations(c(key, group))
      private$.info$traitid <- list(key = key, group = group, equation_type = equation_type)

      private$.traitid <- do.call(get_equation, equations)
    }
  })


## Modify Identity --------------------------------------------------------

#' @title Modify Identity
#'
#' @name method-modify-identity
#' @aliases modify_identity
#' @family InteRactModel methods
#'
#' @description The `$modify_identity()` method does this and that..
#'
#' @param events data.frame(M = c(...), I = (...))
#'
#' @return a new dictionary rating for ththe modified identity
#'
InteRactModel$set(
  "public", "modify_identity",
  function(events) {

    if (is.null(private$.traitid)) {
      cli::cli_abort("must first set up `traitid` equation with the `$add_equation` method")
    }

    events <- validate_mi_events(events, private$.dictionary)
    validate_modify_identity(names(events))

    fundamentals <- stack_mi_ratings(events, private$.dictionary)
    fundamentals <- as.data.frame(fundamentals)
    M <- get_data_matrix(fundamentals, private$.traitid)
    out <- M %*% private$.traitid

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

})



## Characteristic Emotion -------------------------------------------------

InteRactModel$set(
  "public", "characteristic_emotion",
  function(events) {

    if (is.null(private$.emotionid)) {
      cli::cli_abort("must first set up `emotionid` equation with the `$add_equation` method")
    }

    events <- validate_ce_events(events, private$.dictionary)

    fundamentals <- private$.dictionary |>
      dplyr::filter(.data$term %in% events[["I"]], .data$component == "identity") |>
      dplyr::pull("ratings")

    fundamentals <- dplyr::bind_rows(fundamentals)
    colnames(fundamentals) <- paste0("I", colnames(fundamentals))

    data <- dplyr::bind_cols(fundamentals, dplyr::tibble(Me = 1, Mp = 1, Ma = 1))
    M <- get_data_matrix(data, private$.emotionid)

    r <- fundamentals |> unlist()
    ## See chapter 14 of Heise (2006)

    if (requireNamespace("pbapply", quietly = TRUE)) {
      loop <- pbapply::pbapply  ## for progress bars
    } else {
      loop <- base::apply
    }

    apply(M, )




    X <- sweep(private$.emotionid, 1, M, `*`) |> t()      ## I am clever :)

    S <- sapply(paste0("M", c("e", "p", "a")), function(m) {
      as.integer(grepl(m, colnames(X)))
    }, simplify = TRUE)                   ## selector matrix

    g <- matrix(1 - rowSums(S), ncol = 1) ## selector vector

    out <- solve(X %*% S, r - X %*% g)

    return(dplyr::as_tibble(t(out)))

  })


