
#' @title Create a new `InteRactModel` object
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

#' @title InteRactModel objects
#'
#' @name InteRactModel
#' @description An `InteRactModel` object is an [R6][R6::R6Class] object created
#'   by the [interact()] function.
#'
#'   The object stores (1) a `$dictionary` of EPA ratings and (2) a set of `$equations` used to calculate "transient impressions."
#'
#'
#' @section Methods:
#'  `InteRactModel` objects have the following associated methods, many of which have their own documentation pages:
#'
#'  - [`$deflection()`][method-deflection]
#'  - [`$optimal_behavior()`][method-optimal-behavior]
#'  - [`$reidentify()`][method-reidentify]
#'  - [`$max_confirm()`][method-max-confirm]
#'  - [`$closest_terms()`][method-closest-terms]
#'  - [`$modify_identity()`][method-modify-identity]
#'  - [`$characteristic_emotion()`][method-characteristic-emotion]
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


#' @title Lookup EPA ratings of terms in a `$dictionary`
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

#' @title Calculate ABO event deflection scores
#'
#' @name method-deflection
#' @aliases deflection
#' @family InteRactModel methods
#'
#' @description The `$deflection()` method calculates the deflection of an ABO event.
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
deflection <- function(events) {

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
}
InteRactModel$set("public", "deflection", value = deflection)


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
optimal_behavior <- function(x, who = c("actor", "object")) {

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
}
InteRactModel$set("public", "optimal_behavior", value = optimal_behavior)


#' @title Reidentification of actor or object, following an event
#'
#' @name method-reidentify
#' @aliases reidentify
#' @family InteRactModel methods
#'
#' @description The `$reidentify()` method does this and that..
#'
#' @param x an "event deflection" object created by the `$deflection()` method
#'
#' @param who (character) either "actor" or "object"
#'
#' @return a data frame of EPA profiles for the optimal reidentification
#'
reidentify <- function(x, who = c("actor", "object")) {

    who <- match.arg(who)
    if (!inherits(x, "event_deflection")) stop(call. = FALSE, "`x` must be a data frame created by the $deflection() method")

    col_select <- switch(who,
      "actor" = epa_selector("A"),
      "object" = epa_selector("O")
    )

    fundamentals <- get_fundamentals(x)
    fundamentals[col_select] <- 1

    Im <- cbind(fundamentals, get_data_matrix(fundamentals, private$.impressionabo))
    S <- private$.selection_matrix[, col_select]
    H <- get_h_matrix(private$.impressionabo)

    out <- solve_equations(Im, S, H)
    return(dplyr::as_tibble(out))
}
InteRactModel$set("public", "reidentify", value = reidentify)


#' @title Identify the behavior that would maximally confirm the identities of actor
#' in an actor-object pairing
#'
#' @name method-max-confirm
#' @aliases max_confirm
#' @family InteRactModel methods
#'
#' @description The `$max_confirm(..., solve_for = "behavior")` method identify the behavior that would maximally confirm the identities of actor
#' in an actor-object pairing.
#'
#' does this and that..
#'
#' @param events A data frame with only only a pair of A, B, or O.
#' @param solve_for "behavior", "actor", or "object"
#'
#' @return A data frame with the maximally confirming EPA profiles
#'
#' @examples
#' \dontrun{
#' act$max_confirm(
#'   events = tibble(A = "god", O = "deadbeat"),
#'   solve_for = "behavior"
#' )
#'
#' act$max_confirm(
#'   events = list(A = "god", B = "kill"),
#'   solve_for = "object"
#' )
#'
#' act$max_confirm(
#'   events = data.frame(B = "kill", O = "deadbeat"),
#'   solve_for = "actor"
#' )
#' }
#'
max_confirm <- function(events, solve_for = c("behavior", "actor", "object")) {

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
InteRactModel$set("public", "max_confirm", value = max_confirm)

#' @title Get closest terms to an EPA profile
#'
#' @name method-closest-terms
#' @aliases closest_terms
#' @family InteRactModel methods
#'
#' @description The `$closest_terms()` method does this and that..
#'
#' @param epa a vector or list of epa ratings
#' @param component a vector or list of epa ratings
#' @param max_dist a positive real number
#'
#' It also works with one row data frame with `e`, `p`, and `a` columns
#'
#' @return a list of closest terms found in `$dictionary`, sorted by closeness.
#'
closest_terms <- function(epa, component = c("identity", "behavior", "modifier"), max_dist = 1) {
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
}
InteRactModel$set("public", "closest_terms", value = closest_terms)

# Extra Methods -------------------------------------------------------

# To add:
# - Self Direction
# - Settings

#' @title Set up additional transformation equations
#'
#' @name method-add-equation
#' @aliases add_equation
#' @family InteRactModel methods
#'
#' @description The `$add_equation()` method allows for the inclusion of further equations.
#'
#' Adding `"emotionid"` is needed to use the `$characteristic_emotion()` method.
#'
#' Adding `"traitid"` is needed to use the `$modify_identity()` method.
#'
#' @param type one of `"emotionid"` or `"traitid"`
#' @param group one of `"all"`, `"female"`, or `"male"`
#'
add_equation <- function(type = c("emotionid", "traitid"), group = c("all", "female", "male")) {

    equation_type <- match.arg(type)
    group <- match.arg(group)

    if (equation_type == "emotionid") {
      key <- private$.info$impressionabo$key
      equations <- validate_emotionid_equations(c(key, group))
      private$.info$traitid <- list(key = key, group = group, equation_type = equation_type)
      cli::cli_bullets(c("v" = "emotionid = list(key = \"{key}\", group = \"{group}\")"))
      private$.emotionid <- do.call(get_equation, equations)
    }

    if (equation_type == "traitid") {
      key <- private$.info$impressionabo$key
      equations <- validate_traitid_equations(c(key, group))
      private$.info$traitid <- list(key = key, group = group, equation_type = equation_type)
      cli::cli_bullets(c("v" = "traitid = list(key = \"{key}\", group = \"{group}\")"))
      private$.traitid <- do.call(get_equation, equations)
    }
}
InteRactModel$set("public", "add_equation", value = add_equation)


#' @title Create EPA profile for modified identities
#'
#' @name method-modify-identity
#' @aliases modify_identity
#' @family InteRactModel methods
#'
#' @description The `$modify_identity()` applies the modifier equations (`traitid`)
#' to find the EPA profile of a modified identity.
#'
#' @param events A data frame with `M` (modifier) and `I` (identity) columns
#'
#' @return A new dictionary rating for the modified identities
#'
modify_identity <- function(events) {

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
}
InteRactModel$set("public", "modify_identity", modify_identity)


#' @title Estimate EPA profile of a characteristic emotion
#'
#' @name method-characteristic-emotion
#' @aliases characteristic_emotion
#' @family InteRactModel methods
#'
#' @param events A data frame with an `I` column (for "identity")
#'
characteristic_emotion <- function(events) {

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

    if (requireNamespace("pbapply", quietly = TRUE)) {
      loop <- pbapply::pbapply  ## for progress bars
    } else {
      loop <- base::apply
    }

    ## pre-compute selector matrix
    S <- sapply(paste0("M", c("e", "p", "a")), function(m) {
      as.integer(grepl(m, colnames(M)))
    }, simplify = TRUE)

    ## pre-compute selector vector
    g <- matrix(1 - rowSums(S), ncol = 1)

    out <- loop(M, MARGIN = 1, function(row) {
      ## See chapter 14 of Heise (2006)
      r <- row[c("Ie", "Ip", "Ia")]
      X <- sweep(private$.emotionid, 1, row, `*`) |> t()  ## I am clever :)
      solve(X %*% S, r - X %*% g)

    }, simplify = TRUE)

    rownames(out) <- colnames(S)
    return(dplyr::as_tibble(t(out)))

}
InteRactModel$set("public", name = "characteristic_emotion", value = characteristic_emotion)


