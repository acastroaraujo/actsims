
# Validation --------------------------------------------------------------

validate_new_dictionary <- function(dict) {

  ## this gets used in the active binding dictionary field.

  df_ok <- all(c("term", "component", "ratings") %in% colnames(dict))

  if (df_ok) {
    cols_ok <- all(purrr::map_lgl(dict$ratings, \(x) all(names(x) == c("e", "p", "a"))))
    if (!cols_ok) cli::cli_abort("The ratings column must be a list of epa ratings", class = NULL)
    return(dict)
  }

  ok <- all(c("term", "component", "ratings", "e", "p", "a") %in% colnames(dict))

  if (ok) {
    out <- dict[c("term", "component")]
    out$ratings <- apply(dict[c("e", "p", "a")], 1, c, simplify = FALSE)
    return(out)

  } else {
    cli::cli_abort("The dictionary is malformed.", class = NULL)
  }

}

validate_dictionary <- function(x) {

  ## this gets used during initialize

  if (length(x) > 2) stop(call. = FALSE, "`dictionary` argument is malformed")
  if (length(x) == 1) {
    x[[2]] <- "all"
    cli::cli_bullets(c("v" = "dictionary = list(dataset = \"{x[[1]]}\", group = \"all\")"))
  }

  names(x) <- c("dataset", "group")

  dicts <- purrr::map(actdata::get_dicts(), \(x) x@groups)
  names(dicts) <- purrr::map_chr(actdata::get_dicts(), \(x) x@key)

  ok <- x[["dataset"]] %in% names(dicts)

  if (!ok) {
    cli::cli_abort("`{x[['dataset']]}` not found in {.pkg `actdata`} package", call = NULL)
  }

  groups <- dicts[[x[['dataset']]]]
  ok <- x[["group"]] %in% groups

  if (!ok) {
    cli::cli_alert_warning("`{x[['dataset']]}` groups: {groups}")
    cli::cli_abort("`{x['group']}` group not found in `{x[['dataset']]}` dictionary in {.pkg `actdata`} package", call = NULL)
  }

  out <- as.list(x) ## must return list for do.call in InteRactModel
  return(out)

}

validate_impressionabo_equations <- function(x) {

  ## this gets used during initialize

  if (length(x) > 2) stop(call. = FALSE, "`equations` argument is malformed")
  if (length(x) == 1) {
    x[[2]] <- "all"
    cli::cli_bullets(c("v" = "equations = list(key = \"{x[[1]]}\", group = \"all\")"))
  }

  names(x) <- c("key", "group")

  sub_eq <- dplyr::filter(actdata::equations, .data$key == !!x[["key"]])

  if (!nrow(sub_eq) >= 1) {
    cli::cli_abort("`{x[['key']]}` not found in {.pkg `actdata`} package", call = NULL)
  }

  ok <- "impressionabo" %in% unique(sub_eq[["equation_type"]])

  if (!ok) {
    cli::cli_abort("`{x[['key']]}` must have an `impressionabo` equation type in {.pkg `actdata`}", call = NULL)
  }

  groups <- sub_eq[sub_eq$equation_type == "impressionabo", ][["group"]]
  ok <- x[["group"]] %in% groups

  if (!ok) {
    cli::cli_alert_warning("equations groups: {groups}")
    cli::cli_abort("`{x[['group']]}` not found in `{x[['key']]}` equations in {.pkg `actdata`} package", call = NULL)
  }

  out <- as.list(x) ## must return list for do.call in InteRactModel
  out$equation_type <- "impressionabo"

  return(out)

}

validate_max_confirm <- function(event_nms, solve_for) {

  nms <- c("A", "B", "O")
  x <- toupper(substr(solve_for, 1, 1))
  required_events <- setdiff(nms, x)

  ignore <- intersect(x, event_nms)
  ok <- all(required_events %in% event_nms)

  if (!ok) {
    cli::cli_abort(
      message = "`solve_for = {solve_for}` requires `events` to have {required_events} names",
      call = NULL
    )
  }

  if (!purrr::is_empty(ignore)) {
    cli::cli_warn("Ignoring `{ignore}` elements in `events`")
  }

}

validate_events <- function(events, dict) {

  ok <- all(purrr::map_lgl(events, is.character))
  if (!ok) { ## avoid subsetting with factors [!]
    events[] <- lapply(events, as.character)
  }
  identities <- dict[dict$component == "identity", ][["term"]]
  behaviors <- dict[dict$component == "behavior", ][["term"]]

  for (x in names(events)) {
    if (x == "B") {
      terms <- unique(events[[x]])
      i <- terms %in% behaviors
      ok <- all(i)
      if (!ok) cli::cli_abort("`{terms[!i]} is not a `behavior` in `$dictionary`", call = NULL)
    } else {
      terms <- unique(events[[x]])
      i <- terms %in% identities
      ok <- all(i)
      if (!ok) cli::cli_abort("`{terms[!i]} is not an `identity` in `$dictionary`", call = NULL)
    }
  }

  return(events)

}

validate_deflection <- function(event_nms) {

  ok <- all(c("A", "B", "O") %in% event_nms)

  if (!ok) {
    cli::cli_abort("`events` must have A, B, and O elements", call = NULL)
  }

}

## To do: change this to generic function

validate_epa <- function(epa) {

  nms <- c("e", "p", "a")

  if (inherits(epa, what = "fundamentals")) {
    out <- epa[nms]
  }

  if (inherits(epa, what = "numeric")) {
    ok <- all(nms %in% names(epa))
    if (!ok) cli::cli_abort("`epa` must be a named vector", call = NULL)
    out <- as.data.frame(rbind(epa))
    rownames(out) <- NULL
  }

  if (inherits(epa, what = "list")) {
    ok <- all(nms %in% names(epa))
    if (!ok) cli::cli_abort("`epa` must be a named list", call = NULL)
    out <- as.data.frame(epa)
  }

  if (inherits(epa, what = "data.frame") & !inherits(epa, what = "fundamentals")) {

    # Case when each column has two letters,
    # check that first letters are equal and second letters are e, p, a

    one_letter <- all(nchar(names(epa)) == 1)
    two_letter <- all(nchar(names(epa)) == 2)

    if (two_letter) {
      prefix <- substr(names(epa), 1, 1)
      suffix <- substr(names(epa), 2, 2)

      ok <- length(unique(prefix)) == 1L & all(suffix %in% nms)
      if (!ok) cli::cli_abort("`epa` data frame is malformed", call = NULL)

      names(epa) <- suffix
      out <- epa[nms]
    }

    if (one_letter) {

      ok <- all(names(epa) %in% nms)
      if (!ok) cli::cli_abort("`epa` data frame is malformed", call = NULL)

      out <- epa[nms]
    }
  }

  return(out)

}


validate_traitid_equations <- function(x) {

  if (length(x) > 2) stop(call. = FALSE, "`equations` argument is malformed")
  if (length(x) == 1) {
    x[[2]] <- "all"
    cli::cli_bullets(c(">" = "equations = list(key = \"{x[[1]]}\", group = \"all\")"))
  }

  names(x) <- c("key", "group")

  sub_eq <- dplyr::filter(actdata::equations, .data$key == !!x[["key"]])

  if (!nrow(sub_eq) >= 1) {
    cli::cli_abort("`{x[['key']]}` not found in {.pkg `actdata`} package", call = NULL)
  }

  ok <- "traitid" %in% unique(sub_eq[["equation_type"]])

  if (!ok) {
    cli::cli_abort("`{x[['key']]}` must have an `traitid` equation type in {.pkg `actdata`}", call = NULL)
  }

  groups <- sub_eq[sub_eq$equation_type == "traitid", ][["group"]]
  ok <- x[["group"]] %in% groups

  if (!ok) {
    cli::cli_alert_warning("equations groups: {groups}")
    cli::cli_abort("`{x[['group']]}` not found in `{x[['key']]}` equations in {.pkg `actdata`} package", call = NULL)
  }

  out <- as.list(x) ## must return list for do.call
  out$equation_type <- "traitid"

  return(out)

}

validate_emotionid_equations <- function(x) {

  if (length(x) > 2) stop(call. = FALSE, "`equations` argument is malformed")
  if (length(x) == 1) {
    x[[2]] <- "all"
    cli::cli_bullets(c(">" = "equations = list(key = \"{x[[1]]}\", group = \"all\")"))
  }

  names(x) <- c("key", "group")

  sub_eq <- dplyr::filter(actdata::equations, .data$key == !!x[["key"]])

  if (!nrow(sub_eq) >= 1) {
    cli::cli_abort("`{x[['key']]}` not found in {.pkg `actdata`} package", call = NULL)
  }

  ok <- "emotionid" %in% unique(sub_eq[["equation_type"]])

  if (!ok) {
    cli::cli_abort("`{x[['key']]}` must have an `traitid` equation type in {.pkg `actdata`}", call = NULL)
  }

  groups <- sub_eq[sub_eq$equation_type == "emotionid", ][["group"]]
  ok <- x[["group"]] %in% groups

  if (!ok) {
    cli::cli_alert_warning("equations groups: {groups}")
    cli::cli_abort("`{x[['group']]}` not found in `{x[['key']]}` equations in {.pkg `actdata`} package", call = NULL)
  }

  out <- as.list(x) ## must return list for do.call
  out$equation_type <- "emotionid"

  return(out)

}


validate_mi_events <- function(events, dict) {

  ok <- all(purrr::map_lgl(events, is.character))
  if (!ok) { ## avoid subsetting with factors [!]
    events[] <- lapply(events, as.character)
  }

  identities <- dict[dict$component == "identity", ][["term"]]
  modifiers <- dict[dict$component == "modifier", ][["term"]]

  terms <- unique(events[["M"]])
  i <- terms %in% modifiers
  ok <- all(i)

  if (!ok) {
    cli::cli_abort("`{terms[!i]} is not a `modifier` in `$dictionary`", call = NULL)
  }

  terms <- unique(events[["I"]])
  i <- terms %in% identities
  ok <- all(i)

  if (!ok) {
    cli::cli_abort("`{terms[!i]} is not an `identity` in `$dictionary`", call = NULL)
  }

  return(events)

}


validate_emotionid_equations <- function(x) {

  if (length(x) > 2) stop(call. = FALSE, "`equations` argument is malformed")
  if (length(x) == 1) {
    x[[2]] <- "all"
    cli::cli_bullets(c(">" = "equations = list(key = \"{x[[1]]}\", group = \"all\")"))
  }

  names(x) <- c("key", "group")

  sub_eq <- dplyr::filter(actdata::equations, .data$key == !!x[["key"]])

  if (!nrow(sub_eq) >= 1) {
    cli::cli_abort("`{x[['key']]}` not found in {.pkg `actdata`} package", call = NULL)
  }

  ok <- "emotionid" %in% unique(sub_eq[["equation_type"]])

  if (!ok) {
    cli::cli_abort("`{x[['key']]}` must have an `traitid` equation type in {.pkg `actdata`}", call = NULL)
  }

  groups <- sub_eq[sub_eq$equation_type == "emotionid", ][["group"]]
  ok <- x[["group"]] %in% groups

  if (!ok) {
    cli::cli_alert_warning("equations groups: {groups}")
    cli::cli_abort("`{x[['group']]}` not found in `{x[['key']]}` equations in {.pkg `actdata`} package", call = NULL)
  }

  out <- as.list(x) ## must return list for do.call
  out$equation_type <- "emotionid"

  return(out)

}


validate_ce_events <- function(events, dict) {

  ok <- all(purrr::map_lgl(events, is.character))
  if (!ok) { ## avoid subsetting with factors [!]
    events[] <- lapply(events, as.character)
  }

  ok <- all(c("I") %in% names(events)) ## this validation pattern should be added to validate_characteristic_emotion for consistency with other functions....
  if (!ok) {
    cli::cli_abort("`events` must have an I element", call = NULL)
  }

  identities <- dict[dict$component == "identity", ][["term"]]

  terms <- unique(events[["I"]])
  i <- terms %in% identities
  ok <- all(i)

  if (!ok) {
    cli::cli_abort("`{terms[!i]} is not an `identity` in `$dictionary`", call = NULL)
  }

  return(events)

}


validate_modify_identity <- function(event_nms) {

  ok <- all(c("M", "I") %in% event_nms)

  if (!ok) {
    cli::cli_abort("`events` must have M and I elements", call = NULL)
  }

}


# Print Methods -----------------------------------------------------------

#' @export
tbl_format_header.event_deflection <- function(x, setup, ...) {
  c(cli::col_blue("# Event deflection"), cli::col_blue("# A data frame: ", setup$tbl_sum))
}

#' @export
tbl_format_header.fundamentals <- function(x, setup, ...) {
  txt <- paste0(attr(x, "dictionary"), " (", attr(x, "group"), ")")
  pillar::style_subtle(paste(c("# Source:", "# A data frame:"), c(txt, setup$tbl_sum)))
}

