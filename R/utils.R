
# Dictionaries ------------------------------------------------------------

validate_new_dictionary <- function(dict) {

  ok <- all(c("term", "ratings") %in% colnames(dict))
  if (!ok) stop(call. = FALSE, "New dictionary must be a data frame with <term> and <ratings> columns")

  ok <- all(purrr::map_lgl(dict$ratings, \(x) all(names(x) == c("e", "p", "a"))))
  if (!ok) stop(call. = FALSE, "The ratings column must be a named list of epa ratings")

}

# Print Methods -----------------------------------------------------------

#' @export
tbl_format_header.event_deflection <- function(x, setup, ...) {
  c(cli::col_blue("# Event deflection"), cli::col_blue("# A data frame: ", setup$tbl_sum))
}

