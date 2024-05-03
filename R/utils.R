
coef_dict <- c(
  "Z000000000" = "(Intercept)",
  "Z100000000" = "Ae",
  "Z010000000" = "Ap",
  "Z001000000" = "Aa",
  "Z000100000" = "Be",
  "Z000010000" = "Bp",
  "Z000001000" = "Ba",
  "Z000000100" = "Oe",
  "Z000000010" = "Op",
  "Z000000001" = "Oa",
  "Z100100000" = "Ae:Be",
  "Z100010000" = "Ae:Bp",
  "Z100001000" = "Ae:Ba",
  "Z100000100" = "Ae:Oe",
  "Z100000010" = "Ae:Op",
  "Z100000001" = "Ae:Oa",
  "Z010100000" = "Ap:Be",
  "Z010010000" = "Ap:Bp",
  "Z010001000" = "Ap:Ba",
  "Z010000100" = "Ap:Oe",
  "Z010000010" = "Ap:Op",
  "Z010000001" = "Ap:Oa",
  "Z001100000" = "Aa:Be",
  "Z001010000" = "Aa:Bp",
  "Z001001000" = "Aa:Ba",
  "Z001000100" = "Aa:Oe",
  "Z001000010" = "Aa:Op",
  "Z001000001" = "Aa:Oa",
  "Z000100100" = "Be:Oe",
  "Z000100010" = "Be:Op",
  "Z000100001" = "Be:Oa",
  "Z000010100" = "Bp:Oe",
  "Z000010010" = "Bp:Op",
  "Z000010001" = "Bp:Oa",
  "Z000001100" = "Ba:Oe",
  "Z000001010" = "Ba:Op",
  "Z000001001" = "Ba:Oa",
  "Z100100100" = "Ae:Be:Oe",
  "Z100100010" = "Ae:Be:Op",
  "Z100100001" = "Ae:Be:Oa",
  "Z100010100" = "Ae:Bp:Oe",
  "Z100010010" = "Ae:Bp:Op",
  "Z100010001" = "Ae:Bp:Oa",
  "Z100001100" = "Ae:Ba:Oe",
  "Z100001010" = "Ae:Ba:Op",
  "Z100001001" = "Ae:Ba:Oa",
  "Z010100100" = "Ap:Be:Oe",
  "Z010100010" = "Ap:Be:Op",
  "Z010100001" = "Ap:Be:Oa",
  "Z010010100" = "Ap:Bp:Oe",
  "Z010010010" = "Ap:Bp:Op",
  "Z010010001" = "Ap:Bp:Oa",
  "Z010001100" = "Ap:Ba:Oe",
  "Z010001010" = "Ap:Ba:Op",
  "Z010001001" = "Ap:Ba:Oa",
  "Z001100100" = "Aa:Be:Oe",
  "Z001100010" = "Aa:Be:Op",
  "Z001100001" = "Aa:Be:Oa",
  "Z001010100" = "Aa:Bp:Oe",
  "Z001010010" = "Aa:Bp:Op",
  "Z001010001" = "Aa:Bp:Oa",
  "Z001001100" = "Aa:Ba:Oe",
  "Z001001010" = "Aa:Ba:Op",
  "Z001001001" = "Aa:Ba:Oa"
)

get_equation <- function(key = "us2010", group = "all") {

  eq_df <- equations |> ## internal data
    dplyr::filter(.data$key == !!key, .data$equation_type == "impressionabo", .data$group == !!group) |>
    dplyr::pull(.data$df) |>
    unlist(recursive = FALSE) |>
    as.data.frame()

  out <- as.matrix(eq_df[, -1])
  rownames(out) <- coef_dict[eq_df$V1]
  return(out)

}

stack_epa_ratings <- function(events, dict) {

  stopifnot(all(purrr::map_lgl(events, \(x) class(x) == "character")))

  mat <- do.call(rbind, dict[["ratings"]])
  rownames(mat) <- dict[["term"]]

  out <- cbind(
    mat[events[["A"]], , drop = FALSE],
    mat[events[["B"]], , drop = FALSE],
    mat[events[["O"]], , drop = FALSE]
  )

  rownames(out) <- NULL
  colnames(out) <- paste0(rep(c("A", "B", "O"), each = 3), rep(c("e", "p", "a"), times = 3))

  return(out)
}

get_data_matrix <- function(data, eq) {
  terms <- rownames(eq)[-1]
  form <- stats::reformulate(paste(terms, sep = "+"))
  X <- stats::model.matrix(form, data = as.data.frame(data))
  return(X)
}



