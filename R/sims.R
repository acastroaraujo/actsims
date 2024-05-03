

# rename equations matrix columns
# see if you can make stack_epa_ratings faster



#' Set up a design
#'
#' @param dictionary dictionary name
#' @param equations equation name
#'
#' @return an "InteRact" object
#' @export
#'
interact <- function(dictionary, equations) {
  InteRact$new(dictionary, equations)
}


InteRact <- R6::R6Class(
  classname = "InteRact",

  public = list(
    dictionary = NULL,
    equations = NULL,

    initialize = function(dictionary = match.arg(dictionaries), equations) {

      self$dictionary <- usfullsurveyor2015 ## modify to be compatible with multiple dicts
      self$equations <- get_equation(key = equations, group = "all")

      private$.group <- "all"
      private$.dict_name <- dictionary
      private$.eq_name <- equations

    },

    deflection = function(events) {
      epa_matrix <- stack_epa_ratings(events, self$dictionary)
      X <- get_data_matrix(epa_matrix, self$equations)
      ## get transient impressions
      t <- X %*% self$equations
      ## get deflection
      rowSums((t - epa_matrix)^2)
    },

    print = function(...) {

      cat("Dictionary: ", private$.dict_name, "\n")
      cat("    group : ", private$.group, "\n")
      cat("Equations : ", private$.eq_name, "\n")

    }
  ),

  private = list(
    .group = NULL,
    .dict_name = NULL,
    .eq_name = NULL
  )
)









