

##### rename equations matrix columns
###### see if you can make stack_epa_ratings faster

#' Set up and interact object.
#'
#' @param dictionary dictionary name.
#' @param equations equation name.
#'
#' @return an "InteRact" object.
#'
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
      private$info <- list(group = "all", dict = dictionary, eq = equations)

    },

    print = function(...) {

      cat("<Dictionary>: ", private$info$dict, "\n")
      cat("    group   : ", private$info$group, "\n")
      cat("<Equations> : ", private$info$eq, "\n")

    }
  ),

  private = list(
    info = NULL
  )
)

InteRact$set(
  "public", "deflection",
  function(events) {
    epa_matrix <- stack_epa_ratings(events, self$dictionary)
    X <- get_data_matrix(epa_matrix, self$equations)
    t <- X %*% self$equations                 ## get transient impressions
    deflection <- rowSums((t - epa_matrix)^2) ## get deflection
    return(as.vector(deflection))
})

get_actor <- function(events, dict, eq) {

  f <- stack_epa_ratings(events, dict)
  X <- get_data_matrix(f, eq)
  t <- X %*% eq
  f[grepl("B", colnames(f))] <- 1
  t[grepl("B", colnames(t))] <- 1
  cbind(f, get_data_matrix(t, eq))

}

InteRact$set(
  "public", "optimal_behavior",
  function(events, who = c("actor", "object")) {
    #who <- match.arg(who) ## FINISH THE ACTOR VS OBJECT
    data <- get_actor(events, self$dictionary, self$equations)
    Ib <- apply(data, 1, diag, simplify = FALSE)

    identity <- diag(ncol(self$equations))
    h <- rbind(identity, -1*self$equations) %*% cbind(identity, -1*t(self$equations))

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

get_object <- function(events, dict, eq) {

  f <- stack_epa_ratings(events, dict)
  X <- get_data_matrix(f, eq)
  t <- X %*% eq
  f[grepl("O", colnames(f))] <- 1
  t[grepl("O", colnames(t))] <- 1
  cbind(f, get_data_matrix(t, eq))

}

InteRact$set(
  "public", "reidentify_object",
  function(events) {

    data <- get_object(events, self$dictionary, self$equations)
    Ib <- apply(data, 1, diag, simplify = FALSE)

    identity <- diag(ncol(self$equations))
    h <- rbind(identity, -1*self$equations) %*% cbind(identity, -1*t(self$equations))

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

    do.call(rbind, out)

  })

## Matrix has bdiag function



# Closest Term ------------------------------------------------------------

InteRact$set(
  "public", "closest_term",
  function(ratings, component = c("identity", "behavior", "modifier"), max_dist = 1) {

    x <- match.arg(component)

    lookup <- self$dictionary[self$dictionary$component == x, ]
    dict_epa <- do.call(rbind, lookup$ratings)
    rownames(dict_epa) <- lookup$term

    out <- apply(ratings, 1, function(x) {

      ssd <- rowSums(sweep(dict_epa, MARGIN = 2, unlist(x))^2)
      i <- which(ssd <= max_dist)
      sort(ssd[i])

    }, simplify = FALSE)

    return(out)

})



