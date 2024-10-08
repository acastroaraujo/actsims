% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InteRactModel.R
\name{method-add-equation}
\alias{method-add-equation}
\alias{add_equation}
\title{Set up additional transformation equations}
\usage{
add_equation(
  type = c("emotionid", "traitid"),
  group = c("all", "female", "male")
)
}
\arguments{
\item{type}{one of \code{"emotionid"} or \code{"traitid"}}

\item{group}{one of \code{"all"}, \code{"female"}, or \code{"male"}}
}
\description{
The \verb{$add_equation()} method allows for the inclusion of further equations.

Adding \code{"emotionid"} is needed to use the \verb{$characteristic_emotion()} method.

Adding \code{"traitid"} is needed to use the \verb{$modify_identity()} method.
}
\examples{
act <- interact()
act

act$add_equation(type = "traitid", group = "all")
act

act$add_equation(type = "emotionid", group = "male")
act

}
\seealso{
Other InteRactModel methods: 
\code{\link{method-characteristic-emotion}},
\code{\link{method-closest-terms}},
\code{\link{method-deflection}},
\code{\link{method-fundamentals}},
\code{\link{method-max-confirm}},
\code{\link{method-modify-identity}},
\code{\link{method-optimal-behavior}},
\code{\link{method-reidentify}}
}
\concept{InteRactModel methods}
