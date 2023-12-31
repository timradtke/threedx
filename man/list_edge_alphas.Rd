% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_alphas.R
\name{list_edge_alphas}
\alias{list_edge_alphas}
\title{Generate a data frame of possible alpha values to evaluate during training}
\usage{
list_edge_alphas()
}
\value{
A data frame with columns \code{alpha}, \code{alpha_seasonal}, and
\code{alpha_seasonal_decay}, and five rows; all values are between 0 and 1
}
\description{
The parameter combinations generated by \code{\link[=list_edge_alphas]{list_edge_alphas()}} encode a set of
important edge cases between which a \code{threedx} model can interpolate.
}
\examples{
list_edge_alphas()
}
\seealso{
\code{\link[=list_sampled_alphas]{list_sampled_alphas()}}, \code{\link[=learn_weights]{learn_weights()}}
}
