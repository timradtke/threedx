% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{plot_forecast}
\alias{plot_forecast}
\title{Plot the marginal quantile forecast of a \code{threedx} model}
\usage{
plot_forecast(object, date = NULL, date_future = NULL, show_params = TRUE)
}
\arguments{
\item{object}{An object of class \code{threedx_paths} as returned by
\code{predict.threedx()}}

\item{date}{Optional additional vector with dates in format that can be cast
to \code{YYYY-MM-DD} with same length as \code{object$y}, used to create x-axis}

\item{date_future}{Optional additional vector with dates in format that can
be cast to \code{YYYY-MM-DD} with same length as \code{object$y}, used to create
x-axis for forecast paths}

\item{show_params}{Logical; if \code{TRUE} (default) then fitted params will be
displayed using \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}}
}
\description{
This function requires the \link[ggplot2:ggplot2-package]{ggplot2}. Whether its
namespace is available will be checked when the function is run. \code{ggplot2} is
only suggested, not a default import.
}
\examples{
set.seed(4278)
y <- rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6)))

model <- learn_weights(
  y = y,
  alphas_grid = list_sampled_alphas(n_target = 50L),
  period_length = 12L,
  loss_function = loss_mae
)

paths <- predict(
  object = model,
  horizon = 12,
  n_samples = 2500L,
  observation_driven = TRUE
)

threedx:::plot_forecast(object = paths)

}
\keyword{internal}
