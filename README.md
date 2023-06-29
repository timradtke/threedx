
<!-- README.md is generated from README.Rmd. Please edit that file -->

# threedx (3DX)

<!-- badges: start -->

[![R-CMD-check](https://github.com/timradtke/threedx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/timradtke/threedx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/timradtke/threedx/branch/main/graph/badge.svg)](https://app.codecov.io/gh/timradtke/threedx?branch=main)
<!-- badges: end -->

Use 3DX to generate interpretable probabilistic forecasts purely by
weighting values from the observed time series.

What’s unique about 3DX is that it can be used to derive forecasts not
only based on a “latent state” but also observation-driven, drawing
future realizations purely from observed values. This can be effective
for count or intermittent series.

## Installation

You can install the development version of threedx from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("timradtke/threedx")
```

Written in pure R, and with the `checkmate` package as the only direct
dependency, installing `threedx` is a breeze.

## Get Started

``` r
y <- rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6)))
```

``` r
library(threedx)

alphas_grid <- list_sampled_alphas(
  n_target = 1000L,
  include_edge_cases = TRUE
)

model <- learn_weights(
  y = y,
  alphas_grid = alphas_grid,
  period_length = 12L,
  loss_function = loss_mae
)

forecast <- predict(
  object = model,
  horizon = 12L,
  n_samples = 2500L,
  observation_driven = TRUE
)
```

When `ggplot2` is available, we can use `autoplot()` to visualize the
forecast object:

``` r
library(ggplot2)
autoplot(forecast)
```

<img src="man/figures/README-plot_forecast-1.svg" width="100%" />

## References

Alexander Alexandrov et al. (2019). *GluonTS: Probabilistic Time Series
Models in Python*. <https://arxiv.org/abs/1906.05264>

Jan Gasthaus (2016). *Non-parametric time series forecaster*. Technical
report, Amazon, 2016.
