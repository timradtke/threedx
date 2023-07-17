#' Draw i.i.d. innovations from a Normal distribution with non-zero mean
#' 
#' @param n The number of innovations to draw
#' @param errors The residual errors that are used to define the distribution
#'   from which the innovations are drawn
#' @param ... Additional arguments passed from [predict.threedx()], ignored
#' 
#' @return A vector of same type as `errors` and of length `n`
#' 
#' @seealso [draw_normal_with_zero_mean()], [draw_bootstrap()],
#'   [loss_rmse_ignoring_bias()], [loss_mae_ignoring_bias()]
#' @export
#' 
#' @examples
#' model <- learn_weights(
#'   y = 1:50,
#'   period_length = 12L,
#'   alphas_grid = list_sampled_alphas(n_target = 25),
#'   loss_function = loss_mae
#' )
#' 
#' forecast <- predict(
#'   object = model,
#'   horizon = 12L,
#'   n_samples = 1000L,
#'   observation_driven = FALSE,
#'   innovation_function = draw_normal_with_drift,
#' )
draw_normal_with_drift <- function(n, errors, ...) {
  checkmate::assert_integerish(x = n, lower = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(x = errors, finite = TRUE, any.missing = FALSE)
  
  suppressWarnings(
    stats::rnorm(
      n = n,
      mean = mean(errors),
      sd = stats::sd(errors)
    )
  )
}

#' Draw i.i.d. innovations from a Normal distribution with zero mean
#' 
#' @param n The number of innovations to draw
#' @param errors The residual errors that are used to define the distribution
#'   from which the innovations are drawn
#' @param ... Additional arguments passed from [predict.threedx()], ignored
#' 
#' @return A vector of same type as `errors` and of length `n`
#' @export
#' 
#' @examples
#' model <- learn_weights(
#'   y = 1:50,
#'   period_length = 12L,
#'   alphas_grid = list_sampled_alphas(n_target = 25),
#'   loss_function = loss_mae
#' )
#' 
#' forecast <- predict(
#'   object = model,
#'   horizon = 12L,
#'   n_samples = 1000L,
#'   observation_driven = FALSE,
#'   innovation_function = draw_normal_with_zero_mean,
#' )
draw_normal_with_zero_mean <- function(n, errors, ...) {
  checkmate::assert_integerish(x = n, lower = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(x = errors, finite = TRUE, any.missing = FALSE)
  
  suppressWarnings(
    stats::rnorm(
      n = n,
      mean = 0,
      sd = stats::sd(errors)
    )
  )
}

#' Draw innovations by bootstrapping from weighted residual errors
#' 
#' @param n The number of innovations to draw
#' @param errors The residual errors that are used to define the distribution
#'   from which the innovations are drawn
#' @param weight_function A function that takes `errors` as sole argument and
#'   must return a numeric vector of same length as `errors` to be used as
#'   `prob` argument by the underlying `sample()` call
#' @param ... Additional arguments passed from [predict.threedx()], ignored
#' 
#' @return A vector of same type as `errors` and of length `n`
#' @export
#' 
#' @examples
#' model <- learn_weights(
#'   y = rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6))),
#'   period_length = 12L,
#'   alphas_grid = list_sampled_alphas(n_target = 25),
#'   loss_function = loss_mae
#' )
#' 
#' forecast <- predict(
#'   object = model,
#'   horizon = 12L,
#'   n_samples = 1000L,
#'   observation_driven = FALSE,
#'   innovation_function = draw_bootstrap_weighted,
#'   weight_function = function(x) {
#'     weights_exponential(alpha = model$alpha, n = length(x))
#'   }
#' )
draw_bootstrap_weighted <- function(n, errors, weight_function, ...) {
  checkmate::assert_integerish(x = n, lower = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(
    x = errors, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_function(
    x = weight_function,
    nargs = 1L
  )
  
  if (length(errors) == 1L) {
    return(rep(errors, times = n))
  }
  
  sample(x = errors, size = n, replace = TRUE, prob = weight_function(errors))
}

#' Draw innovations by bootstrapping from unweighted residual errors
#' 
#' @param n The number of innovations to draw
#' @param errors The residual errors that are used to define the distribution
#'   from which the innovations are drawn
#' @param ... Additional arguments passed from [predict.threedx()], ignored
#' 
#' @return A vector of same type as `errors` and of length `n`
#' @export
#' 
#' @examples
#' model <- learn_weights(
#'   y = rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6))),
#'   period_length = 12L,
#'   alphas_grid = list_sampled_alphas(n_target = 25),
#'   loss_function = loss_mae
#' )
#' 
#' forecast <- predict(
#'   object = model,
#'   horizon = 12L,
#'   n_samples = 1000L,
#'   observation_driven = FALSE,
#'   innovation_function = draw_bootstrap
#' )
#' 
draw_bootstrap <- function(n, errors, ...) {
  checkmate::assert_integerish(x = n, lower = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(
    x = errors, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  
  if (length(errors) == 1L) {
    return(rep(errors, times = n))
  }
  
  sample(x = errors, size = n, replace = TRUE, prob = NULL)
}

#' Draw innovations by bootstrapping from unweighted zero-mean residual errors
#' 
#' Subtracts the mean from the observed residuals before forming the bootstrap,
#' thereby enforcing zero-mean samples in expectation.
#' 
#' @seealso [draw_normal_with_zero_mean()], [draw_bootstrap()]
#' 
#' @param n The number of innovations to draw
#' @param errors The residual errors that are used to define the distribution
#'   from which the innovations are drawn
#' @param ... Additional arguments passed from [predict.threedx()], ignored
#' 
#' @return A vector of same type as `errors` and of length `n`
#' @export
#' 
#' @examples
#' model <- learn_weights(
#'   y = rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6))),
#'   period_length = 12L,
#'   alphas_grid = list_sampled_alphas(n_target = 25),
#'   loss_function = loss_mae
#' )
#' 
#' forecast <- predict(
#'   object = model,
#'   horizon = 12L,
#'   n_samples = 1000L,
#'   observation_driven = FALSE,
#'   innovation_function = draw_bootstrap_zero_mean
#' )
#' 
draw_bootstrap_zero_mean <- function(n, errors, ...) {
  checkmate::assert_integerish(x = n, lower = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(
    x = errors, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  
  if (length(errors) == 1L) {
    return(rep(0, times = n))
  }
  
  sample(x = errors - mean(errors), size = n, replace = TRUE, prob = NULL)
}
