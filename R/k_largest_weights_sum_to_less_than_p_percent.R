
#' Do the `k` largest weights sum up to less than `p`% of the total weights?
#' 
#' Use this function to dynamically set `observation_driven` based
#' on the fitted model's weights.
#' 
#' This is useful to judge whether a trained `threedx` model's prediction is
#' the combination of at least a few historical observations. When using
#' `observation_driven` during prediction, prediction intervals collapse to a
#' single point when a single historical observation has (close to) 100% of the
#' weights (which happens, for example, for the random walk or seasonal naive
#' models). In those cases it can be better to switch to
#' `observation_driven = FALSE` to get a non-collapsed prediction interval
#' based on sampling residuals.
#' 
#' `k_largest_weights_sum_to_less_than_p_percent()` can be used to make this
#' switch dynamically, based on the fitted weights. See also the examples below.
#' 
#' @param weights A vector of weights that sum to 1, as part of the model
#'   object returned by [learn_weights()]
#' @param k The number of weights to consider, in decreasing order of their size
#' @param p The threshold of cumulative probability under which the `k` weights
#'   might be
#' 
#' @return A logical of length 1
#' 
#' @seealso [predict.threedx()], [learn_weights()]
#' 
#' @export
#' @examples
#' k_largest_weights_sum_to_less_than_p_percent(
#'   weights = c(0.02, 0.05, 0.05, 0.04, 0.8, 0.03, 0.01),
#'   k = 3,
#'   p = 0.9
#' )
#' 
#' k_largest_weights_sum_to_less_than_p_percent(
#'   weights = c(0.02, 0.05, 0.05, 0.04, 0.8, 0.03, 0.01),
#'   k = 4,
#'   p = 0.9
#' )
#' 
#' # Now apply it dynamically during prediction to set `observation_driven`
#' 
#' set.seed(9284)
#' y <- stats::rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi((5 + 1:55 )/ 6)))
#' 
#' model <- learn_weights(
#'   y = y,
#'   alphas_grid = list_sampled_alphas(
#'     n_target = 1000L,
#'     include_edge_cases = TRUE
#'   ),
#'   period_length = 12L,
#'   loss_function = loss_mae
#' )
#' 
#' forecast <- predict(
#'   object = model,
#'   horizon = 12L,
#'   n_samples = 2500L,
#'   innovation_function = draw_normal_with_zero_mean,
#'   observation_driven = k_largest_weights_sum_to_less_than_p_percent(
#'     weights = model$weights,
#'     k = 4,
#'     p = 0.95
#'   )
#' )
#' 
#' print(forecast$observation_driven)
#' 
k_largest_weights_sum_to_less_than_p_percent <- function(weights, k, p) {
  checkmate::assert_numeric(
    x = weights, lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = k, lower = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = p, lower = 0, upper = 1, len = 1, any.missing = FALSE
  )
  
  if (p == 1) {
    return(TRUE)
  }
  
  if (length(weights) == 0L || p == 0) {
    return(FALSE)
  }
  
  # assumes the vector `weights` is a vector of probabilities summing up to 1
  sum(head(sort(weights, decreasing = TRUE), k)) <= p
}
