#' Postprocess to samples from a moment-matched negative-binomial distribution
#' 
#' Use this function to postprocess samples from the forecast distribution to
#' samples from a negative binomial distribution that was fitted by
#' moment-matching (using mean and variance) to the input samples `x`. This is
#' useful to convert samples generated via a different `innovation_function`
#' like `[draw_normal_with_zero_mean()]` or `[draw_bootstrap()]` to a
#' count distribution.
#' 
#' If the required forecast output are count data (as when forecasting the
#' demand for products), this can be better than using `round()` or similar to
#' preserve features of the forecast distribution.
#' 
#' @param x Numeric vector of non-negative samples of at least length 2
#' 
#' @return Numeric vector of length equal to `x`
#' 
#' @seealso [predict.threedx()], [to_non_negative_with_identical_mean()]
#' 
#' @export
#' @examples
#' x <- to_non_negative_with_identical_mean(
#'   stats::rnorm(n = 10000, mean = 5, sd = 3)
#' )
#' y <- to_moment_matched_nbinom(x = x)
#' summary(x); stats::var(x)
#' summary(y); stats::var(y)
#' 
#' # Forecasting simple count data
#' 
#' set.seed(992)
#' y <- stats::rnbinom(n = 50, mu = 3, size = 3/2)
#' model <- learn_weights(
#'   y = y,
#'   alphas_grid = list_sampled_alphas(
#'     n_target = 1000L,
#'     include_edge_cases = TRUE
#'   ),
#'   period_length = 12L,
#'   loss_function = loss_rmse
#' )
#' 
#' forecast <- predict(
#'   object = model,
#'   horizon = 12L,
#'   n_samples = 2501L,
#'   observation_driven = FALSE,
#'   innovation_function = draw_normal_with_zero_mean,
#'   postprocess = to_moment_matched_nbinom
#' )
#' 
#' forecast$paths[1:5, ]
#' 
#' if (require("ggplot2")) {
#'   autoplot(forecast)
#'   autoplot(forecast, method = "paths")
#' }
#' 
to_moment_matched_nbinom <- function(x) {
  if (length(x) < 2) {
    stop("To moment-match the variance, `x` must have at least 2 observations.")
  }
  if (anyNA(x)) {
    stop("Can't postprocess sample values that are NA.")
  }
  
  mu <- mean(x)
  if (mu < 0) {
    stop("Can't moment-match a negative mean.")
  }
  
  sigma_squared <- stats::var(x)
  
  if (sigma_squared == 0) {
    return(round(x))
  }
  
  size_denominator <- sigma_squared - mu
  
  if (size_denominator > 0.01) {
    size <- mu^2 / size_denominator
    y <- stats::rnbinom(n = length(x), size = size, mu = mu)
  } else {
    # Necessary as `size_denominator` must be strictly positive (since `size`
    # must be strictly positive).
    # For `sigma_squared == mu`, this will be correct;
    # for `sigma_squared < mu` this will lead to a larger variance.
    y <- stats::rpois(n = length(x), lambda = mu)
  }
  
  # Preserve the ranking of samples as in the original vector
  y <- sort(y)[rank(x, ties.method = "first")]
  
  return(y)
}
