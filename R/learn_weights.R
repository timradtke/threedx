
#' Fit a 3DX model to a time series
#' 
#' @param y The time series to be forecasted
#' @param season_length The presumed length of a seasonal period for `y`
#' @param alphas_grid A list of possible parameter combinations to generate the
#'   weights of the final model. The optimal parameter set will be chosen based
#'   on the minimization of `loss_function`. Each entry of `alphas_grid` must
#'   be a numeric vector of length three.
#'   This list can be generated via [list_sampled_alphas()] or
#'   [list_edge_alphas()], for example.
#' @param loss_function A function with first argument `y_hat` and optionally
#'   more arguments. Usually, to compute a loss, at least `y` is required to
#'   compute errors. Must be able to handle additional parameters via `...` to
#'   allow for potential future changes in the set of arguments passed to
#'   `loss_function` by [learn_weights()]. For examples, see [loss_mae()] or
#'   [loss_mae_with_observation_weight()].
#'   The arguments `y_hat` and `y` passed by [learn_weights()] are numeric
#'   vectors of the same length.
#'   The provided `loss_function` must return a numeric scalar value.
#' 
#' @return A fitted model object of class `threedx`
#' 
#' @export
learn_weights <- function(y,
                          season_length,
                          alphas_grid,
                          loss_function) {
  
  checkmate::assert_numeric(
    x = y, any.missing = FALSE, min.len = 2
  )
  checkmate::assert_integerish(
    x = season_length, lower = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_list(
    x = alphas_grid, min.len = 1, any.missing = FALSE, types = "numeric"
  )
  checkmate::assert_function(
    x = loss_function, args = "y_hat", ordered = TRUE
  )
  
  n <- length(y)
  
  offset <- season_length
  if (n <= offset) {
    offset <- 1
  }
  
  step_ahead_predictions <- matrix(
    data = NA_real_,
    nrow = n,
    ncol = length(alphas_grid)
  )
  
  for (i_steps_back in 1:(n - offset)) {
    step_ahead_predictions[n - i_steps_back + 1, ] <- 
      predict_one_step_ahead_with_grid(
        y = y[seq_len(n - i_steps_back)],
        alphas_grid = alphas_grid,
        n = n - i_steps_back,
        season_length = season_length
      )
  }
  
  step_ahead_loss <- apply(
    X = step_ahead_predictions[-seq_len(offset), ],
    MARGIN = 2,
    FUN = loss_function,
    y = y[-seq_len(offset)]
  )
  
  best_alphas_idx <- which.min(step_ahead_loss)
  best_alphas <- alphas_grid[[best_alphas_idx]]
  
  step_ahead_residuals <- y - step_ahead_predictions
  
  result <- structure(
    list(
      weights = weights_threedx(
        alpha = best_alphas[1],
        alpha_seasonal = best_alphas[2],
        alpha_seasonal_decay = best_alphas[3],
        n = n,
        season_length = season_length
      ),
      alpha = best_alphas[1],
      alpha_seasonal = best_alphas[2],
      alpha_seasonal_decay = best_alphas[3],
      fitted = step_ahead_predictions[, best_alphas_idx, drop = TRUE],
      residuals = step_ahead_residuals[, best_alphas_idx, drop = TRUE],
      y = y,
      n = n,
      season_length = season_length,
      loss_function = loss_function,
      loss = step_ahead_loss[best_alphas_idx],
      full = list(
        best_alphas_idx = best_alphas_idx,
        best_alphas = best_alphas,
        residuals = step_ahead_residuals,
        fitted = step_ahead_predictions,
        loss = step_ahead_loss
      )
    ),
    class = "threedx"
  )
  
  return(result)
}

#' Predict one-step ahead for each of the alpha specifications
#' 
#' Generates a matrix of weights in which each column contains the weights for
#' one of the possible paramater values that are being evaluated during
#' training. Then derives one-step-ahead predictions for each set of weights
#' by matrix multiplication with the time series vector.
#' 
#' @param y The time series vector of length `n`
#' @param alphas_grid The list of possible parameter combinations
#' @param n The number of observations in the time series
#' @param season_length The length of the seasonal period
#' 
#' @return A numeric vector of length `length(alphas_grid)`
#' 
#' @keywords internal
predict_one_step_ahead_with_grid <- function(y, alphas_grid, n, season_length) {
  weight_grid <- vapply(
    X = alphas_grid,
    FUN = function(alphas) {
      weights_threedx(
        alpha = alphas[1],
        alpha_seasonal = alphas[2],
        alpha_seasonal_decay = alphas[3],
        n = n,
        season_length = season_length
      )
    },
    FUN.VALUE = numeric(length = n),
    USE.NAMES = FALSE
  )
  
  matrix(data = y, nrow = 1) %*% weight_grid
}
