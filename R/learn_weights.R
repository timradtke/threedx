
#' Fit a 3DX model to a time series
#' 
#' @param y The time series to be forecasted as numeric vector (not as `ts()`
#'   object)
#' @param period_length The presumed length of `y`'s seasonal period
#' @param alphas_grid A data frame of possible parameter combinations to
#'   generate the weights of the final model. The optimal parameter set will be
#'   chosen based on the minimization of `loss_function`. The expected columns
#'   are numeric and called `alpha`, `alpha_seasonal`, `alpha_seasonal_decay`.
#'   At least one row must be provided. All values must be between 0 and 1.
#'   This list can be generated via [list_sampled_alphas()] or
#'   [list_edge_alphas()], for example, but you can generate it in any way you
#'   like.
#' @param loss_function A function with first argument `y_hat` and optionally
#'   more arguments. Usually, to compute a loss, at least an additional `y`
#'   argument is required---to compute errors. Must be able to handle additional
#'   parameters via `...` to allow for potential future changes in the set of
#'   arguments passed to `loss_function` by [learn_weights()].
#'   For examples, see [loss_mae()] or [loss_mae_with_observation_weight()].
#'   It can be assumed that the arguments `y_hat` and `y` passed by
#'   [learn_weights()] are numeric vectors of equal length.
#'   The provided `loss_function` must return a numeric scalar value.
#' 
#' @return A fitted model object of class `threedx`, which is a list of:
#' * A numeric vector `weights` of the same length as the input `y`, assigning
#'   a weight to each index of the past observations. The weights sum to 1.
#'   These weights are the fitted weights used to produce forecasts.
#' * A numeric scalar `alpha`, the optimal paramater for the exponential
#'   smoothing component chosen during model training
#' * A numeric scalar `alpha_seasonal`, the optimal paramater for the
#'   seasonal exponential smoothing component chosen during model training
#' * A numeric scalar `alpha_seasonal_decay`, the optimal paramater for the
#'   seasonal exponential decay smoothing component chosen during model training
#' * A numeric vector `fitted` containing fitted values for each index of `y`;
#'   the up to `period_length`-first observations may be missing.
#' * A numeric vector `residuals` containing the residuals for the training data
#'   as computed by `y - fitted`, thus the up to `period_length`-first
#'   observations may be missing.
#' * A numeric vector `y`, the input `y`
#' * A scalar `n`, the number of observations provided via `y`
#' * A scalar `period_length`, the input `period_length`
#' * A function `loss_function`, the provided `loss_function`
#' * A scalar `loss`, the value computed by the provided `loss_function` based
#'   on the input `y` and the fitted values (ignoring the initial missing
#'   values) for the loss minimizing set of parameters reported in `alpha`,
#'   `alpha_seasonal`, `alpha_seasonal_decay`
#' * A list `full` containing intermediate results observed during model
#'   optimization for all other parameter combinations provided via
#'   `alphas_grid`
#' 
#' @seealso [predict.threedx()], [list_sampled_alphas()], [loss_mae()]
#' 
#' @export
#' @examples
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
#' if (require("ggplot2")) {
#'   autoplot(model)
#' }
#'
learn_weights <- function(y,
                          period_length,
                          alphas_grid,
                          loss_function) {
  
  checkmate::assert_numeric(
    x = y, any.missing = FALSE, min.len = 2
  )
  
  if (stats::is.ts(y)) {
    stop("Please provide `y` not as time series but as a generic numeric vector.")
  }
  
  checkmate::assert_integerish(
    x = period_length, lower = 3, len = 1, any.missing = FALSE
  )
  checkmate::assert_data_frame(
    x = alphas_grid, min.rows = 1, any.missing = FALSE, types = "numeric"
  )
  checkmate::assert_names(
    x = names(alphas_grid),
    permutation.of = c("alpha", "alpha_seasonal", "alpha_seasonal_decay")
  )
  checkmate::assert_function(
    x = loss_function, args = c("y_hat", "..."), ordered = FALSE
  )
  
  n <- length(y)
  
  offset <- period_length
  # use early residuals while time series is short compared to period length
  if (n <= 2 * offset) {
    offset <- max(1, n - period_length)
  }
  
  step_ahead_predictions <- matrix(
    data = NA_real_,
    nrow = n,
    ncol = nrow(alphas_grid)
  )
  
  for (i_steps_back in 1:(n - offset)) {
    step_ahead_predictions[n - i_steps_back + 1, ] <- 
      predict_one_step_ahead_with_grid(
        y = y[seq_len(n - i_steps_back)],
        alphas_grid = alphas_grid,
        n = n - i_steps_back,
        period_length = period_length
      )
  }
  
  step_ahead_loss <- apply(
    X = step_ahead_predictions[-seq_len(offset), , drop = FALSE],
    MARGIN = 2,
    FUN = loss_function,
    y = y[-seq_len(offset)]
  )
  
  best_alphas_idx <- which.min(step_ahead_loss)
  best_alphas <- alphas_grid[best_alphas_idx, ]
  row.names(best_alphas) <- NULL
  
  step_ahead_residuals <- y - step_ahead_predictions
  
  result <- structure(
    list(
      weights = weights_threedx(
        alpha = best_alphas$alpha,
        alpha_seasonal = best_alphas$alpha_seasonal,
        alpha_seasonal_decay = best_alphas$alpha_seasonal_decay,
        n = n,
        period_length = period_length
      ),
      alpha = best_alphas$alpha,
      alpha_seasonal = best_alphas$alpha_seasonal,
      alpha_seasonal_decay = best_alphas$alpha_seasonal_decay,
      fitted = step_ahead_predictions[, best_alphas_idx, drop = TRUE],
      residuals = step_ahead_residuals[, best_alphas_idx, drop = TRUE],
      y = y,
      n = n,
      period_length = period_length,
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
#' @param alphas_grid The data frame of possible parameter combinations
#' @param n The number of observations in the time series
#' @param period_length The length of the seasonal period
#' 
#' @return A numeric vector of length `nrow(alphas_grid)`
#' 
#' @keywords internal
predict_one_step_ahead_with_grid <- function(y, alphas_grid, n, period_length) {
  weight_grid <- weights_threedx_vec(
    alphas = alphas_grid$alpha,
    alphas_seasonal = alphas_grid$alpha_seasonal,
    alphas_seasonal_decay = alphas_grid$alpha_seasonal_decay,
    n = n,
    period_length = period_length
  )
  
  matrix(data = y, nrow = 1) %*% t(weight_grid)
}
