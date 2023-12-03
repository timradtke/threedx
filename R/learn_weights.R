#' Fit a 3DX model to a time series
#' 
#' Returns a `threedx` model applied to time series `y` after learning the
#' optimal set of parameters by minimizing a provided loss function. Use
#' [predict.threedx()] to generate a forecast based on the fitted model.
#' 
#' @param y The time series to be forecasted as numeric vector (not as `ts()`
#'   object)
#' @param period_length The presumed length of `y`'s seasonal period; for
#'   example, `12L` for monthly observations, `7L` for daily observations, ...
#' @param alphas_grid A data frame of possible parameter combinations to
#'   generate the weights of the final model. The optimal parameter set will be
#'   chosen based on the minimization of `loss_function`. The expected columns
#'   are numeric and called `alpha`, `alpha_seasonal`, `alpha_seasonal_decay`.
#'   At least one row must be provided. All values must be between 0 and 1.
#'   Use, for example, [list_sampled_alphas()] or
#'   [list_edge_alphas()] to generate this data frame, or generate it in any way
#'   you like.
#' @param loss_function A function with first argument `y_hat` and optionally
#'   more arguments. Usually, to compute a loss, at least an additional `y`
#'   argument is required to compute errors. Must be able to handle additional
#'   parameters via `...` to allow for potential future changes in the set of
#'   arguments passed to `loss_function` by [learn_weights()].
#'   For examples, see [loss_mae()] or [loss_mae_with_observation_weight()].
#'   It can be assumed that the arguments `y_hat` and `y` passed by
#'   [learn_weights()] are numeric vectors of equal length.
#'   The provided `loss_function` must return a numeric scalar value.
#' @param penalize Logical, `FALSE` by default. If `TRUE`, will try to pick
#'   a set of parameters that are simpler while not increasing the loss too 
#'   much. The allowed increase in loss in percentage points is defined via the 
#'   `loss_increase`. A model is simpler if more of its parameters are
#'   equal to exactly zero or one, as these correspond to the edge cases.
#' @param loss_increase A non-negative scalar value by which the loss may be
#'   increased compared to the best possible loss, in percentage points. This
#'   argument is ignored unless `penalize = TRUE`. The default of `1`
#'   corresponds to a range of up to a 1 percentage point increase in loss.
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
#' * A logical `penalize`, identical to the provided function argument
#' * A scalar `loss_increase`, identical to the provided function argument
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
#' alphas_grid <- list_sampled_alphas(
#'   n_target = 1000L,
#'   include_edge_cases = TRUE
#' )
#'
#' model <- learn_weights(
#'   y = y,
#'   alphas_grid = alphas_grid,
#'   period_length = 12L,
#'   loss_function = loss_mae
#' )
#'
#' if (require("ggplot2")) {
#'   autoplot(model)
#' }
#' 
#' model_penalized <- learn_weights(
#'   y = y,
#'   alphas_grid = alphas_grid,
#'   period_length = 12L,
#'   loss_function = loss_mae,
#'   penalize = TRUE,
#'   loss_increase = 10
#' )
#' 
#' model$full$best_alphas
#' model_penalized$full$best_alphas
#'
#' if (require("ggplot2")) {
#'   autoplot(model_penalized)
#' }
#'
learn_weights <- function(y,
                          period_length,
                          alphas_grid,
                          loss_function,
                          penalize = FALSE,
                          loss_increase = 1,
                          pass_over_anomalies = FALSE,
                          anomaly_threshold = 5,
                          anomaly_start = 2 * period_length) {
  
  checkmate::assert_numeric(x = y, any.missing = FALSE, min.len = 2)
  
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
  checkmate::assert_logical(x = penalize, any.missing = FALSE, len = 1)
  
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
  
  # each column has a copy of the input data
  y_cleaned <- matrix(
    data = y,
    nrow = n,
    ncol = nrow(alphas_grid)
  )
  
  y_residual <- matrix(
    data = NA_real_,
    nrow = n,
    ncol = nrow(alphas_grid)
  )
  
  sd_cumulative <- matrix(
    data = NA_real_,
    nrow = n,
    ncol = nrow(alphas_grid)
  )
  
  is_anomaly <- matrix(
    data = FALSE,
    nrow = n,
    ncol = nrow(alphas_grid)
  )
  
  for (i_steps_back in (n - offset):1) {
    i <- n - i_steps_back + 1
    
    step_ahead_predictions[i, ] <- predict_one_step_ahead_with_grid(
        y = y_cleaned[seq_len(n - i_steps_back), ],
        alphas_grid = alphas_grid,
        n = n - i_steps_back,
        period_length = period_length
      )
    
    if (pass_over_anomalies) {
      sd_cumulative[i, ] <- apply_sd(y_residual)
      y_residual[i, ] <- y_cleaned[i, ] - step_ahead_predictions[i, ]
      
      # can't start sooner than (i > (period_length + 2))
      if (i > max(period_length + 2, anomaly_start)) {
        is_anomaly[i, ] <- abs(y_residual[i, ]) > anomaly_threshold * sd_cumulative[i, ]
        is_anomaly[i, ] <- ifelse(is.na(is_anomaly[i, ]), FALSE, is_anomaly[i, ])
      }
      
      y_cleaned[i, is_anomaly[i, ]] <- step_ahead_predictions[i, is_anomaly[i, ]]
      y_residual[i, is_anomaly[i, ]] <- NA_real_
    }
  }
  
  step_ahead_loss <- apply(
    X = step_ahead_predictions[-seq_len(offset), , drop = FALSE],
    MARGIN = 2,
    FUN = loss_function,
    y = y[-seq_len(offset)]
  )
  
  best_alphas_idx <- which.min(step_ahead_loss)
  if (penalize) {
    best_alphas_idx <- trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = step_ahead_loss,
      increase = loss_increase
    )
  }
  
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
      is_anomaly = is_anomaly[, best_alphas_idx, drop = TRUE],
      anomaly_bound_upper = step_ahead_predictions[, best_alphas_idx, drop = TRUE] +
        anomaly_threshold * sd_cumulative[, best_alphas_idx, drop = TRUE],
      anomaly_bound_lower = step_ahead_predictions[, best_alphas_idx, drop = TRUE] -
        anomaly_threshold * sd_cumulative[, best_alphas_idx, drop = TRUE],
      y = y,
      n = n,
      period_length = period_length,
      loss_function = loss_function,
      loss = step_ahead_loss[best_alphas_idx],
      penalize = penalize,
      loss_increase = loss_increase,
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
  
  colSums(y * t(weight_grid))
  
  # matrix(data = y, nrow = 1) %*% t(weight_grid)
}

apply_sd <- function(x) {
  # TODO: first non NaN sd is systematically 0
  # TODO: implement numerically safe alternative
  
  # using `colSums` doesn't provide speed up as we have to count non-NAs
  (colMeans(x^2, na.rm = TRUE) - colMeans(x, na.rm = TRUE)^2)^0.5
}
