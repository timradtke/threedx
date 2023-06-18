
#' Draw forecast sample paths from a fitted 3DX model
#' 
#' @export
#' 
predict.threedx <- function(object,
                            horizon,
                            n_samples,
                            observation_driven,
                            ...) {
  
  checkmate::assert_class(x = object, classes = "threedx")
  checkmate::assert_integerish(
    x = horizon, lower = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n_samples, lower = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_logical(
    x = observation_driven, len = 1, any.missing = FALSE
  )
  
  y_m <- matrix(
    data = object$y,
    nrow = n_samples,
    ncol = object$n,
    byrow = TRUE
  )
  
  if (observation_driven) {
    paths <- predict_with_observations(
      y_m = y_m,
      object = object,
      horizon = horizon,
      n_samples = n_samples
    )
  } else {
    paths <- predict_with_state(
      y_m = y_m,
      object = object,
      horizon = horizon,
      n_samples = n_samples
    )
  }
  
  result <- structure(
    list(
      paths = paths,
      model = object
    ),
    class = "threedx_paths"
  )
  
  return(result)
}

#' Generate sample paths using latent state
#' 
#' @keywords internal
predict_with_state <- function(y_m,
                               object,
                               horizon,
                               n_samples) {
  y_hat_m <- matrix(
    data = stats::rnorm(
      n = horizon * n_samples,
      mean = mean(object$residuals, na.rm = TRUE),
      sd = stats::sd(object$residuals, na.rm = TRUE)
    ),
    nrow = n_samples,
    ncol = horizon
  )
  
  for (idx in seq_len(horizon)) {
    y_hat_m[, idx] <- y_hat_m[, idx] +
      cbind(y_m, y_hat_m[, seq_len(idx-1), drop = FALSE]) %*%
      matrix(
        data = weights_threedx(
          alpha = object$alpha,
          alpha_seasonal = object$alpha_seasonal,
          alpha_seasonal_decay = object$alpha_seasonal_decay,
          n = object$n + idx - 1,
          season_length = object$season_length
        ),
        ncol = 1
      )
  }
  
  return(y_hat_m)
}

#' Generate sample paths using observations
#' 
#' @keywords internal
predict_with_observations <- function(y_m,
                                      object,
                                      horizon,
                                      n_samples) {
  y_hat_m <- matrix(
    data = NA_real_,
    nrow = n_samples,
    ncol = horizon
  )
  
  for (idx in seq_len(horizon)) {
    sample_indices <- sample(
      x = seq_len(object$n + idx - 1),
      size = n_samples,
      replace = TRUE,
      prob = weights_threedx(
        alpha = object$alpha,
        alpha_seasonal = object$alpha_seasonal,
        alpha_seasonal_decay = object$alpha_seasonal_decay,
        n = object$n + idx - 1,
        season_length = object$season_length
      )
    )
    
    tmp_y_m <- cbind(y_m, y_hat_m[, seq_len(idx-1), drop = FALSE])
    
    for (sample_idx in seq_len(n_samples)) {
      y_hat_m[sample_idx, idx] <- tmp_y_m[
        sample_idx, sample_indices[sample_idx]
      ]
    }
  }
  
  return(y_hat_m)
}

