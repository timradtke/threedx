#' Derive seasonal exponential weights
#' 
#' @export
weights_seasonal <- function(alpha_seasonal, n, season_length) {
  checkmate::assert_numeric(
    x = alpha_seasonal, lower = 0, upper = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n, len = 1, lower = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = season_length, len = 1, lower = 1, any.missing = FALSE
  )
  
  if (season_length == 1) {
    return(rep(1/n, times = n))
  }
  
  if (alpha_seasonal == 1 && n < season_length) {
    # handle edge case where otherwise all weights would be zero (or NaN)
    return(weights_exponential(alpha = alpha_seasonal, n = n))
  }
  
  seasons <- ceiling(n / season_length)
  
  n_left <- ceiling(season_length / 2) + (1 - ceiling(season_length %% 2))
  n_right <- ceiling(season_length / 2)
  
  weights_left <- rev(weights_exponential(alpha = alpha_seasonal, n = n_left))
  weights_right <- weights_exponential(alpha = alpha_seasonal, n = n_left)[-n_left]
  
  length_needed_right <- season_length - n_left
  
  weights <- c(
    weights_left,
    weights_right[(length(weights_right) - length_needed_right + 1):length(weights_right)][seq_len(length_needed_right)]
  )
  
  weights <- rep(weights, times = seasons)[
    (seasons * season_length - n + 1):(seasons * season_length)
  ]
  
  weights <- weights / sum(weights)
  return(weights)
}

#' Derive weights with seasonal exponential decay
#' 
#' @export
weights_seasonal_decay <- function(alpha_seasonal_decay,
                                   n,
                                   season_length) {
  checkmate::assert_numeric(
    x = alpha_seasonal_decay, lower = 0, upper = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n, len = 1, lower = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = season_length, len = 1, lower = 1, any.missing = FALSE
  )
  
  seasons <- ceiling(n / season_length)
  
  weights <- weights_exponential(alpha = alpha_seasonal_decay, n = seasons)
  weights <- rep(weights, each = season_length)[
    (seasons * season_length - n + 1):(seasons * season_length)
  ]
  
  weights <- weights / sum(weights)
  return(weights)
}
