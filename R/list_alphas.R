#' Generate a list of possible alpha values that are evaluated during training
#' 
#' @export
list_edge_alphas <- function() {
  list(
    c(0,0,0), # mean
    c(1,0,0), # random walk
    # c(1,0,1), # random walk
    # c(1,1,0), # NaN
    # c(1,1,1), # NaN
    c(0,1,1), # seasonal naive
    c(0,1,0), # seasonal mean
    c(0,0,1)  # last year's mean
  )
}

#' Generate a list of possible alpha values that are evaluated during training
#' 
#' @export
#' @examples
#' alphas_grid <- list_sampled_alphas(n_target = 250L)
#' 
#' ggplot2::ggplot(
#'   alphas_list_to_data_frame(alphas_grid),
#'   ggplot2::aes(x = alpha, y = alpha_seasonal, fill = alpha_seasonal_decay)
#' ) +
#' ggplot2::geom_point(pch = 21, color = "white") +
#' ggplot2::scale_fill_gradient2(midpoint = 0.5) +
#' ggplot2::labs(title = "Sampled Grid of Parameters to Evaluate")
#' 
list_sampled_alphas <- function(n_target = 100,
                                alpha_lower = 0,
                                alpha_upper = 1,
                                alpha_seasonal_lower = 0,
                                alpha_seasonal_upper = 1,
                                alpha_seasonal_decay_lower = 0,
                                alpha_seasonal_decay_upper = 1,
                                oversample_lower = 0.05,
                                oversample_upper = 0.05,
                                include_edge_cases = TRUE,
                                seed = NULL) {
  
  checkmate::assert_integerish(
    x = n_target, lower = 0, len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha_lower, lower = 0, len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha_upper, lower = alpha_lower, len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha_seasonal_lower, lower = 0, len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha_seasonal_upper, lower = alpha_seasonal_lower,
    len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha_seasonal_decay_lower, lower = 0, len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha_seasonal_decay_upper, lower = alpha_seasonal_decay_lower,
    len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = oversample_lower, lower = 0, len = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = oversample_upper, lower = 0, len = 1, any.missing = FALSE
  )
  checkmate::assert_logical(
    x = include_edge_cases, len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = seed, len = 1, null.ok = TRUE, any.missing = FALSE
  )
  
  if (!is.null(seed)) {
    # checking for existence to avoid issues when building vignettes
    if (exists(".Random.seed")) {
      random_seed_current <- .Random.seed
      on.exit({.Random.seed <<- random_seed_current})
    }
    set.seed(seed = seed)
  }
  
  sample_alpha <- function(lower, upper) {
    pmax(
      lower,
      pmin(
        upper,
        stats::runif(
          n = n_target,
          min = lower - oversample_lower,
          max = upper + oversample_upper
        )
      )
    )
  }
  
  grid <- matrix(
    c(
      sample_alpha(
        lower = alpha_lower,
        upper = alpha_upper
      ),
      sample_alpha(
        lower = alpha_seasonal_lower,
        upper = alpha_seasonal_upper
      ),
      sample_alpha(
        lower = alpha_seasonal_decay_lower,
        upper = alpha_seasonal_decay_upper
      )
    ),
    ncol = 3
  )
  
  grid <- grid[grid[,1] != 1 & grid[,2] != 1, ]
  
  if (include_edge_cases) {
    grid <- rbind(
      matrix(
        data = c(
          0, 0, 0,
          1, 0, 0,
          0, 1, 1,
          0, 1, 1,
          0, 1, 0,
          0, 0, 1
        ),
        ncol = 3,
        byrow = TRUE
      ),
      grid
    )
  }
  
  grid <- unique(x = grid, MARGIN = 1)
  
  # trim grid down to `n_target` rows in case it has extended due to edge cases
  grid <- grid[seq_len(pmin(n_target, nrow(grid))), ]
  
  lapply(
    X = seq_len(nrow(grid)),
    FUN = function(idx) as.numeric(grid[idx, ])
  )
}

#' Cast a list of alphas as data frame
#' 
#' @export
#' @examples
#' alphas_list_to_data_frame(list_sampled_alphas(n_target = 10L))
#' 
alphas_list_to_data_frame <- function(alphas_grid) {
  alphas_df <- as.data.frame(
    t(
      vapply(
        X = alphas_grid,
        FUN = function(x) return(x),
        FUN.VALUE = numeric(length = 3L)
      )
    )
  )
  
  names(alphas_df) <- c("alpha", "alpha_seasonal", "alpha_seasonal_decay")
  return(alphas_df)
}
