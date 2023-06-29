utils::globalVariables(
  c("y_hat", "y", "value", "weight", "y_hat_1l", "y_hat_2l", "y_hat_3l",
    "y_hat_1u", "y_hat_2u", "y_hat_3u", "y_hat_median", "sample_index")
)

#' Autoplot method for `threedx` objects
#'
#' Use `ggplot2` to visualize the fitted values and weights of a fitted model
#' of class `threedx`
#'
#' @param object Fitted model object of class `threedx` returned by
#'   [learn_weights()]
#' @param ... ignored
#' @param date Optional additional vector with dates in format that can be cast
#'   to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param show_params Logical; if `TRUE` (default) then fitted parameters will
#'   be displayed using [ggplot2::facet_wrap()]
#'
autoplot.threedx <- function(object,
                           ...,
                           date = NULL,
                           show_params = TRUE) {
    plot_fitted(
      object = object,
      date = date,
      show_params = show_params
    )
}

#' Autoplot method for `threedx_paths` objects
#'
#' Use `ggplot2` to visualize the marginal forecast quantiles, or a few sample
#' paths of a `threedx` forecast object of class `threedx_paths`
#'
#' Note: This function will use [base::sample()] to randomly select paths that
#' are added to the plot. Set a seed if you require reproducibility.
#'
#' @param object An object of class `threedx_paths` as returned by
#'     `[predict.threedx()]`
#' @param ... ignored
#' @param method One of `forecast` for visualization of quantiles of the
#'     marginal forecast distribution (i.e., the usual fanchart), or `paths` to
#'     visualize a few sample paths from the joint forecast distribution
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param date_future Optional additional vector with dates in format that can
#'     be cast to `YYYY-MM-DD` with same length as `object$y`, used to create
#'     x-axis for forecast path
#' @param show_params Logical; if `TRUE` (default) then fitted params will be
#'     displayed using [ggplot2::facet_wrap()]
#' @param n Number of paths to add to plot, a small number is recommended to be
#'    able to see the individual paths; positive scalar integer; used
#'    when `method` is `"paths"`
#' @param alpha The transparency parameter used when adding the paths to the
#'    plot, provided to [ggplot2::geom_point()] and [ggplot2::geom_line()]; used
#'    when `method` is `"paths"`
#'
autoplot.threedx_paths <- function(object,
                                   ...,
                                   method = c("forecast", "paths")[1],
                                   date = NULL,
                                   date_future = NULL,
                                   show_params = TRUE,
                                   n = 5,
                                   alpha = 0.75) {
  checkmate::assert_choice(
    x = method,
    choices = c("forecast", "paths"),
    null.ok = FALSE
  )
  
  if (method == "forecast") {
    plot_forecast(
      object,
      date = date,
      date_future = date_future,
      show_params = show_params
    )
  } else {
    plot_paths(
      object = object,
      date = date,
      date_future = date_future,
      n = n,
      alpha = alpha
    )
  }
}

#' Plot fitted values of an `threedx` model
#'
#' This function requires the [ggplot2][ggplot2::ggplot2-package]. Whether its
#' namespace is available will be checked when the function is run. `ggplot2` is
#' only suggested, not a default import.
#'
#' @param object Fitted model object returned by [learn_weights()]
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param show_params Logical; if `TRUE` (default) then fitted parameters will
#'   be displayed using [ggplot2::facet_wrap()]
#'
#' @keywords internal
#'
#' @examples
#' set.seed(4278)
#' y <- rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6)))
#' 
#' model <- learn_weights(
#'   y = y,
#'   alphas_grid = list_sampled_alphas(n_target = 50L),
#'   period_length = 12L,
#'   loss_function = loss_mae
#' )
#' threedx:::plot_fitted(object = model)
#'
plot_fitted <- function(object,
                        date = NULL,
                        show_params = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  checkmate::assert_date(
    x = date,
    len = length(object$y),
    null.ok = TRUE,
    any.missing = FALSE
  )
  if (is.null(date)) {
    date <- seq_along(object$y)
  }
  
  df <- data.frame(
    date = date,
    y = object$y,
    y_hat = object$fitted,
    params = paste0("alpha: ", round(object$alpha, 4),
                    "; seasonal: ", round(object$alpha_seasonal, 4),
                    "; decay: ", round(object$alpha_seasonal_decay, 4))
  )
  
  ggp <- ggplot2::ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = y),
      color = "grey"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = y),
      color = "white", fill = "black", size = 3, pch = 21
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = y_hat),
      color = "black", fill = "white", size = 2, pch = 21
    ) +
    ggplot2::labs(
      x = "Date",
      y = "Value"
    )
  
  if (show_params) {
    ggp <- ggp + ggplot2::facet_wrap(~ params)
  }
  
  return(ggp)
}

#' Plot a few forecast sample paths of a `threedx` model
#'
#' This function requires the [ggplot2][ggplot2::ggplot2-package]. Whether its
#' namespace is available will be checked when the function is run. `ggplot2` is
#' only suggested, not a default import.
#'
#' Note: This function will use [base::sample()] to randomly select paths that
#' are added to the plot. Set a seed if you require reproducibility.
#'
#' @param object An object of class `threedx_paths` as returned by
#'     `predict.threedx()`
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD`, used to create x-axis
#' @param date_future Optional additional vector with dates in format that can
#'     be cast to `YYYY-MM-DD`, used to create
#'     x-axis for forecast paths
#' @param n Number of paths to add to plot, a small number is recommended to be
#'    able to see the individual paths; positive scalar integer
#' @param alpha The transparency parameter used when adding the paths to the
#'    plot, provided to [ggplot2::geom_point()] and [ggplot2::geom_line()]
#' @param show_params Logical; if `TRUE` (default) then fitted params will be
#'     displayed using [ggplot2::facet_wrap()]
#'
#' @keywords internal
#'
#' @examples
#' set.seed(4278)
#' y <- rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6)))
#' 
#' model <- learn_weights(
#'   y = y,
#'   alphas_grid = list_sampled_alphas(n_target = 50L),
#'   period_length = 12L,
#'   loss_function = loss_mae
#' )
#' 
#' paths <- predict(
#'   object = model,
#'   horizon = 12,
#'   n_samples = 2500L,
#'   observation_driven = TRUE
#' )
#'
#' threedx:::plot_paths(object = paths, n = 3)
#'
plot_paths <- function(object,
                       date = NULL,
                       date_future = NULL,
                       n = 5,
                       alpha = 0.75,
                       show_params = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  checkmate::assert_class(x = object, classes = "threedx_paths")
  
  paths <- t(object$paths)
  model <- object$model
  
  checkmate::assert_matrix(x = paths, mode = "numeric")
  horizon <- dim(paths)[1]
  
  checkmate::assert_integerish(
    x = n, lower = 1, len = 1, null.ok = FALSE, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha,
    lower = 0,
    upper = 1,
    len = 1,
    null.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::assert_date(
    x = date, len = length(model$y), null.ok = TRUE, any.missing = FALSE
  )
  checkmate::assert_date(
    x = date_future, len = horizon, null.ok = TRUE, any.missing = FALSE
  )
  checkmate::assert_logical(
    x = show_params, len = 1, null.ok = FALSE, any.missing = FALSE
  )
  
  params <- paste0("alpha: ", round(model$alpha, 4),
                   "; seasonal: ", round(model$alpha_seasonal, 4),
                   "; decay: ", round(model$alpha_seasonal_decay, 4))
  
  observation_driven <- paste0("Is observation-driven: ",
                               object$observation_driven)
  
  if (is.null(date) || is.null(date_future)) {
    date_label <- NA
    date <- 1:length(model$y)
    date_future <- (length(model$y) + 1):(length(model$y) + horizon)
  } else {
    date_label <- "Date"
  }
  
  sample_idx <- sort(sample(x = 1:dim(paths)[2], size = n, replace = FALSE))
  
  # Pivot the wide matrix into a long data frame (without using `tidyr`)
  
  df_future <- data.frame(
    params = params,
    observation_driven = observation_driven,
    date = rep(date_future, times = n),
    sample_index = rep(sample_idx, each = dim(paths)[1]),
    value = NA
  )
  df_future$sample_index <- factor(df_future$sample_index, ordered = TRUE)
  
  for (i in seq_along(sample_idx)) {
    df_future$value[((i-1) * horizon + 1):(i * horizon)] <-
      paths[, sample_idx[i]]
  }
  
  df_input <- data.frame(
    params = params,
    observation_driven = observation_driven,
    date = date,
    value = model$y
  )
  
  ggp <- ggplot2::ggplot(mapping = ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = value),
      data = df_input,
      color = "grey"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = value),
      data = df_input,
      color = "white", fill = "black", size = 1.5, pch = 21
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = value, group = sample_index, color = sample_index
      ),
      data = df_future, alpha = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = value, group = sample_index, fill = sample_index
      ),
      data = df_future,
      color = "white", size = 1.5, pch = 21, alpha = 0.75
    ) +
    ggplot2::labs(y = "Value") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_ordinal(name = "Path Index") +
    ggplot2::scale_color_ordinal(name = "Path Index")
  
  if (is.na(date_label)) {
    ggp <- ggp +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    ggp <- ggp + ggplot2::labs(x = date_label)
  }
  
  if (show_params) {
    ggp <- ggp + ggplot2::facet_wrap(~ params + observation_driven)
  }
  
  return(ggp)
}

#' Plot the marginal quantile forecast of a `threedx` model
#'
#' This function requires the [ggplot2][ggplot2::ggplot2-package]. Whether its
#' namespace is available will be checked when the function is run. `ggplot2` is
#' only suggested, not a default import.
#'
#' @param object An object of class `threedx_paths` as returned by
#'     `predict.threedx()`
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param date_future Optional additional vector with dates in format that can
#'     be cast to `YYYY-MM-DD` with same length as `object$y`, used to create
#'     x-axis for forecast paths
#' @param show_params Logical; if `TRUE` (default) then fitted params will be
#'     displayed using [ggplot2::facet_wrap()]
#'
#' @keywords internal
#'
#' @examples
#' set.seed(4278)
#' y <- rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi(1:55 / 6)))
#' 
#' model <- learn_weights(
#'   y = y,
#'   alphas_grid = list_sampled_alphas(n_target = 50L),
#'   period_length = 12L,
#'   loss_function = loss_mae
#' )
#' 
#' paths <- predict(
#'   object = model,
#'   horizon = 12,
#'   n_samples = 2500L,
#'   observation_driven = TRUE
#' )
#'
#' threedx:::plot_forecast(object = paths)
#'
plot_forecast <- function(object,
                          date = NULL,
                          date_future = NULL,
                          show_params = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  checkmate::assert_class(x = object, classes = "threedx_paths")
  
  paths <- t(object$paths)
  model <- object$model
  
  checkmate::assert_logical(
    x = show_params, len = 1, null.ok = FALSE, any.missing = FALSE
  )
  checkmate::assert_matrix(x = paths, mode = "numeric", all.missing = FALSE)
  h <- dim(paths)[1]
  n_paths <- dim(paths)[2]
  
  if (anyNA(paths)) {
    warning(paste0(
      "Some of the object's sample paths contain NAs. The displayed quantiles will be based on less than ", # nolint
      n_paths, " samples, using `na.rm = TRUE`."
    ))
  }
  
  checkmate::assert_date(
    x = date, len = length(model$y), null.ok = TRUE, any.missing = FALSE
  )
  checkmate::assert_date(
    x = date_future, len = h, null.ok = TRUE, any.missing = FALSE
  )
  
  if (is.null(date) || is.null(date_future)) {
    date_label <- NA
    date <- 1:length(model$y)
    date_future <- (length(model$y) + 1):(length(model$y) + h)
  } else {
    date_label <- "Date"
  }
  
  params <- paste0("alpha: ", round(model$alpha, 4),
                   "; seasonal: ", round(model$alpha_seasonal, 4),
                   "; decay: ", round(model$alpha_seasonal_decay, 4))
  
  observation_driven <- paste0("Is observation-driven: ",
                               object$observation_driven)
  
  df_input <- data.frame(
    date = date,
    value = model$y,
    weight = model$weights,
    params = params,
    observation_driven = observation_driven
  )
  
  df_future <- data.frame(
    date = date_future,
    params = params,
    observation_driven = observation_driven,
    y_hat_1l = apply(paths, 1, stats::quantile, 0.5 / 12, na.rm = TRUE),
    y_hat_2l = apply(paths, 1, stats::quantile, 2 / 12, na.rm = TRUE),
    y_hat_3l = apply(paths, 1, stats::quantile, 3 / 12, na.rm = TRUE),
    y_hat_median = apply(paths, 1, stats::quantile, 0.5, na.rm = TRUE),
    y_hat_3u = apply(paths, 1, stats::quantile, 9 / 12, na.rm = TRUE),
    y_hat_2u = apply(paths, 1, stats::quantile, 10 / 12, na.rm = TRUE),
    y_hat_1u = apply(paths, 1, stats::quantile, 11.5 / 12, na.rm = TRUE)
  )
  
  interval_text <- "Forecast intervals at 50%, 66%, and 92%."
  if (model$period_length == 12) {
    interval_text <- paste0(
      interval_text,
      "\nThis corresponds to falling outside the interval for half of the year, once per quarter, once per year.") # nolint
  }
  
  ggp <- ggplot2::ggplot(mapping = ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = value),
      data = df_input,
      color = "grey"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = value, size = weight),
      data = df_input,
      color = "white", fill = "black", pch = 21
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_hat_1l, ymax = y_hat_1u),
      fill = "blue", alpha = 2/12, data = df_future
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_hat_2l, ymax = y_hat_2u),
      fill = "blue", alpha = 2/12, data = df_future
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_hat_3l, ymax = y_hat_3u),
      fill = "blue", alpha = 2/12, data = df_future
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = y_hat_median),
      data = df_future, color = "darkblue"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = y_hat_median),
      data = df_future,
      color = "white", size = 1.5, pch = 21, fill = "darkblue"
    ) +
    ggplot2::labs(y = "Value",
                  caption = interval_text) +
    ggplot2::theme(legend.position = "none")
  
  if (is.na(date_label)) {
    ggp <- ggp +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    ggp <- ggp + ggplot2::labs(x = date_label)
  }
  
  if (show_params) {
    ggp <- ggp + ggplot2::facet_wrap(~ params + observation_driven)
  }
  
  return(ggp)
}

