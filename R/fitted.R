#' Extract fitted values from a `threedx` model
#' 
#' Returns the fitted one-step-ahead predictions for the training data of the
#' `threedx` model. At least the first value will be missing.
#' 
#' @param object A `threedx` model as returned by [learn_weights()]
#' @param ... Other arguments passed to `fitted()`, ignored
#' 
#' @return A numeric vector of same length as `object$y`
#' 
#' @export
fitted.threedx <- function(object, ...) {
  checkmate::assert_class(x = object, classes = "threedx")
  return(object$fitted)
}
