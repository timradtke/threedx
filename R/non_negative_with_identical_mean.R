#' Turn values non-negative while preserving their sample mean
#' 
#' Postprocess function turning forecast samples non-negative while keeping the
#' mean at the same value as it was before through scaling of the samples 
#' (unless the sample mean was originally negative).
#' 
#' While the identical sample mean can be enforced, this will generally decrease
#' the sample variance as this is the only way to maintain the mean while
#' turning negative samples to zero to enforce the non-negativity constraint.
#' 
#' The returned samples will always be non-negative. The sample mean will not be
#' identical if it cannot be achieved given the non-negative samples.
#' 
#' @param x A numeric matrix of dimensions (k, 1)
#' 
#' @return A numeric matrix of dimensions (k, 1)
#' 
#' @seealso [predict.threedx()]
#' 
#' @export
#' @examples
#' non_negative_with_identical_mean(x = matrix(c(-1, 0, 1, 2), ncol = 1))
#' 
#' x <- matrix(rnorm(100, 0.5), ncol = 1)
#' summary(x)
#' summary(non_negative_with_identical_mean(x))
#' 
#' x <- matrix(rnorm(100, -1), ncol = 1)
#' summary(x)
#' # can't keep identical mean as original mean is negative
#' summary(non_negative_with_identical_mean(x))
#' 
non_negative_with_identical_mean <- function(x) {
  checkmate::assert_matrix(x = x, min.cols = 1, max.cols = 1)
  if (nrow(x) == 0) {
    return(x)
  }
  if (anyNA(x)) {
    x[, 1] <- NA_real_
    return(x)
  }
  
  x_non_negative <- x
  x_non_negative[, 1] <- pmax(0, x)
  
  if (mean(x_non_negative) == 0 || mean(x) < 0) {
    return(x_non_negative)
  }
  
  x_non_negative / mean(x_non_negative) * mean(x)
}
