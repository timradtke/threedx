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
#' @param x A numeric vector
#' 
#' @return Numeric vector of length equal to `x`
#' 
#' @seealso [predict.threedx()], [to_moment_matched_nbinom()]
#' 
#' @export
#' @examples
#' to_non_negative_with_identical_mean(x = c(-1, 0, 1, 2))
#' 
#' x <- rnorm(100, 0.5)
#' summary(x)
#' summary(to_non_negative_with_identical_mean(x))
#' 
#' x <- rnorm(100, -1)
#' summary(x)
#' # can't keep identical mean as original mean is negative
#' summary(to_non_negative_with_identical_mean(x))
#' 
to_non_negative_with_identical_mean <- function(x) {
  checkmate::assert_numeric(x = x)
  if (length(x) == 0) {
    return(x)
  }
  if (anyNA(x)) {
    stop("Can't postprocess sample values that are NA.")
  }
  
  x_non_negative <- pmax(0, x)
  
  if (mean(x_non_negative) == 0 || mean(x) < 0) {
    return(x_non_negative)
  }
  
  x_non_negative / mean(x_non_negative) * mean(x)
}
