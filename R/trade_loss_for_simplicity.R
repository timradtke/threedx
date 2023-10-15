#' Allow for an increase in loss to find a simpler model
#' 
#' @param alphas_grid The data frame of model parameters being evaluated
#' @param losses A vector of length equal to rows of `alphas_grid`, representing
#'   the loss associated with each set of model parameters
#' @param increase The allowed increase in loss in percentage points compared
#'   to the best observed loss
#' 
#' @return A scalar integer
#' 
#' @seealso [learn_weights()]
#' 
#' @export
#' @examples
#' # Returns `1` when all options are equal
#' 
#' alphas_grid <- data.frame(
#'   alpha = rep(0, 10),
#'   alpha_seasonal = rep(0, 10),
#'   alpha_seasonal_decay = rep(0, 10)
#' )
#' 
#' losses <- rep(1, 10)
#' 
#' trade_loss_for_simplicity(
#'   alphas_grid = alphas_grid,
#'   losses = losses,
#'   increase = 1
#' )
#' 
#' # Considers only options with 0 loss if best loss is 0
#' # (here, the originally best index with 0 loss has a less simple model)
#' 
#' alphas_grid <- data.frame(
#'   alpha = c(0.5, rep(0, 2)),
#'   alpha_seasonal = rep(0, 3),
#'   alpha_seasonal_decay = rep(0, 3)
#' )
#' 
#' losses <- c(0, 1, 0)
#' 
#' trade_loss_for_simplicity(
#'   alphas_grid = alphas_grid,
#'   losses = losses,
#'   increase = 1
#' )
#' 
#' # When multiple options exist in the allowed range of performance reduction,
#' # the best loss of those with the highest simplicity dominates
#' 
#' alphas_grid <- data.frame(
#'   alpha = c(0.5, 0.5, 0.5, 0.5, 0),
#'   alpha_seasonal = c(0.5, 0.5, 0, 0, 0),
#'   alpha_seasonal_decay = c(0.5, 0, 0, 0, 0)
#' )
#' 
#' losses <- c(100, 101, 103, 102, 110)
#' 
#' trade_loss_for_simplicity(
#'   alphas_grid = alphas_grid,
#'   losses = losses,
#'   increase = 5
#' )
#' 
trade_loss_for_simplicity <- function(alphas_grid,
                                      losses,
                                      increase) {
  
  checkmate::assert_numeric(
    x = increase, lower = 0, len = 1, any.missing = FALSE
  )
  
  best_loss <- min(losses)
  
  # only if the `best_loss` isn't zero we can apply percentage-based comparison
  challenger_index <- which(losses == best_loss)
  if (best_loss != 0) {
    challenger_index <- which(
      abs((losses - best_loss) / best_loss) <= (increase / 100)
    )
  }
  
  # at this point, the `challenger_index` includes by definition the globally
  # best index, thus it's always a vector of at least length 1
  
  # model simplicity is measured by the amount of zero-or-one-valued parameters
  challenger_counts <- apply(
    alphas_grid[challenger_index, , drop = FALSE],
    1,
    function(x) sum(as.numeric(x) %in% c(0, 1))
  )
  
  # keep only those challengers that have the lowest complexity; if no
  # challenger has a lower complexity than the originally best model, the
  # originally best model is still part of this index and will be returned
  challenger_index <- challenger_index[challenger_counts ==
                                         max(challenger_counts)]
  
  # of the challengers with the lowest complexity, return the one with min loss
  best_challenger_index <- which.min(losses[challenger_index])
  revised_best_index <- challenger_index[best_challenger_index]
  
  return(revised_best_index[1]) # final tie breaker included
}
