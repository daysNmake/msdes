#'
#' @description
#' Function to summarise PSA samples
#' 
#' @details
#' 'summarise_samples()'returns the mean and 95% Credible Intervals
#' 
#' @param x data to be supplied
#' @export
summarise_samples <- function(x, n_digits = 2) {
  return(
    paste0(format(mean(x), digits = n_digits, nsmall = n_digits, big.mark=","), 
           " (", 
           format(quantile(x, probs = 0.025), digits = n_digits, nsmall = n_digits, big.mark=","), 
           ", ",
           format(quantile(x, probs = 0.975), digits = n_digits, nsmall = n_digits, big.mark=","), 
           ")"
    )
  )
}
