# Function to summarise PSA samples
# Howard Thom 10-Oct-2024

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
