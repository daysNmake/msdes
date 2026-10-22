#' Draw time to Event custom function
#' 
#' `draw_tte_new_warden()` adds additional capability for the modelling of the UK life tables
#'
#' Adaptation of [WARDEN::draw_tte()] to include piece wise exponential distributions Utilizing [hesim::rpwexp()]
#' 
#' @param dcoef1 should be a vector of log-rates over time intervals
#' @param time should be the start times of the intervals (last is assumed to go to infinity)
#' 
#' @export
draw_tte_new_warden <- function (n_chosen, dist, coef1 = NULL, coef2 = NULL, coef3 = NULL, hr = 1, time = NULL, 
          ..., beta_tx = 1, seed = NULL) 
{
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (any(length(coef1) > 1, length(coef2) > 1, length(coef3) > 
          1) & dist != "mvnorm") {
    message("Provided a coefficient parameter that is a vector")
  }
  if (!(dist %in% c("lnorm", "norm", "mvnorm", "weibullPH", 
                    "weibull", "llogis", "gompertz", "gengamma", "gamma", 
                    "exp", "beta", "poisgamma", "pwexp"))) {
    stop("Invalid distribution. Distribution must be one of: 'lnorm','norm','mvnorm','weibullPH','weibull','llogis','gompertz','gengamma','gamma','exp','beta','poisgamma','pwexp'")
  }
  draw.out <- switch(dist, lnorm = rlnorm(n_chosen, meanlog = coef1 - 
                                            log(beta_tx), sdlog = exp(coef2), ...), norm = rnorm(n_chosen, 
                                                                                                 mean = coef1 - beta_tx, sd = coef2, ...), mvnorm = mvrnorm(n_chosen, 
                                                                                                                                                            mu = coef1 - beta_tx, Sigma = coef2, ...), weibullPH = rweibullPH(n_chosen, 
                                                                                                                                                                                                                              shape = exp(coef1), scale = exp(coef2 + log(beta_tx))), 
                     weibull = rweibull(n_chosen, shape = exp(coef1), scale = exp(coef2 + 
                                                                                    log(beta_tx))), llogis = rllogis(n_chosen, shape = exp(coef1), 
                                                                                                                     scale = exp(coef2 + log(beta_tx))), gompertz = rgompertz(n_chosen, 
                                                                                                                                                                              shape = coef1, rate = exp(coef2 + log(beta_tx))), 
                     gengamma = rgengamma(n_chosen, mu = coef1 + log(beta_tx), 
                                          sigma = exp(coef2), Q = coef3), gamma = rgamma(n_chosen, 
                                                                                         shape = exp(coef1), rate = exp(coef2 + log(beta_tx))), 
                     exp = rexp(n_chosen, rate = exp(coef1 + log(beta_tx))), 
                     beta = rbeta(n_chosen, shape1 = coef1, shape2 = coef2), 
                     poisgamma = rpoisgamma(n_chosen, rate = coef1, theta = coef2,...),
                     # Piecewise exponential added as needed for background mortality
                     pwexp = hesim::rpwexp(n_chosen, rate = exp(coef1[[1]] + log(hr[[1]])), time = time[[1]]), stop("Invalid distribution. Distribution must be one of: 'lnorm','weibullPH','weibull','llogis','gompertz','gengamma','gamma','exp','beta','poisgamma','pwexp'"))
  return(draw.out)
}

