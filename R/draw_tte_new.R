# Adaptation of draw_tte_new to include piecewise exponential distributions
# Howard Thom 4-Sep-2024
# coef1 should be a vector of log-rates over time intervals
# time should be the start times of the intervals (last is assumed to go to infinity)
# coef2 is a vector of hazard ratios over the time intervals

# Utilises the hesim package
# hesim::rpwexp(1, rate = exp(c(0.0004, 0.02, 1.0)), time = c(0, 30, 50))
# Example usage:
# draw_tte_new(n_chosen = 1, dist = "pwexp", coef1 = c(0.0004, 0.02, 1.0), time = c(0, 30, 50))

draw_tte_new <- function (n_chosen = 1, dist = "exp", coef1 = 1, coef2 = NULL, 
                      coef3 = NULL, hr = 1, seed = NULL, time = NULL) 
{
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Only report warning about vectors of parameters if not piecewise exponential
  if (any(length(coef1) > 1, length(coef2) > 1, length(coef3) > 
          1) & dist != "pwexp") {
    warning("Provided a coefficient parameter that is a vector")
  }
  if (dist == "lnorm") {
    draw.out <- rlnorm(n_chosen, meanlog = coef1 - log(hr), 
                       sdlog = exp(coef2))
  }
  else if (dist == "weibullPH") {
    draw.out <- flexsurv::rweibullPH(n_chosen, shape = exp(coef1), 
                                     scale = exp(coef2 + log(hr)))
  }
  else if (dist == "weibull") {
    draw.out <- rweibull(n_chosen, shape = exp(coef1), scale = exp(coef2 - 
                                                                     log(hr)))
  }
  else if (dist == "llogis") {
    draw.out <- flexsurv::rllogis(n_chosen, shape = exp(coef1), 
                                  scale = exp(coef2 - log(hr)))
  }
  else if (dist == "gompertz") {
    draw.out <- flexsurv::rgompertz(n_chosen, shape = coef1, 
                                    rate = exp(coef2 + log(hr)))
  }
  else if (dist == "gengamma") {
    draw.out <- flexsurv::rgengamma(n_chosen, mu = coef1 - 
                                      log(hr), sigma = exp(coef2), Q = coef3)
  }
  else if (dist == "gamma") {
    draw.out <- rgamma(n_chosen, shape = exp(coef1), rate = exp(coef2 + 
                                                                  log(hr)))
  }
  else if (dist == "exp") {
    draw.out <- rexp(n_chosen, rate = exp(coef1 + log(hr)))
  }
  # Piecewise exponential added as needed for background mortality
  else if (dist == "pwexp") {
    draw.out <- hesim::rpwexp(n_chosen, rate = exp(coef1 + log(hr)), time = time)
  }
  else {
    stop("Invalid distribution")
  }
  return(draw.out)
}

