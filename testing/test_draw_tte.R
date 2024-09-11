# Test the draw_tte() function

# Not yet done: Check if all parameters on correct scale from perspective
# of base R and flexsurv functions
# Not critical to NICE MS MTA as only using exponentially distributed events

test_draw_tte <- function (n_chosen = 1, dist = "exp", coef1 = 1, coef2 = NULL, 
                           coef3 = NULL, hr = 1, seed = NULL) 
{
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Coefficients have to be scalars but hr can be a vector
  if (any(length(coef1) > 1, length(coef2) > 1, length(coef3) > 
          1)) {
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
    # Be careful as rweibull and flexsurv::rweibullPH have very different
    # meanings for the scale parameter
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
  else {
    stop("Invalid distribution")
  }
  return(draw.out)
}

# Test of seeding functionality - always returns same value if supplying a seed
test_draw_tte(coef1 = 1, seed = 5264)
# And matches global setting of seed
set.seed(5264)
test_draw_tte(coef1 = 1)

# Exploration of weibull
test_draw_tte(dist = "weibullPH", coef1 = 1, coef2 = 1.5, hr = 1.1, seed = 3434)
# Different to above as shape and scale have different meanings in the two
# parametrisations, despite same names in descem::draw_tte()
test_draw_tte(dist = "weibull", coef1 = 1, coef2 = 1.5, hr =  1 / 1.1, seed = 3434)
