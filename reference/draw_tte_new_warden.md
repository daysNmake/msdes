# Draw time to Event custom function

`draw_tte_new_warden()` adds additional capability for the modelling of
the UK life tables

## Usage

``` r
draw_tte_new_warden(
  n_chosen,
  dist,
  coef1 = NULL,
  coef2 = NULL,
  coef3 = NULL,
  hr = 1,
  time = NULL,
  ...,
  beta_tx = 1,
  seed = NULL
)
```

## Arguments

- time:

  should be the start times of the intervals (last is assumed to go to
  infinity)

- dcoef1:

  should be a vector of log-rates over time intervals

## Details

Adaptation of
[`WARDEN::draw_tte()`](https://jsanchezalv.github.io/WARDEN/reference/draw_tte.html)
to include piece wise exponential distributions Utilizing
[`hesim::rpwexp()`](https://hesim-dev.github.io/hesim/reference/rpwexp.html)
