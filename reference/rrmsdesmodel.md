# Run the RRMS Discrete Event Simulation Model

Executes the RRMS DES model for a specified number of patients and
probabilistic sensitivity analysis (PSA) samples using the WARDEN
simulation framework. Results are written to the global environment for
downstream use in the Shiny application or interactive analysis.

## Usage

``` r
rrmsdesmodel(n_patients = n_patients, n_samples = n_samples)
```

## Arguments

- n_patients:

  Integer. Number of patients to simulate per PSA sample. Defaults to
  the value of `n_patients` in the calling environment.

- n_samples:

  Integer. Number of PSA samples (simulations) to run. Defaults to the
  value of `n_samples` in the calling environment.

## See also

[`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md),
[`summarise_ipd`](https://daysnmake.github.io/msdes/reference/summarise_ipd.md),
[`plot_mortality_curves`](https://daysnmake.github.io/msdes/reference/plot_mortality_curves.md),
`run_sim_new`

## Examples

``` r
if (FALSE) { # \dontrun{
rrmsdessetup(
  drc          = 0.035,
  drq          = 0.035,
  time_horizon = 70,
  p_eid        = 0,
  jcvtest_cost = 0
)

rrmsdesmodel(n_patients = 10, n_samples = 100)

# Results are now available in the global environment
ipd <- msdes::summarise_ipd(
  res_list   = results[[1]],
  n_samples  = 100,
  n_patients = 10
)
} # }

```
