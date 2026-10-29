# Plot Predicted Mortality Over Time by Treatment Arm

Extracts patient death times from WARDEN DES simulation output, fits a
Kaplan-Meier survival curve per treatment arm pooled across all PSA
samples, and returns a `ggplot` step plot of predicted overall survival.

## Usage

``` r
plot_mortality_curves(
  runresults,
  n_samples,
  n_patients,
  n_treatments,
  trt_names
)
```

## Arguments

- runresults:

  List of length `n_samples`. Each element is a WARDEN simulation result
  containing a `merged_df` data frame with columns `pat_id`, `arm`,
  `evtname`, and `evttime`.

- n_samples:

  Integer. Number of PSA samples run.

- n_patients:

  Integer. Number of simulated patients per sample.

- n_treatments:

  Integer. Number of treatment arms.

- trt_names:

  Character vector of length `n_treatments` giving treatment arm names.
  Must match values in `merged_df$arm`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. The plot shows survival probability (y-axis, 0 to 1) against
time in years (x-axis, 0 to 65), with one step line per treatment arm
distinguished by colour and linetype. The legend is positioned at the
bottom.

## Details

Death times are extracted by filtering `merged_df` to rows where
`evtname == "death"` for each combination of sample, treatment arm, and
patient. Times are stored in a three-dimensional array
`[n_samples x n_treatments x n_patients]` and sorted within each
`[sample, treatment]` slice before KM fitting.

KM curves are fitted using
[`survfit`](https://rdrr.io/pkg/survival/man/survfit.html) with all
patients pooled across PSA samples for each treatment arm, treating
every patient as experiencing the event (no censoring). Linetype cycles
through `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"` if there are more
than four treatment arms.

## See also

[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md),
[`survfit`](https://rdrr.io/pkg/survival/man/survfit.html)

## Examples

``` r
if (FALSE) { # \dontrun{
msdes::rrmsdessetup(drc = 0.035, drq = 0.035, time_horizon = 70,
                    p_eid = 0, jcvtest_cost = 0)
msdes::rrmsdesmodel(n_patients = 10, n_samples = 100)

msdes::plot_mortality_curves(
  runresults   = results[[1]],
  n_samples    = 100,
  n_patients   = 10,
  n_treatments = n_treatments,
  trt_names    = v_treatment_names
)
} # }
```
