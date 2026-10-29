# Summarise Individual Patient Data from DES Results

Extracts total costs and QALYs from raw WARDEN DES simulation output,
organises them into a \[samples x treatments x patients\] array, and
returns the mean across patients for each sample and treatment arm.

## Usage

``` r
summarise_ipd(res_list, n_samples, n_patients)
```

## Arguments

- res_list:

  List of length `n_samples`. Each element is a WARDEN simulation result
  containing a `merged_df` data frame with columns `evtname`, `arm`,
  `pat_id`, `total_costs`, and `total_qalys`.

- n_samples:

  Integer. Number of PSA samples (outer loop iterations).

- n_patients:

  Integer. Number of simulated patients per sample.

## Value

A named list with two elements:

- `costs`:

  Numeric matrix \[n_samples x n_treatments\] of mean total discounted
  costs per patient, averaged across `n_patients`.

- `qalys`:

  Numeric matrix \[n_samples x n_treatments\] of mean total discounted
  QALYs per patient, averaged across `n_patients`.

## Details

Only rows where `evtname == "death"` are used, as these carry each
patient's cumulative costs and QALYs at the end of their simulated
lifetime. The function relies on two objects from the global environment
set by
[`msdes::rrmsdessetup()`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md):

- `v_treatment_names` — character vector of treatment arm names, used to
  name array dimensions and build the treatment index.

- `n_treatments` — integer count of treatment arms, used to size the
  array.

## See also

[`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md),
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rrmsdessetup(drc = 0.035, drq = 0.035, time_horizon = 70,
                    p_eid = 0, jcvtest_cost = 0)
rrmsdesmodel(n_patients = 10, n_samples = 10)

ipdcesummary <- summarise_ipd(
  res_list   = results[[1]],
  n_samples  = 10,
  n_patients = 10
)

} # }
```
