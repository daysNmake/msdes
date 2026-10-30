# Set Up RRMS DES Model Inputs

## Usage

``` r
rrmsdessetup(
  drc = drc,
  drq = drq,
  time_horizon = time_horizon,
  p_eid = p_eid,
  jcvtest_cost = jcvtest_cost
)
```

## Arguments

- drc:

  Numeric. Annual discount rate for costs, expressed as a proportion
  (e.g. `0.035` for 3.5\\

  drqNumeric. Annual discount rate for QALYs, expressed as a proportion
  (e.g. `0.035` for 3.5\time_horizonInteger. Model time horizon in
  years.p_eidNumeric. Proportion of patients receiving extended interval
  dosing (EID) of Natalizumab, expressed as a proportion (e.g. `0.6` for
  60\\ for all patients.jcvtest_costNumeric. Unit cost (£) of a JC virus
  (JCV) antibody test. Set to `0` if the test is assumed to be provided
  free of charge (e.g. via manufacturer home service). Initialises all
  structural model parameters, treatment definitions, event states, and
  cost and utility inputs required to run the RRMS DES model. Must be
  called before
  [`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).
  [`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md),
  [`summarise_ipd`](https://daysnmake.github.io/msdes/reference/summarise_ipd.md),
  [`plot_mortality_curves`](https://daysnmake.github.io/msdes/reference/plot_mortality_curves.md)
