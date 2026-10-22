# Summarise Cost-effectiveness results

`summarise_ce_results()` Function to tabulate the PSA results at
willingness to pay (WTP) thresholds

## Usage

``` r
summarise_ce_results(
  wtp1 = wtp1,
  wtp2 = wtp2,
  total_qalys,
  total_costs,
  reference = "Natalizumab-IV"
)
```

## Arguments

- wtp1:

  set the willingness to pay threshold, NICE default is £25,000

- wtp2:

  set the willingness to pay threshold, NICE default is £35,000

- total_qalys:

  input from the model results: the average total quality adjusted life
  years per treatment arm

- total_costs:

  input from the model results: the average total costs per treatment
  arm

- reference:

  The treatment arm specified as the reference for the comparison

## Details

Returns the incremental cost and qalys calculates the expected
incremental cost effectiveness ratio calculates the expected incremental
net benefit ratio returns the probability of treatment arms being cost
effective
