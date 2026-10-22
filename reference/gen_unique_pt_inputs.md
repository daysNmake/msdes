# Generate Individual inputs

`gen_unique_pt_inputs()` Individual patient parameters of the model as
they are unique patients and history including treatments

## Usage

``` r
gen_unique_pt_inputs()
```

## Arguments

- sex:

  individual patient sex

- n_relapse:

  number of relapses counter

- n_edssinc_evts:

  number of edss increases counter

## Details

These are called for each new patient so sampled from distribution
specified for heterogeneous characteristics and may depend on treatment
Note that the current treatment is later initialised to trt
