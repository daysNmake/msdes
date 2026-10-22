# Generate Global inputs

`gen_common_all_inputs()` Global input parameters for the model are
common to all patients and treatments.

## Usage

``` r
gen_common_all_inputs()
```

## Arguments

- drc:

  discount rate for costs

- drq:

  discount rate for qalys

- reference:

  The treatment arm specified as the reference for the comparison

## Details

Input parameters here that do not change as we loop through treatments
for a patient. The input parameters are called from 'gen_inputs()'
function or the main analysis script. They are called in with
'WARDEN::add_item()' functions, these are piped with %\>% operator.
Probabilistic input parameters are specified with 'WARDEN::pick_val_v()'
function creating a list of deterministic and probabilistic values
