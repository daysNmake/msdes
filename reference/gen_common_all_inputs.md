# Generate Common Global Model Inputs

Generates input parameters that are common to all patients and treatment
arms, and do not change as the simulation loops through treatments for a
given patient. Returns a WARDEN `add_item()` chain consumed by
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Usage

``` r
gen_common_all_inputs()
```

## Value

A WARDEN `add_item()` object containing all common global inputs, passed
directly to `run_sim_new` via the `common_all_inputs` argument in
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Details

All inputs are read from the global `inputs` list populated by
[`gen_inputs`](https://daysnmake.github.io/msdes/reference/gen_inputs.md)
via
[`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md).
Probabilistic values are specified using
[`pick_val_v`](https://jsanchezalv.github.io/WARDEN/reference/pick_val_v.html),
which returns a list of deterministic and probabilistic values
controlled by `psa_bool` and `sensitivity_bool`. Items are chained with
[`add_item`](https://jsanchezalv.github.io/WARDEN/reference/add_item.html)
using the `%>%` pipe operator.

## See also

[`gen_inputs`](https://daysnmake.github.io/msdes/reference/gen_inputs.md),
[`add_item`](https://jsanchezalv.github.io/WARDEN/reference/add_item.html),
[`pick_val_v`](https://jsanchezalv.github.io/WARDEN/reference/pick_val_v.html),
