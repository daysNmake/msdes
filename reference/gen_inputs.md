# generate inputs

`gen_inputs()` generates model inputs from saved data and returns a list
with the data required for the model to run.

## Usage

``` r
gen_inputs(
  drc = drc,
  drq = drq,
  time_horizon = time_horizon,
  p_eid = p_eid,
  jcvtest_cost = jcvtest_cost
)
```

## Arguments

- drc:

  discount rate for costs

- drq:

  discount rate for qalys

- time_horizon:

  the duration of the patients' life in the model

- p_eid:

  proportion pf patients treated with Natalizumab Extended Interval
  Dosing

- jcvtest_cost:

  JCV test cost assumption

## Value

a list of inputs

## Details

Stored within the function are all the default values of the inputs,
some parameters are modifiable by user here as described below. The
generated inputs will be called and processed by
[`gen_common_all_inputs()`](https://daysnmake.github.io/msdes/reference/gen_common_all_inputs.md)
to specify common and
[`gen_common_all_inputs()`](https://daysnmake.github.io/msdes/reference/gen_common_all_inputs.md)
unique patient inputs, by
[`gen_init_events()`](https://daysnmake.github.io/msdes/reference/gen_init_events.md)
to set initial and
[`gen_react_events()`](https://daysnmake.github.io/msdes/reference/gen_react_events.md)
reaction events. Parameters called by the model are specified here, they
can be modified or new ones can be added.
