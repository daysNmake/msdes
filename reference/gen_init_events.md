# Generate Initial Events for the RRMS DES Model

Generates the initial time-to-event draws for all model events at the
start of each patient's simulation, for all treatment arms. Returns a
WARDEN `add_tte()` object consumed by
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
via the `init_event_list` argument in
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Usage

``` r
gen_init_events(RRMS_events)
```

## Arguments

- RRMS_events:

  Character vector of model event names, passed from `v_event_names` set
  by
  [`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md).
  Defines which events receive an initial time-to-event draw.

## Value

A WARDEN `add_tte()` object containing initial time-to-event draws for
all events in `RRMS_events` across all arms in `v_treatment_names`,
passed to
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
via the `init_event_list` argument in
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Details

Initial event times are drawn once per patient at simulation entry using
[`add_tte`](https://jsanchezalv.github.io/WARDEN/reference/add_tte.html).
Events are generated across all arms in `v_treatment_names`. Each event
uses parameters from the common global inputs
([`gen_common_all_inputs`](https://daysnmake.github.io/msdes/reference/gen_common_all_inputs.md))
and individual patient inputs
([`gen_unique_pt_inputs`](https://daysnmake.github.io/msdes/reference/gen_unique_pt_inputs.md))
available in the WARDEN simulation environment.

The following events are initialised:

**`initialise`, `annual_treatment`, `age`**

Set to 0. These are structural trigger events handled by the reaction
event and updated annually. \#' **`relapse`**

Drawn from an exponential distribution using baseline log-rate for
HARRMS patients modified by the treatment-specific ARR rate ratio from
the NMA.

**`edss_increase`**

Drawn from an exponential distribution using baseline log-rate for
HARRMS patients modified by the treatment-specific CDP6 hazard ratio
from the NMA,

**`edss_decrease`**

Drawn from an exponential distribution for patients.

**`progression`**

Drawn from an exponential distribution. Only applies to patients with
`edss_value > 4` and `spms_flag = 0`. Uses the MS Registry
HARRMS-to-SPMS conversion rate.

**`death`**

Drawn from a piecewise exponential distribution using age-sex-specific
all-cause mortality rates modified by MS-specific mortality multiplier
the EDSS-severity.

**`serious_adverse_event`**

Drawn from an exponential distribution using the baseline SAE rate
modified by the treatment-specific SAE hazard ratio from the NMA .

**`treatment_switch`**

Drawn from an exponential distribution using the baseline
discontinuation hazard modified by the treatment-specific
discontinuation hazard ratios.

## See also

[`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md),
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md),
[`gen_inputs`](https://daysnmake.github.io/msdes/reference/gen_inputs.md),
[`gen_common_all_inputs`](https://daysnmake.github.io/msdes/reference/gen_common_all_inputs.md),
[`gen_unique_pt_inputs`](https://daysnmake.github.io/msdes/reference/gen_unique_pt_inputs.md),
[`gen_react_events`](https://daysnmake.github.io/msdes/reference/gen_react_events.md),
[`add_tte`](https://jsanchezalv.github.io/WARDEN/reference/add_tte.html),
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
