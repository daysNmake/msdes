# Generate Reaction Events for the RRMS DES Model

Defines the model's response to each event, specifying how patient state
variables, costs, utilities, and future events are updated when an event
occurs. Returns a WARDEN `add_reactevt()` chain consumed by
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
via the `evt_react_list` argument in
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Usage

``` r
gen_react_events(RRMS_events)
```

## Arguments

- RRMS_events:

  Character vector of model event names, passed from `v_event_names` set
  by
  [`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md).
  Must match the events initialised in
  [`gen_init_events`](https://daysnmake.github.io/msdes/reference/gen_init_events.md).

## Value

A WARDEN `add_reactevt()` chain object containing reaction logic for all
events in `RRMS_events`, passed to
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
via the `evt_react_list` argument in
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Details

Each event reaction is defined using
[`add_reactevt`](https://jsanchezalv.github.io/WARDEN/reference/add_reactevt.html).
All reaction events update `utilities_disease` and `costs_disease` at
each event time using the EDSS-specific utility and cost.

The following reaction events are defined:

**`initialise`**

Sets `current_treatment` and `current_treatment_code` to the arm being
simulated. Adjusts the `death` event time to be relative to model entry
and caps it at `time_horizon`. Schedules the first `annual_treatment`
event.

**`age`**

Updates `age` and refreshes `utilities_disease` and `costs_disease` for
the current EDSS state.

**`annual_treatment`**

Increments `years_on_treatment` by 1 and updates the treatment-line time
counter (`t_2ndline`, `t_3rdline`, or `t_4thline`) for RRMS patients on
treatment. Schedules the next `annual_treatment` event one year ahead if
`edss_value <= 6`.

Applies instantaneous treatment acquisition, administration, and
monitoring costs (`costs_treatment`) conditional on treatment year and
SPMS status,costs are scaled by the treatment load factor. **`relapse`**

Increments `n_relapse` by 1. Applies instantaneous relapse disutilities
and relapse costs. Schedules the next relapse using the same exponential
draw from an exponential distribution using baseline log-rate for HARRMS
patients modified by the treatment-specific ARR rate ratio from the NMA.

**`edss_increase`**

Increments `edss_value` by 1, capped at (EDSS 9). Increments
`n_edssinc_evts` by 1. Sets `treatment_stop <- 1` if `edss_value > 6`
and treatment has not already stopped — this triggers the cessation of
treatment cost accrual and applies placebo hazard ratios to future
events. Schedules the next `edss_increase` event drawn from an
exponential distribution using baseline log-rate for HARRMS patients
modified by the treatment-specific CDP6 hazard ratio from the NMA.

**`edss_decrease`**

Decrements `edss_value` by 1, floored at (EDSS 0). Increments
`n_edssdec_evts` by 1. Checks and sets `treatment_stop` as in
`edss_increase`. Schedules the next `edss_decrease` event drawn from an
exponential distribution for patients.

**`progression`**

Sets `spms_flag <- 1`, converting the patient from RRMS to SPMS.
Progression can only occur once; no new progression event is scheduled.
Immediately updates `utilities_disease` and `costs_disease` to SPMS
values for the current EDSS state. Only applies to patients with
`edss_value > 4`.

**`death`**

Sets `curtime <- Inf`, ending the patient's simulation. Updates
`utilities_disease` and `costs_disease` for the final EDSS state. No new
events are scheduled. Death can only occur once.

**`serious_adverse_event`**

Increments `n_sae_evts` by 1. Applies the SAE disutility and SAE costs.
Schedules the next SAE' Drawn from an exponential distribution using the
baseline SAE rate modified by the treatment-specific SAE hazard ratio
from the NMA .

**`treatment_switch`**

Increments `treatment_line` by 1 and `n_switch` by 1. Marks the current
treatment as used in `v_prior_treatment_indices`. Resets
`years_on_treatment` to 0.

New treatment selection logic:

- First switch (`n_switch == 1`): samples from available higher-line
  treatments (excluding current and prior treatments), weighted by
  proportion of patients on 3rd line from the registry.

- Second or later switch (`n_switch >= 2`): samples uniformly from
  remaining available treatments.

The discontinuation rate is updated to reflect the protocol assumption
that patients switch at most twice (starting from 2nd line, reaching 4th
line as the final option). Schedules the next `treatment_switch` event
drawn from an exponential distribution using the baseline
discontinuation hazard modified by the treatment-specific
discontinuation hazard ratios.

## See also

[`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md),
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md),
[`gen_inputs`](https://daysnmake.github.io/msdes/reference/gen_inputs.md),
[`gen_common_all_inputs`](https://daysnmake.github.io/msdes/reference/gen_common_all_inputs.md),
[`gen_unique_pt_inputs`](https://daysnmake.github.io/msdes/reference/gen_unique_pt_inputs.md),
[`gen_init_events`](https://daysnmake.github.io/msdes/reference/gen_init_events.md),
[`add_reactevt`](https://jsanchezalv.github.io/WARDEN/reference/add_reactevt.html),
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
