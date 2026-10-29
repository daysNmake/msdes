# Generate Unique Individual Patient Inputs

Generates individual patient-level parameters that are heterogeneous
across patients and may depend on treatment history. Called once per new
patient at the start of their simulation. Returns a WARDEN `add_item()`
object consumed by
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
via the `unique_pt_inputs` argument in
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Usage

``` r
gen_unique_pt_inputs()
```

## Value

A WARDEN `add_item()` object containing all individual patient inputs,
passed directly to `run_sim_new` via the `unique_pt_inputs` argument in
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md).

## Details

Parameters are sampled or initialised for each new patient. The current
treatment arm is initialised here but is later overwritten by WARDEN
with the arm being simulated (`arm`).

The following parameters are generated:

**Sampled at patient entry**

- `edss_value` — baseline EDSS score (0–9), sampled from a
  Dirichlet-weighted discrete distribution using the HARRMS row of
  `m_n_patients_edss` (MS Registry).

- `sex` — patient sex (0 = male, 1 = female), sampled using
  `prop_female` (0.70, AFFIRM).

- `spms_flag` — indicator for baseline SPMS status (0 = RRMS, 1 = SPMS),
  sampled using `initial_spms_prop` (default 0).

- `relapse_hospital_flag` — indicator for whether a relapse leads to
  hospital admission, sampled using `p_relapse_hospital` (Hawton et al.
  2026).

- `current_treatment` — initial treatment arm, sampled from
  `v_treatment_names` weighted by `l_prop_3rdline_trt`. Note:
  overwritten by WARDEN with the arm under simulation (`trt`).

- `current_treatment_code` — numeric code of `current_treatment`, looked
  up from `v_treatment_codes`.

**Event history counters (initialised to 0)**

- `n_relapse` — cumulative number of relapses.

- `n_sae_evts` — cumulative number of serious adverse events.

- `n_edssinc_evts` — cumulative number of EDSS increases.

- `n_edssdec_evts` — cumulative number of EDSS decreases.

- `n_switch` — cumulative number of treatment switches (sense check
  counter).

**Time trackers (initialised to 0)**

- `t_progression` — time spent in progression.

- `t_2ndline`, `t_3rdline`, `t_4thline` — time spent on 2nd, 3rd, and
  4th line treatments respectively.

- `tto_dp1` to `tto_dp7` — time to each disability progression event, up
  to 7 events.

- `tto_rel1` to `tto_rel7` — time to each relapse event, up to 7 events.

- `yrs_edss_0` to `yrs_edss_9` — annual time spent in each EDSS severity
  state (0–9).

- `years_on_treatment` — cumulative years on current treatment, used for
  drug acquisition cost calculation.

**State flags (initialised to 0)**

- `waning` — waning of treatment effect flag. Set to 0 by default (no
  waning); multiplied into the hazard ratio as `HR x (1 - waning)`.

- `treatment_stop` — flag set to 1 when EDSS reaches 7 or above,
  stopping treatment acquisition costs.

**Treatment and patient state (initialised)**

- `treatment_line` — current treatment line, initialised to 2 (all
  patients start on second-line therapy).

- `age` — baseline age, set to `inputs[["initial_age"]]`.

**Cost and utility accumulators (initialised to 0)**

- `utilities_disease` — accumulated disease utility.

- `costs_disease` — accumulated disease costs.

- `costs_treatment` — accumulated treatment costs.

- `utilities_relapse` — accumulated relapse utility.

- `costs_relapse` — accumulated relapse costs.

- `utilities_sae` — accumulated SAE utility.

- `costs_sae` — accumulated SAE costs.

## See also

[`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md),
[`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md),
[`gen_inputs`](https://daysnmake.github.io/msdes/reference/gen_inputs.md),
[`add_item`](https://jsanchezalv.github.io/WARDEN/reference/add_item.html),
[`run_sim_new`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
