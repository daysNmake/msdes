# Run Sim new

`run_sim_new()` modified run_sim() function

## Usage

``` r
run_sim_new(
  arm_list = c("int", "noint"),
  sensitivity_inputs = NULL,
  common_all_inputs = NULL,
  common_pt_inputs = NULL,
  unique_pt_inputs = NULL,
  init_event_list = NULL,
  evt_react_list = evt_react_list,
  util_ongoing_list = NULL,
  util_instant_list = NULL,
  util_cycle_list = NULL,
  cost_ongoing_list = NULL,
  cost_instant_list = NULL,
  cost_cycle_list = NULL,
  other_ongoing_list = NULL,
  other_instant_list = NULL,
  npats = 500,
  n_sim = 1,
  psa_bool = NULL,
  sensitivity_bool = FALSE,
  sensitivity_names = NULL,
  n_sensitivity = 1,
  input_out = character(),
  ipd = 1,
  constrained = FALSE,
  timed_freq = NULL,
  debug = FALSE,
  accum_backwards = FALSE,
  continue_on_error = FALSE,
  seed = NULL,
  save_input_matrix = FALSE
)
```

## Details

This runs the model to allow for extra functionality for VOI analysis
Appends input_matrix to results, this contains the paramters sampled per
simulation
