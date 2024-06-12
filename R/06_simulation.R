
#Logic is: per patient, per intervention, per event, react to that event.
results <- RunSim(  
  npats=10,                              # number of patients to be simulated
  n_sim=1,                                  # number of simulations to run
  psa_bool = T,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
  trt_list = c("int", "noint"),             # intervention list
  common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
  common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
  unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
  init_event_list = init_event_list,        # initial event list
  evt_react_list = evt_react_list,          # reaction of events
  #util_ongoing_list = util_ongoing,
  #cost_ongoing_list = cost_ongoing,
  ncores = 2,                               # number of cores to use, recommended not to use all
  drc = 0.035,                              # discount rate for costs
  drq = 0.035,                              # discount rate for QALYs
  input_out = c(                            # list of additional outputs (Flags, etc) that the user wants to export for each patient and event
    "nat.mort",
    "sex_pt"
  )
)


# results
summary_results_det(results$final_output)

psa_ipd <- bind_rows(map(results$output_psa, "merged_df")) 

psa_ipd[1:10,] %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))