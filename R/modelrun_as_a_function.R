#' RunModel as a function
#' 
#' `rrmsdesmodel()`RRMS DES model run written as a function to be called from Shiny APP
#' 
#' For quick deployment, also used in ShinyApp.
#' 
#' @export
rrmsdesmodel<- function(n_patients=n_patients, n_samples=n_samples){
  
  
  options(scipen = 999)
  options(tibble.print_max = 50)


## create New functions to draw from piece wise exponential distribution (ask JSA for help)
#source("R/draw_tte_new_warden.R")

## set a seed 
n_seed<-243523452
set.seed(n_seed)

## number of patients and samples
n_patients <<- n_patients
n_samples <<- n_samples



# Global parameters of the model as they are common to all patients and treatments, WARDEN specific
indicators <<-1

common_all_inputs <<- gen_common_all_inputs()

# patient parameters reflecting heterogeneous characteristics that depend on treatment, the same for both packages
unique_pt_inputs <<- gen_unique_pt_inputs()


# Add time to event for each patient for each of the treatments
# generate initial events
init_event_list <<- gen_init_events(RRMS_events=v_event_names)
# generate reaction events 
evt_react_list <<- gen_react_events(RRMS_events=v_event_names) 


# Add costs and utilities
util_ongoing <<- "utilities_disease"
cost_ongoing <<- "costs_disease"
util_instant <<- c("utilities_relapse", "utilities_sae")
cost_instant <<- c("costs_treatment", "costs_relapse", "costs_sae")


environment(run_sim_new) <- asNamespace("WARDEN") 

# Run the simulation
total_runtime <<- system.time({
  results <<- run_sim_new(                                  # use run_sim_parallel() for more than 1 simulation, although I've experienced errors
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = TRUE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    debug = FALSE,
    sensitivity_bool= FALSE,                  # sensitivity analysis not implemented,
    arm_list = v_treatment_names,             # intervention list
    common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
    #common_pt_inputs = common_pt_inputs,     # inputs that change within a simulation but are not affected by the intervention
    unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
    init_event_list = init_event_list,        # initial event list
    evt_react_list = evt_react_list,          # reaction of events
    util_ongoing_list = util_ongoing,         # ongoing rrms or spms disease utilities
    cost_ongoing_list = cost_ongoing,         # ongoing rrms or spms disease costs
    util_instant_list = util_instant,
    cost_instant_list = cost_instant,         # instantaneous treatment costs
    input_out=c("age", "sex", "spms_flag", "edss_value", "n_relapse", "n_sae_evts", "n_edssinc_evts", "n_edssdec_evts", "current_treatment_code",
                "treatment_line", "n_switch", "years_on_treatment", "treatment_stop", "t_2ndline", "t_3rdline", "t_4thline", "waning"),
    save_input_matrix = TRUE # Export the input matrix for EVPPI
  )
})
}

