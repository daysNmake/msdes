#### RRMS DES using the WARDN package ####
## Authored: Howard Thom & Ayman Sadek ###
## Validated: Javier Sanchez Alvarez ####
## ACM2 Committee preferred assumptions##
# Public Prices  #############

#model setup with inputs allowing for reactive changes to take place in shiny
rrmsdessetup <- function(p_eid=p_eid){
  ## Treatments at second line 
  v_treatment_names <<- c("Natalizumab-IV",
                          "Natalizumab-SC",
                          "Natalizumab biosimilar-IV",
                          "Natalizumab biosimilar-SC",
                          "Cladribine", 
                          "Ublituximab" ,
                          "Ofatumumab",
                          "Ocrelizumab")
  
  ## number of treatments
  n_treatments <<- length(v_treatment_names)
  
  ## specify codes for the treatments
  v_treatment_codes <<- c(seq(1, n_treatments))
  names(v_treatment_codes) <<- v_treatment_names
  
  ## Treatments that patients can move to at 3rd and higher line 
  v_higher_line_treatments <<- c("Natalizumab-IV",
                                 "Natalizumab-SC",
                                 "Natalizumab biosimilar-IV",
                                 "Natalizumab biosimilar-SC",
                                 "Cladribine",
                                 "Ublituximab",
                                 "Ofatumumab",
                                 "Ocrelizumab"
  )
  v_higher_line_treatment_codes <<- c(seq(1, length(v_higher_line_treatments)))
  names(v_higher_line_treatment_codes) <<- v_higher_line_treatments
  
  ## prior treatment index
  v_prior_treatment_indices <<- c(rep(0, length(v_higher_line_treatments)))
  names(v_prior_treatment_indices) <<- v_higher_line_treatments
  
  ## Model event states
  v_event_names <<- c("initialise","age" , "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "annual_treatment", "serious_adverse_event", "death"
  )
  
  ## Define the possible EDSS values
  v_edss_values <<-  c(0:9)
  names(v_edss_values) <<- paste("EDSS", v_edss_values)
  
  # Number of EDSS categories
  n_edss_values <<- length(v_edss_values)
  
  #Function to generate inputs, the same for both packages
  source("R/generate_inputs.R")
  ## Model inputs generated using the generate inputs function
  # saves the input in a list
  #environment(gen_inputs) <- environment()
  inputs <<- gen_inputs(p_eid=p_eid)
}


#model setup and run saved as a function for use in shiny

rrmsdesmodel<- function(n_patients, n_samples){
  
  
  options(scipen = 999)
  options(tibble.print_max = 50)


## create New functions to draw from piece wise exponential distribution (ask JSA for help)
source("R/draw_tte_new_warden.R")

## set a seed 
n_seed<-243523452
set.seed(n_seed)

## number of patients and samples
n_patients <- n_patients
n_samples <- n_samples



# Global parameters of the model as they are common to all patients and treatments, WARDEN specific (not complete)
source("R/generate_global_inputs_warden.R")

#Function to extract data from environments containing lists while indexing on a vector of treatments or severity states
source("R/pluck_vector_from_env.R")

# patient parameters reflecting heterogeneous characteristics that depend on treatment, the same for both packages
source("R/generate_individual_inputs_warden.R")

#functions to generate initial events and reaction events, WARDEN specific (not complete)
source("R/generate_initial_events_warden.R")
source("R/generate_reaction_events_warden.R")

# Add time to event for each patient for each of the treatments
# Setting trt to all treatments should mean that the event applies to all treatments
#environment(gen_init_events) <- environment()
init_event_list <<- gen_init_events(RRMS_events=v_event_names) #functions do not accept named vectors of EDSS0-9 or dmt names
#environment(gen_react_events) <- environment()
evt_react_list <<- gen_react_events(RRMS_events=v_event_names) #functions do not accept named vectors of EDSS0-9 or dmt names

#util_ongoing_list <- gen_util_ongoing(dmt=v_treatment_names, severity = v_edss_values, RRMS_events=v_event_names)

util_ongoing <<- "utilities_disease"
cost_ongoing <<- "costs_disease"
util_instant <<- c("utilities_relapse", "utilities_sae")
cost_instant <<- c("costs_treatment", "costs_relapse", "costs_sae")

indicators <<-0



# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <<- system.time({
  results <<- run_sim(                                  # use run_sim_parallel() for more than 1 simulation
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = TRUE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    debug = FALSE,
    sensitivity_bool= FALSE,                  # sensitivity analysis not implemented,
    #indicators= ? ,
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
    # ncores = 4,                               # number of cores to use, recommended not to use all

     input_out=c("age", "sex", "spms_flag", "edss_value", "n_relapse", "n_sae_evts", "n_edssinc_evts", "n_edssdec_evts", "current_treatment_code",
                 "treatment_line", "n_switch", "years_on_treatment", "treatment_stop", "t_2ndline", "t_3rdline", "t_4thline", "waning"),
    # v_random_seed_sequence = c(1:n_samples), # Sequence of random seeds
    # save_input_matrix = TRUE # Export the input matrix for EVPPI
  )
})
}

