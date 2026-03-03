#### RRMS DES using the WARDN package??? ####
## Authored: Howard Thom & Ayman Sadek ###
## Validated: Javier Sanchez Alvarez ####
## ACM2 Committee preferred assumptions##
# Public Prices  #############

# death excluded for now till we figure out how to model

rm(list = ls())

library(WARDEN)
#library(descem)
library(MASS)
library(VGAM)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)
library(BCEA)
library(ggplot2)
options(scipen = 999)
options(tibble.print_max = 50)

#Function to generate inputs, the same for bothe packages
source("R/generate_inputs.R")

# Global parameters of the model as they are common to all patients and treatments, WARDEN specific (not complete)
source("R/generate_global_inputs_warden.R")

# patient parameters reflecting heterogeneous characteristics that depend on treatment, the same for both packages
source("R/generate_individual_inputs.R")

#functions to generate initial events and reaction events, WARDEN specific (not complete)
source("R/generate_initial_events_warden.R")
source("R/generate_reaction_events_warden.R")

# Function to summarise the results
source("R/summarise_samples.R")
source("R/summarise_ce_results.R")

## create New functions to draw from piece wise exponential distribution (ask JSA for help)


## set a seed 
n_seed<-243523452
set.seed(n_seed)

## number of patients and samples
n_patients <- 10
n_samples <- 10

## Treatments at second line 
v_treatment_names <- c("Natalizumab-IV",
                       "Natalizumab-SC",
                       "Natalizumab biosimilar-IV",
                       "Natalizumab biosimilar-SC",
                       "Cladribine", 
                       "Ublituximab" ,
                       "Ofatumumab",
                       "Ocrelizumab")

## number of treatments
n_treatments <- length(v_treatment_names)

## specify codes for the treatments
v_treatment_codes <- c(seq(1, n_treatments))
names(v_treatment_codes) <- v_treatment_names

## Treatments that patients can move to at 3rd and higher line 
v_higher_line_treatments <- c("Natalizumab-IV",
                              "Natalizumab-SC",
                              "Natalizumab biosimilar-IV",
                              "Natalizumab biosimilar-SC",
                              "Cladribine",
                              "Ublituximab",
                              "Ofatumumab",
                              "Ocrelizumab"
                              )
v_higher_line_treatment_codes <- c(seq(1, length(v_higher_line_treatments)))
names(v_higher_line_treatment_codes) <- v_higher_line_treatments

## prior treatment index
v_prior_treatment_indices <- c(rep(0, length(v_higher_line_treatments)))
names(v_prior_treatment_indices) <- v_higher_line_treatments

## Model event states
v_event_names <- c("initialise","age" , "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "annual_treatment", "serious_adverse_event"
                   #, "death"
                   )

## Define the possible EDSS values
v_edss_values <- c(0:9)
names(v_edss_values) <- paste("EDSS", v_edss_values)

# Number of EDSS categories
n_edss_values <- length(v_edss_values)

## Model inputs generated using the generate inputs function
# saves the input in a list
inputs <- gen_inputs(dmt=v_treatment_names, severity = v_edss_values)

## Standard Deviation for probabilistic analysis when uncertainty not reported
# assumed 1/10 of mean
sd_psa <- 10


## Baseline population age sex and model time horizon
initial_age <- 36  # 36 ± 8 both arms of AFFIRM (N = 942)
time_horizon <- 100-initial_age # Years so lifetime
prop_female <- 0.70 #  (n=660) both arms of AFFIRM (N = 942)




# Add time to event for each patient for each of the treatments
# Setting trt to all treatments should mean that the event applies to all treatments

init_event_list <- gen_init_events(RRMS_events=v_event_names)
evt_react_list <- gen_react_events(RRMS_events=v_event_names)



# # Add ongoing utilities
# util_ongoing <- add_util(evt = v_event_names,
#                          trt = v_treatment_names,
#                          util = (1 - spms_flag) * v_u_rrms_edss[edss_value == v_edss_values] +
#                            spms_flag * v_u_spms_edss[edss_value == v_edss_values]
# )
# 
# # Add ongoing costs
# cost_ongoing <- add_cost(evt = v_event_names,
#                          trt = v_treatment_names,
#                          cost = (1 - spms_flag) * (v_c_rrms_edss[edss_value == v_edss_values]) +
#                            spms_flag * v_c_spms_edss[edss_value == v_edss_values])
# 
# ##Add instant costs
# cost_instant <-
#   add_cost(evt = c("annual_treatment") ,
#            trt = v_treatment_names,
#            cost = if(years_on_treatment <= 1) {(1- treatment_stop ) *(
#              (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr1",current_treatment_code] +
#                v_c_trt_admin["cost_annual_yr1",current_treatment_code] +
#                v_c_trt_monitor["cost_annual_yr1",current_treatment_code]) +
#                spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
#                               (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr1","Siponimod"])))
#            } else if(years_on_treatment == 2){(1- treatment_stop ) * (
#              (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
#                v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
#                v_c_trt_monitor["cost_annual_yr2",current_treatment_code])+
#                spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
#                               (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
#            } else if(years_on_treatment == 3){(1- treatment_stop ) * (
#              (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
#                 v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
#                 v_c_trt_monitor["cost_annual_yr2",current_treatment_code]) * inputs$df_c_tx_load[3,current_treatment_code])+
#               spms_flag *(((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
#                             (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
#            } else if(years_on_treatment == 4){ (1- treatment_stop ) *(
#              (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
#                 v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
#                 v_c_trt_monitor["cost_annual_yr2",current_treatment_code]) * inputs$df_c_tx_load[4,current_treatment_code])+
#                spms_flag *(((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
#                              (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
#            } else {(1- treatment_stop ) *(
#              (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
#                 v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
#                 v_c_trt_monitor["cost_annual_yr2",current_treatment_code]) * inputs$df_c_tx_load[5,current_treatment_code])+
#                spms_flag *(((1 - inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
#                              (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
#            }
#   ) %>% 
#   add_cost(evt = "relapse" ,
#            trt = v_treatment_names,
#            cost = (relapse_hospital_flag * v_c_relapse[6])+
#              ((1-relapse_hospital_flag) * v_c_relapse[4]) ) %>% 
#   add_cost(evt = "serious_adverse_event" ,
#            trt = v_treatment_names,
#            cost = (1- treatment_stop ) * sum(v_c_sae_events * v_weight_sae)) 
# 
# # Add instant utilities
# util_instant <-   add_util(evt = "relapse" ,
#                            trt = v_treatment_names,
#                            util = u_relapse) %>% 
#   add_util(evt = "serious_adverse_event" ,
#            trt = v_treatment_names,
#            util = sum(v_u_sae_events*v_weight_sae)) 


# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <- system.time({
  results <- run_sim(
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = TRUE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    debug = FALSE,
    sensitivity_bool= FALSE,                  # sensitivity analysis not implemented,
    arm_list = v_treatment_names,             # intervention list
    common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
    #common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
    unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
    init_event_list = init_event_list,        # initial event list
    evt_react_list = evt_react_list,          # reaction of events
    # util_ongoing_list = util_ongoing,
    # cost_ongoing_list = cost_ongoing,
    # util_instant_list = util_instant,
    # cost_instant_list = cost_instant,
    # ncores = 4,                               # number of cores to use, recommended not to use all
    # drc = 0.035,                              # discount rate for costs
    # drq = 0.035,                               # discount rate for qaly/lys
    # input_out=c("age", "sex", "spms_flag", "edss_value", "n_relapse", "n_sae_evts", "n_edssinc_evts", "n_edssdec_evts", "current_treatment_code",
    #             "treatment_line", "n_switch", "years_on_treatment", "treatment_stop", "t_2ndline", "t_3rdline", "t_4thline", "waning"),
    # v_random_seed_sequence = c(1:n_samples), # Sequence of random seeds
    # save_input_matrix = TRUE # Export the input matrix for EVPPI
  )
})



# Summarise the results
total_costs_ipd <- total_qalys_ipd <- array(NA, dim = c(n_samples, n_treatments, n_patients),
                                            dimnames = list(NULL, v_treatment_names, NULL))

# Below is not very efficient but only needs to be run once at end of simulation
for(treatment_name in v_treatment_names) {
  for(i_sample in 1:n_samples) {
    for(i_patient in 1:n_patients) {
      # With statement is neater but much slower
      temp_index <- results$output_psa[[i_sample]]$merged_df$pat_id == i_patient &
        results$output_psa[[i_sample]]$merged_df$trt == treatment_name &
        results$output_psa[[i_sample]]$merged_df$evtname == "death"
      
      total_costs_ipd[i_sample, treatment_name, i_patient] <-
        results$output_psa[[i_sample]]$merged_df$total_costs[temp_index]
      
      total_qalys_ipd[i_sample, treatment_name, i_patient] <-
        results$output_psa[[i_sample]]$merged_df$total_qalys[temp_index]
    } # End loop over patients
    # Summarise for just this sample
    
  }
}

options(scipen = 999)
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)


#drop natalizumab-biosimilar-SC and cladrebiene
total_costs <- total_costs[,-c(4,5)]
total_qalys <- total_qalys[,-c(4,5)]

results_table <- summarise_ce_results(total_qalys = total_qalys, 
                                      total_costs = total_costs,
                                      reference = "Natalizumab-IV")

View(results_table)

#plot results
v_results_treatment_names<- v_treatment_names[-c(4,5)]

# Create a BCEA object
des_bcea <- bcea(eff = total_qalys,
                 cost = total_costs,
                 ref = 1,
                 interventions = v_results_treatment_names)
ceplane.plot(des_bcea, wtp = 30000,
             graph = "ggplot2")

#plot a CEAC
des_multi_ce <- multi.ce(des_bcea)
ceac.plot(des_multi_ce, graph = "ggplot2",
          line = list(color = c("darkorange", "palegreen3", "blue", "grey",
                                "cyan3", "black"),
                      size=2 ),
          #ylim=c(0, 0.5),
          pos = c(1, 1)
)

