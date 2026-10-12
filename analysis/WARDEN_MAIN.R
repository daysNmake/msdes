#### RRMS DES using the WARDN package  ####
## Authored: Howard Thom & Ayman Sadek ###
## Validated: Javier Sanchez Alvarez ####
## ACM2 Committee preferred assumptions##
# Public Prices  #############

# death excluded for now till we figure out how to model

rm(list = ls())

library(WARDEN)
library(MASS)
library(VGAM)
library(flexsurv)
library(tidyverse)
library(BCEA)
library(voi)

options(scipen = 999)
options(tibble.print_max = 50)


#load the functions required for the analysis
devtools::load_all() # load all functions

#summarise_ce_results() # Function to summarise the cost effectiveness results
#summarise_samples() # Function to summarise any data
#run_sim_parallel_new() Function includes input matrix for EVPPI and EVSI

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
v_event_names <- c("initialise","age" , "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "annual_treatment", "serious_adverse_event", "death"
                   )

## Define the possible EDSS values
v_edss_values <- c(0:9)
names(v_edss_values) <- paste("EDSS", v_edss_values)

# Number of EDSS categories
n_edss_values <- length(v_edss_values)

#Function to generate inputs, the same for both packages
#source("R/generate_inputs.R")
## Model inputs generated using the generate inputs function
# saves the input in a list
inputs <- gen_inputs(
  # change assumptions used in scenario analyses
  p_eid=0.6 # proportion of patients on external interval dosing
                     )

# Global parameters of the model as they are common to all patients and treatments
indicators <-1
common_all_inputs <- gen_common_all_inputs()

# patient parameters reflecting heterogeneous characteristics that depend on treatment
unique_pt_inputs <- gen_unique_pt_inputs()

# Generate initial events and reaction events
# Add time to event for each patient for each of the treatments
# Setting trt to all treatments should mean that the event applies to all treatments

init_event_list <- gen_init_events(RRMS_events=v_event_names)

evt_react_list <- gen_react_events(RRMS_events=v_event_names)

#util_ongoing_list <- gen_util_ongoing(dmt=v_treatment_names, severity = v_edss_values, RRMS_events=v_event_names)

util_ongoing <- "utilities_disease"
cost_ongoing <- "costs_disease"
util_instant <- c("utilities_relapse", "utilities_sae")
cost_instant <- c("costs_treatment", "costs_relapse", "costs_sae")

#environment(run_sim_parallel_new) <- asNamespace("WARDEN") 
environment(run_sim_new) <- asNamespace("WARDEN") 


# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <- system.time({
  results <- run_sim_new(                                  # use run_sim_parallel() for more than 1 simulation, although I've experienced errors
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
    # v_random_seed_sequence = c(1:n_samples), # Sequence of random seeds
     save_input_matrix = TRUE # Export the input matrix for EVPPI
  )
})



# Summarise the results

dim_results <- c(n_samples, n_treatments, n_patients)
trt_idx <- setNames(seq_along(v_treatment_names), v_treatment_names)
total_costs_ipd <- total_qalys_ipd <-  array(
  NA_real_,
  dim = dim_results,
  dimnames = list(NULL, v_treatment_names, NULL)
)

for (i_sample in seq_len(n_samples)) {
  
  df <- results[[1]][[i_sample]]$merged_df
  
  # Keep only final ("death") rows
  df <- df[df$evtname == "death", ]
  
  idx <- cbind(
    i_sample,
    trt_idx[df$arm],
    df$pat_id
  )
  
  total_costs_ipd[idx]         <- df$total_costs
  total_qalys_ipd[idx]         <- df$total_qalys
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
#write.csv(results_table, paste0(c("results/results warden determenistic", n_samples, "x", n_patients, ".csv"), collapse = " "))

#plot results
v_results_treatment_names<- v_treatment_names[-c(4,5)]

# Create a BCEA object
des_bcea <- bcea(eff = total_qalys,
                 cost = total_costs,
                 ref = 1,
                 interventions = v_results_treatment_names)
ceplane.plot(des_bcea, wtp = 35000,
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



#compare key events by treatment groups
modelresults_summarised <- summary_results_sim(results[[1]])
proportion_spms  <- modelresults_summarised["spms_flag",]
average_numberof_relapses <- modelresults_summarised["n_relapse",] 
average_numberof_edssincreases <- modelresults_summarised["n_edssinc_evts",] 
average_numberof_edssdecreases<- modelresults_summarised["n_edssdec_evts",] 
average_numberof_saes<- modelresults_summarised["n_sae_evts",] 
final_edss_value <- modelresults_summarised["edss_value",]
average_numberof_treatmentswitches <- modelresults_summarised["n_switch",] 
average_numberof_yearsontreatment <- modelresults_summarised["years_on_treatment",] 
average_numberof_yearson2ndline<- modelresults_summarised["t_2ndline",] 
average_numberof_yearson3rdline<- modelresults_summarised["t_3rdline",] 
average_numberof_yearson4thline<- modelresults_summarised["t_4thline",]

key_events_trt_grps <- as.data.frame(rbind(
  proportion_spms,   average_numberof_relapses, 
  average_numberof_edssincreases,   average_numberof_edssdecreases, 
  final_edss_value,   average_numberof_saes,
  average_numberof_treatmentswitches,  average_numberof_yearsontreatment,
  average_numberof_yearson2ndline,  average_numberof_yearson3rdline,
  average_numberof_yearson4thline),
  row.names=c("proportion with spms", "average no. of relapses", 
              "average no. of edss increases", "average no. of edss decreases", 
              "final edss value", "average no. of SAEs",
              "average no. of treatment switches", "average no. of years on treatment",
              "average no. of years on 2ndline", "average no. of years on 3rdline",
              "average no. of years on 4thline")
)
View(key_events_trt_grps)


#  VOI analysis
#total EVPI

total_evpi <- voi::evpi(outputs = list(c = total_costs,
                                       e = total_qalys,
                                       k = c(0, 25000, 50000, 75000, 100000)))

View(total_evpi)
plot(x=total_evpi$k, y=total_evpi$evpi, type="l")

# Total EVPPI
# All  parameters excluding hazard ratios that are constant as it causes errors
df_inputs <- as.data.frame(results[["input_matrix"]][,-c(73:76,81:84,89:92,79:101)])
parameters <- as.list(colnames(df_inputs))

total_evppi <- voi::evppi(outputs = list(c = total_costs,
                                         e = total_qalys,
                                         k = c(25000, 35000)),
                          inputs = df_inputs,
                          pars = parameters,
                          method = "bart", 
                          verbose = TRUE)

View(total_evppi)

#plot in base R split ober A and B panels as there are too many parameters to plot
total_evppi_25k_a <- total_evppi$evppi[total_evppi$k ==25000 & total_evppi$evppi > mean(total_evppi$evppi)]
names(total_evppi_25k_a) <- total_evppi$pars[total_evppi$k ==25000 & total_evppi$evppi > mean(total_evppi$evppi)]
par(las=1, bg="white", mai=c(0.85, 4, 0.5, 0.5))
barplot(
  sort(total_evppi_25k_a, decreasing=FALSE), horiz = TRUE, cex.names=0.9, main="EVPPI WTP 25,000/QALY (A)", xlab= "£"
)

total_evppi_25k_b <- total_evppi$evppi[total_evppi$k ==25000 & total_evppi$evppi < mean(total_evppi$evppi)]
names(total_evppi_25k_b) <- total_evppi$pars[total_evppi$k ==25000 & total_evppi$evppi < mean(total_evppi$evppi)]
barplot(
  sort(total_evppi_25k_b, decreasing=FALSE), horiz = TRUE, cex.names=0.6, main="EVPPI WTP 25,000/QALY (B)", xlab= "£"
)

total_evppi_35k_a <- total_evppi$evppi[total_evppi$k ==35000 & total_evppi$evppi > mean(total_evppi$evppi)]
names(total_evppi_35k_a) <- total_evppi$pars[total_evppi$k ==35000 & total_evppi$evppi > mean(total_evppi$evppi)]
par(las=1, bg="white", mai=c(0.85, 4, 1, 0.5))
barplot(
  sort(total_evppi_35k_a, decreasing=FALSE), horiz = TRUE, cex.names=0.5, main="EVPPI WTP 35,000/QALY (A)", xlab= "£"
)

total_evppi_35k_b <- total_evppi$evppi[total_evppi$k ==35000 & total_evppi$evppi < mean(total_evppi$evppi)]
names(total_evppi_35k_b) <- total_evppi$pars[total_evppi$k ==35000 & total_evppi$evppi < mean(total_evppi$evppi)]
par(las=1, bg="white", mai=c(0.85, 4, 1, 0.5))
barplot(
  sort(total_evppi_35k_b, decreasing=FALSE), horiz = TRUE, cex.names=0.6, main="EVPPI WTP 35,000/QALY (B)",xlab= "£"
)
