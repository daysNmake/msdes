# Revision of the DESCEM tutorial from
# https://r-hta.org/tutorial/descem/

# Howard Thom 23-June-2024
# Modified to include some uncertainty and use BCEA to analyse results.
# Also added a numeric severity score and repeat 'relapse' event
# Modified by Ayman Sadek 01JULY
# Vectorised treatments and , treatment dependent hr, current treatment in individual input
# Modified by Ayman Sadek 03JULY
# Vectorised events and event names
# added 3rd trt
# added in edss initial distribution, increase, utilities & medical costs


library(descem)

library(dplyr)
library(data.table)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)
library(BCEA)
options(scipen = 999)
options(tibble.print_max = 50)

n_patients <- 100
n_samples <- 10
n_treatments <- 3
treatment_names <- c("noint", "int", "intx")
event_names <- c("initial","sicker", "edss_increase", "death")

# Define the possible EDSS values
edss_values <- c(0:9)
names(edss_values) <- paste("EDSS", edss_values)

# RRMS EDSS Natural History
rrms_edss_natural_history <- read.csv("data/df_r_nh_rrms_bc_ta616.csv", header = T, row.names = 1)
colnames(rrms_edss_natural_history) <- row.names(rrms_edss_natural_history) <-paste("EDSS",edss_values)

#edss utilities
u_edss <- read.csv("data/df_u_edss_ta616.csv", header = T, row.names = 1)
edss_utilities <- t(u_edss[,5:6])

#edss costs
c_m_edss <- read.csv("data/df_c_directmedical_edss.csv", header = T, row.names = 1) #need to be inflated
c_nm_edss <- read.csv("data/df_c_direct_nonmedical_edss.csv", header = T, row.names = 1) #need to be inflated
edss_med_costs <- t(c_m_edss[1:10,5:6])
edss_nonmed_costs <- t(c_nm_edss[1:10,5:6])


# Initial distribution of EDSS (dummy but could come from a registry)
initial_edss_distribution <- c(0.1, 0.5, rep(0.4/8, 8))

#Each patient is identified through "i"
#psa_bool is the indicator of whether we want to apply a PSA or not

# Need functions to generate the coefficients and hazard ratios
# specific to each simulation
# For some reason this doesn't work.
#generate_coef_noint <- function(psa_bool) {
 # coef_noint <- rnorm(1, log(0.2), 0.1)
  #if(!psa_bool) coef_noint = log(0.2)
  #return(coef_noint)
#}

#generate_HR_int <- function(psa_bool) {
 # HR_int <- exp(rnorm(1, log(0.8), 0.01))
  #if(!psa_bool) HR_int = log(0.8)
  #return(HR_int)
#}


#Put objects here that do not change on any patient or intervention loop
common_all_inputs <-add_item(
  treatment_names = treatment_names,
  event_names =event_names,
  n_treatments=n_treatments,
  edss_values = edss_values,
  rrms_edss_natural_history = rrms_edss_natural_history,
  edss_utilities = edss_utilities,
  edss_med_costs = edss_med_costs,
  util.initial = 0.8,
  util.sicker = 0.5,
  cost.initial = 3000,
  cost.sicker = 7000,
  cost.int = 1000,
  cost.intx = 1200
  
  ) %>%
  add_item(
    coef_noint = if(psa_bool){
      temp_variable <- 2
      return(rnorm(1, log(0.2), 0.1))
    }
    else{
      log(0.2)
    }
    
  ) %>% add_item(
    # natural history of rrms 10x10 matrix
    baseline_rrms_edss_increase = if(psa_bool) {
      temp <- rrms_edss_natural_history
      for(i_edss in 1:nrow(temp)) {
        for (j_edss in 1:ncol(temp)){
          # Rates increase with higher EDSS, always positive
          temp[i_edss,j_edss] <- pmax(rnorm(1,
                                           mean = temp[i_edss,j_edss],
                                           sd = 0.01),0)}
      }
      temp
    } else {
      rrms_edss_natural_history
    }
    
    ) %>% add_item(
    # Hazard ratios reducing risk of EDSS increase
    HR_int = if(psa_bool) {
      temp <- matrix(nrow = 1, ncol = n_treatments,
                     dimnames = list(NULL, treatment_names))
      # Dummy values but final will come from the NMA
      temp[, "noint"] <- exp(rnorm(1,
                                         mean = log(1),
                                         sd = 0.01))
      temp[, "int"] <- exp(rnorm(1,
                                                    mean = log(0.2),
                                                    sd = 0.00))
      temp[, "intx"] <- exp(rnorm(1,
                                   mean = log(0.4),
                                   sd = 0.00))
      temp
    } else {matrix(c(1, 0.2, 0.4), nrow = 1, ncol = n_treatments,
                   dimnames = list(NULL, treatment_names))}
  ) %>% add_item(  
  # Utilities for RRMS Orme et al allowed to be negative for the edss8&9
  rrms_edss_utils = if(psa_bool) {
    temp <- edss_utilities[1,]
    for(j_edss in 1:length(temp)) {
      temp[j_edss] <- rnorm(1, mean =  edss_utilities[1,j_edss], sd =  edss_utilities[2,j_edss])
    }
    temp
    
  } else {
    edss_utilities[1,]
  }
  
  ) %>% add_item(  
    # Medical costs for RRMS Tyas et al these should inflated and drawn from a gamma distribution & replicate for non-medical costs
    rrms_edss_costs = if(psa_bool) {
      temp <- edss_med_costs[1,]
      for(j_edss in 1:length(temp)) {
        temp[j_edss] <- pmax(rnorm(1, mean =  edss_med_costs[1,j_edss], sd =  edss_med_costs[2,j_edss]),0)
      }
      temp
      
    } else {
      edss_med_costs[1,]
    }
    
    )%>%   add_item(
  #adverse events
    coef_ae = if(psa_bool) {exp(rnorm(1, log(0.5), 0.1))} else {0.5}
  )


#Put objects here that do not change as we loop through treatments for a patient
common_pt_inputs <- add_item(death= max(0.0000001, rnorm(n=1, mean=12, sd=3))) 

#Put objects here that change as we loop through treatments for each patient 
# (e.g. events can affect fl.tx, but events do not affect nat.os.s)
unique_pt_inputs <- add_item(edss_value = sample(size = 1, x = edss_values, # Random 
                                                 prob = initial_edss_distribution),
                             edss_i = edss_value,
                             fl.initial = 1, 
                             severity_score = 1,
                             current_treatment=sample(treatment_names,1,replace=TRUE, prob=c(rep(1/n_treatments, n_treatments)))) 

# Add time to event for each patient for each of the two treatments
init_event_list <- 
  add_tte(trt=treatment_names, evts = event_names,
          input={ #intervention
    initial <- 0
    sicker <- draw_tte(1,dist="exp", coef1=coef_noint, hr = HR_int[, trt == treatment_names])
    edss_increase <- draw_tte(
      1, 
      dist = "exp",
      coef1 = (baseline_rrms_edss_increase[edss_value == edss_values, edss_value == edss_values] * HR_int[, trt == treatment_names]) 
    )
    ae <- draw_tte(1, dist = "exp", coef1 = coef_ae)
  }) 

# Add a reaction to the events
# E.g. change treatment status or EDSS level
evt_react_list <-
  add_reactevt(name_evt = "initial",
               input = {
                 modify_item(list(current_treatment = trt))
               }) %>%
  add_reactevt(name_evt = "sicker",
               input = {
                 modify_item(list(fl.initial = 0,
                                  severity_score = severity_score * 0.8)) 
               }) %>%
  add_reactevt(name_evt = "edss_increase",
               input = {
                 modify_item(list(edss_i = edss_value,
                                  edss_value = min(edss_value + 1, max(edss_values))
                                  ))
                 
                 # Need to simulate the next time to EDSS increase
                 new_event(list(
                   "edss_increase" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     coef1 = (baseline_rrms_edss_increase[edss_i == edss_values, edss_value == edss_values] * HR_int[, trt == treatment_names]) 
                   )
                 ))
               })%>%
  add_reactevt(name_evt = "death",
               input = {
                 modify_item(list(curtime = Inf)) 
               }) %>%
  add_reactevt(name_evt = "ae",
               input = {
                 new_event(list("ae" = curtime + draw_tte(1, dist = "exp", coef1 = coef_ae)))
               }
               ) 


# Add utilities
# Example below is an expression that depends on common_all_inputs
util_ongoing <- add_util(evt = event_names,
                         trt = treatment_names,
                         util = severity_score * (util.initial * fl.initial + util.sicker * (1-fl.initial))
)

# Add costs
cost_ongoing <-
  add_cost(
    evt = event_names ,
    trt = "noint",
    cost = cost.initial * fl.initial + cost.sicker * (1-fl.initial) ) %>%
  add_cost(
    evt = event_names ,
    trt = "int",
    cost = cost.initial * fl.initial + cost.sicker * (1-fl.initial) + cost.int * fl.initial ) %>%
  add_cost(
    evt = event_names ,
    trt = "intx",
    cost = cost.initial * fl.initial + cost.sicker * (1-fl.initial) + cost.intx * fl.initial ) 


# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <- system.time({
  results <- RunSim( 
    debug=F,
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = F,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    trt_list = treatment_names,             # intervention list
    common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
    common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
    unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
    init_event_list = init_event_list,        # initial event list
    evt_react_list = evt_react_list,          # reaction of events
    util_ongoing_list = util_ongoing,
    cost_ongoing_list = cost_ongoing,
    ncores = 2,                               # number of cores to use, recommended not to use all
    drc = 0.035,                              # discount rate for costs
    drq = 0.035,                               # discount rate for qaly/lys
    input_out=c("edss_value", "edss_i", "severity_score", "current_treatment")
  )
})


# Plot the times to event
# Straight vertical line represents the median
data_plot <- results$final_output$merged_df %>%
  filter(evtname != "initial") %>%
  group_by(trt, evtname, simulation) %>%
  mutate(median = median(evttime)) %>%
  ungroup()

ggplot(data_plot) +
  geom_density(aes(fill = trt, x = evttime),
               alpha = 0.3) +
  geom_vline(aes(xintercept = median, col = trt)) +
  facet_wrap( ~ evtname, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()

# Summarise the cost-effectiveness results

summary_results_det(results$final_output)
summary_results_psa(results$output_psa)

# Simulated patients for first simulation
psa_ipd <- bind_rows(map(results$output_psa, "merged_df")) 

head(results$output_psa[[1]]$merged_df)
head(results$output_psa[[2]]$merged_df)

# Get the total costs and QALYs for each patient
total_costs_ipd <- total_qalys_ipd <- array(NA, dim = c(n_samples, n_treatments, n_patients),
                                    dimnames = list(NULL, treatment_names, NULL))

# Below is not very efficient but only needs to be run once at end of simulation
for(treatment_name in treatment_names) {
  for(i_sample in 1:n_samples) {
    for(i_patient in 1:n_patients) {
      # With statement is neater but much slower
      temp_index <- results$output_psa[[i_sample]]$merged_df$pat_id == i_patient &
        results$output_psa[[i_sample]]$merged_df$trt == treatment_name &
        results$output_psa[[i_sample]]$merged_df$evttime == 0
      
      total_costs_ipd[i_sample, treatment_name, i_patient] <-
        results$output_psa[[i_sample]]$merged_df$total_costs[temp_index]
      
      total_qalys_ipd[i_sample, treatment_name, i_patient] <-
        results$output_psa[[i_sample]]$merged_df$total_qalys[temp_index]
    } # End loop over patients
    # Summarise for just this sample
    
  }
}



# Take mean over patients to remove individual variation
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)

# Create a BCEA object
des_bcea <- bcea(eff = total_qalys,
                 cost = total_costs,
                 ref = 1,
                 interventions = treatment_names)

# Plot the CE plane
ceplane.plot(des_bcea, wtp = 20000, xlim = c(-0.05, 0.5), ylim = c(-500, 3000))

#plot a CEAC
des_multi_ce <- multi.ce(des_bcea)
ceac.plot(des_multi_ce, graph = "ggplot",
          line = list(color = c("red", "green")),
          pos = c(0, 0.50))



