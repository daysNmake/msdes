# Experiment to implement RRMS DES using the DESCEM package
# Howard Thom & Ayman Sadek 10 Oct 2024

# V2 changed samples from n_samples to 1 for random parameters
# This doesn't work properly as each simulation looks very similar
# fixed this issue now, you can see the differences between trt groups.

# V3 puts the functions for each parameter set into add_item() expressions
# v4 fixed matrix accessing for parameters (As they only have one row)

# V5 fixed aging and death and modified matrix accessing for parameters to index on rows and columns
# V5.1 added in the full number of treatments and updated data 
    # Added registry data but not working correctly 

# TODO Figure out neater way to add each parameter
# Modularise, optimise, and add RShiny interface

rm(list = ls())

library(descem)
library(MASS)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)
library(BCEA)
options(scipen = 999)
options(tibble.print_max = 50)

source("R/draw_tte_new.R")

set.seed(243523452)

n_patients <- 10
n_samples <- 1

# Treatments at second line (expand to full set)
treatment_names <- c("Natalizumab",
                     "Natalizumab biosimilar",
                     "Fingolimod",
                     "Alemtuzumab",
                     "Cladribine", 
                     "Ponesimod" ,
                     "Ofatumumab",
                     "Ocrelizumab",
                     "PegIFN-β-1 SC 125μg", 
                     "IFNβ-1a SC 22μg" , 
                     "IFNβ-1a SC 44μg" ,
                     "IFNβ-1a IM 30μg", 
                     "IFNβ-1b SC 250μg", 
                     "Glatiramer Acetate 20-40mg" )

n_treatments <- length(treatment_names)
treatment_codes <- c(seq(1, n_treatments))
names(treatment_codes) <- treatment_names

# Treatments that patients can move to at 3rd and higher line (expand to full set)
higher_line_treatments <- c("Natalizumab",
                            "Natalizumab biosimilar",
                            "Alemtuzumab",
                            "Cladribine")
higher_line_treatment_codes <- c(1, 2, 4, 5)
names(higher_line_treatment_codes) <- higher_line_treatments

event_names <- c("initialise", "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "death")

initial_age <- 20 
time_horizon <- 80 # Years so lifetime
prop_female <- 0.50

# Define the possible EDSS values
edss_values <- c(0:9)
names(edss_values) <- paste("EDSS", edss_values)

## RRMS EDSS Natural History Palace etal
#rrms_edss_natural_history <- read.csv("data/df_r_nh_rrms_bc_ta616.csv", header = T, row.names = 1)
#colnames(rrms_edss_natural_history) <- row.names(rrms_edss_natural_history) <-paste("EDSS",edss_values)

## Natural History MS Registry
tto_edss_increase_harrms_mu <- t(read.csv("data/v_tto_edss_increase_harrms_mu.csv", header = T, row.names = 1))
tto_edss_increase_harrms_sigma <- read.csv( file="data/m_tto_edss_increase_harrms_sigma.csv", header = T, row.names = 1)
tto_edss_increase_spms_mu <- t(read.csv("data/v_tto_edss_increase_spms_mu.csv", header = T, row.names = 1))
tto_edss_increase_spms_sigma <- read.csv("data/m_tto_edss_increase_spms_sigma.csv", header = T, row.names = 1)
tto_edss_decrease_rrms_mu <- t(read.csv("data/v_tto_edss_decrease_rrms_mu.csv", header = T, row.names = 1))
tto_edss_decrease_rrms_sigma <- read.csv("data/m_tto_edss_decrease_rrms_sigma.csv", header = T, row.names = 1)
tto_edss_decrease_spms_mu <- t(read.csv("data/v_tto_edss_decrease_spms_mu.csv", header = T, row.names = 1))
tto_edss_decrease_spms_sigma <- read.csv("data/m_tto_edss_decrease_spms_sigma.csv", header = T, row.names = 1)


# Initial distribution of EDSS (dummy but could come from a registry)
initial_edss_distribution <- c(0.8, 0.1, rep(0.0125, 8))

# Initial proportion with SPMS
initial_spms_prop <- 0

## baseline population risk parameters: relapse rates and SMR
df_init_params <- read.csv("data/df_init_params.csv", header = T, row.names = 1)

## base line relapse rates Placebo arm of the clarabidine study in tA616 
bl_rrms_rr <- as.double(c(df_init_params[1], sqrt(437)*(df_init_params[3]-df_init_params[2])/3.92))
r_bl_rrms_rr <- -log(1-bl_rrms_rr)

## Proportion of patients having relapse leading to hospitalisation ~DUMMY DATA~
relapse_hospital_prop <- 0.2

# rates of Adverse events (Cladribine Alemtuzumab Fingolimod Natalizumab Daclizuma] from TA616, rest is ~DUMMY DATA~)
prob_ae <- read.csv("data/df_adverse_events_probabilities_ta616.csv", header = T, row.names = 1)
r_ae <- -log(1-prob_ae)
r_ae

#rates of mortality, "mx" columns of life tables
r_mortality <- as.matrix(read.csv("data/df_all_cause_mortality.csv", header = T, row.names = 1))
r_mortality <- r_mortality[,c(1, 2,8)]
r_mortality[,1] <- as.numeric(r_mortality[,1])
dimnames(r_mortality) <-  list(NULL, c("time" ,"male", "female"))


#edss costs direct medical costs Tyas etal. 2005 inflated to 2022/2023 Edss6 is an average of edss6 and 6.5
c_m_edss <- read.csv("data/df_c_directmedical_edss.csv", header = T, row.names = 1) 
edss_med_costs <- t(c_m_edss[1:10,7:8])
#add 398 with se of 1002 for an spms cost
# do we include non-dirct medical costs? they were previously contreverial

#MS specific standardised mortality rate Manouchehrinia 2016
smr <- as.double(c(df_init_params["smr"], sqrt(437)*(df_init_params["smr_uci"]-df_init_params["smr_lci"])/3.92))

# Annual treatment, administration and monitoring costs TA616 TA312 Beta-IFN-MTA
c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
c_tx <- c_tx[-1:-2,]
c_tx_admin <- t(read.csv("data/df_c_drugs_admin.csv", header = T, row.names = 1))
c_tx_admin <- c_tx_admin[-1:-2,]
c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
c_tx_monitor <- c_tx_monitor[-1:-2,]

## Relapse Costs (Hawton 2016b and Karampampa 2012c)
c_relapse <-read.csv("data/df_c_relapse.csv", header = T, row.names = 1)
c_relapse <- c_relapse[3:8,3:4] #choose inflated Hawton 2016 to 2023
c_relapse <- c_relapse*2 # Annualise these 6 months costs

## Adverse Events costs TA616 # need to be weighted by key trial
c_ae <-read.csv("data/df_c_disutilities_ta616.csv", header = T, row.names =1)

## edss utilities columns 5&6 Orme etal 2007
u_edss <- read.csv("data/df_u_edss_ta616.csv", header = T, row.names = 1)
edss_utilities <- t(u_edss[1:10,5:6])

## Relapse utilities
u_relapse <-read.csv("data/df_u_relapse.csv", header = T, row.names = 1)

## Adverse Events disutilities TA616 # need to be weighted by key trial
u_ae <-read.csv("data/df_c_disutilities_ta616.csv", header = T, row.names =1)


# Global parameters of the model as they are common to all patients and treatments
# If not using PSA just setting to mean of samples. This isn't correct as the 
# true mean should be used
common_all_inputs <-add_item(
  treatment_names = treatment_names,
  treatment_codes = treatment_codes,
  higher_line_treatments = higher_line_treatments,
  higher_line_treatment_codes = higher_line_treatment_codes,
  initial_age = initial_age,
  time_horizon = time_horizon,
  prop_female = prop_female,
  edss_values = edss_values,
  c_tx = c_tx, # make random
  c_tx_admin = c_tx_admin, # make random
  c_tx_monitor = c_tx_monitor, # make random
  event_names = event_names,
  r_bl_rrms_rr = r_bl_rrms_rr, # make random
  r_mortality = r_mortality, # make random
  smr = smr, # make random
  initial_spms_prop = initial_spms_prop,
  relapse_hospital_prop = relapse_hospital_prop # make random
) %>% add_item(
  # Utilities for RRMS Orme etal. allowed to be negative for the edss8&9
  u_rrms_edss = if(psa_bool) {
    temp <- edss_utilities[1,]
    for(j_edss in 1:length(temp)) {
      temp[j_edss] <- rnorm(1, mean =  edss_utilities[1,j_edss], sd =  edss_utilities[2,j_edss])
    }
    temp
    
  } else {
    edss_utilities[1,]
  }
  # Utililities for RRMS assumed to decrease with EDSS
#  rrms_edss_utils = if(psa_bool) {
#    temp <- matrix(nrow = 1, ncol = length(edss_values),
#                   dimnames = list(NULL, names(edss_values)))
    
    # Fill with dummy values (gradual reduction bounded by 0)
#    for(i_edss in 1:length(edss_values)) {
#      temp[i_edss] <- pmax(rnorm(1, mean = 1 - 0.1 * i_edss, sd = 0.01),
#                             0)
#    }
#    temp
#  } else {
#    1 - 0.1 * edss_values
#  }
#) %>% add_item (
  # Placeholder SPMS EDSS utilities assumed to be lower than RRMS
#  spms_edss_utils = if(psa_bool) {
#    temp <- matrix(nrow = 1, ncol = length(edss_values),
#                   dimnames = list(NULL, names(edss_values)))
    
    # Fill with dummy values (gradual reduction bounded by 0)
#    for(i_edss in 1:length(edss_values)) {
#      temp[i_edss] <- pmax(rnorm(1, mean = (1 - 0.1 * i_edss), sd = 0.01),
#                             0)
#    }
#    0.5 * temp
#  } else {
#    0.5 * (1 - 0.1 * edss_values)
#  }
) %>% add_item(
  # Medical costs for RRMS Tyas et al inflated 
  # should be drawn from a gamma distribution? 
  c_rrms_edss = if(psa_bool) {
    temp <- edss_med_costs[1,]
    for(j_edss in 1:length(temp)) {
      temp[j_edss] <- pmax(rnorm(1, mean =  edss_med_costs[1,j_edss], sd =  edss_med_costs[2,j_edss]),0)
    }
    temp
    
  } else {
    edss_med_costs[1,]
  }
  # Dummy values for state costs in RRMS assumed to increase with EDSS
#  rrms_edss_costs = if(psa_bool) {
#    temp <- matrix(nrow = 1, ncol = length(edss_values),
#                   dimnames = list(NULL, names(edss_values)))
    
#    for(i_edss in 1:length(edss_values)) {
#      temp[i_edss] <- pmax(rnorm(1, mean = 100 + 100 * i_edss, sd = 10),
#                             0)
#    }
#    temp
#  } else {
#    100 + 100 * edss_values
#  }
#) %>% add_item(
#  # Costs assumed higher
#  spms_edss_costs = if(psa_bool) {
#    temp <- matrix(nrow = 1, ncol = length(edss_values),
#                   dimnames = list(NULL, names(edss_values)))
#    
#    for(i_edss in 1:length(edss_values)) {
#      temp[i_edss] <- pmax(rnorm(1, mean = 100 + 100 * i_edss, sd = 10),
#                             0)
#    }
#    1.5 * temp
#  } else {
#    1.5 * (100 + 100 * edss_values)
#  }
#) %>% add_item(
#  treatment_costs = treatment_costs
) %>% add_item(
  # Hazard ratios reducing risk of EDSS increase
  hr_edss_increase = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # Dummy values but final will come from the NMA
    temp[treatment_codes["Natalizumab"]] <- exp(rnorm(1,
                                       mean = log(0.8),
                                       sd = 0.01))
    temp[treatment_codes["Natalizumab biosimilar"]] <- exp(rnorm(1,
                                                  mean = log(0.8),
                                                  sd = 0.01))
    temp[treatment_codes["Fingolimod"]] <- exp(rnorm(1,
                                      mean = log(0.7),
                                      sd = 0.01))
    temp[treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
                                       mean = log(0.6),
                                       sd = 0.01))
    temp[treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                      mean = log(0.9),
                                      sd = 0.01))
    temp[treatment_codes["Ponesimod"]] <- exp(rnorm(1,
                                      mean = log(0.45),
                                      sd = 0.01))
    temp[treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                           mean = log(0.4),
                                           sd = 0.01))
    temp[treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                     mean = log(0.4),
                                                     sd = 0.01))
    temp[treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
                                           mean = log(0.35),
                                           sd = 0.01))
    temp[treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
                                           mean = log(0.3),
                                           sd = 0.01))
    temp[treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
                                                             mean = log(0.3),
                                                             sd = 0.01))
    temp[treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
                                           mean = log(0.25),
                                           sd = 0.01))
    temp[treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
                                           mean = log(0.2),
                                           sd = 0.01))
    temp[treatment_codes["Glatiramer Acetate 20-40mg"]] <- exp(rnorm(1,
                                           mean = log(0.15),
                                           sd = 0.01))
    temp
  } else {c(0.8, 0.8, 0.7, 0.6, 0.9, 0.45, 0.4, 0.4, 0.35, 0.3, 0.3, 0.25, 0.2, 0.15)}
  
) %>% add_item(
  baseline_rrms_edss_increase = if(psa_bool) {

    mvrnorm(n = 1, 
            mu = tto_edss_increase_harrms_mu, 
            Sigma = tto_edss_increase_harrms_sigma) 

  } else {tto_edss_increase_harrms_mu }
  
#  baseline_rrms_edss_increase = if(psa_bool) {
#    temp <- matrix(nrow = length(edss_values), ncol = length(edss_values),
#                   dimnames = list(names(edss_values), names(edss_values)))
#    for(i_edss in 1:length(edss_values)) {
#      for(j_edss in 1:length(edss_values)) {
#      # Rates increase with higher EDSS
#      temp[i_edss,j_edss] <- exp(rnorm(1,
#                                  mean = log(0.6 + 0.004 * i_edss*j_edss),
#                                  sd = 0.01))
#    }}
#    temp
#  } else {
#    matrix(rep(0.6 + 0.04 * edss_values, length(edss_values)),byrow=TRUE ,nrow = length(edss_values), ncol = length(edss_values),
#           dimnames = list(names(edss_values), names(edss_values)))
#  }
) %>% add_item(
  rrms_edss_decrease = if(psa_bool) {
    
    mvrnorm(n = 1, 
            mu = tto_edss_decrease_rrms_mu, 
            Sigma = tto_edss_decrease_rrms_sigma) 
    
  } else {tto_edss_decrease_rrms_mu }
  
#  rrms_edss_decrease =  if(psa_bool) {
#    temp <- matrix(nrow = length(edss_values), ncol = length(edss_values),
#                   dimnames = list(names(edss_values), names(edss_values)))
#    for(i_edss in 1:length(edss_values)) {
#      for(j_edss in 1:length(edss_values)) {
      # Rates decrease with higher EDSS
#      temp[i_edss,j_edss] <- exp(rnorm(1,
#                                  mean = log(0.6 - 0.004 * i_edss * j_edss),
#                                  sd = 0.01))
      
#    }}
#    temp
#  } else {
#    matrix(rep(0.6 - 0.04 * edss_values, length(edss_values)),byrow=TRUE ,nrow = length(edss_values), ncol = length(edss_values),
#           dimnames = list(names(edss_values), names(edss_values)))
#  }
  #  rrms_edss_decrease = if(psa_bool) {
  #temp <- matrix(nrow = 1, ncol = length(edss_values),
  #               dimnames = list(NULL, names(edss_values)))
  #for(i_edss in 1:length(edss_values)) {
    # Rates decrease with higher EDSS
  #  temp[i_edss] <- exp(rnorm(1,
  #                            mean = log(0.04 - 0.001 * i_edss),
  #                            sd = 0.01))
  #  
  #}
  #temp
  #} else {
  #  0.04 - 0.001 * edss_values
  #}
  
) %>% add_item(
  # EDSS increase for SPMS patients assumed the same (can modify later)
  # Also assumed independent of treatment so using 'baseline' name
  spms_edss_increase = if(psa_bool) {
    
    mvrnorm(n = 1, 
            mu = tto_edss_increase_spms_mu, 
            Sigma = tto_edss_increase_spms_sigma) 
    
  } else {tto_edss_increase_spms_mu}
  
#  spms_edss_increase  = if(psa_bool) {
#    temp <- matrix(nrow = length(edss_values), ncol = length(edss_values),
#                   dimnames = list(names(edss_values), names(edss_values)))
#    for(i_edss in 1:length(edss_values)) {
#      for(j_edss in 1:length(edss_values)) {
      # Rates increase with higher EDSS
#      temp[i_edss,j_edss] <- exp(rnorm(1,
#                                  mean = log(0.6 + 0.004 * i_edss*j_edss),
#                                  sd = 0.01))
#    }}
#    temp
#  } else {
#    matrix(rep(0.6 + 0.04 * edss_values, length(edss_values)),byrow=TRUE ,nrow = length(edss_values), ncol = length(edss_values),
#           dimnames = list(names(edss_values), names(edss_values)))
    
#  }
  #  spms_edss_increase = if(psa_bool) {
  #temp <- matrix(nrow = 1, ncol = length(edss_values),
  #               dimnames = list(NULL, names(edss_values)))
  #for(i_edss in 1:length(edss_values)) {
    # Rates increase with higher EDSS
  #  temp[i_edss] <- exp(rnorm(1,
  #                            mean = log(0.9 + 0.01 * i_edss),
  #                            sd = 0.01))
  #}
  #temp
  #} else {
  #  0.9 + 0.01 * edss_values
  #}
  
) %>% add_item(
  spms_edss_decrease = if(psa_bool) {
    
    mvrnorm(n = 1, 
            mu = tto_edss_decrease_spms_mu, 
            Sigma = tto_edss_decrease_spms_sigma) 
    
  } else {tto_edss_decrease_spms_mu }
  
  )%>% add_item(
  # Dummy rate of progression to SPMS
  spms_progression =  if(psa_bool) {exp(rnorm(1,
                                              mean = log(0.002),
                                              sd = 0.0001))
  } else {0.002}
) %>% add_item(
  # Baseline annualised relapse rate
  baseline_arr = if(psa_bool) {
    exp(rnorm(1,
              mean = log(0.8),
              sd = 0.03))
  } else {0.3}
) %>% add_item(
  # Hazard ratios on annualised relapse rate
  hr_arr = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # Dummy values but final will come from the NMA
    temp[treatment_codes["Natalizumab"]] <- exp(rnorm(1,
                                       mean = log(0.8),
                                       sd = 0.01))
    temp[treatment_codes["Natalizumab biosimilar"]] <- exp(rnorm(1,
                                                  mean = log(0.8),
                                                  sd = 0.01))
    temp[treatment_codes["Fingolimod"]] <- exp(rnorm(1,
                                      mean = log(0.7),
                                      sd = 0.01))
    temp[treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
                                       mean = log(0.6),
                                       sd = 0.01))
    temp[treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                      mean = log(0.9),
                                      sd = 0.01))
    temp[treatment_codes["Ponesimod"]] <- exp(rnorm(1,
                                                    mean = log(0.45),
                                                    sd = 0.01))
    temp[treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                     mean = log(0.4),
                                                     sd = 0.01))
    temp[treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                      mean = log(0.4),
                                                      sd = 0.01))
    temp[treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
                                                              mean = log(0.35),
                                                              sd = 0.01))
    temp[treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
                                                             mean = log(0.3),
                                                             sd = 0.01))
    temp[treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
                                                             mean = log(0.3),
                                                             sd = 0.01))
    temp[treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
                                                          mean = log(0.25),
                                                          sd = 0.01))
    temp[treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
                                                           mean = log(0.2),
                                                           sd = 0.01))
    temp[treatment_codes["Glatiramer Acetate 20-40mg"]] <- exp(rnorm(1,
                                                                     mean = log(0.15),
                                                                     sd = 0.01))
    temp
  } else {c(0.8, 0.8, 0.7, 0.6, 0.9, 0.45, 0.4, 0.4, 0.35, 0.3, 0.3, 0.25, 0.2, 0.15)}

  
) %>% add_item(
  # All-cause mortality
  # These should come from lifetables
#  mortality = matrix(
#    rep(
#      seq(from = 0.0 +  0.2 / (initial_age + time_horizon), 
#          to = 0.20, 
#          by = 0.2 / (initial_age + time_horizon)),
#      2), 
#    nrow = (initial_age + time_horizon),
#    ncol = 2,
#    dimnames = list(NULL, c("male", "female"))
#  )
#) %>% 
  # Treatment switch follows discontinuation rates
  # Dummy values assume the discontinuation doesn't depend on line of therapy
  # (E.g., cladribine disctoninuation rate is same at 1st and 2nd line)
  # This can be changed by adding line specific discontinuation rates
#  add_item(
    discontinuation_rate = if(psa_bool) {
      temp <- matrix(nrow = 1, ncol = n_treatments)
      for(treatment_code in treatment_codes) {
        temp[treatment_code] <- exp(
          rnorm(1, 
                # Arbitrarily setting the higher treatments (Clad/Alem) to have higher
                # discontinuation than lower treatments (nat/nat bio, fing)
                mean = log(0.05 + 0.005 * treatment_code),
                sd = 0.001)
        )
      }
      temp
    }
    else {0.05 + 0.005 * 1:length(treatment_names)}
  ) %>% add_item(  
    # Relapse costs are treatment & severity agnostic
    c_relapsing = if(psa_bool) {
      temp <- c_relapse[,1]
      for(i in 1:length(temp)) {
        temp[i] <- pmax(rnorm(1, mean =  c_relapse[i,1], sd =  c_relapse[i,2]),0)
      }
      temp
      
    } else {
      c_relapse[,1]
    }
    
  ) %>% add_item(  
    # Relapse dis-utilities are treatment & severity agnostic, in TA616 the same regard less of hospitalisation
    u_relapsing = if(psa_bool) {
      pmin(rnorm(1, mean =  u_relapse[1,1], sd =  u_relapse[1,2]),0)
      
    } else {
      u_relapse[1,1]
    }
    
  ) %>% add_item(
    # costs of adverse events
    c_adverse_events = if(psa_bool) {
      temp <- matrix(nrow = nrow(c_ae), ncol = 1,
                     dimnames = list(rownames(c_ae),NULL ))
      
      temp["Infusion site reaction",1] <- pmax(rnorm(1, mean =  c_ae["Infusion site reaction",1], 
                                                     sd =  c_ae["Infusion site reaction",1]/5),0)
      
      temp["Injection site reaction",1] <- pmax(rnorm(1, mean =  c_ae["Injection site reaction",1], 
                                                      sd =  c_ae["Injection site reaction",1]/5),0)
      
      temp["PML",1] <- pmax(rnorm(1, mean =  c_ae["PML",1], 
                                  sd =  c_ae["PML",1]/5),0)
      
      temp["Severe infection",1] <- pmax(rnorm(1, mean =  c_ae["Severe infection",1], 
                                               sd =  c_ae["Severe infection",1]/5),0)
      
      temp["Macular oedema",1] <- pmax(rnorm(1, mean =  c_ae["Macular oedema",1], 
                                             sd =  c_ae["Macular oedema",1]/5),0)
      
      temp["Gastrointestinal",1] <- pmax(rnorm(1, mean =  c_ae["Gastrointestinal",1], 
                                               sd =  c_ae["Gastrointestinal",1]/5),0)
      
      temp["Hypersensitivity",1] <- pmax(rnorm(1, mean =  c_ae["Hypersensitivity",1], 
                                               sd =  c_ae["Hypersensitivity",1]/5),0)
      
      temp["Autoimmune thyroid-related event",1] <- pmax(rnorm(1, mean =  c_ae["Autoimmune thyroid-related event",1], 
                                                               sd =  c_ae["Autoimmune thyroid-related event",1]/5),0)
      
      temp["Influenza-like symptoms",1] <- pmax(rnorm(1, mean =  c_ae["Influenza-like symptoms",1], 
                                                      sd =  c_ae["Influenza-like symptoms",1]/5),0)
      
      temp["Malignancy",1] <- pmax(rnorm(1, mean =  c_ae["Malignancy",1], 
                                         sd =  c_ae["Malignancy",1]/5),0)
      
      temp["Immune thrombocytopenic purpura",1] <- pmax(rnorm(1, mean =  c_ae["Immune thrombocytopenic purpura",1], 
                                                              sd =  c_ae["Immune thrombocytopenic purpura",1]/5),0)
      temp
    } else {c_ae[,1]}
    
  )  %>% add_item(
    # dis-utility of adverse events
    u_adverse_events = if(psa_bool) {
      temp <- matrix(nrow = nrow(u_ae), ncol = 1,
                     dimnames = list(rownames(c_ae),NULL ))
      
      temp["Infusion site reaction",1] <- pmax(rnorm(1, mean =  u_ae["Infusion site reaction",1], 
                                                     sd =  u_ae["Infusion site reaction",1]/5),0)
      
      temp["Injection site reaction",1] <- pmax(rnorm(1, mean =  u_ae["Injection site reaction",1], 
                                                      sd =  u_ae["Injection site reaction",1]/5),0)
      
      temp["PML",1] <- pmax(rnorm(1, mean =  u_ae["PML",1], 
                                  sd =  u_ae["PML",1]/5),0)
      
      temp["Severe infection",1] <- pmax(rnorm(1, mean =  u_ae["Severe infection",1], 
                                               sd =  u_ae["Severe infection",1]/5),0)
      
      temp["Macular oedema",1] <- pmax(rnorm(1, mean =  u_ae["Macular oedema",1], 
                                             sd =  u_ae["Macular oedema",1]/5),0)
      
      temp["Gastrointestinal",1] <- pmax(rnorm(1, mean =  u_ae["Gastrointestinal",1], 
                                               sd =  u_ae["Gastrointestinal",1]/5),0)
      
      temp["Hypersensitivity",1] <- pmax(rnorm(1, mean =  u_ae["Hypersensitivity",1], 
                                               sd =  u_ae["Hypersensitivity",1]/5),0)
      
      temp["Autoimmune thyroid-related event",1] <- pmax(rnorm(1, mean =  u_ae["Autoimmune thyroid-related event",1], 
                                                               sd =  u_ae["Autoimmune thyroid-related event",1]/5),0)
      
      temp["Influenza-like symptoms",1] <- pmax(rnorm(1, mean =  u_ae["Influenza-like symptoms",1], 
                                                      sd =  u_ae["Influenza-like symptoms",1]/5),0)
      
      temp["Malignancy",1] <- pmax(rnorm(1, mean =  u_ae["Malignancy",1], 
                                         sd =  u_ae["Malignancy",1]/5),0)
      
      temp["Immune thrombocytopenic purpura",1] <- pmax(rnorm(1, mean =  u_ae["Immune thrombocytopenic purpura",1], 
                                                              sd =  u_ae["Immune thrombocytopenic purpura",1]/5),0)
      temp
    } else {u_ae[,1]}
  )  %>% add_item(
    # rates of adverse events
    adverse_events_rates = if(psa_bool) {
      temp <- r_ae
      for(i in 1:nrow(temp)) {
        for (j in 1:ncol(temp)){
          # Rates are specified by AEs in rows [i] and treatments columns[j]
          temp[i,j] <- exp(rnorm(1,
                                 mean = log(temp[i,j]),
                                 sd = temp[i,j]/5))}
      }
      temp
    } else {r_ae}
    
  )  



#Put objects here that do not change as we loop through treatments for a patient
# This is called for each new patient so sample 1 from a distribution of choice
# for heterogeneous characteristics
common_pt_inputs <- add_item(sex = sample(size = 1, x = c(1, 0), # Random 
                                          prob = c(1 - prop_female, prop_female)) #1 is male 0 is female                            
)

# Objects that depend on treatment
# Note that the current treatment is later initialised to trt
unique_pt_inputs <- add_item(edss_value = sample(size = 1, x = edss_values, # Random 
                                                 prob = initial_edss_distribution),
                             edss.i = edss_value,
                             n_relapse = 0,
                             spms_flag = sample(size = 1, x = c(0, 1), # Random
                                                prob = c(1 - initial_spms_prop, initial_spms_prop)),
                             relapse_hospital_flag = sample(size = 1, x = c(0, 1), # Random
                                                            prob = c(1 - relapse_hospital_prop, relapse_hospital_prop)),
                             current_treatment = "Natalizumab",
                             current_treatment_code = 1,
                             treatment_line = 2,
                             n_switch = 0,
                             age = initial_age
)



# Add time to event for each patient for each of the two treatments
# Setting trt to all treatments should mean that the event applies to all treatments

init_event_list <- 
  add_tte(trt = treatment_names,
          evts = event_names,
          input={ 
            initialise <- 0
            
            relapse <- draw_tte(
              1, 
              dist = "exp", 
              # Baseline rate modified by HR of current treatment
              # Current treatment allows treatment switching (i.e., not the trt)
              coef1 = log(baseline_arr),
              hr = hr_arr[current_treatment_code]
              )
            edss_increase <- draw_tte(
              1, 
              dist = "exp",
              coef1 = (1 - spms_flag) * (baseline_rrms_edss_increase["(Intercept)"] +
                 baseline_rrms_edss_increase["AgeDuringEvent"] + # *age covariate mean centred
                 baseline_rrms_edss_increase["GenderMALE"] +  #*sex covariate
                 baseline_rrms_edss_increase["BaselineWebEDSS"] ) + #*edss_value covariate
                       spms_flag * (spms_edss_increase["(Intercept)"] +
                       spms_edss_increase["AgeDuringEvent"]  + # *age covariate mean centred
                       spms_edss_increase["GenderMALE"] +  #*sex covariate
                       spms_edss_increase["BaselineWebEDSS"] ), #*edss_value covariate    
           #   coef1 = (1 - spms_flag) * log(baseline_rrms_edss_increase[edss.i == edss_values, edss_value == edss_values]) +
           #     spms_flag * log(spms_edss_increase[edss.i == edss_values, edss_value == edss_values]),
             # remove effect of hr for SPMS patients by taking log(1)=0 in the expression: rexp(n_chosen, rate = exp(coef1 + log(hr)))
              hr = ((1 - spms_flag) * hr_edss_increase[current_treatment_code])+ spms_flag 
            )
            #edss_increase <- draw_tte(
            #1, 
            #dist = "exp",
            #coef1 = (1 - spms_flag) * log(baseline_rrms_edss_increase[edss_value == edss_values]) +
            #  spms_flag * log(spms_edss_increase[edss_value == edss_values]),
            #hr = hr_edss_increase[current_treatment_code] 
            #)
            edss_decrease <- draw_tte(
              1, 
              dist = "exp",
              # Baseline rate related to current EDSS
              # Below is zero if spms_flag is 1 since patients should have EDSS decrase if SPMS
              coef1 = (1 - spms_flag) * (rrms_edss_decrease["(Intercept)"]
               + rrms_edss_decrease["AgeDuringEvent"]  # *age covariate mean centred
               + rrms_edss_decrease["GenderMALE"]   #*sex covariate
               + rrms_edss_decrease["BaselineWebEDSS"] ) #*edss_value covariate
                             + spms_flag * (spms_edss_decrease["(Intercept)"]
                             + spms_edss_decrease["AgeDuringEvent"]  # *age covariate mean centred
                             + spms_edss_decrease["GenderMALE"]   #*sex covariate
                             + spms_edss_decrease["BaselineWebEDSS"] ) #*edss_value covariate 
            )
             # coef1 = (1 - spms_flag) * log(rrms_edss_decrease[edss.i == edss_values, edss_value == edss_values]))
            #edss_decrease <- draw_tte(
            #  1, 
            #  dist = "exp",
              # Baseline rate related to current EDSS
              # Below is zero if spms_flag is 1 since patients should have EDSS decrase if SPMS
            #  coef1 = (1 - spms_flag) * log(rrms_edss_decrease[edss_value == edss_values]))
            progression <- draw_tte(
              1,
              dist = "exp",
              # Below is zero if already progressed to SPMS
              coef1 = (1 - spms_flag) * log(spms_progression)
            )
            source("R/draw_tte_new.R")
            death <- draw_tte_new(
              1,
              dist = "pwexp",
              coef1 = c(log((1 - prop_female) * r_mortality[(initial_age+1):nrow(r_mortality), c("male")] +
                              prop_female *  r_mortality[(initial_age+1):nrow(r_mortality), c("female")],
              )),
              time = c(r_mortality[(initial_age+1):nrow(r_mortality)]) ,
              hr=smr[1]
            )
            treatment_switch <- draw_tte(
              1,
              dist = "exp",
              # Discontinuation rates can vary by current treatment
              coef1 = log(discontinuation_rate[current_treatment_code])
            )
          })


# Reaction to progression in SPMS - do we need to increase the EDSS as well?
evt_react_list <-
  add_reactevt(name_evt = "initialise",
               input = {
                 # This ensures that the current treatment is the initial treatment
                 # of this strategy.
                 modify_item(list(current_treatment = trt,
                                  current_treatment_code = treatment_codes[trt],
                                  #future death events do not exceed 100 years
                                  "death"= min(cur_evtlist[["death"]], time_horizon + initial_age)
                             ))
               }) %>%
  add_reactevt(name_evt = "age",
               input = {
                 modify_item(list(age = initial_age + round(curtime)
                                  ))
               }) %>%
  add_reactevt(name_evt = "relapse",
               input = {
                 modify_item(list(n_relapse= n_relapse + 1,
                                  age = initial_age + round(curtime)))
                 # Currenly no impact on costs or utilities of relapse
                 # Just need to simulate the next time to relapse
                 new_event(list(
                   "relapse" = curtime + draw_tte(
                     1, 
                     dist = "exp", 
                     coef1 = log(baseline_arr),
                       hr = hr_arr[current_treatment_code])
                 ))
               }) %>%
  add_reactevt(name_evt = "edss_increase",
               input = {
                 modify_item(list(edss.i = edss_value,
                                  edss_value = min(edss_value + 1, max(edss_values)),
                                  age = initial_age + round(curtime)))
                 
                 # Need to simulate the next time to EDSS increase
                 new_event(list(
                   "edss_increase" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     coef1 = (1 - spms_flag) * (baseline_rrms_edss_increase["(Intercept)"]
                           + baseline_rrms_edss_increase["AgeDuringEvent"]  # *age covariate mean centred
                           + baseline_rrms_edss_increase["GenderMALE"]   #*sex covariate
                           + baseline_rrms_edss_increase["BaselineWebEDSS"] ) #*edss_value covariate
                                    +  spms_flag * (spms_edss_increase["(Intercept)"]
                                     + spms_edss_increase["AgeDuringEvent"]  # *age covariate mean centred
                                     + spms_edss_increase["GenderMALE"]   #*sex covariate
                                     + spms_edss_increase["BaselineWebEDSS"] ), #*edss_value covariate  
                   #  coef1 = (1 - spms_flag) * 
                   #   log(baseline_rrms_edss_increase[edss.i == edss_values, edss_value == edss_values]) +
                   #    spms_flag * log(spms_edss_increase[edss.i == edss_values, edss_value == edss_values]),
                     # remove effect of hr for SPMS patients by taking log(1)=0 in the expression: rexp(n_chosen, rate = exp(coef1 + log(hr)))
                     hr = ((1 - spms_flag) * hr_edss_increase[current_treatment_code])+ spms_flag 
                   )
                 ))
#               })   add_reactevt(name_evt = "edss_increase",
#                                 input = {
#                                   modify_item(list(edss_value = min(edss_value + 1, max(edss_values)),
#                                                    age = initial_age + round(curtime)))
                                   
                                   # Need to simulate the next time to EDSS increase
#                                   new_event(list(
#                                     "edss_increase" = curtime + draw_tte(
#                                       1, 
#                                       dist = "exp",
#                                       coef1 = (1 - spms_flag) * 
#                                         log(baseline_rrms_edss_increase[edss_value == edss_values]) +
#                                         spms_flag * log(spms_edss_increase[edss_value == edss_values]),
#                                       hr = hr_edss_increase[current_treatment_code]
#                                     )
#                                   ))
                                 })%>%
  add_reactevt(name_evt = "edss_decrease",
               input = {
                 modify_item(list(edss.i = edss_value,
                                  edss_value = max(edss_value - 1, min(edss_values)),
                                  age = initial_age + round(curtime)))
                 
                 # Need to simulate the next time to EDSS decrease
                 new_event(list(
                   "edss_decrease" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     # Below is zero if spms_flag is 1 since patients should have EDSS decrase if SPMS
                     coef1 = (1 - spms_flag) * (rrms_edss_decrease["(Intercept)"]
                      + rrms_edss_decrease["AgeDuringEvent"]  # *age covariate mean centred
                      + rrms_edss_decrease["GenderMALE"]   #*sex covariate
                      + rrms_edss_decrease["BaselineWebEDSS"] ) #*edss_value covariate
                                    + spms_flag * (spms_edss_decrease["(Intercept)"]
                                    + spms_edss_decrease["AgeDuringEvent"]  # *age covariate mean centred
                                    + spms_edss_decrease["GenderMALE"]   #*sex covariate
                                    + spms_edss_decrease["BaselineWebEDSS"] ) #*edss_value covariate 
                   )
                     #coef1 = (1 - spms_flag) * log(rrms_edss_decrease[edss.i == edss_values, edss_value == edss_values]))
                 ))
#                 add_reactevt(name_evt = "edss_decrease",
#                              input = {
#                                modify_item(list(edss_value = max(edss_value - 1, min(edss_values)),
#                                                 age = initial_age + round(curtime)))
                                
                                # Need to simulate the next time to EDSS decrease
#                                new_event(list(
#                                  "edss_decrease" = curtime + draw_tte(
#                                    1, 
#                                    dist = "exp",
                                    # Below is zero if spms_flag is 1 since patients should have EDSS decrase if SPMS
#                                    coef1 = (1 - spms_flag) * log(rrms_edss_decrease[edss_value == edss_values]))
#                                ))
               }) %>%
  add_reactevt(name_evt = "progression",
               # Progression can only occur once so just change the SPMS status
               input = {
                 modify_item(list(spms_flag = 1,
                                  age = initial_age + round(curtime)))
               }) %>%
  add_reactevt(name_evt = "death",
               # Death only occurs once.
               # Set current time to Inf so simulation ends.
               input = {
                 modify_item(list(curtime = Inf #,
                                  #age = initial_age + round(curtime)
                                  )) 
               }) %>%
  # Currently assumes patients switch only twice so 4th line is final (As in protocol)
  add_reactevt(name_evt = "treatment_switch",
               input = {
                 modify_item(list(current_treatment = sample(x = higher_line_treatments,
                                                             size = 1),
                                  current_treatment_code = sample(x = higher_line_treatment_codes,
                                                                  size = 1),
                                  treatment_line = treatment_line + 1,
                                  discontinuation_rate = (treatment_line < 4) *  discontinuation_rate,
                                  n_switch = n_switch +1,
                                  age = initial_age + round(curtime)))
                 
                 # Sample next discontinuation event. This will be zero if treatment_line >=4
                 new_event(list(
                   "treatment_switch" = curtime + draw_tte(
                     1,
                     dist = "exp",
                     coef1 = log(discontinuation_rate[current_treatment_code])
                   )
                 ))
               })

# Add ongoing utilities
# Can also add util_instant to include disutilities of events (e.g., of relapse or adverse events)
util_ongoing <- add_util(evt = event_names,
                         trt = treatment_names,
                         util = (1 - spms_flag) * u_rrms_edss[edss_value == edss_values] +
                           spms_flag * u_rrms_edss[edss_value == edss_values]
)

# Add ongoing costs
# Can also add instaneous costs with cost_instant (For example cost of managing relapse)
cost_ongoing <- add_cost(evt = event_names,
                         trt = treatment_names,
                         cost = (1 - spms_flag) * (
                           c_rrms_edss[edss_value == edss_values] #+
                             #treatment_costs[current_treatment = trt] treatment costs should be instantaneous not ongoing
                         ) +
                           spms_flag * c_rrms_edss[edss_value == edss_values]
)
#Add instant costs
# Treatment Costs, need to figure out how to manipulate the costs according to year 1 or 2+ of treatment
cost_instant <-
  add_cost(evt = c("initial", "treatment_switch") ,
           trt = treatment_names,
           cost = c_tx[current_treatment_code] +
             c_tx_admin[current_treatment_code] +
             c_tx_monitor[current_treatment_code]) %>% 
  add_cost(evt = "relapse" ,
               trt = treatment_names,
               cost = c_relapsing[4]) 

# Add instant utilities
# Can also add util_instant to include disutilities of events (e.g., of relapse or adverse events)
util_instant <-   add_util(evt = "relapse" ,
                           trt = treatment_names,
                           util = u_relapsing) 


# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <- system.time({
  results <- RunSim(
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = TRUE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    trt_list = treatment_names,             # intervention list
    common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
    common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
    unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
    init_event_list = init_event_list,        # initial event list
    evt_react_list = evt_react_list,          # reaction of events
    util_ongoing_list = util_ongoing,
    cost_ongoing_list = cost_ongoing,
    util_instant_list = util_instant,
    cost_instant_list = cost_instant,
    ncores = 6,                               # number of cores to use, recommended not to use all
    drc = 0.035,                              # discount rate for costs
    drq = 0.035,                               # discount rate for qaly/lys
    input_out=c("age", "sex", "spms_flag", "edss.i", "edss_value", "n_relapse", "current_treatment",
                "treatment_line", "n_switch")           
  )
})

saveRDS(results, file ="descem5_1000x100.RDS")
# Simulated patients for first simulation
psa_ipd <- bind_rows(map(results$output_psa, "merged_df"))
# compare patient characteristics of dead patients
dead <- psa_ipd[psa_ipd$evtname == "death"]

# Plot the times to event
# Include a vertical line for the median event time.
data_plot <- results$final_output$merged_df %>%
  filter(evtname != "initialise") %>%
  group_by(trt, evtname, simulation) %>%
  mutate(median = median(evttime)) %>%
  ungroup()

ggplot(data_plot) +
  geom_density(aes(fill = trt, x = evttime),
               alpha = 0.7) +
  geom_vline(aes(xintercept = median, col = trt)) +
  facet_wrap( ~ evtname, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()


# Summarise the results

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
ceplane.plot(des_bcea, wtp = 20000)

#plot a CEAC
des_multi_ce <- multi.ce(des_bcea)
ceac.plot(des_multi_ce, graph = "ggplot",
          line = list(color = c("red", "green", "blue", "orange", "yellow")),
          pos = c(0, 0.50))


