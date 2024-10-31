# RRMS DES using the DESCEM package
# Howard Thom & Ayman Sadek 30 Oct 2024

# V2 changed samples from n_samples to 1 for random parameters
# This doesn't work properly as each simulation looks very similar
# fixed this issue now, you can see the differences between trt groups.

# V3 puts the functions for each parameter set into add_item() expressions
# v4 fixed matrix accessing for parameters (As they only have one row)

# V5 fixed aging and death and modified matrix accessing for parameters to index on rows and columns
# V5.1 added in the full number of treatments and updated data 
    # Added registry data working correctly (updated to latest set of models as of 24th October)
    # Registry exponential models exclude age and sex co-variates due to sparse data 
    # limited relapses to 0 < edss < 9
    # CDP6 and ARR NMA
    # Uncertainty
      # Utilities drawn from a normal distibution
      # costs drawn from a Gamma distribution


# TODO Figure out neater way to add each parameter
# Modularise, optimise, and add RShiny interface
# NMA results to be exported to a dataframe that can be read read into the model
# CDP6 nma results missing some treatments
# SAE  nma results pending
# Discontinuation nma pending
# specify Annual treatment costs that vary by year 1 or 2+ and stop after 3

rm(list = ls())

library(descem)
library(MASS)
library(VGAM)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)
library(BCEA)
options(scipen = 999)
options(tibble.print_max = 50)

## New functions to draw from piece wise exponential distribution
source("R/draw_tte_new.R")

## set a seed 
set.seed(243523452)

## number of patients and samples
n_patients <- 100
n_samples <- 1

## Treatments at second line 
v_treatment_names <- c("Natalizumab-IV",
                     "Natalizumab-SC",
                     "Natalizumab biosimilar-IV",
                     "Natalizumab biosimilar-SC",
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
                     "Glatiramer Acetate 20mg",
                     "Glatiramer Acetate 40mg")

## number of treatments
n_treatments <- length(v_treatment_names)

## specify codes for the treatments
treatment_codes <- c(seq(1, n_treatments))
names(treatment_codes) <- v_treatment_names

## Treatments that patients can move to at 3rd and higher line 
v_higher_line_treatments <- c("Natalizumab-IV",
                            "Natalizumab-SC",
                            "Natalizumab biosimilar-IV",
                            "Natalizumab biosimilar-SC",
                            "Alemtuzumab",
                            "Cladribine")
v_higher_line_treatment_codes <- c(1, 2, 3, 4, 6, 7)
names(v_higher_line_treatment_codes) <- v_higher_line_treatments

## Model event states
v_event_names <- c("initialise", "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "serious_ae", "death")

## Baseline population age sex and model time horizon
initial_age <- 20 
time_horizon <- 80 # Years so lifetime
prop_female <- 0.50

## Define the possible EDSS values
v_edss_values <- c(0:9)
names(v_edss_values) <- paste("EDSS", v_edss_values)

## RRMS EDSS Natural History Palace etal
#rrms_edss_natural_history <- read.csv("data/df_r_nh_rrms_bc_ta616.csv", header = T, row.names = 1)
#colnames(rrms_edss_natural_history) <- row.names(rrms_edss_natural_history) <-paste("EDSS",v_edss_values)

## Natural History MS Registry
v_tto_edss_increase_harrms_mu <- t(read.csv("data/v_tto_edss_increase_harrms_mu.csv", header = T, row.names = 1))
m_tto_edss_increase_harrms_sigma <- read.csv( file="data/m_tto_edss_increase_harrms_sigma.csv", header = T, row.names = 1)
v_tto_edss_increase_rrms_mu <- t(read.csv("data/v_tto_edss_increase_rrms_mu.csv", header = T, row.names = 1))
m_tto_edss_increase_rrms_sigma <- read.csv( file="data/m_tto_edss_increase_rrms_sigma.csv", header = T, row.names = 1)
v_tto_edss_increase_spms_mu <- t(read.csv("data/v_tto_edss_increase_spms_mu.csv", header = T, row.names = 1))
m_tto_edss_increase_spms_sigma <- read.csv("data/m_tto_edss_increase_spms_sigma.csv", header = T, row.names = 1)

v_tto_edss_decrease_rrms_mu <- t(read.csv("data/v_tto_edss_decrease_rrms_mu.csv", header = T, row.names = 1))
m_tto_edss_decrease_rrms_sigma <- read.csv("data/m_tto_edss_decrease_rrms_sigma.csv", header = T, row.names = 1)
v_tto_edss_decrease_spms_mu <- t(read.csv("data/v_tto_edss_decrease_spms_mu.csv", header = T, row.names = 1))
m_tto_edss_decrease_spms_sigma <- read.csv("data/m_tto_edss_decrease_spms_sigma.csv", header = T, row.names = 1)

v_tto_relapse_harrms_mu <- read.csv("data/v_tto_relapse_harrms_mu.csv", header = T, row.names = 1)
tto_relapse_harrms_sigma<- read.csv("data/m_tto_relapse_harrms_sigma.csv", header = T, row.names = 1)
v_tto_relapse_spms_mu <- read.csv("data/v_tto_relapse_spms_mu.csv", header = T, row.names = 1)
m_tto_relapse_spms_sigma<- read.csv("data/m_tto_relapse_spms_sigma.csv", header = T, row.names = 1)

v_tto_sp_conversion_harrms_mu <- read.csv("data/v_tto_sp_conversion_harrms_mu.csv", header = T, row.names = 1)
m_tto_sp_conversion_harrms_sigma<- read.csv("data/m_tto_sp_conversion_harrms_sigma.csv", header = T, row.names = 1)


## baseline distribution of EDSS health states (AFFIRM baseline both arms)
v_affirm_edss_distribution <- c(49, 273, 311, 193, 88, 24, 1, 1, 1, 1)#


## baseline proportion of patients with SPMS
initial_spms_prop <- 0

## baseline population risk parameters: relapse rates and Standardized Mortality Ratio
df_init_params <- read.csv("data/df_init_params.csv", header = T, row.names = 1)

## base line relapse rate(final analysis) Placebo arm(n=315) AFFIRM Table 2  Polman etal 2006 DOI: 10.1056/NEJMoa044397
# r_bl_rrms_rr <- as.double(c(0.73, (0.87-0.62)/3.92))


## Proportion of patients having relapse leading to hospitalisation ~DUMMY DATA~
relapse_hospital_prop <- 0.2

## rates of Adverse events (Cladribine Alemtuzumab Fingolimod Natalizumab Daclizuma] from TA616, rest is ~DUMMY DATA~)
#prob_ae <- read.csv("data/df_adverse_events_probabilities_ta616.csv", header = T, row.names = 1)

## Incidence & weights of Serious Adverse events in AFFIRM
saes_data <-read.csv(file="data/df_saes_data.csv", header = T, row.names =1)
sae_names <- saes_data[,1]
v_weight_sae <- saes_data[,2] # These weights are used to calculate SAEs costs and utilities as sumproduct
prob_sae <- saes_data[,2]
names(prob_sae)<-sae_names
v_r_sae <- -log(1-prob_sae)
v_r_sae

# #rates of mortality, "mx" columns of life tables
m_r_mortality <- as.matrix(read.csv("data/df_all_cause_mortality.csv", header = T, row.names = 1))
m_r_mortality <- m_r_mortality[,c(1, 2,8)]
m_r_mortality[,1] <- as.numeric(m_r_mortality[,1])
dimnames(m_r_mortality) <-  list(NULL, c("time" ,"male", "female"))


## edss costs direct medical costs Tyas etal. 2005 inflated to 2022/2023 Edss6 is an average of edss6 and 6.5
c_m_edss <- read.csv("data/df_c_directmedical_edss.csv", header = T, row.names = 1) 
df_m_edss_med_costs <- t(c_m_edss[1:10,7:8])
#add 398 with se of 1002 for an spms cost
#do we include non-direct medical costs? they were previously controversial

## MS specific standardised mortality rate Manouchehrinia 2016
v_smr <- as.double(c(df_init_params["smr"], sqrt(437)*(df_init_params["smr_uci"]-df_init_params["smr_lci"])/3.92))

##  Annual treatment, administration and monitoring costs TA616 TA312 Beta-IFN-MTA
df_c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
df_c_tx <- df_c_tx[-1:-2,]
df_c_tx_admin <- t(read.csv("data/df_c_drugs_admin.csv", header = T, row.names = 1))
df_c_tx_admin <- df_c_tx_admin[-1:-2,]
df_c_tx_admin[df_c_tx_admin==0] <-0.1 # set zero costs to positive number stops generating NaNs
df_c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
df_c_tx_monitor <- df_c_tx_monitor[-1:-2,]

## Relapse Costs (Hawton 2016b and Karampampa 2012c)
df_c_relapse <-read.csv("data/df_c_relapse.csv", header = T, row.names = 1)
df_c_relapse <- df_c_relapse[3:8,3:4] #choose inflated Hawton 2016 to 2023
df_c_relapse <- df_c_relapse*2 # Annualised these 6 months costs

## Adverse Events costs TA616 # need to be weighted by key trial
#df_c_ae <-read.csv("data/df_c_disutilities_ta616.csv", header = T, row.names =1)

## Adverse events costs MS-MTA report
df_c_ae <- saes_data[,4]
names(df_c_ae)<-sae_names
  
## edss utilities columns 5&6 Orme etal 2007
u_edss <- read.csv("data/df_u_edss_ta616.csv", header = T, row.names = 1)
df_edss_utilities <- t(u_edss[1:10,5:6])

## Relapse utilities
u_relapse <-read.csv("data/df_u_relapse.csv", header = T, row.names = 1)

## Adverse Events disutilities TA616 # need to be weighted by key trial
#df_u_ae <-read.csv("data/df_c_disutilities_ta616.csv", header = T, row.names =1)

## Adverse events dis-utilities MS-MTA report
df_u_ae <- saes_data[,5]
names(df_u_ae)<-sae_names

## Standard Deviation for probabilistic analysis when uncertainty not reported
# assumed 1/10 of mean
sd_psa <- 10


# Global parameters of the model as they are common to all patients and treatments
# If not using PSA just setting to mean of samples. This isn't correct as the 
# true mean should be used
common_all_inputs <-add_item(
  v_treatment_names = v_treatment_names,
  treatment_codes = treatment_codes,
  v_higher_line_treatments = v_higher_line_treatments,
  v_higher_line_treatment_codes = v_higher_line_treatment_codes,
  initial_age = initial_age,
  #rrms_average_age = rrms_average_age,
  time_horizon = time_horizon,
  prop_female = prop_female,
  v_edss_values = v_edss_values,
  df_c_tx = df_c_tx, # drug costs are not probabilistic
  v_event_names = v_event_names,
  #r_bl_rrms_rr = r_bl_rrms_rr, # make random
  v_affirm_edss_distribution=v_affirm_edss_distribution,
  m_r_mortality = m_r_mortality, # make random
  v_smr = v_smr, # make random
  v_weight_sae=v_weight_sae,
  initial_spms_prop = initial_spms_prop,
  relapse_hospital_prop = relapse_hospital_prop # make random
  
) %>% add_item(
  # Utilities for RRMS Orme etal
  u_rrms_edss = if(psa_bool) {
    temp <- df_edss_utilities[1,]
    for(j_edss in 1:length(temp)) {
      temp[j_edss] <- rnorm(1, mean =  df_edss_utilities[1,j_edss], sd =  df_edss_utilities[2,j_edss])
    }
    temp
    
  } else {
    df_edss_utilities[1,]
  }

) %>% add_item(
  # EDSS Health state Medical costs for RRMS Tyas et al inflated 
  v_c_rrms_edss = if(psa_bool) {
    temp <- df_m_edss_med_costs[1,]
    for(j_edss in 1:length(temp)) {
      temp[j_edss] <- rgamma(1, ((df_m_edss_med_costs[1,j_edss])^2)/(df_m_edss_med_costs[2,j_edss]^2),
                             df_m_edss_med_costs[1,j_edss]/(df_m_edss_med_costs[2,j_edss]^2))
      #temp[j_edss] <- pmax(rnorm(1, mean =  df_m_edss_med_costs[1,j_edss], sd =  df_m_edss_med_costs[2,j_edss]),0)
    }
    temp
    
  } else {
    df_m_edss_med_costs[1,]
  }

) %>% add_item(
  # Hazard ratios reducing risk of EDSS increase
  v_hr_edss_increase = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # CDP6 relative treatment effects log Hr from the NMA with Natalizumab as the reference
    # hard coded but should be imported directly from NMA outputted csv or rds files
    temp[treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
    temp[treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
    temp[treatment_codes["Natalizumab biosimilar-IV"]] <- 1 #missing from NMA
    temp[treatment_codes["Natalizumab biosimilar-SC"]] <- 1 #missing from NMA
    temp[treatment_codes["Fingolimod"]] <- exp(rnorm(1,
                                      mean = 0.89,
                                      sd = 0.22))
    temp[treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
                                       mean = -0.33,
                                       sd = 0.46)) 
    temp[treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                      mean = 0.76,
                                      sd = 0.20))
    temp[treatment_codes["Ponesimod"]] <- exp(rnorm(1,
                                      mean = 0.89,
                                      sd = 0.22)) # to be informed by trt class NMA (for now assumed the same as Fingolimod)
    temp[treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                           mean = -0.15, 
                                           sd = 0.43)) # o be informed by trt class NMA (for now assumed the same as Ocrelizumab)
    temp[treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                               mean = -0.15, 
                                                  sd = 0.43))
    temp[treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
                                           mean = 0.00, 
                                           sd = 0.33)) 
    temp[treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
                                           mean = 0.36, 
                                           sd = 0.39))
    temp[treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
                                                mean = 0.36, 
                                                sd = 0.39)) # assumed same as 44μg
    temp[treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
                                           mean = 0.32,  
                                           sd = 0.34))
    temp[treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
                                           mean = -0.49, 
                                           sd = 0.47))
    temp[treatment_codes["Glatiramer Acetate 20mg"]] <- exp(rnorm(1,
                                           mean = 0.30,
                                           sd = 0.37))
    temp[treatment_codes["Glatiramer Acetate 40mg"]] <- exp(rnorm(1,
                                            mean = 0.30,
                                            sd = 0.37)) # assumed same as 20 mg
    temp
  } else {c(1, 1, 1, 1, exp(0.89), exp(-0.33), exp(0.76), 1, 1, 
            exp(-0.15), 1, exp(0.36), exp(0.36), exp(0.32), exp(-0.49), 0.30, 0.30)}
  
) %>% add_item(
  # MS Registry time to EDSS increase HA RRMS patients
  v_baseline_harrms_edss_increase = if(psa_bool) {
    
    mvrnorm(n = 1, 
            mu = v_tto_edss_increase_harrms_mu, 
            Sigma = m_tto_edss_increase_harrms_sigma) 

  } else {v_tto_edss_increase_harrms_mu }
  
) %>% add_item(
  # MS Registry time to EDSS decrease All RRMS patients
  v_rrms_edss_decrease = if(psa_bool) {
    
    mvrnorm(n = 1, 
            mu = v_tto_edss_decrease_rrms_mu, 
            Sigma = m_tto_edss_decrease_rrms_sigma) 
    
  } else {v_tto_edss_decrease_rrms_mu }
  
) %>% add_item(
  # MS Registry time to EDSS increase SPMS patients
  v_spms_edss_increase = if(psa_bool) {
    
    mvrnorm(n = 1, 
            mu = v_tto_edss_increase_spms_mu, 
            Sigma = m_tto_edss_increase_spms_sigma) 
    
  } else {v_tto_edss_increase_spms_mu}
  
  
) %>% add_item(
  # MS Registry time to EDSS decrease SPMS patients
  v_spms_edss_decrease = if(psa_bool) {
    
   mvrnorm(n = 1, 
           
           mu = v_tto_edss_decrease_spms_mu, 
           Sigma = m_tto_edss_decrease_spms_sigma
           ) 
    
  } else {v_tto_edss_decrease_spms_mu 
          }
  
  )%>% add_item(
    # MS Registry time to progression RRMS to SPMS 
    v_spms_progression = if(psa_bool) {
                                  mvrnorm(n = 1, 
                                  mu = t(v_tto_sp_conversion_harrms_mu), 
                                  Sigma = m_tto_sp_conversion_harrms_sigma) 
  
  } else {v_tto_sp_conversion_harrms_mu}
 
) %>% add_item(
  # MS Registry time to relapse HARRMS patients
  v_baseline_harrms_arr = if(psa_bool) {
                                mvrnorm(n = 1, 
                                mu = t(v_tto_relapse_harrms_mu), 
                                Sigma = tto_relapse_harrms_sigma) 
    
  } else {v_tto_relapse_harrms_mu}
  
) %>% add_item(
  # MS Registry time to relapse SPMS patients
  v_baseline_spms_arr = if(psa_bool) {
    
    mvrnorm(n = 1, 
            mu = t(v_tto_relapse_spms_mu), 
            Sigma = m_tto_relapse_spms_sigma
    ) 
    
  } else {v_tto_relapse_spms_mu
  }
  
) %>% add_item(
  # Annualized relapse rate relative trt effects NMA with Natalizumab as the reference
  v_hr_arr = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # hard coded but should be imported directly from NMA outputted csv or rds files
    temp[treatment_codes["Natalizumab-IV"]] <- 1
    temp[treatment_codes["Natalizumab-SC"]] <- 1
    temp[treatment_codes["Natalizumab biosimilar-IV"]] <- exp(rnorm(1,
                                                             mean = 0.44,
                                                             sd = 0.34))
    temp[treatment_codes["Natalizumab biosimilar-SC"]] <- exp(rnorm(1,
                                                              mean = 0.44,
                                                              sd = 0.34))
    temp[treatment_codes["Fingolimod"]] <- exp(rnorm(1,
                                                     mean = 0.39,
                                                     sd = 0.14))
    temp[treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
                                                  mean = -0.15,
                                                  sd = 0.2)) 
    temp[treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                                     mean = 0.33,
                                                     sd = 0.17))
    temp[treatment_codes["Ponesimod"]] <- exp(rnorm(1,
                                                    mean = 0.91,
                                                    sd = 0.29))
    temp[treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                     mean = 0.47, 
                                                     sd = 0.31))
    temp[treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                      mean = 0.11, 
                                                      sd = 0.17))
    temp[treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
                                                              mean = 0.72, 
                                                              sd = 0.18))
    temp[treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
                                                          mean = 0.74, 
                                                          sd = 0.14))
    temp[treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
                                                          mean = 0.81, 
                                                          sd = 0.16))
    temp[treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
                                                          mean = 1.0,  
                                                          sd = 0.14))
    temp[treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
                                                           mean = 0.82, 
                                                           sd = 0.14))
    temp[treatment_codes["Glatiramer Acetate 20mg"]] <- exp(rnorm(1,
                                                                  mean = 0.79,
                                                                  sd = 0.13))
    temp[treatment_codes["Glatiramer Acetate 40mg"]] <- exp(rnorm(1,
                                                                  mean = 0.82,
                                                                  sd = 0.15))
    temp
  } else {c(1, 1, exp(0.44), exp(0.44), exp(0.39), exp(-0.15), exp(0.33), exp(0.91), 
            exp(0.47), exp(0.11), exp(0.72), exp(0.74), exp(0.81), exp(1.0), exp(0.82), 
            exp(0.79), exp(0.82))}
  
  
) %>% add_item(
 
  # Treatment switch follows discontinuation rates
  # Dummy values assume the discontinuation doesn't depend on line of therapy
  # (E.g., cladribine disctoninuation rate is same at 1st and 2nd line)
  # This can be changed by adding line specific discontinuation rates

    v_discontinuation_rate = if(psa_bool) {
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
    else {0.05 + 0.005 * 1:length(v_treatment_names)}
    
) %>% add_item(
  # Treatment administration costs
  v_c_trt_admin = if(psa_bool) {
    temp <- t(matrix(nrow = length(df_c_tx_admin), ncol = 1,
                     dimnames = list(names(df_c_tx_admin), "administration_cost")))
    for(i in 1:length(temp)) {
      temp[i] <- rgamma(1, ((df_c_tx_admin[i])^2)/((df_c_tx_admin[i]/sd_psa)^2),
                        df_c_tx_admin[i]/((df_c_tx_admin[i]/sd_psa)^2))
    }
    temp
    
  } else {
    t(matrix(df_c_tx_admin, nrow = length(df_c_tx_admin), ncol = 1,
             dimnames = list(names(df_c_tx_admin), "administration_cost")))
  }
  
) %>% add_item(
  # Treatment monitoring costs
  v_c_trt_monitor = if(psa_bool) {
    temp <- t(matrix(nrow = length(df_c_tx_monitor), ncol = 1,
                     dimnames = list(names(df_c_tx_monitor), "monitoring_cost")))
    for(i in 1:length(temp)) {
      temp[i] <- rgamma(1, ((df_c_tx_monitor[i])^2)/((df_c_tx_monitor[i]/sd_psa)^2),
                        df_c_tx_monitor[i]/((df_c_tx_monitor[i]/sd_psa)^2))
    }
    temp
    
  } else {
    t(matrix(nrow = length(df_c_tx_monitor), ncol = 1,
             dimnames = list(names(df_c_tx_monitor), "monitoring_cost")))
  }
  
  ) %>% add_item(  
    # Relapse costs are treatment & severity agnostic
    
    v_c_relapsing = if(psa_bool) {
      temp <- df_c_relapse[,1]
      for(i in 1:length(temp)) {
        temp[i] <- pmax(rnorm(1, mean =  df_c_relapse[i,1], sd =  df_c_relapse[i,2]),0)
      }
      temp
      
    } else {
      df_c_relapse[,1]
    }
    
  ) %>% add_item(  
    # Relapse dis-utilities are treatment & severity agnostic, in TA616 the same regard less of hospitalisation
    u_relapsing = if(psa_bool) {
      pmin(rnorm(1, mean =  u_relapse[1,1], sd =  u_relapse[1,2]),0)
      
    } else {
      u_relapse[1,1]
    }
    
  ) %>% add_item(
    # costs of serious adverse events
    v_c_adverse_events = if(psa_bool) {
      temp <- t(matrix(nrow = length(df_c_ae), ncol = 1,
                       dimnames = list(names(df_c_ae), "sae_cost")))
      for(i in 1:length(temp)) {
        temp[i] <- rgamma(1, ((df_c_ae[i])^2)/((df_c_ae[i]/sd_psa)^2),
                          df_c_ae[i]/((df_c_ae[i]/sd_psa)^2))
      }
      temp
      
    } else {
      t(matrix(df_c_ae, nrow = length(df_c_ae), ncol = 1,
               dimnames = list(names(df_c_ae), "sae_cost")))
    }
    
  ) %>% add_item(
    # disutilities of serious adverse events
    v_u_adverse_events = if(psa_bool) {
      temp <- t(matrix(nrow = length(df_u_ae), ncol = 1,
                       dimnames = list(names(df_u_ae), "sae_disutility")))
      for(i in 1:length(temp)) {
        temp[i] <- pmin(rnorm(1, mean =  df_u_ae[i], sd =  -df_u_ae[i]/sd_psa),0)
      }
      temp
      
    } else {
      t(matrix(df_u_ae, nrow = length(df_u_ae), ncol = 1,
               dimnames = list(names(df_u_ae), "sae_disutility")))
    }
  )  %>% add_item(
    # rates of adverse events
    adverse_events_rates = if(psa_bool) {
      temp <- v_r_sae
      # Rates are specified by SAEs for Natalizumab
        for (j in 1:length(temp)){
          temp[j] <- exp(rnorm(1,
                                 mean = log(temp[j]),
                                sd = temp[j]/10))
          }
          
     temp
    } else {v_r_sae}
    
  )  



#Put objects here that do not change as we loop through treatments for a patient
# This is called for each new patient so sample 1 from a distribution of choice
# for heterogeneous characteristics
common_pt_inputs <- add_item(sex = sample(size = 1, x = c("male", "female"), # Random 
                                          prob = c(1 - prop_female, prop_female)) #1 is male 0 is female                            
)

# Objects that depend on treatment
# Note that the current treatment is later initialised to trt
unique_pt_inputs <- add_item(edss_value = sample(size = 1, x = v_edss_values, # Random 
                                                 prob = rdiric(1, v_affirm_edss_distribution)),
                             #edss.i = edss_value,
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



# Add time to event for each patient for each of the treatments
# Setting trt to all treatments should mean that the event applies to all treatments

init_event_list <- 
  add_tte(trt = v_treatment_names,
          evts = v_event_names,
          input={ 
            initialise <- 0
            
            relapse <- draw_tte(
              1, 
              dist = "exp", 
              # Current treatment code allows treatment switching (i.e., not the trt)
              # MS registry time to relapse HA RRMS
              coef1 = (1 - spms_flag) * (v_baseline_harrms_arr["(Intercept)"] +
                                         v_baseline_harrms_arr["BaselineWebEDSS"]*(edss_value)  +
                                         v_baseline_harrms_arr["DMTNatalizumab"]) +
              # MS registry time to relapse SPMS
                      spms_flag * (v_baseline_spms_arr["rate"] +
                                   v_baseline_spms_arr["BaselineWebEDSS"]*(edss_value)),
              # Baseline rate modified by HR of current treatment
              hr = ((1 - spms_flag) * v_hr_arr[current_treatment_code]) + spms_flag
              )
            edss_increase <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss increase HARRMS
              coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                                         v_baseline_harrms_edss_increase["BaselineWebEDSS"]*(edss_value)  + 
                                         v_baseline_harrms_edss_increase["DMTNatalizumab"]) + 
              #MS registry time to edss increase SPMS
                      spms_flag * (v_spms_edss_increase["rate"] + 
                                   v_spms_edss_increase["BaselineWebEDSS"]*(edss_value) ),    
              # Baseline rate modified by HR of current treatment
              hr = ((1 - spms_flag) * v_hr_edss_increase[current_treatment_code])+ spms_flag 
            )
            edss_decrease <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss decrease All RRMS patients
              coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                        v_rrms_edss_decrease["BaselineWebEDSS"]*(edss_value)) + 
                #MS registry time to edss increase SPMS
                              spms_flag * (v_spms_edss_decrease["rate"] +
                              v_spms_edss_decrease["BaselineWebEDSS"]*(edss_value) )
            )

            progression <- draw_tte(
              1,
              dist = "exp",
              # MS registry time to progression All RRMS patients to SPMS
              # Below is zero if already progressed to SPMS
              coef1 = (1 - spms_flag) * (v_spms_progression["(Intercept)"] +
                                         v_spms_progression["BaselineWebEDSS"]*(edss_value) ) + 
                                         v_spms_progression["DMTNatalizumab"]
            )
            #source("R/draw_tte_new.R")
            death <- draw_tte_new(
              1,
              dist = "pwexp",
              # population mortality drawn using piece wise exponential
              coef1 = c(log((1 - prop_female) * m_r_mortality[(initial_age+1):nrow(m_r_mortality), c("male")] +
                              prop_female *  m_r_mortality[(initial_age+1):nrow(m_r_mortality), c("female")],
              )),
              time = c(m_r_mortality[(initial_age+1):nrow(m_r_mortality)]) ,
              # MS specific mortality multiplier
              hr=v_smr[1]
            )
            serious_adverse_event <- draw_tte(
              1,
              dist = "exp",
              # SAE rates same for all treatments
              # change to include HR from safety NMA
              coef1 = log(mean(v_r_sae))
            )
            treatment_switch <- draw_tte(
              1,
              dist = "exp",
              # Discontinuation rates can vary by current treatment (Dummy data)
              coef1 = log(v_discontinuation_rate[current_treatment_code])
            )
          })


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
                     coef1 = (1 - spms_flag) * (v_baseline_harrms_arr["(Intercept)"] +
                                                  v_baseline_harrms_arr["BaselineWebEDSS"]*(edss_value+1)  +
                                                  v_baseline_harrms_arr["DMTNatalizumab"])+
                       spms_flag * (v_baseline_spms_arr["rate"] +
                                      v_baseline_spms_arr["BaselineWebEDSS"]*(edss_value+1)),
                     hr = ((1 - spms_flag) * v_hr_arr[current_treatment_code]) + spms_flag
                     )
                 ))
               }) %>%
  add_reactevt(name_evt = "edss_increase",
               input = {
                 modify_item(list(#edss.i = edss_value,
                                  edss_value = min(edss_value + 1, max(v_edss_values)),
                                  age = initial_age + round(curtime)))
                 
                 # Need to simulate the next time to EDSS increase
                 if (edss_value < 9) {new_event(list(
                   "edss_increase" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                            v_baseline_harrms_edss_increase["BaselineWebEDSS"]*(edss_value+1) ) + 
                            v_baseline_harrms_edss_increase["DMTNatalizumab"] +
                                      spms_flag * (v_spms_edss_increase["rate"] +
                                      v_spms_edss_increase["BaselineWebEDSS"]*(edss_value+1)),  
                     hr = ((1 - spms_flag) * v_hr_edss_increase[current_treatment_code])+ spms_flag 
                   )
                   
                 ))}
               }) %>%
  add_reactevt(name_evt = "edss_decrease",
               input = {
                 modify_item(list(#edss.i = edss_value,
                                  edss_value = max(edss_value - 1, min(v_edss_values)),
                                  age = initial_age + round(curtime)))
                 
                 # Need to simulate the next time to EDSS decrease
                 if (edss_value > 0) {new_event(list(
                   "edss_decrease" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     # Below is zero if spms_flag is 1 since patients should have EDSS decrase if SPMS
                     coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                                v_rrms_edss_decrease["BaselineWebEDSS"]*(edss_value+1) )  + 
                                   spms_flag * (v_spms_edss_decrease["rate"]  +
                                                v_spms_edss_decrease["BaselineWebEDSS"]*(edss_value+1) ) 
                   )
                 ) )}
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
  add_reactevt(name_evt = "serious_adverse_event",
               #input = {               }
               # Sample next discontinuation event. T
               new_event(list(
                 "serious_adverse_event" = curtime + draw_tte(
                   1,
                   dist = "exp",
                   # SAE rates same for all treatments
                   # change to include HR from safety NMA
                   coef1 = log(mean(v_r_sae))
                 )
               ))
               ) %>%
  # Currently assumes patients switch only twice so 4th line is final (As in protocol)
  add_reactevt(name_evt = "treatment_switch",
               input = {
                 modify_item(list(current_treatment = sample(x = v_higher_line_treatments,
                                                             size = 1),
                                  current_treatment_code = sample(x = v_higher_line_treatment_codes,
                                                                  size = 1),
                                  treatment_line = treatment_line + 1,
                                  v_discontinuation_rate = (treatment_line < 4) *  v_discontinuation_rate,
                                  n_switch = n_switch +1,
                                  age = initial_age + round(curtime)))
                 
                 # Sample next discontinuation event. This will be zero if treatment_line >=4
                 new_event(list(
                   "treatment_switch" = curtime + draw_tte(
                     1,
                     dist = "exp",
                     coef1 = log(v_discontinuation_rate[current_treatment_code])
                   )
                 ))
               })

# Add ongoing utilities
# Can also add util_instant to include disutilities of events (e.g., of relapse or adverse events)
util_ongoing <- add_util(evt = v_event_names,
                         trt = v_treatment_names,
                         util = (1 - spms_flag) * u_rrms_edss[edss_value == v_edss_values] +
                           spms_flag * u_rrms_edss[edss_value == v_edss_values]
)

# Add ongoing costs
# Can also add instaneous costs with cost_instant (For example cost of managing relapse)
cost_ongoing <- add_cost(evt = v_event_names,
                         trt = v_treatment_names,
                         cost = (1 - spms_flag) * (
                           v_c_rrms_edss[edss_value == v_edss_values] #+
                             #treatment_costs[current_treatment = trt] treatment costs should be instantaneous not ongoing
                         ) +
                           spms_flag * v_c_rrms_edss[edss_value == v_edss_values]
)

##Add instant costs

# Treatment Costs right now using an average but need to figure out how to manipulate the costs according to year 1 or 2+ of treatment
cost_instant <-
  add_cost(evt = c("initial", "treatment_switch") ,
           trt = v_treatment_names,
           cost = df_c_tx[current_treatment_code] +
             v_c_trt_admin[current_treatment_code] +
             v_c_trt_monitor[current_treatment_code]) %>% 
  add_cost(evt = "relapse" ,
               trt = v_treatment_names,
               cost = v_c_relapsing[4]) %>% 
  add_cost(evt = "serious_adverse_event" ,
           trt = v_treatment_names,
           cost = sum(v_c_adverse_events*v_weight_sae)) 

# Add instant utilities
# Can also add util_instant to include disutilities of events (e.g., of relapse or adverse events)
util_instant <-   add_util(evt = "relapse" ,
                           trt = v_treatment_names,
                           util = u_relapsing) %>% 
                  add_util(evt = "serious_adverse_event" ,
                           trt = v_treatment_names,
                           util = sum(v_u_adverse_events*v_weight_sae)) 


# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <- system.time({
  results <- RunSim(
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = TRUE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    debug = TRUE,
    trt_list = v_treatment_names,             # intervention list
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
    input_out=c("age", "sex", "spms_flag", "edss_value", "n_relapse", "current_treatment",
                "treatment_line", "n_switch") #"edss.i"          
  )
})


#save results
#saveRDS(results, file ="results/descem5_1 100x100_draft_report 16_oct.RDS")
# Simulated patients for first simulation
psa_ipd <- bind_rows(map(results$output_psa, "merged_df"))
# compare patient characteristics of dead patients
#dead <- psa_ipd[psa_ipd$evtname == "death"]

# Plot the times to event
# Include a vertical line for the median event time.
#data_plot <- results$final_output$merged_df %>%
#  filter(evtname != "initialise") %>%
#  group_by(trt, evtname, simulation) %>%
#  mutate(median = median(evttime)) %>%
#  ungroup()

#ggplot(data_plot) +
#  geom_density(aes(fill = trt, x = evttime),
#               alpha = 0.7) +
#  geom_vline(aes(xintercept = median, col = trt)) +
#  facet_wrap( ~ evtname, scales = "free") +
#  scale_y_continuous(expand = c(0, 0)) +
#  scale_x_continuous(expand = c(0, 0)) +
#  theme_bw()


# Summarise the results
#results<-readRDS(file ="results/descem5_1 100x100_draft_report 16_oct.RDS") 
# Get the total costs and QALYs for each patient
total_costs_ipd <- total_qalys_ipd <- array(NA, dim = c(n_samples, n_treatments, n_patients),
                                            dimnames = list(NULL, v_treatment_names, NULL))

# Below is not very efficient but only needs to be run once at end of simulation
for(treatment_name in v_treatment_names) {
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

#saveRDS(total_qalys_ipd, file ="results/total_qalys_ipd_draft_report 16_oct.RDS")
#saveRDS(total_costs_ipd, file ="results/total_costs_ipd_draft_report 16_oct.RDS")

# Take mean over patients to remove individual variation
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)

# Create a BCEA object
des_bcea <- bcea(eff = total_qalys,
                 cost = total_costs,
                 ref = 1,
                 interventions = v_treatment_names)

# Plot the CE plane
library(ggplot2)
ceplane.plot(des_bcea, wtp = 30000,
             graph = "ggplot2",
             icer = list(color = c("red", "green", "blue", "orange", "yellow",
                                   "pink", "darkgreen", "lightblue", "darkorange","lightyellow",
                                   "purple", "brown", "skyblue", "magenta", "cyan", "darkred")),
             point = list(color = c("red", "green", "blue", "orange", "yellow",
                                    "pink", "darkgreen", "lightblue", "darkorange","lightyellow",
                                    "purple", "brown", "skyblue", "magenta", "cyan", "darkred")))

#plot a CEAC
des_multi_ce <- multi.ce(des_bcea)
ceac.plot(des_multi_ce, graph = "ggplot2",
          line = list(color = c("red", "green", "blue", "orange", "yellow",
                                "pink", "darkgreen", "lightblue", "darkorange","lightyellow",
                                "purple", "brown", "skyblue", "magenta", "cyan", "darkred")),
          pos = c(0, 0.50))

# Incremental Net Benefit
lambda <- 30000
net_benefit <- lambda * total_qalys - total_costs
mean_net_benefit <- apply(net_benefit, 2, mean) 
INB_vs1 <- mean_net_benefit - mean_net_benefit[1]
INB_table <- as.data.frame(t(rbind(v_treatment_names,mean_net_benefit, INB_vs1)))
INB_table <-INB_table[order(INB_table[,"INB_vs1"]),]
library(openxlsx)
write.xlsx(INB_table, file="results/INB 16 Oct draft report.xlsx")

#Incremental analysis
IA_names <- c("Costs", "Effects", "Strictly dominated", "ICER", "Extendedly dominated")
IA <- as.data.frame(matrix(nrow=n_treatments, ncol=length(IA_names)))
colnames(IA) <- IA_names
rownames(IA) <- v_treatment_names
IA[,"Costs"] <-  apply(total_costs, 2, mean)
IA[,"Effects"] <-  apply(total_qalys, 2, mean)
IA <- IA[order(IA[,"Costs"]),]

for(i in 1:n_treatments){
  IA[i,"Strictly dominated"] <- IA[i,"Effects"] < max(IA[1:(i-1),"Effects"])
}

IA[,"Extendedly dominated"] <- FALSE
for(i in 2:n_treatments){
  if(IA[i,"Strictly dominated"]) {
     IA[i,"Extendedly dominated"] <- TRUE
  } else{
    c <-max(which(!IA[1:(i-1),"Strictly dominated"] |
                    !IA[1:(i-1),"Extendedly dominated"]))
    IA[i,"ICER"] <- (IA[i,"Costs"] - IA[c,"Costs"]) / (IA[i,"Effects"] - IA[c,"Effects"])
  }
}

IA
write.xlsx(IA, file="results/IA 16 Oct draft report.xlsx")
