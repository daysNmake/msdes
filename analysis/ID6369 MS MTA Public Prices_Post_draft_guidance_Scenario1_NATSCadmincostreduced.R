#### RRMS DES using the DESCEM package ####
## Authored: Howard Thom & Ayman Sadek ###
## Validated: Javier Sanchez Alvarez ####
##### 11/11/2024 #######################


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

# Function to summarise the results
source("R/summarise_samples.R")
source("R/summarise_ce_results.R")

## New functions to draw from piece wise exponential distribution
source("R/draw_tte_new.R")
## New function to run the simulation saving input parameters
# and with arbitrary random seeds
source("R/RunSim_new.R")

## set a seed 
n_seed<-243523452
set.seed(n_seed)

## number of patients and samples
n_patients <- 100
n_samples <- 100

## Treatments at second line 
v_treatment_names <- c("Natalizumab-IV",
                       "Natalizumab-SC",
                       "Natalizumab biosimilar-IV",
                       "Natalizumab biosimilar-SC",
                      # "Fingolimod",
                      # "Alemtuzumab",
                       "Cladribine", 
                       "Ublituximab" ,
                       "Ofatumumab",
                       "Ocrelizumab" #,
                      # "PegIFN-β-1 SC 125μg", 
                      # "IFNβ-1a SC 22μg" , 
                      # "IFNβ-1a SC 44μg" ,
                      # "IFNβ-1a IM 30μg", 
                      # "IFNβ-1b SC 250μg", 
                      # "Glatiramer Acetate 20mg",
                      # "Glatiramer Acetate 40mg"
                       )

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
v_event_names <- c("initialise", "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "annual_treatment", "serious_adverse_event", "death")

## Standard Deviation for probabilistic analysis when uncertainty not reported
# assumed 1/10 of mean
sd_psa <- 10


## Baseline population age sex and model time horizon
initial_age <- 36  # 36 ± 8 both arms of AFFIRM (N = 942)
time_horizon <- 100-initial_age # Years so lifetime
prop_female <- 0.70 #  (n=660) both arms of AFFIRM (N = 942)

## Define the possible EDSS values
v_edss_values <- c(0:9)
names(v_edss_values) <- paste("EDSS", v_edss_values)

# Number of EDSS categories
n_edss_values <- length(v_edss_values)

## Natural History MS Registry
## Vectors of coefficients and matrices of variance and covariance
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


## baseline distribution of EDSS health states (MS Registry)
m_n_patients_edss <- as.matrix(read.csv("data/m_n_patients_edss.csv", header = T, row.names = 1))
colnames(m_n_patients_edss) <- names(v_edss_values)

## baseline proportion of patients with SPMS
initial_spms_prop <- 0

## Proportion of patients having relapse leading to hospital_admission Hawton et al 2026
v_p_relapse_severity <- read.csv(file="data/v_p_relapse_severity.csv", header = T, row.names =1)
p_relapse_hospital<- v_p_relapse_severity[,"hospital_admission"]

## Incidence & weights of Serious Adverse events in AFFIRM
saes_data <-read.csv(file="data/df_saes_data.csv", header = T, row.names =1)
sae_names <- saes_data[,"Serious.Adverse.events"]
# Number of serious adverse events in the model
n_sae <- length(sae_names)
# The weights are used to calculate SAEs costs and utilities 
v_weight_sae <- saes_data[, "Weight"] 

# % on Natalizumab would have experienced SAEs AFFIRM (Polman 2006)
m_saes <- as.matrix(read.csv("data/m_saes.csv", header = T, row.names = 1))

##rates of mortality, ONS
m_r_mortality <- as.matrix(read.csv("data/df_all_cause_mortality.csv", header = T, row.names = 1))
m_r_mortality <- m_r_mortality[,c("age", "mx", "mx.1")]
m_r_mortality[,"age"] <- as.numeric(m_r_mortality[,"age"]) #check if this step is needed?
dimnames(m_r_mortality) <-  list(NULL, c("time" ,"male", "female"))


## edss costs direct medical costs Tyas etal. 2005 inflated to 2022/2023 
m_c_edss <- read.csv("data/m_c_edss.csv", header = T, row.names = 1) 
m_c_edss_rrms <- m_c_edss[,c("rrms_mean","rrms_se")]
m_c_edss_spms <- m_c_edss[,c("spms_mean","spms_se")]

## Excess all cause mortality for MS patients Jick 2014 
v_smr <- read.csv("data/v_smr.csv", header = T, row.names = 1)

## Excess all cause mortality for MS patients HR by severity edss<4 (Jick et al), edss>=4 (Harding et al)
v_smr_severity_harding <- as.matrix(read.csv("data/v_smr_severity_Harding.csv", header = T, row.names = 1))
v_smr_severity_harding[,c(9:10)] <- v_smr_severity_harding[,8] # Assuming SMR on EDSS 8 & 9 are equal to EDSS7 
dimnames(v_smr_severity_harding) <- list(c("smr", "sd", "logsd"), v_edss_values)

##  Annual treatment, administration and monitoring costs 
#public list prices
df_c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
df_c_tx <- df_c_tx[,intersect(colnames(df_c_tx),v_treatment_names)]
df_c_tx[,1:4] <- (df_c_tx[,1:4]*0.6*(round(52/5.55, 2)/13)) + df_c_tx[,1:4]*0.4 # 60% on EID interval dosing

# These are direct medical costs we excluded the indirect medical costs
df_c_tx_admin <- t(read.csv("data/df_c_drugs_admin.csv", header = T, row.names = 1))
df_c_tx_admin[df_c_tx_admin==0] <-0.1 # set zero costs to positive number stops generating NaNs
df_c_tx_admin <- df_c_tx_admin[,intersect(colnames(df_c_tx_admin),v_treatment_names)]
df_c_tx_admin[,1:4] <- (df_c_tx_admin[,1:4]*0.6*(round(52/5.55, 2)/13)) + df_c_tx_admin[,1:4]*0.4 # 60% on EID interval dosing
df_c_tx_admin[,"Natalizumab-SC"] <- (df_c_tx_admin[,"Natalizumab-SC"] * 0.5) # Scenario for reduced admin cost

df_c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
df_c_tx_monitor <- df_c_tx_monitor[,intersect(colnames(df_c_tx_monitor),v_treatment_names)]
df_c_tx_monitor[,"Natalizumab-IV"] <- (df_c_tx_monitor[,"Natalizumab-IV"] - 247) #  free JCV test 
df_c_tx_monitor[,"Natalizumab-SC"] <- (df_c_tx_monitor[,"Natalizumab-SC"] - 247) #  free JCV test 
df_c_tx_monitor[,"Natalizumab biosimilar-IV"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-IV"] - 247) #  free JCV test 
df_c_tx_monitor[,"Natalizumab biosimilar-SC"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-SC"] - 247) #  free JCV test

# SPMS Costs
df_c_spms<- read.csv("data/df_c_spms.csv", header = T, row.names = 1)
colnames(df_c_spms)[1] <- "PegIFN-β-1 SC 125μg"
p_siponimod <-0.556962025 #proportion treated with Siponimod based on MS registry data

## Annual treatment scheduled
df_c_tx_load <-t(read.csv("data/df_c_retreatment.csv", header = T, row.names = 1))
df_c_tx_load <- df_c_tx_load[,intersect(colnames(df_c_tx_load),v_treatment_names)]
rownames(df_c_tx_load)<- c(seq(1,5,1))

## Relapse Costs inflated Hawton 2016 to 2023
m_c_relapse <-read.csv("data/m_c_relapse.csv", header = T, row.names = 1)
m_c_relapse <- m_c_relapse * 2 # Annualised these 6 months costs

## Adverse events costs MS-MTA report
df_c_sae <- saes_data[,4]
names(df_c_sae)<-sae_names

## RRMS & SPMS edss utilities Orme 
u_edss <- read.csv("data/m_u_edss.csv", header = T, row.names = 1)
m_u_edss_rrms <- u_edss[,c("rrms_mean","rrms_sd")]
m_u_edss_spms <- u_edss[,c("spms_mean","spms_sd")]

## Relapse utilities
df_u_relapse <-read.csv("data/df_u_relapse.csv", header = T, row.names = 1)

## Carer disutilities # add to costs calculation
m_u_carer <- read.csv("data/m_u_carer.csv", header = T, row.names = 1)

## Adverse events dis-utilities MS-MTA report
df_u_sae <- saes_data[,5]
names(df_u_sae)<-sae_names

## baseline discontinuation AFFIRM and Antilope
m_discontinuation <- as.matrix(read.csv("data/m_discontinuation.csv", header = T, row.names = 1))

## NMA data
# CDP3 NMA
hr_rrms_cdp3_fe <- read.csv("data/Releff_RRMS_CDP3_FE.csv",
                            header = TRUE, row.names=1)
m_hr_rrms_cdp3_fe  <- hr_rrms_cdp3_fe[,c("mean", "sd")]
# CDP6 NMA
hr_rrms_cdp6_fe <- read.csv("data/Releff_RRMS_CDP6_FE.csv",
                            header = TRUE, row.names=1)
m_hr_rrms_cdp6_fe  <- hr_rrms_cdp6_fe[,c("mean", "sd")] 
# ARR NMA
hr_rrms_arr_fe <- read.csv("data/Releff_RRMS_ARR_FE.csv",
                           header = TRUE, row.names=1)
m_hr_rrms_arr_fe  <- hr_rrms_arr_fe[,c("mean", "sd")] 
# Serious Adverse Events NMA
hr_rrms_saes_fe <- read.csv("data/Releff_RRMS_SeriousAEs_FE.csv",
                            header = TRUE, row.names=1)
m_hr_rrms_saes_fe  <- as.matrix(hr_rrms_saes_fe[,c("mean", "sd")] )
# Discontinuation NMA
hr_rrms_discontinuation_fe <- read.csv("data/Releff_RRMS_Discontinuation_FE.csv",
                                       header = TRUE, row.names=1)
m_hr_rrms_discontinuation_fe <- as.matrix(hr_rrms_discontinuation_fe[,c("mean", "sd")]) 

## proportions on 2nd and 3rd line treatment, assuming Cladribine and Ublituximab are equivalent to Natalizumab
m_proportion_ontrtline<- read.csv("data/m_proportion_ontrtline.csv", header = T, row.names = 1)
colnames(m_proportion_ontrtline) <- v_treatment_names

# Global parameters of the model as they are common to all patients and treatments
common_all_inputs <-add_item(
  v_treatment_names = v_treatment_names,
  v_treatment_codes = v_treatment_codes,
  v_higher_line_treatments = v_higher_line_treatments,
  v_higher_line_treatment_codes = v_higher_line_treatment_codes,
  v_prior_treatment_indices = v_prior_treatment_indices,
  initial_age = initial_age,
  time_horizon = time_horizon,
  prop_female = prop_female,
  v_edss_values = v_edss_values,
  df_c_tx = df_c_tx, # drug costs are not probabilistic
  p_siponimod=p_siponimod, 
  v_event_names = v_event_names,
  m_n_patients_edss = m_n_patients_edss,
  m_r_mortality = m_r_mortality,
  v_smr = v_smr, 
  v_weight_sae = v_weight_sae,
  initial_spms_prop = initial_spms_prop,
  p_relapse_hospital = p_relapse_hospital
  
) %>% add_item(
  # Utilities for RRMS Orme et al and carergiver disutilities Acaster et al.
  v_u_rrms_edss = if(psa_bool) {
    temp <- m_u_edss_rrms[, "rrms_mean"]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- log(rlnorm(1, mean =  (m_u_edss_rrms[j_edss, "rrms_mean"]+ m_u_carer[j_edss, "Mean"]),
                                 sd =  m_u_edss_rrms[j_edss, "rrms_sd"]))
    }
    temp
    
  } else {
    m_u_edss_rrms[,"rrms_mean"]
  }
  
) %>% add_item(
  # Utilities for SPMS Orme et al and carergiver disutilities Acaster et al.
  v_u_spms_edss = if(psa_bool) {
    temp <- m_u_edss_spms[, "spms_mean"]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- log(rlnorm(1, mean =  (m_u_edss_spms[j_edss, "spms_mean"] + m_u_carer[j_edss, "Mean"]),
                                    sd =  m_u_edss_spms[j_edss, "spms_sd"]))
    }
    temp
    
  } else {
    (m_u_edss_spms[,"spms_mean"] + m_u_carer[j_edss, "Mean"])
  }
) %>% add_item(
  # RRMS EDSS Health state Direct Medical costs for RRMS Tyas et al inflated 
  v_c_rrms_edss = if(psa_bool) {
    temp <- m_c_edss_rrms[,"rrms_mean" ]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- rgamma(1, ((m_c_edss_rrms[j_edss,"rrms_mean"])^2) / (m_c_edss_rrms[j_edss, "rrms_se"]^2),
                             m_c_edss_rrms[j_edss, "rrms_mean"] / (m_c_edss_rrms[j_edss, "rrms_se"]^2))
    }
    temp
    
  } else {
    m_c_edss_rrms[,"rrms_mean"]
  }
  
) %>% add_item(
  # SPMS EDSS Health state Direct Medical costs for RRMS Tyas et al inflated 
  v_c_spms_edss = if(psa_bool) {
    temp <- m_c_edss_spms[,"spms_mean" ]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- rgamma(1, ((m_c_edss_spms[j_edss,"spms_mean"])^2) / (m_c_edss_spms[j_edss, "spms_se"]^2),
                             m_c_edss_spms[j_edss, "spms_mean"] / (m_c_edss_spms[j_edss, "spms_se"]^2))
    }
    temp
    
  } else {
    m_c_edss_spms[,"spms_mean"]
  }
  
) %>% add_item(
  # smr by severity Harding centered on Jick
  v_hr_smr_edss = if(psa_bool) {
    temp <- v_smr_severity_harding["smr",]

    for(j_edss in 1:10) {
      temp[j_edss] <- exp(
        rnorm(1, mean= log(v_smr_severity_harding["smr",j_edss]) - log(v_smr_severity_harding["smr",5]) , 
              sd=v_smr_severity_harding["logsd",j_edss]) 
      ) + rnorm(1000, mean=  log(v_smr_severity_harding["smr",1]), 
                sd=v_smr_severity_harding["logsd",1]
      )
    }
    temp
    
    
  } else {
    (v_smr_severity_harding["smr",] / v_smr_severity_harding["smr",5]) *v_smr_severity_harding["smr",1]
  }
  
) %>% add_item(
  # Hazard ratios reducing risk of EDSS increase
  v_hr_edss_increase = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # CDP6relative treatment effects log Hr from the NMA with Natalizumab as the reference
    temp[v_treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1 #missing from NMA
    temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1 #missing from NMA
#    temp[v_treatment_codes["Fingolimod"]] <- exp(rnorm(1,
#                                                       mean = m_hr_rrms_cdp6_fe["Fingolimod O0.5","mean"],
#                                                       sd = m_hr_rrms_cdp6_fe["Fingolimod O0.5","sd"]))
#    temp[v_treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
#                                                        mean = m_hr_rrms_cdp6_fe["Alemtuzumab IV12","mean"],
#                                                        sd = m_hr_rrms_cdp6_fe["Alemtuzumab IV12","sd"])) 
    temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                                       mean = m_hr_rrms_cdp6_fe["Cladribine O3.5","mean"],
                                                       sd = m_hr_rrms_cdp6_fe["Cladribine O3.5","sd"]))
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                      mean = m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"],
                                                      sd = m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"])) #  trt class 
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean = m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"], 
                                                       sd = m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"])) #  trt class 
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean = m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"], 
                                                        sd = m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"]))
#    temp[v_treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
#                                                                mean = m_hr_rrms_cdp6_fe["Peginterferon beta 1a SC125","mean"], 
#                                                                sd = m_hr_rrms_cdp6_fe["Peginterferon beta 1a SC125","sd"])) 
#    temp[v_treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_cdp6_fe["Interferon beta 1a SC44","mean"], 
#                                                            sd = m_hr_rrms_cdp6_fe["Interferon beta 1a SC44","sd"]))
#    temp[v_treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_cdp6_fe["Interferon beta 1a SC44","mean"], 
#                                                            sd = m_hr_rrms_cdp6_fe["Interferon beta 1a SC44","sd"])) # assumed same as 44μg
#    temp[v_treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_cdp6_fe["Interferon beta 1a IM30","mean"],  
#                                                            sd = m_hr_rrms_cdp6_fe["Interferon beta 1a IM30","sd"]))
#    temp[v_treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
#                                                             mean = m_hr_rrms_cdp6_fe["Interferon beta 1b IM 250","mean"], 
#                                                             sd = m_hr_rrms_cdp6_fe["Interferon beta 1b IM 250","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 20mg"]] <- exp(rnorm(1,
#                                                                    mean = m_hr_rrms_cdp6_fe["Glatiramer acetate SC20","mean"],
#                                                                    sd = m_hr_rrms_cdp6_fe["Glatiramer acetate SC20","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 40mg"]] <- exp(rnorm(1,
#                                                                    mean = m_hr_rrms_cdp6_fe["Glatiramer acetate SC20","mean"],
#                                                                    sd = m_hr_rrms_cdp6_fe["Glatiramer acetate SC20","sd"])) # assumed same as 20 mg
    temp
  } else {matrix(c(1, 1, 1, 1, 
#                   exp(m_hr_rrms_cdp6_fe["Fingolimod O0.5","mean"]), exp(m_hr_rrms_cdp6_fe["Alemtuzumab IV12","mean"]), 
                    exp(m_hr_rrms_cdp6_fe["Cladribine O3.5","mean"]), 
                    exp(m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), 
                    exp(m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), exp(m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]) #, 
#                    exp(m_hr_rrms_cdp6_fe["Peginterferon beta 1a SC125","mean"]), 
#                   exp(m_hr_rrms_cdp6_fe["Interferon beta 1a SC44","mean"]), exp(m_hr_rrms_cdp6_fe["Interferon beta 1a SC44","mean"]), 
#                   exp(m_hr_rrms_cdp6_fe["Interferon beta 1a IM30","mean"]), exp(m_hr_rrms_cdp6_fe["Interferon beta 1b IM 250","mean"]),
#                   exp(m_hr_rrms_cdp6_fe["Glatiramer acetate SC20","mean"]), exp(m_hr_rrms_cdp6_fe["Glatiramer acetate SC20","mean"])
                    ) ,
                 nrow = 1, ncol = n_treatments)
  }
  
) %>% add_item(
  # MS Registry time to EDSS increase HA RRMS patients
  v_baseline_harrms_edss_increase = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = v_tto_edss_increase_harrms_mu, 
            Sigma = m_tto_edss_increase_harrms_sigma) 
    
  } else {v_tto_edss_increase_harrms_mu}
  
) %>% add_item(
  # MS Registry time to EDSS decrease All RRMS patients
  v_rrms_edss_decrease = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = v_tto_edss_decrease_rrms_mu, 
            Sigma = m_tto_edss_decrease_rrms_sigma) 
    
  } else {v_tto_edss_decrease_rrms_mu}
  
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
            Sigma = m_tto_edss_decrease_spms_sigma) 
    
  } else {v_tto_edss_decrease_spms_mu}
  
)%>% add_item(
  # MS Registry time to progression HARRMS to SPMS 
  v_spms_progression = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = t(v_tto_sp_conversion_harrms_mu), 
            Sigma = m_tto_sp_conversion_harrms_sigma) 
    
  } else {setNames(v_tto_sp_conversion_harrms_mu[,1],rownames(v_tto_sp_conversion_harrms_mu))}
  
) %>% add_item(
  # MS Registry time to relapse HARRMS patients
  v_baseline_harrms_larr = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = t(v_tto_relapse_harrms_mu), 
            Sigma = tto_relapse_harrms_sigma) 
    
  } else {setNames(v_tto_relapse_harrms_mu[,1],rownames(v_tto_relapse_harrms_mu))}
  
) %>% add_item(
  # MS Registry time to relapse SPMS patients
  v_baseline_spms_larr = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = t(v_tto_relapse_spms_mu), 
            Sigma = m_tto_relapse_spms_sigma) 
    
  } else {setNames(v_tto_relapse_spms_mu[,1], rownames(v_tto_relapse_spms_mu))}
  
) %>% add_item(
  # Annualized relapse rate relative trt effects NMA with Natalizumab as the reference
  v_hr_arr = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    
    temp[v_treatment_codes["Natalizumab-IV"]] <- 1
    temp[v_treatment_codes["Natalizumab-SC"]] <- 1
    temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- exp(rnorm(1,
                                                                      mean =  m_hr_rrms_arr_fe["Natalizumab biosimilar","mean"],
                                                                      sd = m_hr_rrms_arr_fe["Natalizumab biosimilar","sd"]))
    temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- exp(rnorm(1,
                                                                      mean =  m_hr_rrms_arr_fe["Natalizumab biosimilar","mean"],
                                                                      sd = m_hr_rrms_arr_fe["Natalizumab biosimilar","sd"]))
#    temp[v_treatment_codes["Fingolimod"]] <- exp(rnorm(1,
#                                                       mean =  m_hr_rrms_arr_fe["Fingolimod O0.5","mean"],
#                                                       sd = m_hr_rrms_arr_fe["Fingolimod O0.5","sd"]))
#    temp[v_treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
#                                                        mean =  m_hr_rrms_arr_fe["Alemtuzumab IV12","mean"],
#                                                        sd = m_hr_rrms_arr_fe["Alemtuzumab IV12","sd"])) 
    temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                                       mean =  m_hr_rrms_arr_fe["Cladribine O3.5","mean"],
                                                       sd = m_hr_rrms_arr_fe["Cladribine O3.5","sd"]))
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                      mean =  m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"],
                                                      sd = m_hr_rrms_arr_fe["Ocrelizumab IV600","sd"]))
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean =  m_hr_rrms_arr_fe["Ofatumumab SC20","mean"],
                                                       sd = m_hr_rrms_arr_fe["Ofatumumab SC20","sd"]))
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean =  m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"],
                                                        sd = m_hr_rrms_arr_fe["Ocrelizumab IV600","sd"]))
#    temp[v_treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
#                                                                mean =  m_hr_rrms_arr_fe["Peginterferon beta 1a SC125","mean"],
#                                                                sd = m_hr_rrms_arr_fe["Peginterferon beta 1a SC125","sd"]))
#    temp[v_treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
#                                                            mean =  m_hr_rrms_arr_fe["Interferon beta 1a SC44","mean"],
#                                                            sd = m_hr_rrms_arr_fe["Interferon beta 1a SC44","sd"]))
#    temp[v_treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
#                                                            mean =  m_hr_rrms_arr_fe["Interferon beta 1a SC22","mean"],
#                                                            sd = m_hr_rrms_arr_fe["Interferon beta 1a SC22","sd"]))
#    temp[v_treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
#                                                            mean =  m_hr_rrms_arr_fe["Interferon beta 1a IM30","mean"],
#                                                            sd = m_hr_rrms_arr_fe["Interferon beta 1a IM30","sd"]))
#    temp[v_treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
#                                                             mean =  m_hr_rrms_arr_fe["Interferon beta 1b IM 250","mean"],
#                                                             sd = m_hr_rrms_arr_fe["Interferon beta 1b IM 250","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 20mg"]] <- exp(rnorm(1,
#                                                                    mean =  m_hr_rrms_arr_fe["Glatiramer acetate SC20","mean"],
#                                                                    sd = m_hr_rrms_arr_fe["Glatiramer acetate SC20","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 40mg"]] <- exp(rnorm(1,
#                                                                    mean = m_hr_rrms_arr_fe["Glatiramer acetate SC40","mean"],
#                                                                    sd = m_hr_rrms_arr_fe["Glatiramer acetate SC40","sd"]))
    temp
  } else {matrix(c(1, 1, exp(m_hr_rrms_arr_fe["Natalizumab biosimilar","mean"]), 
                   exp(m_hr_rrms_arr_fe["Natalizumab biosimilar","mean"]), 
#                   exp(m_hr_rrms_arr_fe["Fingolimod O0.5","mean"]), exp(m_hr_rrms_arr_fe["Alemtuzumab IV12","mean"]),  
                   exp(m_hr_rrms_arr_fe["Cladribine O3.5","mean"]), 
                    exp(m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]), 
                   exp(m_hr_rrms_arr_fe["Ofatumumab SC20","mean"]), exp(m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]) #, 
#                   exp(m_hr_rrms_arr_fe["Peginterferon beta 1a SC125","mean"]), 
#                   exp(m_hr_rrms_arr_fe["Interferon beta 1a SC44","mean"]), exp(m_hr_rrms_arr_fe["Interferon beta 1a SC22","mean"]), 
#                   exp(m_hr_rrms_arr_fe["Interferon beta 1a IM30","mean"]), exp(m_hr_rrms_arr_fe["Interferon beta 1b IM 250","mean"]), 
#                   exp(m_hr_rrms_arr_fe["Glatiramer acetate SC20","mean"]), exp(m_hr_rrms_arr_fe["Glatiramer acetate SC40","mean"])
  ), nrow = 1, ncol = n_treatments)}
  
) %>% add_item(
  
  # Treatment switch follows discontinuation rates
  # the discontinuation doesn't depend on line of therapy
  # (E.g., cladribine discontinuation rate is same at 1st and 2nd line)
  discontinuation_rate = if(psa_bool) {
    
    # rates of discontination due to adverse events
    #  rate is specific to Natalizumab arm of affirm
    temp <- -log(1-(rbeta(1,m_discontinuation["AFFIRM Natalizumab IV300","alpha"], 
                          m_discontinuation["AFFIRM Natalizumab IV300","beta"]))
    )/m_discontinuation["AFFIRM Natalizumab IV300","follow_up"]
    
    temp
  } else {
    -log(1-(rbeta(1,m_discontinuation["AFFIRM Natalizumab IV300","alpha"], 
                  m_discontinuation["AFFIRM Natalizumab IV300","beta"]))
    )/m_discontinuation["AFFIRM Natalizumab IV300","follow_up"]
  }
)%>% add_item(
  # Hazard ratios for treatment discontinuation due to SAEs
  v_hr_discontinuation = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # relative treatment effects log Hr from the NMA with Natalizumab as the reference
    temp[v_treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- exp(rnorm(1,
                                                                      mean = m_hr_rrms_discontinuation_fe["Natalizumab biosimilar","mean"],
                                                                      sd = m_hr_rrms_discontinuation_fe["Natalizumab biosimilar","sd"]))
    temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- exp(rnorm(1,
                                                                      mean = m_hr_rrms_discontinuation_fe["Natalizumab biosimilar","mean"],
                                                                      sd = m_hr_rrms_discontinuation_fe["Natalizumab biosimilar","sd"]))
#    temp[v_treatment_codes["Fingolimod"]] <- exp(rnorm(1,
#                                                       mean = m_hr_rrms_discontinuation_fe["Fingolimod O0.5","mean"],
#                                                       sd = m_hr_rrms_discontinuation_fe["Fingolimod O0.5","sd"]))
#    temp[v_treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
#                                                        mean = m_hr_rrms_discontinuation_fe["Alemtuzumab IV12","mean"],
#                                                        sd = m_hr_rrms_discontinuation_fe["Alemtuzumab IV12","sd"])) 
    temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                                       mean = m_hr_rrms_discontinuation_fe["Cladribine O3.5","mean"],
                                                       sd = m_hr_rrms_discontinuation_fe["Cladribine O3.5","sd"]))
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                      mean = m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"],
                                                      sd = m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"])) # trt class  
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean = m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"], 
                                                       sd = m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"])) #  trt class 
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean = m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"], 
                                                        sd = m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"]))
#    temp[v_treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
#                                                                mean = m_hr_rrms_discontinuation_fe["Peginterferon beta 1a SC125","mean"], 
#                                                                sd = m_hr_rrms_discontinuation_fe["Peginterferon beta 1a SC125","sd"])) 
#    temp[v_treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_discontinuation_fe["Interferon beta 1a SC44","mean"], 
#                                                            sd = m_hr_rrms_discontinuation_fe["Interferon beta 1a SC44","sd"]))
#    temp[v_treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_discontinuation_fe["Interferon beta 1a SC44","mean"], 
#                                                            sd = m_hr_rrms_discontinuation_fe["Interferon beta 1a SC44","sd"])) # assumed same as 44μg
#    temp[v_treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_discontinuation_fe["Interferon beta 1a IM30","mean"],  
#                                                            sd = m_hr_rrms_discontinuation_fe["Interferon beta 1a IM30","sd"]))
#    temp[v_treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
#                                                             mean = m_hr_rrms_discontinuation_fe["Interferon beta 1b IM 250","mean"], 
#                                                             sd = m_hr_rrms_discontinuation_fe["Interferon beta 1b IM 250","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 20mg"]] <- exp(rnorm(1,
#                                                                    mean = m_hr_rrms_discontinuation_fe["Glatiramer acetate SC20","mean"],
#                                                                    sd = m_hr_rrms_discontinuation_fe["Glatiramer acetate SC20","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 40mg"]] <- exp(rnorm(1,
#                                                                    mean = m_hr_rrms_discontinuation_fe["Glatiramer acetate SC20","mean"],
#                                                                    sd = m_hr_rrms_discontinuation_fe["Glatiramer acetate SC20","sd"])) # assumed same as 20 mg
    temp
  } else {matrix(c(1, 1, exp(m_hr_rrms_discontinuation_fe["Natalizumab biosimilar","mean"]), 
                   exp(m_hr_rrms_discontinuation_fe["Natalizumab biosimilar","mean"]), 
#                   exp(m_hr_rrms_discontinuation_fe["Fingolimod O0.5","mean"]), exp(m_hr_rrms_discontinuation_fe["Alemtuzumab IV12","mean"]), 
                   exp(m_hr_rrms_discontinuation_fe["Cladribine O3.5","mean"]), 
                   exp(m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), 
                   exp(m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), exp(m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]) #, 
#                   exp(m_hr_rrms_discontinuation_fe["Peginterferon beta 1a SC125","mean"]), 
#                   exp(m_hr_rrms_discontinuation_fe["Interferon beta 1a SC44","mean"]), exp(m_hr_rrms_discontinuation_fe["Interferon beta 1a SC44","mean"]), 
#                   exp(m_hr_rrms_discontinuation_fe["Interferon beta 1a IM30","mean"]), exp(m_hr_rrms_discontinuation_fe["Interferon beta 1b IM 250","mean"]),
#                   exp(m_hr_rrms_discontinuation_fe["Glatiramer acetate SC20","mean"]), exp(m_hr_rrms_discontinuation_fe["Glatiramer acetate SC20","mean"])
                   ) ,
                 nrow = 1, ncol = n_treatments)
  }
) %>% add_item(
  # Treatment administration costs
  v_c_trt_admin = if(psa_bool) {
    temp <- matrix(nrow = nrow(df_c_tx_admin), ncol = n_treatments,
                   dimnames = list(row.names(df_c_tx_admin), v_treatment_names))
    for (j_cost in 1:nrow(df_c_tx_admin)) {
      for(i_trt in 1:n_treatments) {
        temp[j_cost,i_trt] <- rgamma(1, ((df_c_tx_admin[j_cost,i_trt])^2) / ((df_c_tx_admin[j_cost,i_trt] / sd_psa)^2),
                                     df_c_tx_admin[j_cost,i_trt] / ((df_c_tx_admin[j_cost,i_trt]/sd_psa)^2))
      }}
    temp
    
  } else {
    matrix(df_c_tx_admin, nrow = nrow(df_c_tx_admin), ncol = n_treatments,
           dimnames = list(row.names(df_c_tx_admin), v_treatment_names))
  }
  
) %>% add_item(
  # Treatment monitoring costs
  v_c_trt_monitor = if(psa_bool) {
    temp <- matrix(nrow = nrow(df_c_tx_monitor), ncol = n_treatments,
                   dimnames = list(row.names(df_c_tx_monitor), v_treatment_names))
    for (j_cost in 1:nrow(df_c_tx_monitor)) {
      for(i_trt in 1:n_treatments) {
        temp[j_cost,i_trt] <- rgamma(1, ((df_c_tx_monitor[j_cost,i_trt])^2) / ((df_c_tx_monitor[j_cost,i_trt]/sd_psa)^2),
                                     df_c_tx_monitor[j_cost,i_trt] / ((df_c_tx_monitor[j_cost,i_trt]/sd_psa)^2))
      }}
    temp
    
  } else {
    matrix(df_c_tx_monitor, nrow = nrow(df_c_tx_monitor), ncol = n_treatments,
           dimnames = list(row.names(df_c_tx_monitor), v_treatment_names))
  }
  
) %>% add_item(  
  # Relapse costs are treatment & severity agnostic
  
  v_c_relapse = if(psa_bool) {
    temp <- m_c_relapse[, "mean"]
    for(i_cost in 1:length(temp)) {
      temp[i_cost] <- rgamma(1, ((m_c_relapse[i_cost,"mean"])^2) / ((m_c_relapse[i_cost,"sd"]/sd_psa)^2),
                             m_c_relapse[i_cost,"mean"] / ((m_c_relapse[i_cost,"sd"]/sd_psa)^2))
    }
    temp
    
  } else {
    m_c_relapse[, "mean"]
  }
  
) %>% add_item(  
  # Relapse dis-utilities are treatment
  u_relapse = if(psa_bool) {
    pmin(rnorm(1, mean =  df_u_relapse["Orme etal", "u_mean_relapse_hospital"], sd =  df_u_relapse["Orme etal", "u_sd_relapse_hospital"]),0)
    
  } else {
    df_u_relapse[1, 1]
  }
  
) %>% add_item(
  # costs of serious adverse events
  v_c_sae_events = if(psa_bool) {
    temp <- t(matrix(nrow = n_sae, ncol = 1,
                     dimnames = list(sae_names, "sae_cost")))
    for(i_sae in 1:n_sae) {
      temp[i_sae] <- rgamma(1, ((df_c_sae[i_sae])^2) / ((df_c_sae[i_sae]/sd_psa)^2),
                            df_c_sae[i_sae] / ((df_c_sae[i_sae]/sd_psa)^2))
    }
    temp
    
  } else {
    t(matrix(df_c_sae, nrow = n_sae, ncol = 1,
             dimnames = list(sae_names, "sae_cost")))
  }
  
) %>% add_item(
  # disutilities of serious adverse events
  v_u_sae_events = if(psa_bool) {
    temp <- t(matrix(nrow = n_sae, ncol = 1,
                     dimnames = list(sae_names, "sae_disutility")))
    for(i_sae in 1:n_sae) {
      temp[i_sae] <- pmin(rnorm(1, 
                                mean =  df_u_sae[i_sae], 
                                sd =  -df_u_sae[i_sae] / sd_psa), 0)
    }
    temp
    
  } else {
    t(matrix(df_u_sae, nrow = n_sae, ncol = 1,
             dimnames = list(sae_names, "sae_disutility")))
  }
)  %>% add_item(
  # rates of serious adverse events
  sae_events_rate = if(psa_bool) {
    temp <- -log(1-(rbeta(1,m_saes["AFFIRM Natalizumab IV300","alpha"], 
                          m_saes["AFFIRM Natalizumab IV300","beta"]))
    )/m_saes["AFFIRM Natalizumab IV300","follow_up"]
    
    temp
  } else { -log(1-(rbeta(1,m_saes["AFFIRM Natalizumab IV300","alpha"], 
                         m_saes["AFFIRM Natalizumab IV300","beta"]))
  )/m_saes["AFFIRM Natalizumab IV300","follow_up"]
  }
) %>% add_item(
  # Hazard ratios SAEs
  v_hr_saes = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # relative treatment effects log Hr from the NMA with Natalizumab as the reference
    temp[v_treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1 #assumption
    temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1 #assumption
#    temp[v_treatment_codes["Fingolimod"]] <- exp(rnorm(1,
#                                                       mean = m_hr_rrms_saes_fe["Fingolimod O0.5","mean"],
#                                                       sd = m_hr_rrms_saes_fe["Fingolimod O0.5","sd"]))
#    temp[v_treatment_codes["Alemtuzumab"]] <- exp(rnorm(1,
#                                                        mean = m_hr_rrms_saes_fe["Alemtuzumab IV12","mean"],
#                                                        sd = m_hr_rrms_saes_fe["Alemtuzumab IV12","sd"])) 
    temp[v_treatment_codes["Cladribine"]] <- 1 #assumption
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                      mean = m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"],
                                                      sd = m_hr_rrms_saes_fe["Ocrelizumab IV600","sd"])) 
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean = m_hr_rrms_saes_fe["Ofatumumab SC20","mean"], 
                                                       sd = m_hr_rrms_saes_fe["Ofatumumab SC20","sd"])) 
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean = m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"], 
                                                        sd = m_hr_rrms_saes_fe["Ocrelizumab IV600","sd"]))
#    temp[v_treatment_codes["PegIFN-β-1 SC 125μg"]] <- exp(rnorm(1,
#                                                                mean = m_hr_rrms_saes_fe["Peginterferon beta 1a SC125","mean"], 
#                                                                sd = m_hr_rrms_saes_fe["Peginterferon beta 1a SC125","sd"])) 
#    temp[v_treatment_codes["IFNβ-1a SC 44μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_saes_fe["Interferon beta 1a SC44","mean"], 
#                                                            sd = m_hr_rrms_saes_fe["Interferon beta 1a SC44","sd"]))
#    temp[v_treatment_codes["IFNβ-1a SC 22μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_saes_fe["Interferon beta 1a SC44","mean"], 
#                                                            sd = m_hr_rrms_saes_fe["Interferon beta 1a SC44","sd"])) # assumed same as 44μg
#    temp[v_treatment_codes["IFNβ-1a IM 30μg"]] <- exp(rnorm(1,
#                                                            mean = m_hr_rrms_saes_fe["Interferon beta 1a IM30","mean"],  
#                                                            sd = m_hr_rrms_saes_fe["Interferon beta 1a IM30","sd"]))
#    temp[v_treatment_codes["IFNβ-1b SC 250μg"]] <- exp(rnorm(1,
#                                                             mean = m_hr_rrms_saes_fe["Interferon beta 1b IM 250","mean"], 
#                                                             sd = m_hr_rrms_saes_fe["Interferon beta 1b IM 250","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 20mg"]] <- exp(rnorm(1,
#                                                                    mean = m_hr_rrms_saes_fe["Glatiramer acetate SC20","mean"],
#                                                                    sd = m_hr_rrms_saes_fe["Glatiramer acetate SC20","sd"]))
#    temp[v_treatment_codes["Glatiramer Acetate 40mg"]] <- exp(rnorm(1,
#                                                                    mean = m_hr_rrms_saes_fe["Glatiramer acetate SC40","mean"],
#                                                                    sd = m_hr_rrms_saes_fe["Glatiramer acetate SC40","sd"])) 
    temp
  } else {matrix(c(1, 1, 1, 1, 
#                   exp(m_hr_rrms_saes_fe["Fingolimod O0.5","mean"]), exp(m_hr_rrms_saes_fe["Alemtuzumab IV12","mean"]), 
                   1, 
                   exp(m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"]), 
                   exp(m_hr_rrms_saes_fe["Ofatumumab SC20","mean"]), exp(m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"]) #, 
#                   exp(m_hr_rrms_saes_fe["Peginterferon beta 1a SC125","mean"]), 
#                   exp(m_hr_rrms_saes_fe["Interferon beta 1a SC44","mean"]), exp(m_hr_rrms_saes_fe["Interferon beta 1a SC44","mean"]), 
#                   exp(m_hr_rrms_saes_fe["Interferon beta 1a IM30","mean"]), exp(m_hr_rrms_saes_fe["Interferon beta 1b IM 250","mean"]),
#                   exp(m_hr_rrms_saes_fe["Glatiramer acetate SC20","mean"]), exp(m_hr_rrms_saes_fe["Glatiramer acetate SC40","mean"])
                   ) ,
                 nrow = 1, ncol = n_treatments)
  }
) %>% add_item(
  # Hazard ratios for Placebo group when patients stop treatment
  v_hr_pbo = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = 4, dimnames = list(NULL, c("edss_increase", "arr", "discontinuation","saes")))
    # CDP6
    temp[,"edss_increase"] <- exp(rnorm(1,
                                       mean = m_hr_rrms_cdp6_fe["Placebo","mean"],
                                       sd = m_hr_rrms_cdp6_fe["Placebo","sd"]))
    # Annualized relapse rate
    temp[, "arr"] <- exp(rnorm(1,
                              mean = m_hr_rrms_arr_fe["Placebo","mean"],
                              sd = m_hr_rrms_arr_fe["Placebo","sd"]))
    # discontinuation due to SAEs
    temp[, "discontinuation"] <- exp(rnorm(1,
                                         mean = m_hr_rrms_discontinuation_fe["Placebo","mean"],
                                         sd = m_hr_rrms_discontinuation_fe["Placebo","sd"]))
    #SAEs
    temp[, "saes"] <- exp(rnorm(1,
                                       mean = m_hr_rrms_saes_fe["Placebo","mean"],
                                       sd = m_hr_rrms_saes_fe["Placebo","sd"]))
    temp
  }else {matrix(c(exp(m_hr_rrms_cdp6_fe["Placebo","mean"]), exp(m_hr_rrms_arr_fe["Placebo","mean"]), 
                  exp(m_hr_rrms_discontinuation_fe["Placebo","mean"]), exp(m_hr_rrms_saes_fe["Placebo","mean"]), 
                  nrow = 1, ncol = 4, dimnames = list(NULL, c("edss_increase", "arr", "discontinuation","saes"))))}

  ) %>% add_item(
    #proportions on 2nd and 3rd line treatments
   m_prop_2nd_3rd_trt = if(psa_bool) {
    
    temp<- matrix(nrow= 2, ncol = ncol(m_proportion_ontrtline), 
                  dimnames = list(c("2", "3"), colnames(m_proportion_ontrtline)))

    for(i_trt in 1:ncol(m_proportion_ontrtline)) {
      temp[1, i_trt] <- rbeta(1, shape1= m_proportion_ontrtline["alpha_2ndline", i_trt], shape2 = m_proportion_ontrtline["beta_2ndline", i_trt])
      temp[2, i_trt] <- rbeta(1, shape1= m_proportion_ontrtline["alpha_3rdline", i_trt], shape2 = m_proportion_ontrtline["beta_3rdline", i_trt])
    }
  
  temp
  } else {matrix(c(m_proportion_ontrtline["alpha_2ndline",] /m_proportion_ontrtline["beta_2ndline",], m_proportion_ontrtline["alpha_3rdline",] /m_proportion_ontrtline["beta_3rdline",]), 
                 byrow=TRUE, nrow= 2, ncol = ncol(m_proportion_ontrtline), 
                 dimnames = list(c("2", "3"), colnames(m_proportion_ontrtline)
                 ))
    
  }
  
)


#Put objects here that do not change as we loop through treatments for a patient
# This is called for each new patient so sample 1 from a distribution of choice
# for heterogeneous characteristics
#common_pt_inputs <- add_item(                            
#)

# Objects that depend on treatment
# Note that the current treatment is later initialised to trt
unique_pt_inputs <- add_item(edss_value = sample(size = 1, x = v_edss_values,  
                                                 # rrms larger sample size, option to use harrms
                                                 prob = rdiric(1, m_n_patients_edss["harrms",])),
                             #(male=0, female=1)
                             sex = sample(size = 1, x = c(0, 1), 
                                          prob = c(1 - prop_female, prop_female)),
                             n_relapse = 0,
                             n_sae_evts = 0,
                             n_edssinc_evts = 0,
                             n_edssdec_evts = 0,
                             t_progression = 0,
                             t_2ndline = 0, t_3rdline = 0, t_4thline = 0,
                             tto_dp1 = 0, tto_dp2 = 0, tto_dp3 = 0, tto_dp4 = 0, tto_dp5 = 0, tto_dp6 = 0, tto_dp7 = 0,  #time to progression for each event upto 7 events
                             tto_rel1 = 0, tto_rel2 = 0, tto_rel3 = 0, tto_rel4 = 0, tto_rel5 = 0, tto_rel6 = 0, tto_rel7 = 0,  #time to progression for each event upto 7 events
                             yrs_edss_0 = 0, yrs_edss_1 = 0, yrs_edss_2 = 0, yrs_edss_3 = 0, yrs_edss_4 = 0, yrs_edss_5 = 0, yrs_edss_6 = 0, yrs_edss_7 = 0, yrs_edss_8 = 0, yrs_edss_9 = 0, #  annual severity state counter
                             waning = 0, # waning flag set to zero will have no effect on HR x (1-waning)
                             spms_flag = sample(size = 1, 
                                                x = c(0, 1), # Random
                                                prob = c(1 - initial_spms_prop, initial_spms_prop)),
                             relapse_hospital_flag = sample(size = 1, 
                                                            x = c(0, 1), # Random
                                                            prob = c(1 - p_relapse_hospital,p_relapse_hospital)),
                             #current_treatment = "Natalizumab", # This is later changed to trt
                             current_treatment = sample(x = v_treatment_names, size=1, prob = m_prop_2nd_3rd_trt["3",]),
                             #current_treatment_code = 1, # This is later changed to code of trt
                             current_treatment_code = v_treatment_codes[current_treatment],
                             treatment_line = 2, # all patients starting on 2nd line treatment
                             n_switch = 0, # treatment switch counter for sense checking
                             years_on_treatment  = 0, #  annual treatment counter for drug acquisition costs
                             treatment_stop = 0, # all patients start on treatment, flag set to 1 when edss is 7+
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
              # Current treatment code allows treatment switching 
              # (i.e., not the trt which is only initial assignment)
              # MS registry time to relapse HA RRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (v_baseline_harrms_larr["rate"] +
                                           v_baseline_harrms_larr["BaselineWebEDSS"] * (edss_value)  +
                                           v_baseline_harrms_larr["DMTNatalizumab"]) +
                # MS registry time to relapse SPMS log rate
                spms_flag * (v_baseline_spms_larr["rate"] +
                               v_baseline_spms_larr["BaselineWebEDSS"] * (edss_value)),
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              hr = (1 - spms_flag) * ((1 - treatment_stop) * (v_hr_arr[current_treatment_code]*(1-waning)) +
              treatment_stop * v_hr_pbo[, "arr"]) + spms_flag
            )
            edss_increase <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss increase HARRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                                           v_baseline_harrms_edss_increase["BaselineWebEDSS"] * (edss_value) + 
                                           v_baseline_harrms_edss_increase["DMTNatalizumab"]) + 
                # MS registry time to edss increase SPMS log rate
                spms_flag * (v_spms_edss_increase["rate"] + 
                               v_spms_edss_increase["BaselineWebEDSS"]*(edss_value) ),    
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              hr = (1 - spms_flag ) * ((1 - treatment_stop) * (v_hr_edss_increase[current_treatment_code])*(1-waning) + 
                                         treatment_stop * v_hr_pbo[, "edss_increase"]) + spms_flag
            )
            edss_decrease <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss decrease All RRMS patients log rate 
              coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                           v_rrms_edss_decrease["BaselineWebEDSS"] * (edss_value)) + 
                #we are setting a very high event time to edss decrease for SPMS patients
                (spms_flag * -10)
            )
            
            progression <- draw_tte(
              1,
              dist = "exp",
              # MS registry time to progression All RRMS patients to SPMS log rate on natalizumab
              # Below is zero if already progressed to SPMS
              coef1 = (1 - spms_flag) * (v_spms_progression["rate"] +
                                           v_spms_progression["BaselineWebEDSS"] * (edss_value) + 
                                           v_spms_progression["DMTNatalizumab"]) + 
                #setting a very high event time to progression for SPMS patients
                (spms_flag * -10)
            )
            death <- draw_tte_new(
              1,
              dist = "pwexp",
              # population mortality drawn using piece wise exponential (male=0, female=1)
              coef1 = c(log((1 - sex) * m_r_mortality[(age + 1):nrow(m_r_mortality), c("male")] +
                              sex *  m_r_mortality[(age + 1):nrow(m_r_mortality), c("female")],
              )),
              time = c(m_r_mortality[(age + 1):nrow(m_r_mortality)]) ,
              # MS specific mortality multiplier
              hr = v_hr_smr_edss[edss_value == v_edss_values]
            ) - age
            serious_adverse_event <- draw_tte(
              1,
              dist = "exp",
              # SAE rates same for all treatments
              # change to include HR from safety NMA
              coef1 = log(sae_events_rate),
              hr=v_hr_saes
            )
            treatment_switch <- draw_tte(
              1,
              dist = "exp",
              # Discontinuation rates depend on current treatment
              coef1 = ((1 - treatment_stop)  * log(discontinuation_rate)) + (treatment_stop * log(0.01)),
              hr= ((1 - treatment_stop)  * v_hr_discontinuation[current_treatment_code]) + (treatment_stop * 0.01)
            )
          })


evt_react_list <-
  add_reactevt(name_evt = "initialise",
               input = {
                 # This ensures that the current treatment is the initial treatment
                 # of this strategy.
                 current_treatment = trt
                 current_treatment_code = v_treatment_codes[trt]
                 modify_item(list(current_treatment = current_treatment,
                                  current_treatment_code = current_treatment_code
                 ))
                 modify_event(list(#future death events do not exceed time horizon
                                  "death"= min(cur_evtlist[["death"]], time_horizon)
                 ))
                 # annual cost events
                 new_event(list(
                   "annual_treatment" = curtime 
                 ))
               }) %>%
  add_reactevt(name_evt = "age",
               input = {
                 age = initial_age + round(curtime)
                 modify_item(list(age = age
                 ))
               }) %>%
  
  
  #add annual cost events
  add_reactevt(name_evt = "annual_treatment",
               input = { 
                 years_on_treatment = years_on_treatment  + 1
                 age = initial_age + round(curtime)
                 modify_item(list(
                   years_on_treatment = years_on_treatment,
                   age = age
                 ))
                 
                 # time on treatment-line counter 
                 if(treatment_line == 2 & spms_flag == 0 & treatment_stop == 0) {
                   modify_item(list(t_2ndline = years_on_treatment))
                 } else if (treatment_line == 3 & spms_flag == 0 & treatment_stop == 0){
                   modify_item(list(t_3rdline = years_on_treatment))
                 }else if (treatment_line == 4 & spms_flag == 0 & treatment_stop == 0){
                   modify_item(list(t_4thline = years_on_treatment))
                 }
                 
                 # treatment waning flag sets weight to 0.25 at 3>= t <5 or 0.5 at t >=5 otherwise 0
                 #                 if (spms_flag == 0 & treatment_stop == 0) {
                 #                   if (years_on_treatment <= 2){modify_item(list(waning = 0))
                 #                   } else if (years_on_treatment > 2 & years_on_treatment <5 ){modify_item(list(waning = 0.25))
                 #                   } else if (years_on_treatment >= 5){modify_item(list(waning = 0.5))}
                 #                 } else if(spms_flag == 1 | treatment_stop == 1) {modify_item(list(waning = 0))}
                 
                 if (edss_value <= 6 & treatment_stop == 0) {new_event(list(
                   "annual_treatment" = curtime + 1
                 ))}
                 
               }
  ) %>%
  add_reactevt(name_evt = "relapse",
               input = {
                 n_relapse= n_relapse + 1
                 age = initial_age + round(curtime)
                 modify_item(list(n_relapse= n_relapse,
                                  age = age))
                 
                 #  next time to relapse
                 new_event(list(
                   "relapse" = curtime + draw_tte(
                     1, 
                     dist = "exp", 
                     coef1 = (1 - spms_flag) * (v_baseline_harrms_larr["rate"] +
                                                  v_baseline_harrms_larr["BaselineWebEDSS"] * (edss_value)  +
                                                  v_baseline_harrms_larr["DMTNatalizumab"]) +
                       spms_flag * (v_baseline_spms_larr["rate"] +
                                      v_baseline_spms_larr["BaselineWebEDSS"] * (edss_value)),
                     hr = (1 - spms_flag) * ((1 - treatment_stop) * ( v_hr_arr[current_treatment_code]*(1 - waning)) +
                                               treatment_stop * v_hr_pbo[, "arr"]) + spms_flag
                 )))
               }) %>%
  add_reactevt(name_evt = "edss_increase",
               input = {
                 edss_value = min(edss_value + 1, max(v_edss_values))
                 age = initial_age + round(curtime)
                 n_edssinc_evts = n_edssinc_evts + 1
                 modify_item(list(edss_value = edss_value,
                                  age = age,
                                  n_edssinc_evts = n_edssinc_evts))
                 if (edss_value > 6 & treatment_stop == 0) {modify_item(list(treatment_stop = 1))}
                 
                 # simulate the next time to EDSS increase
                 if (edss_value < 9) {new_event(list(
                   "edss_increase" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                                                  v_baseline_harrms_edss_increase["BaselineWebEDSS"] * (edss_value) ) + 
                       v_baseline_harrms_edss_increase["DMTNatalizumab"] +
                       spms_flag * (v_spms_edss_increase["rate"] +
                                      v_spms_edss_increase["BaselineWebEDSS"] * (edss_value)),  
                     hr = (1 - spms_flag ) * ((1 - treatment_stop) * ( v_hr_arr[current_treatment_code]*(1 - waning)) + 
                                                treatment_stop * v_hr_pbo[, "edss_increase"]) + spms_flag
                   )
                 ))}
               }) %>%
  add_reactevt(name_evt = "edss_decrease",
               input = {
                 edss_value = max(edss_value - 1, min(v_edss_values))
                 age = initial_age + round(curtime)
                 n_edssdec_evts = n_edssdec_evts + 1
                 modify_item(list(edss_value = edss_value,
                                  age = age,
                                  n_edssdec_evts = n_edssdec_evts))
                 if (edss_value > 6 & treatment_stop == 0) {modify_item(list(treatment_stop = 1))}
                 
                 #  next time to EDSS decrease
                 if (edss_value > 0) {new_event(list(
                   "edss_decrease" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     # EDSS decrease  
                     coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                                  v_rrms_edss_decrease["BaselineWebEDSS"]*(edss_value) )  + 
                       #setting a very high event time to edss decrease for SPMS patients
                       (spms_flag * -10 )
                   )
                 ) )}
               }) %>%
  add_reactevt(name_evt = "progression",
               # Progression can only occur once so just change the SPMS status
               input = {
                 age = initial_age + round(curtime)
                 modify_item(list(spms_flag = 1,
                                  age = age))
               }) %>%
  add_reactevt(name_evt = "death",
               # Death only occurs once.
               # Set current time to Inf so simulation ends.
               input = {
                 age = initial_age + round(curtime)
                 modify_item(list(curtime = Inf ,
                                  age = age
                 )) 
               }) %>%
  add_reactevt(name_evt = "serious_adverse_event",
               input = {  
                 age = initial_age + round(curtime)
                 n_sae_evts = n_sae_evts + 1
                 modify_item(list(age = age,
                                  n_sae_evts = n_sae_evts))           
                 
                 # Sample next serious event. T
                 new_event(list(
                   "serious_adverse_event" = curtime + draw_tte(
                     1,
                     dist = "exp",
                     # SAE rates same for all treatments
                     # change to include HR from safety NMA
                     coef1 = log(sae_events_rate),
                     hr= ((1 - treatment_stop) * v_hr_saes) + (treatment_stop *  v_hr_pbo[, "saes"])
                   )
                 ))
               }) %>%
  # Patients switch only twice (Starting from 2nd line) so 4th line is final (As in protocol)
  add_reactevt(name_evt = "treatment_switch",
               input = {
                 treatment_line = treatment_line + 1
                 discontinuation_rate = (treatment_line < 4) *  discontinuation_rate
                 v_prior_treatment_indices[current_treatment_code] = 1
                 n_switch = n_switch + 1
                 if(n_switch == 1) {
                   current_treatment = sample(x=v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]], 
                                              size=1, prob = m_prop_2nd_3rd_trt["3",current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]])
                 } else if (n_switch >= 2) {
                   current_treatment = sample(x = v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]],
                                              size = 1) 
                 }
                 
                 current_treatment_code = v_higher_line_treatment_codes[current_treatment]
                 
                 age = initial_age + round(curtime)
                 modify_item(list(current_treatment = current_treatment,
                                  current_treatment_code = current_treatment_code,
                                  v_prior_treatment_indices = v_prior_treatment_indices,
                                  treatment_line = treatment_line,
                                  discontinuation_rate = discontinuation_rate,
                                  n_switch = n_switch,
                                  years_on_treatment = 0,   
                                  age = age))
                 
                 # Sample next discontinuation event. This will be zero if treatment_line >=4
                 new_event(list(
                   "treatment_switch" = curtime + draw_tte(
                     1,
                     dist = "exp",
                     coef1 = ((1 - treatment_stop)  * log(discontinuation_rate)) + (treatment_stop * log(0.01)),
                     hr= ((1 - treatment_stop)  * v_hr_discontinuation[current_treatment_code]) + (treatment_stop * 0.01)
                   )
                 ))

               })

# Add ongoing utilities
util_ongoing <- add_util(evt = v_event_names,
                         trt = v_treatment_names,
                         util = (1 - spms_flag) * v_u_rrms_edss[edss_value == v_edss_values] +
                           spms_flag * v_u_spms_edss[edss_value == v_edss_values]
)

# Add ongoing costs
cost_ongoing <- add_cost(evt = v_event_names,
                         trt = v_treatment_names,
                         cost = (1 - spms_flag) * (v_c_rrms_edss[edss_value == v_edss_values]) +
                           spms_flag * v_c_spms_edss[edss_value == v_edss_values])

##Add instant costs
cost_instant <-
  add_cost(evt = c("annual_treatment") ,
           trt = v_treatment_names,
           cost = if(years_on_treatment <= 1) {(1- treatment_stop ) *(
             (1 - spms_flag) * (df_c_tx["cost_annual_yr1",current_treatment_code] +
               v_c_trt_admin["cost_annual_yr1",current_treatment_code] +
               v_c_trt_monitor["cost_annual_yr1",current_treatment_code]) +
               spms_flag * (((1-p_siponimod) * df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
                              (p_siponimod * df_c_spms["cost_annual_yr1","Siponimod"])))
           } else if(years_on_treatment == 2){(1- treatment_stop ) * (
             (1 - spms_flag) * (df_c_tx["cost_annual_yr2",current_treatment_code] +
               v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
               v_c_trt_monitor["cost_annual_yr2",current_treatment_code])+
               spms_flag * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                              (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
           } else if(years_on_treatment == 3){(1- treatment_stop ) * (
             (1 - spms_flag) *((df_c_tx["cost_annual_yr2",current_treatment_code] +
                v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
                v_c_trt_monitor["cost_annual_yr2",current_treatment_code]) * df_c_tx_load[3,current_treatment_code])+
              spms_flag *(((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
           } else if(years_on_treatment == 4){ (1- treatment_stop ) *(
             (1 - spms_flag) *((df_c_tx["cost_annual_yr2",current_treatment_code] +
                v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
                v_c_trt_monitor["cost_annual_yr2",current_treatment_code]) * df_c_tx_load[4,current_treatment_code])+
               spms_flag *(((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                             (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
           } else {(1- treatment_stop ) *(
             (1 - spms_flag) *((df_c_tx["cost_annual_yr2",current_treatment_code] +
                v_c_trt_admin["cost_annual_yr2",current_treatment_code] +
                v_c_trt_monitor["cost_annual_yr2",current_treatment_code]) * df_c_tx_load[5,current_treatment_code])+
               spms_flag *(((1 - p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                             (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
           }
  ) %>% 
  add_cost(evt = "relapse" ,
           trt = v_treatment_names,
           cost = (relapse_hospital_flag * v_c_relapse[6])+
             ((1-relapse_hospital_flag) * v_c_relapse[4]) ) %>% 
  add_cost(evt = "serious_adverse_event" ,
           trt = v_treatment_names,
           cost = (1- treatment_stop ) * sum(v_c_sae_events * v_weight_sae)) 

# Add instant utilities
util_instant <-   add_util(evt = "relapse" ,
                           trt = v_treatment_names,
                           util = u_relapse) %>% 
  add_util(evt = "serious_adverse_event" ,
           trt = v_treatment_names,
           util = sum(v_u_sae_events*v_weight_sae)) 


# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <- system.time({
  results <- RunSim_new(
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = TRUE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    debug = TRUE,
    trt_list = v_treatment_names,             # intervention list
    common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
    #common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
    unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
    init_event_list = init_event_list,        # initial event list
    evt_react_list = evt_react_list,          # reaction of events
    util_ongoing_list = util_ongoing,
    cost_ongoing_list = cost_ongoing,
    util_instant_list = util_instant,
    cost_instant_list = cost_instant,
    ncores = 4,                               # number of cores to use, recommended not to use all
    drc = 0.035,                              # discount rate for costs
    drq = 0.035,                               # discount rate for qaly/lys
    input_out=c("age", "sex", "spms_flag", "edss_value", "n_relapse", "n_sae_evts", "n_edssinc_evts", "n_edssdec_evts", "current_treatment_code",
                "treatment_line", "n_switch", "years_on_treatment", "treatment_stop", "t_2ndline", "t_3rdline", "t_4thline", "waning"),
    v_random_seed_sequence = c(1:n_samples), # Sequence of random seeds
    save_input_matrix = TRUE # Export the input matrix for EVPPI
  )
})



# Summarise the results
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


#drop natalizumab-biosimilar-SC
total_costs <- total_costs[,-c(4)]
total_qalys <- total_qalys[,-c(4)]

results_table <- summarise_ce_results(total_qalys = total_qalys, 
                                      total_costs = total_costs,
                                      reference = "Natalizumab-IV")


setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/too large for git")
write.csv(results_table, "results/results_public/results_s1_public_prices_100x100_01_may.csv")
#write.csv(results_table, "results/results_conf/results_s1_conf_prices_100x100_01_may.csv")
setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/MS-MTA")
