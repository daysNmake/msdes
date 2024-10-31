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
