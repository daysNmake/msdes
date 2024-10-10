## updated to match descem_rrms_5_1.R
# load packages
library(descem)
library(MASS)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)

###load data
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


#edss costs direct medical costs Tyas etal. 2005
c_m_edss <- read.csv("data/df_c_directmedical_edss.csv", header = T, row.names = 1) #inflated to 2022/2023 Edss6 is an average of edss6 and 6.5
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
