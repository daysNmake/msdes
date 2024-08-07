                     ### MS MTA Patient Simulation Model ###
                       ###  Howard Thom & Ayman Sadek ###

### data :
    #h# RRMS natural history average age of onset 28 years {Palace etal.}
    #h# SPMS natural history {~DUMMY data~}
    #h# Initial severity (EDSS) distribution {~DUMMY data~}
    #h# Proportion of patients starting in SPMS {~DUMMY data~}
    #h# Rates of progression on SPMS {~DUMMY data~}
    #h# Baseline annual relapse rate  {Placebo arm of the clarabidine trial}
    #h# Proportion of patients having relapses leading to hospitalisation {~DUMMY data~}
    #m# Rates of mortality for the general population  {UK Lifetables}
    #m# MS specific standardised mortality rate {Manouchehrinia 2016}
    #t# Treatment specific Disability progression HRs for RRMS Patients {~DUMMY data~}
    #t# Treatment specific Annual Relapse Rates HRs for RRMS Patients {~DUMMY data~}
    #t# Treatment specific discontinuation as proxy for switching {~DUMMY data~}
    #t# Treatment specific Rates of Adverse Events  {TA616 & ~DUMMY data~}
    #c# EDSS direct medical costs Tyas etal. 2005
    #c# Treatment (drug) costs from TA616 TA312 and Beta-Interferon-MTA
    #c# Treatment administration costs from TA616 TA312
    #c# Treatment monitoring costs from TA616 and Beta-Interferon-MTA
    #c# Relapse Costs {Hawton 2016} {Karampampa 2012}
    #c# Adverse Events Costs{TA616 }
    #u# EDSS utilities Orme etal. 2007 
    #u# Adverse Events disutilities {TA616 }

### Natural history:
    # Patients start in either RRMS or SPMS according to an initial dist. of severity
    # RRMS patients experience severity increase 
    # RRMS patients progress onto SPMS
    # SPMS patients experience SPMS severity decrease

### treatments:
    # 2nd line;         13 treatments
    # 3rd and 4th line;  4 treatments

### Treatment Effects:
    # HR Disability Progression
    # HR Annual Relapse Rates
    # Discontinuation Rates (proxy for switching)
    # Rates of Adverse Events

### Mortality :
    # initial age and sex dependent
    # MS specific Standardized Mortality ratio multiplier

### Individual patient logical flags/counters:
    # current edss
    # next edss
    # no of relapses
    # SPMS
    # relapse hospitalisation 
    # current_treatment
    # treatment_line
    # no of treatment switches 


### Features not modeled/ to be added? :
    # Time horizon
    # higher_line treatments throws up an (Error in discontinuation_rate[, current_treatment] : 
        #subscript out of bounds) when no of trts does not match the no of 2nd line trts
    # SPMS patients could experience severity increase 
    # Relapse rates from year 2 onwards Baseline_ARR(t-1)*RR (5-year RR from Tremlett 2010 as in TA616)
    # Could the duration of relapses be used to limit the number of relapses happening?    
    # NMA data: HRs ARRs AEs
    # TA616 adjusted Progression Hazard Ratios for the highly active subgroups using HR=ln(1-Pa)/ln(1-Pb)
    # Costs utilities per annual cycle (yr1 vs yr2+)
    # Inflate costs to 2024
    # Relapse costs specific to Oral vs intravenous steroids
    # SMR changes according to disease severity
    # Currently one AE modeled, would it be possible to loop through them?
    # Costs and dis utilities once a final list of AEs agreed

### Not sure about:
    # How to interpret event times and track the age of patients in the model? 
    # Does mortality change depending on age of patients over the life course?
    # competing risks?




library(descem)
library(readr)
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

## simulation
n_patients <- 10
n_samples <- 1

## 13 treatments at 2nd line
treatment_names <- c("Natalizumab","Natalizumab biosimilar","Fingolimod",
                     "Alemtuzumab","Cladribine", "Daclizumab" , "Dimethyl fumarate",
                     "PegIFN-β-1 SC 125μg", "IFNβ-1a SC 44μg" , "IFNβ-1a SC 22μg", 
                     "IFNβ-1a IM 30μg", "IFNβ-1b SC 250μg", "Glatiramer Acetate" )

n_treatments <- length(treatment_names)

higher_line_treatments <- c("Natalizumab","Natalizumab biosimilar","Fingolimod",
                            "Alemtuzumab","Cladribine", "Daclizumab" , "Dimethyl fumarate",
                            "PegIFN-β-1 SC 125μg", "IFNβ-1a SC 44μg" , "IFNβ-1a SC 22μg", 
                            "IFNβ-1a IM 30μg", "IFNβ-1b SC 250μg", "Glatiramer Acetate" )

## model's events population
event_names <- c("initial", "relapse", "edss_increase", "edss_decrease", "progression","treatment_switch", "ae" , "death")

initial_age <- 20 
time_horizon <- 80 # Years so lifetime
prop_female <- 0.50

## Define the possible EDSS values
edss_values <- c(0:9)
names(edss_values) <- paste("EDSS", edss_values)

## RRMS EDSS Natural History Palace etal
rrms_edss_natural_history <- read.csv("data/df_r_nh_rrms_bc_ta616.csv", header = T, row.names = 1)
colnames(rrms_edss_natural_history) <- row.names(rrms_edss_natural_history) <-paste("EDSS",edss_values)

## Initial distribution of EDSS ~DUMMY DATA~
initial_edss_distribution <- c(0.1, 0.5, rep(0.4/8, 8))

## Initial proportion with SPMS ~DUMMY DATA~
initial_spms_prop <- 0.2

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
r_mortality <- read.csv("data/df_all_cause_mortality.csv", header = T, row.names = 1)
r_mortality <- r_mortality[,c(2,8)]
colnames(r_mortality) <- c("male", "female")

#MS specific standardised mortality rate Manouchehrinia 2016
smr <- as.double(c(df_init_params["smr"], sqrt(437)*(df_init_params["smr_uci"]-df_init_params["smr_lci"])/3.92))

#edss costs direct medical costs Tyas etal. 2005
c_m_edss <- read.csv("data/df_c_directmedical_edss.csv", header = T, row.names = 1) #need to be inflated
edss_med_costs <- t(c_m_edss[1:10,5:6])

# Annual treatment, administration and monitoring costs TA616 TA312 Beta-IFN-MTA
c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
c_tx <- c_tx[-2,]
c_tx_admin <- t(read.csv("data/df_c_drugs_admin.csv", header = T, row.names = 1))
c_tx_admin <- c_tx_admin[-2,]
c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
c_tx_monitor <- c_tx_monitor[-2,]

## Relapse Costs (Hawton 2016b and Karampampa 2012c)
c_relapse <-read.csv("data/df_c_relapse.csv", header = T, row.names = 1)
c_relapse <- c_relapse[5:9,1:2]

## Adverse Events costs TA616
c_ae <-read.csv("data/df_c_disutilities_ta616.csv", header = T, row.names =1)

## edss utilities columns 5&6 Orme etal 2007
u_edss <- read.csv("data/df_u_edss_ta616.csv", header = T, row.names = 1)
edss_utilities <- t(u_edss[,5:6])

## Relapse utilities
u_relapse <-read.csv("data/df_u_relapse.csv", header = T, row.names = 1)

## Adverse Events disutilities TA616 
u_ae <-read.csv("data/df_c_disutilities_ta616.csv", header = T, row.names =1)



#Put objects here that do not change on any patient or intervention loop
common_all_inputs <-add_item(
  treatment_names = treatment_names, # fixed
  higher_line_treatments = higher_line_treatments, # fixed
  event_names = event_names, # fixed
  n_treatments = n_treatments, # fixed
  edss_values = edss_values, # fixed
  #rrms_edss_natural_history = rrms_edss_natural_history, #random
  #initial_edss_distribution=initial_edss_distribution, #random
  #edss_utilities = edss_utilities, #random
  #edss_med_costs = edss_med_costs, #random
  c_tx = c_tx, # fixed
  c_tx_admin = c_tx_admin, # fixed
  c_tx_monitor = c_tx_monitor, # fixed
  #c_relapse = c_relapse, #random
  #u_relapse = u_relapse, #random
  #c_ae = c_ae,#random
  #u_ae = u_ae, #random
  #r_ae, #random
  # rates of ae?
  r_bl_rrms_rr = r_bl_rrms_rr, # fixed
  r_mortality = r_mortality, # fixed
  smr = smr, # fixed
  initial_spms_prop = initial_spms_prop, # fixed
  relapse_hospital_prop = relapse_hospital_prop # fixed
  )  %>% add_item(
    # natural history of rrms 10x10 matrix
    baseline_rrms_edss_increase = if(psa_bool) {
      temp <- rrms_edss_natural_history
      for(i_edss in 1:nrow(temp)) {
        for (j_edss in 1:ncol(temp)){
          # Rates increase with higher EDSS, always positive
          temp[i_edss,j_edss] <- exp(rnorm(1,
                                           mean = log(temp[i_edss,j_edss]),
                                           sd = 0.01))}
      }
      temp
    } else {
      rrms_edss_natural_history
    }
    
    ) %>% add_item(
      # dummy data, replace with natural history of spms 10x10 matrix
      rrms_edss_decrease = if(psa_bool) {
        temp <- matrix(nrow = length(edss_values), ncol = length(edss_values),
                       dimnames = list(names(edss_values), names(edss_values)))
        for(i_edss in 1:nrow(temp)) {
          for (j_edss in 1:ncol(temp)){
            # Rates increase with higher EDSS, always positive
            temp[i_edss,j_edss] <- exp(rnorm(1,
                                             mean = log(0.1 - 0.001 *i_edss*j_edss),
                                             sd = 0.01))}
        }
        temp
      } else {
        temp <- matrix(nrow = length(edss_values), ncol = length(edss_values),
                       dimnames = list(names(edss_values), names(edss_values)))
        for(i_edss in 1:nrow(temp)) {
          for (j_edss in 1:ncol(temp)){
            # Rates increase with higher EDSS, always positive
            temp[i_edss,j_edss] <- exp(rnorm(1,
                                             mean = log(0.1 - 0.001 *i_edss*j_edss),
                                             sd = 0.01))}
        }
        temp
      }
    ) %>% add_item(
      
      # Dummy rate of progression to SPMS
      spms_progression =  if(psa_bool) {exp(rnorm(1,
                                                  mean = log(0.002),
                                                  sd = 0.0001))
      } else {0.002}
      
    ) %>% add_item(
      
      # Baseline annualised relapse rate placebo arm of the Clarabidine Trial TA616
      # could add baseline rates from all trials and select based on trt
      baseline_arr = if(psa_bool) {
         exp(rnorm(1,
                  mean = log(r_bl_rrms_rr[1]),
                  sd = 0.01))
      } else {r_bl_rrms_rr[1]}
      
    )%>% add_item(
    
      # Hazard ratios reducing risk of EDSS increase
    HR_int = if(psa_bool) {
      temp <- matrix(nrow = 1, ncol = n_treatments,
                     dimnames = list(NULL, treatment_names))
      # Dummy values but final will come from the NMA
      temp[, "Natalizumab"] <- exp(rnorm(1,
                                   mean = log(0.9),
                                   sd = 0.01))
      temp[, "Natalizumab biosimilar"] <- exp(rnorm(1,
                                  mean = log(0.9),
                                  sd = 0.01))
      temp[, "Fingolimod"] <- exp(rnorm(1,
                                   mean = log(0.4),
                                   sd = 0.01))
      temp[, "Alemtuzumab"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "Cladribine"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "Daclizumab"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "Dimethyl fumarate"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "PegIFN-β-1 SC 125μg"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "IFNβ-1a SC 44μg"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "IFNβ-1a SC 22μg"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "IFNβ-1a IM 30μg"] <- exp(rnorm(1,
                                  mean = log(0.4),
                                  sd = 0.01))
      temp[, "IFNβ-1b SC 250μg"] <- exp(rnorm(1,
                                             mean = log(0.4),
                                             sd = 0.01))
      temp[, "Glatiramer Acetate"] <- exp(rnorm(1,
                                             mean = log(0.4),
                                             sd = 0.01))
      temp
    } else {matrix(c(0.9, 0.9, rep(0.4, 11)), nrow = 1, ncol = n_treatments,
                   dimnames = list(NULL, treatment_names))}
    
  ) %>% add_item(
    
    # Hazard ratios on annualised relapse rate
    hr_arr = if(psa_bool) {
      temp <- matrix(nrow = 1, ncol = n_treatments,
                     dimnames = list(NULL, treatment_names))
      # Dummy values but final will come from the NMA
      temp[, "Natalizumab"] <- exp(rnorm(1,
                                         mean = log(0.9),
                                         sd = 0.01))
      temp[, "Natalizumab biosimilar"] <- exp(rnorm(1,
                                                    mean = log(0.9),
                                                    sd = 0.01))
      temp[, "Fingolimod"] <- exp(rnorm(1,
                                        mean = log(0.4),
                                        sd = 0.01))
      temp[, "Alemtuzumab"] <- exp(rnorm(1,
                                         mean = log(0.4),
                                         sd = 0.01))
      temp[, "Cladribine"] <- exp(rnorm(1,
                                        mean = log(0.4),
                                        sd = 0.01))
      temp[, "Daclizumab"] <- exp(rnorm(1,
                                        mean = log(0.4),
                                        sd = 0.01))
      temp[, "Dimethyl fumarate"] <- exp(rnorm(1,
                                               mean = log(0.4),
                                               sd = 0.01))
      temp[, "PegIFN-β-1 SC 125μg"] <- exp(rnorm(1,
                                                 mean = log(0.4),
                                                 sd = 0.01))
      temp[, "IFNβ-1a SC 44μg"] <- exp(rnorm(1,
                                             mean = log(0.4),
                                             sd = 0.01))
      temp[, "IFNβ-1a SC 22μg"] <- exp(rnorm(1,
                                             mean = log(0.4),
                                             sd = 0.01))
      temp[, "IFNβ-1a IM 30μg"] <- exp(rnorm(1,
                                             mean = log(0.4),
                                             sd = 0.01))
      temp[, "IFNβ-1b SC 250μg"] <- exp(rnorm(1,
                                              mean = log(0.4),
                                              sd = 0.01))
      temp[, "Glatiramer Acetate"] <- exp(rnorm(1,
                                                mean = log(0.4),
                                                sd = 0.01))
      temp
    } else {matrix(c(0.9, 0.9, rep(0.4, 11)), nrow = 1, ncol = n_treatments,
                   dimnames = list(NULL, treatment_names))}
  ) %>% 
  # Treatment switch follows discontinuation rates
  # Dummy values assume the discontinuation doesn't depend on line of therapy
  # (E.g., cladribine disctoninuation rate is same at 1st and 2nd line)
  # This can be changed by adding line specific discontinuation rates
  add_item(
    discontinuation_rate = if(psa_bool) {
      temp <- matrix(nrow = 1, ncol = n_treatments,
                     dimnames = list(NULL, treatment_names))
      for(i in 1:length(treatment_names)) {
        temp[i] <- exp(
          rnorm(1, 
                # Arbitrarily setting the higher treatments (Clad/Alem) to have higher
                # discontinuation than lower treatments (nat/nat bio, fing)
                mean = log(0.05 + 0.005 * i),
                sd = 0.001)
        )
      }
      temp
    }
    else {0.05 + 0.005 * 1:length(treatment_names)}
    
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
  
  ) %>% add_item(  
    # Medical costs for RRMS Tyas et al these should inflated and drawn from a gamma distribution & replicate for non-medical costs
    c_rrms_edss = if(psa_bool) {
      temp <- edss_med_costs[1,]
      for(j_edss in 1:length(temp)) {
        temp[j_edss] <- pmax(rnorm(1, mean =  edss_med_costs[1,j_edss], sd =  edss_med_costs[2,j_edss]),0)
      }
      temp
      
    } else {
      edss_med_costs[1,]
    }
    
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
         pmax(rnorm(1, mean =  u_relapse[1,1], sd =  u_relapse[1,2]),0)
        
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
common_pt_inputs <- add_item(age = initial_age, # Fixed
                             sex = sample(size = 1, x = c("male", "female"), # Random 
                                          prob = c(1 - prop_female, prop_female)),
                             death = draw_tte(1, dist = "exp",
                                            coef1 = r_mortality[age, sex == c("male", "female")], hr=smr[1])
                            )

#Put objects here that change as we loop through treatments for each patient 
# (e.g. events can affect fl.tx, but events do not affect nat.os.s)
unique_pt_inputs <- add_item(edss_value = sample(size = 1, x = edss_values, # Random 
                                                 prob = initial_edss_distribution),
                             edss_i = edss_value,
                             n_relapse = 0,
                             spms_flag = sample(size = 1, x = c(0, 1), # Random
                                                prob = c(1 - initial_spms_prop, initial_spms_prop)),
                             relapse_hospital_flag = sample(size = 1, x = c(0, 1), # Random
                                                prob = c(1 - relapse_hospital_prop, relapse_hospital_prop)),
                             current_treatment=trt, 
                             treatment_line = 2,
                             n_switch = 0
                             ) 

# Add time to event for each patient for each of the treatments
init_event_list <- 
  add_tte(trt=treatment_names, evts = event_names,
          input={ #interventions
    initial <- 0
    

    relapse <- draw_tte(
      1, 
      dist = "exp", 
      # Baseline rate modified by HR of current treatment
      # Current treatment to be implemented in later version
      coef1 = baseline_arr, hr=hr_arr[, current_treatment == treatment_names])
    
    #edss decrease if spms else edss increase & spms progression
    if (spms_flag) {
             edss_decrease <- draw_tte(
            1, 
            dist = "exp",
            # Baseline rate related to current EDSS
            # Below is zero if spms_flag is 1 since patients should have EDSS decease if SPMS
            # do  this flags work? Ayman
            coef1 = (1 - spms_flag) * rrms_edss_decrease[edss_value == edss_values, edss_value == edss_values])
     } else {
              edss_increase <- draw_tte(
                1, 
                dist = "exp",
                coef1 = baseline_rrms_edss_increase[edss_value == edss_values, edss_value == edss_values],
                   hr = HR_int[, current_treatment == treatment_names] 
              )
              progression <- draw_tte(
                1,
                dist = "exp",
                # Below is zero if already progressed to SPMS
                coef1 = (1 - spms_flag) * spms_progression
              )
    }
    
    treatment_switch <- draw_tte(
      1,
      dist = "exp",
      # Discontinuation rates can vary by current treatment
      coef1 = discontinuation_rate[,current_treatment]
    )
    
    ae <- draw_tte(1, dist = "exp",
                   #Infusion site reaction adverse event. To repeat for each ae ( in a loop?)
                   coef1 = adverse_events_rates[1, current_treatment == treatment_names])
  }) 



# Add a reaction to the events
# E.g. change treatment status or EDSS level
evt_react_list <-
  add_reactevt(name_evt = "initial",
               input = {
                 modify_item(list(current_treatment = trt))
                 
               }) %>%
  add_reactevt(name_evt = "relapse",
               input = {
                 modify_item(list(n_relapse= n_relapse + 1))
                 
                 # Currently no impact on costs or utilities of relapse
                 # Just need to simulate the next time to relapse
                 new_event(list(
                   "relapse" = curtime + draw_tte(
                     1, 
                     dist = "exp", 
                     coef1 = baseline_arr, hr = hr_arr[, current_treatment == treatment_names])
                 ))
                 
               })%>%
  add_reactevt(name_evt = "edss_increase",
               input = {
                 modify_item(list(edss_i = edss_value,
                                  edss_value = min(edss_value + 1, max(edss_values))
                                  ))
                 
                 # Need to simulate the next time to EDSS increase
                 if (edss_i < 9 | edss_value < 9) {new_event(list(
                   "edss_increase" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     coef1 = baseline_rrms_edss_increase[edss_i == edss_values, edss_value == edss_values],
                        hr = HR_int[, current_treatment == treatment_names]
                   )
                 ))}
               }) %>%
  
      add_reactevt(name_evt = "edss_decrease",
                   input = {
                            modify_item(list(edss_i = edss_value,
                            edss_value = max(edss_value - 1, min(edss_values))
                            ))
                     
                     # Need to simulate the next time to EDSS decrease
                     if (edss_i > 0 | edss_value > 0) {  new_event(list(
                       "edss_decrease" = curtime + draw_tte(
                         1, 
                         dist = "exp",
                         # Below is zero if spms_flag is 1 since patients should have EDSS decease if SPMS
                         # BUT does this flag do what its meant to? Ayman
                         coef1 = (1 - spms_flag) * rrms_edss_decrease[edss_i == edss_values, edss_value == edss_values])
                     ))}
                   
                   }) %>%
  add_reactevt(name_evt = "progression",
               # Progression can only occur once so just change the SPMS status
               input = {
                 modify_item(list(spms_flag = 1))
                 
               }) %>%
  # Currently assumes patients switch only twice so 4th line is final (As in protocol)
      add_reactevt(name_evt = "treatment_switch",
             input = {
               modify_item(list(current_treatment = sample(x = higher_line_treatments[-match(current_treatment, higher_line_treatments)],
                                                           size = 1),
                                treatment_line = treatment_line + 1,
                                n_switch = n_switch +1)
               )
               
               # Sample next discontinuation event. This will be zero if treatment_line >=4
              if (treatment_line >= 2 & treatment_line <4)  {new_event(list(
                 "treatment_switch" = curtime + draw_tte(
                   1,
                   dist = "exp",
                   coef1 = discontinuation_rate[,current_treatment]
                 )
               ))
               }
             }
) %>%
  add_reactevt(name_evt = "death",
               input = {
                 modify_item(list(curtime = Inf)) 
                 
               }) %>%
  add_reactevt(name_evt = "ae",
               input = {
                 new_event(
                   list(
                     "ae" = curtime + draw_tte(
                                          1, 
                                        dist = "exp", 
                                        #Infusion site reaction adverse event. To repeat for each ae ( in a loop?)
                                        coef1 = adverse_events_rates[1, current_treatment == treatment_names])))
               }) 
    


# Add ongoing utilities
util_ongoing <- 
  add_util(evt = c("initial", "edss_increase", "edss_decrease"),
          trt = treatment_names,
          util = u_rrms_edss[edss_value == edss_values]
          )

# Add ongoing costs
cost_ongoing <-
  add_cost(evt = c("initial", "edss_increase", "edss_decrease") ,
            trt = treatment_names,
            cost = c_rrms_edss[edss_value == edss_values]
            ) 

### add in Adverse Events ###
# Add instant utilities
#util_ongoing <- 
 # add_util(evt =c("initial", "relapse", "treatment_switch"),
  #         trt = treatment_names,
   #        util = u_tx[, current_treatment == treatment_names]
  #)

 #Add instant costs
 # Treatment Costs, need to figure out how to manipulate the costs according to year 1 or 2+ of treatment
 cost_instant <-
  add_cost(evt = c("initial", "relapse", "treatment_switch") ,
           trt = treatment_names,
           cost = c_tx[ current_treatment == treatment_names] +
                  c_tx_admin[ current_treatment == treatment_names] +
                  c_tx_monitor[ current_treatment == treatment_names]
  ) 

# Run the simulation
#Logic is: per patient, per intervention, per event, react to that event.
total_runtime <- system.time({
  results <- RunSim( 
    debug = T,
    npats = n_patients,                               # number of patients to be simulated
    n_sim = n_samples,                                  # number of simulations to run
    psa_bool = T,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    trt_list = treatment_names,             # intervention list
    common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
    common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
    unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
    init_event_list = init_event_list,        # initial event list
    evt_react_list = evt_react_list,          # reaction of events
    util_ongoing_list = util_ongoing,
    cost_ongoing_list = cost_ongoing,
    #util_instant_list = util_instant,
    cost_instant_list = cost_instant,
    ncores = 6,                               # number of cores to use, recommended not to use all
    drc = 0.035,                              # discount rate for costs
    drq = 0.035,                               # discount rate for qaly/lys
    input_out=c("age", "spms_flag", "edss_i", "edss_value", "n_relapse", "current_treatment",
                "treatment_line", "n_switch")
  )
})


# Plot the Overall median times to event regardless of treatment switching
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

# Plot the times to event on second treatment line
# Straight vertical line represents the median
data_plot_trtln2 <- results$final_output$merged_df[results$final_output$merged_df$treatment_line ==2 ] %>%
  filter(evtname != "initial") %>%
  group_by(current_treatment, evtname, simulation) %>%
  mutate(median = median(evttime)) %>%
  ungroup()

ggplot(data_plot_trtln2) +
  geom_density(aes(fill = current_treatment, x = evttime),
               alpha = 0.3) +
  geom_vline(aes(xintercept = median, col = current_treatment)) +
  facet_wrap( ~ evtname, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "median time on 2ndline current_treatment ")+
  theme_bw()

# Plot the times to event on third treatment line
# Straight vertical line represents the median
data_plot_trtln3 <- results$final_output$merged_df[results$final_output$merged_df$treatment_line ==3, ] %>%
  filter(evtname != "initial") %>%
  group_by(current_treatment, evtname, simulation) %>%
  mutate(median = median(evttime)) %>%
  ungroup()

ggplot(data_plot_trtln3) +
  geom_density(aes(fill = current_treatment, x = evttime),
               alpha = 0.3) +
  geom_vline(aes(xintercept = median, col = current_treatment)) +
  facet_wrap( ~ evtname, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "median time on 3rdline current_treatment")+
  theme_bw()

# Plot the times to event on fourth treatment line
# Straight vertical line represents the median
data_plot_trtln4 <- results$final_output$merged_df[results$final_output$merged_df$treatment_line ==4 ] %>%
  filter(evtname != "initial") %>%
  group_by(current_treatment, evtname, simulation) %>%
  mutate(median = median(evttime)) %>%
  ungroup()

ggplot(data_plot_trtln4) +
  geom_density(aes(fill = current_treatment, x = evttime),
               alpha = 0.3) +
  geom_vline(aes(xintercept = median, col = current_treatment)) +
  facet_wrap( ~ evtname, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "median time on 4th current_treatment")+
  theme_bw()


# Summarise the cost-effectiveness results

summary_results_det(results$final_output)
summary_results_psa(results$output_psa)

# Simulated patients for first simulation
psa_ipd <- bind_rows(map(results$output_psa, "merged_df")) 

head(results$output_psa[[1]]$merged_df)
head(results$output_psa[[2]]$merged_df)

###compare the results per group if interest
# compare number of relapses across trt groups
dead <- psa_ipd[psa_ipd$evtname == "death"]
n_rel_trt<- c(rep(0, length(treatment_names)))
names(n_rel_trt) <- treatment_names
for (i  in 1:length(treatment_names)) {
  n_rel_trt[i] <- sum(dead$n_relapse[dead$trt == names(n_rel_trt[i])])
}
n_rel_trt

# compare number of dead patients in edss groups
edss_dis <- matrix(rep(1:10,length(treatment_names)), byrow =TRUE, nrow = length(treatment_names), 
                   dimnames = list(treatment_names, paste("edss", rep(0:9))))
for (i in 1:nrow(edss_dis)){
for (j in 1:ncol(edss_dis)){
edss_dis[i,j]<- length(dead$edss_value[dead$trt == names(edss_dis[,j]) & dead$edss_value==j-1])
}}
edss_dis

# add in a flag for alive patients and repeat for alive patients if any are alive

# for(i in 1:n_patients){
# x <- psa_ipd[psa_ipd$pat_id ==i & psa_ipd$evttime == max(psa_ipd$evttime) & psa_ipd$evtname != "death"]
#print(x)
#}

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
          line = list(color = c("red", "green", "blue", "yellow", "darkgreen", 
                                "pink", "grey", "lightblue", "orange", "darkred",
                                "black", "cyan", "magenta")),
          pos = c(0, 0.50))



