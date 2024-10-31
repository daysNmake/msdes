
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

