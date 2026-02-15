
# Global parameters of the model as they are common to all patients and treatments
#Put objects here that do not change as we loop through treatments for a patient
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
  df_c_tx = inputs$df_c_tx, # drug costs are not probabilistic
  p_siponimod = inputs$p_siponimod, 
  v_event_names = v_event_names,
  m_n_patients_edss = inputs$m_n_patients_edss,
  m_r_mortality = inputs$m_r_mortality,
  v_smr = inputs$v_smr, 
  v_weight_sae = inputs$v_weight_sae,
  initial_spms_prop = inputs$initial_spms_prop,
  p_relapse_hospital = inputs$p_relapse_hospital
  
) %>% add_item(
  # Utilities for RRMS Orme et al and carergiver disutilities Acaster et al.
  v_u_rrms_edss = if(psa_bool) {
    temp <- inputs$m_u_edss_rrms[, "rrms_mean"]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- log(rlnorm(1, mean =  (inputs$m_u_edss_rrms[j_edss, "rrms_mean"]+ inputs$m_u_carer[j_edss, "Mean"]),
                                 sd =  inputs$m_u_edss_rrms[j_edss, "rrms_sd"]))
    }
    temp
    
  } else {
    inputs$m_u_edss_rrms[,"rrms_mean"]
  }
  
) %>% add_item(
  # Utilities for SPMS Orme et al and carergiver disutilities Acaster et al.
  v_u_spms_edss = if(psa_bool) {
    temp <- inputs$m_u_edss_spms[, "spms_mean"]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- log(rlnorm(1, mean =  (inputs$m_u_edss_spms[j_edss, "spms_mean"] + inputs$m_u_carer[j_edss, "Mean"]),
                                 sd =  inputs$m_u_edss_spms[j_edss, "spms_sd"]))
    }
    temp
    
  } else {
    (inputs$m_u_edss_spms[,"spms_mean"] + inputs$m_u_carer[j_edss, "Mean"])
  }
) %>% add_item(
  # RRMS EDSS Health state Direct Medical costs for RRMS Tyas et al inflated 
  v_c_rrms_edss = if(psa_bool) {
    temp <- inputs$m_c_edss_rrms[,"rrms_mean" ]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- rgamma(1, ((inputs$m_c_edss_rrms[j_edss,"rrms_mean"])^2) / (inputs$m_c_edss_rrms[j_edss, "rrms_se"]^2),
                             inputs$m_c_edss_rrms[j_edss, "rrms_mean"] / (inputs$m_c_edss_rrms[j_edss, "rrms_se"]^2))
    }
    temp
    
  } else {
    inputs$m_c_edss_rrms[,"rrms_mean"]
  }
  
) %>% add_item(
  # SPMS EDSS Health state Direct Medical costs for RRMS Tyas et al inflated 
  v_c_spms_edss = if(psa_bool) {
    temp <- inputs$m_c_edss_spms[,"spms_mean" ]
    for(j_edss in 1:n_edss_values) {
      temp[j_edss] <- rgamma(1, ((inputs$m_c_edss_spms[j_edss,"spms_mean"])^2) / (inputs$m_c_edss_spms[j_edss, "spms_se"]^2),
                             inputs$m_c_edss_spms[j_edss, "spms_mean"] / (inputs$m_c_edss_spms[j_edss, "spms_se"]^2))
    }
    temp
    
  } else {
    inputs$m_c_edss_spms[,"spms_mean"]
  }
  
) %>% add_item(
  # smr by severity Harding centered on Jick
  v_hr_smr_edss = if(psa_bool) {
    temp <- inputs$v_smr_severity_harding["smr",]
    
    for(j_edss in 1:10) {
      temp[j_edss] <- exp(
        rnorm(1, mean= log(inputs$v_smr_severity_harding["smr",j_edss]) - log(inputs$v_smr_severity_harding["smr",5]) , 
              sd=inputs$v_smr_severity_harding["logsd",j_edss]) 
      ) + rnorm(1000, mean=  log(inputs$v_smr_severity_harding["smr",1]), 
                sd=inputs$v_smr_severity_harding["logsd",1]
      )
    }
    temp
    
    
  } else {
    (inputs$v_smr_severity_harding["smr",] / inputs$v_smr_severity_harding["smr",5]) *inputs$v_smr_severity_harding["smr",1]
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
    
    temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                                       mean = inputs$m_hr_rrms_cdp6_fe["Cladribine O3.5","mean"],
                                                       sd = inputs$m_hr_rrms_cdp6_fe["Cladribine O3.5","sd"]))
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                        mean = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"],
                                                        sd = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"])) #  trt class 
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"], 
                                                       sd = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"])) #  trt class 
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"], 
                                                        sd = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"]))
    temp
  } else {matrix(c(1, 1, 1, 1, 
                   exp(inputs$m_hr_rrms_cdp6_fe["Cladribine O3.5","mean"]), 
                   exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), 
                   exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), 
                   exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]) ) ,
                 nrow = 1, ncol = n_treatments)
  }
  
) %>% add_item(
  # MS Registry time to EDSS increase HA RRMS patients
  v_baseline_harrms_edss_increase = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = inputs$v_tto_edss_increase_harrms_mu, 
            Sigma = inputs$m_tto_edss_increase_harrms_sigma) 
    
  } else {inputs$v_tto_edss_increase_harrms_mu}
  
) %>% add_item(
  # MS Registry time to EDSS decrease All RRMS patients
  v_rrms_edss_decrease = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = inputs$v_tto_edss_decrease_rrms_mu, 
            Sigma = inputs$m_tto_edss_decrease_rrms_sigma) 
    
  } else {inputs$v_tto_edss_decrease_rrms_mu}
  
) %>% add_item(
  # MS Registry time to EDSS increase SPMS patients
  v_spms_edss_increase = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = inputs$v_tto_edss_increase_spms_mu, 
            Sigma = inputs$m_tto_edss_increase_spms_sigma) 
    
  } else {inputs$v_tto_edss_increase_spms_mu}
  
  
) %>% add_item(
  # MS Registry time to EDSS decrease SPMS patients
  v_spms_edss_decrease = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = inputs$v_tto_edss_decrease_spms_mu, 
            Sigma = inputs$m_tto_edss_decrease_spms_sigma) 
    
  } else {inputs$v_tto_edss_decrease_spms_mu}
  
)%>% add_item(
  # MS Registry time to progression HARRMS to SPMS 
  v_spms_progression = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = t(inputs$v_tto_sp_conversion_harrms_mu), 
            Sigma = inputs$m_tto_sp_conversion_harrms_sigma) 
    
  } else {setNames(inputs$v_tto_sp_conversion_harrms_mu[,1],rownames(inputs$v_tto_sp_conversion_harrms_mu))}
  
) %>% add_item(
  # MS Registry time to relapse HARRMS patients
  v_baseline_harrms_larr = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = t(inputs$v_tto_relapse_harrms_mu), 
            Sigma = inputs$tto_relapse_harrms_sigma) 
    
  } else {setNames(inputs$v_tto_relapse_harrms_mu[,1],rownames(inputs$v_tto_relapse_harrms_mu))}
  
) %>% add_item(
  # MS Registry time to relapse SPMS patients
  v_baseline_spms_larr = if(psa_bool) {
    mvrnorm(n = 1, 
            mu = t(inputs$v_tto_relapse_spms_mu), 
            Sigma = inputs$m_tto_relapse_spms_sigma) 
    
  } else {setNames(inputs$v_tto_relapse_spms_mu[,1], rownames(inputs$v_tto_relapse_spms_mu))}
  
) %>% add_item(
  # Annualized relapse rate relative trt effects NMA with Natalizumab as the reference
  v_hr_arr = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    
    temp[v_treatment_codes["Natalizumab-IV"]] <- 1
    temp[v_treatment_codes["Natalizumab-SC"]] <- 1
    temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1
    temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1
    temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                                       mean =  inputs$m_hr_rrms_arr_fe["Cladribine O3.5","mean"],
                                                       sd = inputs$m_hr_rrms_arr_fe["Cladribine O3.5","sd"]))
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                        mean =  inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"],
                                                        sd = inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","sd"]))
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean =  inputs$m_hr_rrms_arr_fe["Ofatumumab SC20","mean"],
                                                       sd = inputs$m_hr_rrms_arr_fe["Ofatumumab SC20","sd"]))
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean =  inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"],
                                                        sd = inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","sd"]))
    temp
  } else {matrix(c(1, 1, 1, 1, 
                   exp(inputs$m_hr_rrms_arr_fe["Cladribine O3.5","mean"]), 
                   exp(inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]), 
                   exp(inputs$m_hr_rrms_arr_fe["Ofatumumab SC20","mean"]), 
                   exp(inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"])), 
                 nrow = 1, ncol = n_treatments)}
  
) %>% add_item(
  
  # Treatment switch follows discontinuation rates
  # the discontinuation doesn't depend on line of therapy
  # (E.g., cladribine discontinuation rate is same at 1st and 2nd line)
  discontinuation_rate = if(psa_bool) {
    
    # rates of discontination due to adverse events
    #  rate is specific to Natalizumab arm of affirm
    temp <- -log(1-(rbeta(1,inputs$m_discontinuation["AFFIRM Natalizumab IV300","alpha"], 
                          inputs$m_discontinuation["AFFIRM Natalizumab IV300","beta"]))
    )/inputs$m_discontinuation["AFFIRM Natalizumab IV300","follow_up"]
    
    temp
  } else {
    -log(1-(rbeta(1,inputs$m_discontinuation["AFFIRM Natalizumab IV300","alpha"], 
                  inputs$m_discontinuation["AFFIRM Natalizumab IV300","beta"]))
    )/inputs$m_discontinuation["AFFIRM Natalizumab IV300","follow_up"]
  }
)%>% add_item(
  # Hazard ratios for treatment discontinuation due to SAEs
  v_hr_discontinuation = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = n_treatments)
    # relative treatment effects log Hr from the NMA with Natalizumab as the reference
    temp[v_treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
    temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1
    temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1
    temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1,
                                                       mean = inputs$m_hr_rrms_discontinuation_fe["Cladribine O3.5","mean"],
                                                       sd = inputs$m_hr_rrms_discontinuation_fe["Cladribine O3.5","sd"]))
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                        mean = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"],
                                                        sd = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"])) # trt class  
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"], 
                                                       sd = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"])) #  trt class 
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"], 
                                                        sd = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"]))
    temp
  } else {matrix(c(1, 1, 1, 1, 
                   exp(inputs$m_hr_rrms_discontinuation_fe["Cladribine O3.5","mean"]), 
                   exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), 
                   exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), 
                   exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"])) ,
                 nrow = 1, ncol = n_treatments)
  }
) %>% add_item(
  # Treatment administration costs
  v_c_trt_admin = if(psa_bool) {
    temp <- matrix(nrow = nrow(inputs$df_c_tx_admin), ncol = n_treatments,
                   dimnames = list(row.names(inputs$df_c_tx_admin), v_treatment_names))
    for (j_cost in 1:nrow(inputs$df_c_tx_admin)) {
      for(i_trt in 1:n_treatments) {
        temp[j_cost,i_trt] <- rgamma(1, ((inputs$df_c_tx_admin[j_cost,i_trt])^2) / ((inputs$df_c_tx_admin[j_cost,i_trt] / sd_psa)^2),
                                     inputs$df_c_tx_admin[j_cost,i_trt] / ((inputs$df_c_tx_admin[j_cost,i_trt]/sd_psa)^2))
      }}
    temp
    
  } else {
    matrix(inputs$df_c_tx_admin, nrow = nrow(inputs$df_c_tx_admin), ncol = n_treatments,
           dimnames = list(row.names(inputs$df_c_tx_admin), v_treatment_names))
  }
  
) %>% add_item(
  # Treatment monitoring costs
  v_c_trt_monitor = if(psa_bool) {
    temp <- matrix(nrow = nrow(inputs$df_c_tx_monitor), ncol = n_treatments,
                   dimnames = list(row.names(inputs$df_c_tx_monitor), v_treatment_names))
    for (j_cost in 1:nrow(inputs$df_c_tx_monitor)) {
      for(i_trt in 1:n_treatments) {
        temp[j_cost,i_trt] <- rgamma(1, ((inputs$df_c_tx_monitor[j_cost,i_trt])^2) / ((inputs$df_c_tx_monitor[j_cost,i_trt]/sd_psa)^2),
                                     inputs$df_c_tx_monitor[j_cost,i_trt] / ((inputs$df_c_tx_monitor[j_cost,i_trt]/sd_psa)^2))
      }}
    temp
    
  } else {
    matrix(inputs$df_c_tx_monitor, nrow = nrow(inputs$df_c_tx_monitor), ncol = n_treatments,
           dimnames = list(row.names(inputs$df_c_tx_monitor), v_treatment_names))
  }
  
) %>% add_item(  
  # Relapse costs are treatment & severity agnostic
  
  v_c_relapse = if(psa_bool) {
    temp <- inputs$m_c_relapse[, "mean"]
    for(i_cost in 1:length(temp)) {
      temp[i_cost] <- rgamma(1, ((inputs$m_c_relapse[i_cost,"mean"])^2) / ((inputs$m_c_relapse[i_cost,"sd"]/sd_psa)^2),
                             inputs$m_c_relapse[i_cost,"mean"] / ((inputs$m_c_relapse[i_cost,"sd"]/sd_psa)^2))
    }
    temp
    
  } else {
    inputs$m_c_relapse[, "mean"]
  }
  
) %>% add_item(  
  # Relapse dis-utilities are treatment
  u_relapse = if(psa_bool) {
    pmin(rnorm(1, mean =  inputs$df_u_relapse["Orme etal", "u_mean_relapse_hospital"], sd =  inputs$df_u_relapse["Orme etal", "u_sd_relapse_hospital"]),0)
    
  } else {
    inputs$df_u_relapse[1, 1]
  }
  
) %>% add_item(
  # costs of serious adverse events
  v_c_sae_events = if(psa_bool) {
    temp <- t(matrix(nrow = inputs$n_sae, ncol = 1,
                     dimnames = list(inputs$sae_names, "sae_cost")))
    for(i_sae in 1:inputs$n_sae) {
      temp[i_sae] <- rgamma(1, ((inputs$df_c_sae[i_sae])^2) / ((inputs$df_c_sae[i_sae]/sd_psa)^2),
                            inputs$df_c_sae[i_sae] / ((inputs$df_c_sae[i_sae]/sd_psa)^2))
    }
    temp
    
  } else {
    t(matrix(inputs$df_c_sae, nrow = inputs$n_sae, ncol = 1,
             dimnames = list(inputs$sae_names, "sae_cost")))
  }
  
) %>% add_item(
  # disutilities of serious adverse events
  v_u_sae_events = if(psa_bool) {
    temp <- t(matrix(nrow = inputs$n_sae, ncol = 1,
                     dimnames = list(inputs$sae_names, "sae_disutility")))
    for(i_sae in 1:inputs$n_sae) {
      temp[i_sae] <- pmin(rnorm(1, 
                                mean =  inputs$df_u_sae[i_sae], 
                                sd =  -inputs$df_u_sae[i_sae] / sd_psa), 0)
    }
    temp
    
  } else {
    t(matrix(inputs$df_u_sae, nrow = inputs$n_sae, ncol = 1,
             dimnames = list(inputs$sae_names, "sae_disutility")))
  }
)  %>% add_item(
  # rates of serious adverse events
  sae_events_rate = if(psa_bool) {
    temp <- -log(1-(rbeta(1,inputs$m_saes["AFFIRM Natalizumab IV300","alpha"], 
                          inputs$m_saes["AFFIRM Natalizumab IV300","beta"]))
    )/inputs$m_saes["AFFIRM Natalizumab IV300","follow_up"]
    
    temp
  } else { -log(1-(rbeta(1,inputs$m_saes["AFFIRM Natalizumab IV300","alpha"], 
                         inputs$m_saes["AFFIRM Natalizumab IV300","beta"]))
  )/inputs$m_saes["AFFIRM Natalizumab IV300","follow_up"]
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
    temp[v_treatment_codes["Cladribine"]] <- 1 #assumption
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1,
                                                        mean = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"],
                                                        sd = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","sd"])) 
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1,
                                                       mean = inputs$m_hr_rrms_saes_fe["Ofatumumab SC20","mean"], 
                                                       sd = inputs$m_hr_rrms_saes_fe["Ofatumumab SC20","sd"])) 
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,
                                                        mean = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"], 
                                                        sd = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","sd"]))
    temp
  } else {matrix(c(1, 1, 1, 1, 1, 
                   exp(inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"]), 
                   exp(inputs$m_hr_rrms_saes_fe["Ofatumumab SC20","mean"]), 
                   exp(inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"]) ) ,
                 nrow = 1, ncol = n_treatments)
  }
) %>% add_item(
  # Hazard ratios for Placebo group when patients stop treatment
  v_hr_pbo = if(psa_bool) {
    temp <- matrix(nrow = 1, ncol = 4, dimnames = list(NULL, c("edss_increase", "arr", "discontinuation","saes")))
    # CDP6
    temp[,"edss_increase"] <- exp(rnorm(1,
                                        mean = inputs$m_hr_rrms_cdp6_fe["Placebo","mean"],
                                        sd = inputs$m_hr_rrms_cdp6_fe["Placebo","sd"]))
    # Annualized relapse rate
    temp[, "arr"] <- exp(rnorm(1,
                               mean = inputs$m_hr_rrms_arr_fe["Placebo","mean"],
                               sd = inputs$m_hr_rrms_arr_fe["Placebo","sd"]))
    # discontinuation due to SAEs
    temp[, "discontinuation"] <- exp(rnorm(1,
                                           mean = inputs$m_hr_rrms_discontinuation_fe["Placebo","mean"],
                                           sd = inputs$m_hr_rrms_discontinuation_fe["Placebo","sd"]))
    #SAEs
    temp[, "saes"] <- exp(rnorm(1,
                                mean = inputs$m_hr_rrms_saes_fe["Placebo","mean"],
                                sd = inputs$m_hr_rrms_saes_fe["Placebo","sd"]))
    temp
  }else {matrix(c(exp(inputs$m_hr_rrms_cdp6_fe["Placebo","mean"]), exp(inputs$m_hr_rrms_arr_fe["Placebo","mean"]), 
                  exp(inputs$m_hr_rrms_discontinuation_fe["Placebo","mean"]), exp(inputs$m_hr_rrms_saes_fe["Placebo","mean"]), 
                  nrow = 1, ncol = 4, dimnames = list(NULL, c("edss_increase", "arr", "discontinuation","saes"))))}
  
) %>% add_item(
  #proportions on 2nd and 3rd line treatments
  m_prop_2nd_3rd_trt = if(psa_bool) {
    
    temp<- matrix(nrow= 2, ncol = ncol(inputs$m_proportion_ontrtline), 
                  dimnames = list(c("2", "3"), colnames(inputs$m_proportion_ontrtline)))
    
    for(i_trt in 1:ncol(inputs$m_proportion_ontrtline)) {
      temp[1, i_trt] <- rbeta(1, shape1= inputs$m_proportion_ontrtline["alpha_2ndline", i_trt], shape2 = inputs$m_proportion_ontrtline["beta_2ndline", i_trt])
      temp[2, i_trt] <- rbeta(1, shape1= inputs$m_proportion_ontrtline["alpha_3rdline", i_trt], shape2 = inputs$m_proportion_ontrtline["beta_3rdline", i_trt])
    }
    
    temp
  } else {matrix(c(inputs$m_proportion_ontrtline["alpha_2ndline",] /inputs$m_proportion_ontrtline["beta_2ndline",], inputs$m_proportion_ontrtline["alpha_3rdline",] /inputs$m_proportion_ontrtline["beta_3rdline",]), 
                 byrow=TRUE, nrow= 2, ncol = ncol(inputs$m_proportion_ontrtline), 
                 dimnames = list(c("2", "3"), colnames(inputs$m_proportion_ontrtline)
                 ))
    
  }
  
)
