#investigating why event rates do not match between descem or warden

psa_bool=FALSE
sensitivity_bool=1

spms_flag =0
edss_value=1
treatment_stop=0
current_treatment_code=1
waning=0
indicators <-0
sex<-1
age<-30

pick_val_v(
  base = t(inputs$v_tto_relapse_harrms_mu),
  psa =  mvrnorm(n = 1, mu = t(inputs$v_tto_relapse_harrms_mu), 
                 Sigma = inputs$tto_relapse_harrms_sigma),
  #sens = , 
  psa_ind = psa_bool,
  sens_ind = sensitivity_bool,    
  indicator = rep(indicators, length(t(inputs$v_tto_relapse_harrms_mu))),    
  names_out = paste0(rep("baseline_harrms_larr_", nrow(inputs$v_tto_relapse_harrms_mu)), rownames(inputs$v_tto_relapse_harrms_mu))
    #rownames(inputs$v_tto_relapse_harrms_mu)
)
l_baseline_spms_larr <- pick_val_v(
  base = t(inputs$v_tto_relapse_spms_mu),
  psa =  mvrnorm(n = 1, mu = t(inputs$v_tto_relapse_spms_mu), 
                 Sigma = inputs$m_tto_relapse_spms_sigma),
  #sens = , 
  psa_ind = psa_bool,
  sens_ind = sensitivity_bool,    
  indicator = rep(indicators, length(t(inputs$v_tto_relapse_spms_mu))),    
  names_out = rownames(inputs$v_tto_relapse_spms_mu)
)
l_hr_arr <- pick_val_v(
  base = c(1, 1, 1, 1, 
           exp(inputs$m_hr_rrms_arr_fe["Cladribine O3.5","mean"]), 
           exp(inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]), 
           exp(inputs$m_hr_rrms_arr_fe["Ofatumumab SC20","mean"]), 
           exp(inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]) ),
  psa = {
    temp <- c(rep(NA,n_treatments))
    # ARR relative treatment effects log Hr from the NMA with Natalizumab as the reference
    temp[v_treatment_codes["Natalizumab-IV"]] <- 1
    temp[v_treatment_codes["Natalizumab-SC"]] <- 1
    temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1
    temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1
    temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1, mean =  inputs$m_hr_rrms_arr_fe["Cladribine O3.5","mean"],
                                                       sd = inputs$m_hr_rrms_arr_fe["Cladribine O3.5","sd"]))
    temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1, mean =  inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"],
                                                        sd = inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","sd"]))
    temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1, mean =  inputs$m_hr_rrms_arr_fe["Ofatumumab SC20","mean"],
                                                       sd = inputs$m_hr_rrms_arr_fe["Ofatumumab SC20","sd"]))
    temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,mean =  inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"],
                                                        sd = inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","sd"]))
    temp 
  },
  #sens = , 
  psa_ind = psa_bool,
  sens_ind = sensitivity_bool,    
  indicator = rep(indicators, length(v_treatment_codes)),    
  names_out = names(v_treatment_codes)
)

 pick_val_v(
  base = c(exp(inputs$m_hr_rrms_cdp6_fe["Placebo","mean"]), exp(inputs$m_hr_rrms_arr_fe["Placebo","mean"]), 
           exp(inputs$m_hr_rrms_discontinuation_fe["Placebo","mean"]), exp(inputs$m_hr_rrms_saes_fe["Placebo","mean"])),
  psa = {
    temp <- c(exp(inputs$m_hr_rrms_cdp6_fe["Placebo","mean"]), exp(inputs$m_hr_rrms_arr_fe["Placebo","mean"]), 
              exp(inputs$m_hr_rrms_discontinuation_fe["Placebo","mean"]), exp(inputs$m_hr_rrms_saes_fe["Placebo","mean"]))
    
    # CDP6
    temp[1] <- exp(rnorm(1, mean = inputs$m_hr_rrms_cdp6_fe["Placebo","mean"],
                         sd = inputs$m_hr_rrms_cdp6_fe["Placebo","sd"]))
    # Annualized relapse rate
    temp[2] <- exp(rnorm(1, mean = inputs$m_hr_rrms_arr_fe["Placebo","mean"],
                         sd = inputs$m_hr_rrms_arr_fe["Placebo","sd"]))
    # discontinuation due to SAEs
    temp[3] <- exp(rnorm(1, mean = inputs$m_hr_rrms_discontinuation_fe["Placebo","mean"],
                         sd = inputs$m_hr_rrms_discontinuation_fe["Placebo","sd"]))
    #SAEs
    temp[4] <- exp(rnorm(1, mean = inputs$m_hr_rrms_saes_fe["Placebo","mean"],
                         sd = inputs$m_hr_rrms_saes_fe["Placebo","sd"]))
    temp
  },
  #sens = , 
  psa_ind = psa_bool,
  sens_ind = sensitivity_bool,    
  indicator = rep(indicators, 4),    
  names_out = paste0(rep("hr_pbo_",4),c("edss_increase", "arr", "discontinuation","saes"))
)




warden_tte_relapse <- draw_tte(
  1000, 
  dist = "exp", 
  # Current treatment code allows treatment switching 
  # (i.e., not the trt which is only initial assignment)
  # MS registry time to relapse HA RRMS log rate on natalizumab
  coef1 = (1 - spms_flag) * (baseline_harrms_larr_rate +
                               (baseline_harrms_larr_BaselineWebEDSS * edss_value) +
                               baseline_harrms_larr_DMTNatalizumab ) +
    # MS registry time to relapse SPMS log rate
    spms_flag * (baseline_harrms_larr_rate +
                   (baseline_harrms_larr_BaselineWebEDSS * edss_value)),
  # Baseline rate modified by HR of current treatment
  # No treatment effect if SPMS
  # placebo ARR HR from NMA applied when treatment stopped 
  beta_tx = (1 - spms_flag) * ((1 - treatment_stop) * (pluck_vector_from_env(l_hr_arr, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))*(1-waning)) +
                                 treatment_stop * hr_pbo_arr ) + spms_flag
)

#DESCEM
v_baseline_harrms_larr = if(psa_bool) {
  mvrnorm(n = 1, 
          mu = t(inputs$v_tto_relapse_harrms_mu), 
          Sigma = inputs$tto_relapse_harrms_sigma) 
  
} else {setNames(inputs$v_tto_relapse_harrms_mu[,1],rownames(inputs$v_tto_relapse_harrms_mu))}

v_baseline_spms_larr = if(psa_bool) {
  mvrnorm(n = 1, 
          mu = t(inputs$v_tto_relapse_spms_mu), 
          Sigma = inputs$m_tto_relapse_spms_sigma) 
  
} else {setNames(inputs$v_tto_relapse_spms_mu[,1], rownames(inputs$v_tto_relapse_spms_mu))}


descem_tte_relapse <- draw_tte(
  1000, 
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

summarise_samples(descem_tte_relapse)
summarise_samples(warden_tte_relapse)
