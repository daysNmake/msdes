#'
#' @description
#' Global input parameters for the model are common to all patients and treatments.
#' 
#' @details
#' Input parameters here that do not change as we loop through treatments for a patient.
#' The input parameters are called from 'gen_inputs()' function or the main analysis script.
#' They are called in with 'WARDEN::add_item()' functions, these are piped with %>% operator.
#' Probabilistic input parameters are specified with 'WARDEN::pick_val_v()' function creating a list of 
#' deterministic and probabilistic values
#' 
#' @param drc discount rate for costs
#' @param drq discount rate for qalys
#' @param reference The treatment arm specified as the reference for the comparison
#' @export
gen_common_all_inputs <- function() {
  add_item(
  v_treatment_names = v_treatment_names,
  v_treatment_codes = v_treatment_codes,
  v_higher_line_treatments = v_higher_line_treatments,
  v_higher_line_treatment_codes = v_higher_line_treatment_codes,
  v_prior_treatment_indices = v_prior_treatment_indices,
  initial_age = inputs[["initial_age"]],
  time_horizon = inputs[["time_horizon"]],
  drc = inputs[["drc"]],                              # discount rate for costs
  drq = inputs[["drq"]],                               # discount rate for qaly/lys
  prop_female = inputs[["prop_female"]],
  v_edss_values = v_edss_values,
  df_c_tx = inputs[["df_c_tx"]], # drug costs are not probabilistic
  p_siponimod = inputs[["p_siponimod"]], 
  v_event_names = v_event_names,
  m_n_patients_edss = inputs[["m_n_patients_edss"]],
  m_r_mortality = inputs[["m_r_mortality"]],
  v_weight_sae = inputs[["v_weight_sae"]],
  initial_spms_prop = inputs[["initial_spms_prop"]],
  p_relapse_hospital = inputs[["p_relapse_hospital"]]
  
) %>% add_item(
  # list of Utilities for RRMS Orme et al and carergiver disutilities Acaster et al.
  l_u_rrms_edss <- pick_val_v(
    base = inputs[["m_u_edss_rrms"]][,"rrms_mean"],
    psa = {
      temp <- inputs[["m_u_edss_rrms"]][, "rrms_mean"]
        for(j_edss in 1:n_edss_values) {
          temp[j_edss] <- log(rlnorm(1, mean =  (inputs[["m_u_edss_rrms"]][j_edss, "rrms_mean"]+ inputs$m_u_carer[j_edss, "Mean"]),
                                     sd =  inputs[["m_u_edss_rrms"]][j_edss, "rrms_sd"]))
        } 
      temp 
      },
    #sens = ,
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs[["m_u_edss_rrms"]][,"rrms_mean"])),    
    names_out = names(v_edss_values),
    deploy_env= FALSE
    )
  
) %>% add_item(
  # list of Utilities for SPMS Orme et al and carergiver disutilities Acaster et al.
  l_u_spms_edss <- pick_val_v(
    base = (inputs$m_u_edss_spms[,"spms_mean"] + inputs$m_u_carer[, "Mean"]),
    psa = {
      temp <- inputs$m_u_edss_spms[, "spms_mean"]
      for(j_edss in 1:n_edss_values) {
        temp[j_edss] <- log(rlnorm(1, mean =  (inputs$m_u_edss_spms[j_edss, "spms_mean"] + inputs$m_u_carer[j_edss, "Mean"]),
                                   sd =  inputs$m_u_edss_spms[j_edss, "spms_sd"]))
      } 
      temp 
      },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length((inputs$m_u_edss_spms[,"spms_mean"] + inputs$m_u_carer[, "Mean"]))),    
    names_out = names(v_edss_values),
    deploy_env= FALSE
  )
) %>% add_item(
  # List of RRMS EDSS Health state Direct Medical costs for RRMS Tyas et al inflated 
  l_c_rrms_edss <- pick_val_v(
    base = inputs$m_c_edss_rrms[,"rrms_mean" ],
    psa = {
      temp <- inputs$m_c_edss_rrms[,"rrms_mean" ]
      for(j_edss in 1:n_edss_values) {
        temp[j_edss] <- rgamma(1, ((inputs$m_c_edss_rrms[j_edss,"rrms_mean"])^2) / (inputs$m_c_edss_rrms[j_edss, "rrms_se"]^2),
                               inputs$m_c_edss_rrms[j_edss, "rrms_mean"] / (inputs$m_c_edss_rrms[j_edss, "rrms_se"]^2))
      } 
      temp 
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$m_c_edss_rrms[,"rrms_mean" ])),    
    names_out = names(v_edss_values),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # list of SPMS EDSS Health state Direct Medical costs for RRMS Tyas et al inflated 
  l_c_spms_edss <- pick_val_v(
    base = inputs$m_c_edss_spms[,"spms_mean"],
    psa = {
      temp <- inputs$m_c_edss_spms[,"spms_mean" ]
      for(j_edss in 1:n_edss_values) {
        temp[j_edss] <- rgamma(1, ((inputs$m_c_edss_spms[j_edss,"spms_mean"])^2) / (inputs$m_c_edss_spms[j_edss, "spms_se"]^2),
                               inputs$m_c_edss_spms[j_edss, "spms_mean"] / (inputs$m_c_edss_spms[j_edss, "spms_se"]^2))
      } 
      temp 
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$m_c_edss_spms[,"spms_mean"])),    
    names_out = names(v_edss_values),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # list of smr by severity Harding centered on Jick
  l_hr_smr_edss <- pick_val_v(
    base = (inputs$v_smr_severity_harding["smr",] / inputs$v_smr_severity_harding["smr",5]) *inputs$v_smr_severity_harding["smr",1],
    psa = {
      temp <- inputs$v_smr_severity_harding["smr",]
      
      for(j_edss in 1:10) {
        temp[j_edss] <- exp(
          rnorm(1, mean= log(inputs$v_smr_severity_harding["smr",j_edss]) - log(inputs$v_smr_severity_harding["smr",5]) , 
                sd=inputs$v_smr_severity_harding["logsd",j_edss]) 
         + rnorm(1, mean=  log(inputs$v_smr_severity_harding["smr",1]), sd=inputs$v_smr_severity_harding["logsd",1]
        ))
      } 
      temp 
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$v_smr_severity_harding["smr",])),    
    names_out = names(v_edss_values),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # List of Hazard ratios reducing risk of EDSS increase
  l_hr_edss_increase <- pick_val_v(
    base = c(1, 1, 1, 1, 
             exp(inputs$m_hr_rrms_cdp6_fe["Cladribine O3.5","mean"]), 
             exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), 
             exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), 
             exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]) ),
    psa = {
      temp <- c(rep(NA,n_treatments))
      # CDP6relative treatment effects log Hr from the NMA with Natalizumab as the reference
      temp[v_treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
      temp[v_treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
      temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1 #missing from NMA
      temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1 #missing from NMA
      
      temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_cdp6_fe["Cladribine O3.5","mean"],
                                                         sd = inputs$m_hr_rrms_cdp6_fe["Cladribine O3.5","sd"]))
      temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"],
                                                          sd = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"])) #  trt class 
      temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"], 
                                                         sd = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"])) #  trt class 
      temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"], 
                                                          sd = inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","sd"]))
      temp 
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(v_treatment_codes)),    
    names_out = names(v_treatment_codes),
    deploy_env= FALSE
  )
  
  
) %>% add_item(
  # MS Registry time to EDSS increase HA RRMS patients
  l_baseline_harrms_edss_increase <- pick_val_v(
    base = inputs$v_tto_edss_increase_harrms_mu,
    psa =     mvrnorm(n = 1,mu = inputs$v_tto_edss_increase_harrms_mu, 
                      Sigma = inputs$m_tto_edss_increase_harrms_sigma),
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$v_tto_edss_increase_harrms_mu)),    
    names_out = colnames(inputs$v_tto_edss_increase_harrms_mu),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # MS Registry time to EDSS decrease All RRMS patients
  l_rrms_edss_decrease <- pick_val_v(
    base = inputs$v_tto_edss_decrease_rrms_mu,
    psa =     mvrnorm(n = 1, mu = inputs$v_tto_edss_decrease_rrms_mu, 
                      Sigma = inputs$m_tto_edss_decrease_rrms_sigma),
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$v_tto_edss_decrease_rrms_mu)),    
    names_out = colnames(inputs$v_tto_edss_decrease_rrms_mu),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # MS Registry time to EDSS increase SPMS patients
  l_spms_edss_increase <- pick_val_v(
    base = inputs$v_tto_edss_increase_spms_mu,
    psa =       mvrnorm(n = 1, mu = inputs$v_tto_edss_increase_spms_mu, 
                        Sigma = inputs$m_tto_edss_increase_spms_sigma) ,
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$v_tto_edss_increase_spms_mu)),    
    names_out = colnames(inputs$v_tto_edss_increase_spms_mu),
    deploy_env= FALSE
  )
  
  
# ) %>% add_item(
#   # MS Registry time to EDSS decrease SPMS patients
#   l_spms_edss_decrease <- pick_val_v(
#     base = inputs$v_tto_edss_decrease_spms_mu,
#     psa =         mvrnorm(n = 1, mu = inputs$v_tto_edss_decrease_spms_mu, 
#                           Sigma = inputs$m_tto_edss_decrease_spms_sigma) ,
#     #sens = , 
#     psa_ind = psa_bool,
#     sens_ind = sensitivity_bool,    
#     indicator = rep(indicators, length(inputs$v_tto_edss_decrease_spms_mu)),    
#     names_out = colnames(inputs$v_tto_edss_decrease_spms_mu)
#   )
  
)%>% add_item(
  # MS Registry time to progression HARRMS to SPMS 
  l_spms_progression <- pick_val_v(
    base = t(inputs$v_tto_sp_conversion_harrms_mu),
    psa =   mvrnorm(n = 1, mu = t(inputs$v_tto_sp_conversion_harrms_mu), 
                    Sigma = inputs$m_tto_sp_conversion_harrms_sigma),
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(t(inputs$v_tto_sp_conversion_harrms_mu))),    
    names_out = rownames(inputs$v_tto_sp_conversion_harrms_mu),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # MS Registry time to relapse HARRMS patients
   l_baseline_harrms_larr<- pick_val_v(
    base = t(inputs$v_tto_relapse_harrms_mu),
    psa =  mvrnorm(n = 1, mu = t(inputs$v_tto_relapse_harrms_mu), 
                   Sigma = inputs$tto_relapse_harrms_sigma),
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(t(inputs$v_tto_relapse_harrms_mu))),    
    names_out = rownames(inputs$v_tto_relapse_harrms_mu),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # MS Registry time to relapse SPMS patients
  l_baseline_spms_larr <- pick_val_v(
    base = t(inputs$v_tto_relapse_spms_mu),
    psa =  mvrnorm(n = 1, mu = t(inputs$v_tto_relapse_spms_mu), 
                   Sigma = inputs$m_tto_relapse_spms_sigma),
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(t(inputs$v_tto_relapse_spms_mu))),    
    names_out = rownames(inputs$v_tto_relapse_spms_mu),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # list of Annualized relapse rate relative trt effects 
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
    names_out = names(v_treatment_codes),
    deploy_env= FALSE
  )
  
) %>% add_item(
    # Treatment switch follows discontinuation rates due to adverse events specific to Natalizumab arm of affirm
  # the discontinuation doesn't depend on line of therapy (E.g., cladribine discontinuation rate is same at 1st and 2nd line)
  discontinuation_rate <- pick_val_v(
    base = -log(1-(rbeta(1,inputs$m_discontinuation["AFFIRM Natalizumab IV300","alpha"], 
                         inputs$m_discontinuation["AFFIRM Natalizumab IV300","beta"]))
                )/inputs$m_discontinuation["AFFIRM Natalizumab IV300","follow_up"],
    
    psa = { temp <- -log(1-(rbeta(1,inputs$m_discontinuation["AFFIRM Natalizumab IV300","alpha"], 
                            inputs$m_discontinuation["AFFIRM Natalizumab IV300","beta"]))
                      )/inputs$m_discontinuation["AFFIRM Natalizumab IV300","follow_up"]
      
      temp },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, 1),    
    names_out = "rate",
    deploy_env= FALSE
  )
)%>% add_item(
  # List of Hazard ratios for treatment discontinuation due to SAEs
  l_hr_discontinuation <- pick_val_v(
    base = c(1, 1, 1, 1, 
             exp(inputs$m_hr_rrms_discontinuation_fe["Cladribine O3.5","mean"]), 
             exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), 
             exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), 
             exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]) ),
    psa = {
      temp <- c(rep(NA,n_treatments))
      # discontinuation relative treatment effects log Hr from the NMA with Natalizumab as the reference
      temp[v_treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
      temp[v_treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
      temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1
      temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1
      temp[v_treatment_codes["Cladribine"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_discontinuation_fe["Cladribine O3.5","mean"],
                                                         sd = inputs$m_hr_rrms_discontinuation_fe["Cladribine O3.5","sd"]))
      temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"],
                                                          sd = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"])) # trt class  
      temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"], 
                                                         sd = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"])) #  trt class 
      temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"], 
                                                          sd = inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","sd"]))
      temp 
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(v_treatment_codes)),    
    names_out = names(v_treatment_codes),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # Treatment administration costs year 1
  l_c_trt_admin_yr1 <- pick_val_v(
    base = inputs$df_c_tx_admin["cost_annual_yr1",],
    psa = {
      temp <- inputs$df_c_tx_admin["cost_annual_yr1",]
      for(i_trt in 1:n_treatments) {
        temp[i_trt] <- rgamma(1, ((inputs$df_c_tx_admin["cost_annual_yr1",i_trt])^2) / ((inputs$df_c_tx_admin["cost_annual_yr1",i_trt] / inputs[["sd_psa"]])^2),
                              inputs$df_c_tx_admin["cost_annual_yr1",i_trt] / ((inputs$df_c_tx_admin["cost_annual_yr1",i_trt]/inputs[["sd_psa"]])^2))
      }
      temp
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$df_c_tx_admin["cost_annual_yr1",])),    
    names_out = colnames(inputs$df_c_tx_admin),
    deploy_env= FALSE
  )

) %>% add_item(
  # Treatment administration costs year 2
  l_c_trt_admin_yr2 <- pick_val_v(
    base = inputs$df_c_tx_admin["cost_annual_yr2",],
    psa = {
      temp <- inputs$df_c_tx_admin["cost_annual_yr2",]
      for(i_trt in 1:n_treatments) {
        temp[i_trt] <- rgamma(1, ((inputs$df_c_tx_admin["cost_annual_yr2",i_trt])^2) / ((inputs$df_c_tx_admin["cost_annual_yr2",i_trt] / inputs[["sd_psa"]])^2),
                              inputs$df_c_tx_admin["cost_annual_yr2",i_trt] / ((inputs$df_c_tx_admin["cost_annual_yr2",i_trt]/inputs[["sd_psa"]])^2))
      }
      temp
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$df_c_tx_admin["cost_annual_yr2",])),    
    names_out = colnames(inputs$df_c_tx_admin),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # Treatment monitoring costs year 1
  l_c_trt_monitor_yr1 <- pick_val_v(
    base = inputs$df_c_tx_monitor["cost_annual_yr1",],
    psa = {
      temp <- inputs$df_c_tx_monitor["cost_annual_yr1",]
      for(i_trt in 1:n_treatments) {
        temp[i_trt] <- rgamma(1, ((inputs$df_c_tx_monitor["cost_annual_yr1",i_trt])^2) / ((inputs$df_c_tx_monitor["cost_annual_yr1",i_trt] / inputs[["sd_psa"]])^2),
                              inputs$df_c_tx_monitor["cost_annual_yr1",i_trt] / ((inputs$df_c_tx_monitor["cost_annual_yr1",i_trt]/inputs[["sd_psa"]])^2))
      }
      temp
      
    },
    
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$df_c_tx_monitor["cost_annual_yr1",])),    
    names_out = colnames(inputs$df_c_tx_monitor),
    deploy_env= FALSE
  )
  
) %>% add_item(
  # Treatment monitoring costs year 2
  l_c_trt_monitor_yr2 <- pick_val_v(
    base = inputs$df_c_tx_monitor["cost_annual_yr2",],
    psa = {
      temp <- inputs$df_c_tx_monitor["cost_annual_yr2",]
      for(i_trt in 1:n_treatments) {
        temp[i_trt] <- rgamma(1, ((inputs$df_c_tx_monitor["cost_annual_yr2",i_trt])^2) / ((inputs$df_c_tx_monitor["cost_annual_yr2",i_trt] / inputs[["sd_psa"]])^2),
                              inputs$df_c_tx_monitor["cost_annual_yr2",i_trt] / ((inputs$df_c_tx_monitor["cost_annual_yr2",i_trt]/inputs[["sd_psa"]])^2))
      }
      temp
      
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$df_c_tx_monitor["cost_annual_yr2",])),    
    names_out = colnames(inputs$df_c_tx_monitor),
    deploy_env= FALSE
  )
  
) %>% add_item(  
  # Relapse costs are treatment & severity agnostic
  l_c_relapse <- pick_val_v(
    base = inputs$m_c_relapse[, "mean"] ,
    psa = {
      temp <- inputs$m_c_relapse[, "mean"]
      for(i_cost in 1:length(temp)) {
        temp[i_cost] <- rgamma(1, ((inputs$m_c_relapse[i_cost,"mean"])^2) / ((inputs$m_c_relapse[i_cost,"sd"]/inputs[["sd_psa"]])^2),
                               inputs$m_c_relapse[i_cost,"mean"] / ((inputs$m_c_relapse[i_cost,"sd"]/inputs[["sd_psa"]])^2))
      }
      temp
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, nrow(inputs$m_c_relaps)),    
    names_out = rownames(inputs$m_c_relapse),
    deploy_env= FALSE
  )
  
) %>% add_item(  
  # Relapse dis-utilities are treatment
  l_u_mean_relapse_hospital <- pick_val_v(
    base = inputs$df_u_relapse["Orme etal", "u_mean_relapse_hospital"] ,
    psa = {
      pmin(rnorm(1, mean =  inputs$df_u_relapse["Orme etal", "u_mean_relapse_hospital"], sd =  inputs$df_u_relapse["Orme etal", "u_sd_relapse_hospital"]),0)
      
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(inputs$df_u_relapse["Orme etal", "u_mean_relapse_hospital"])),    
    names_out = "utility",
    deploy_env= FALSE
  )
  
) %>% add_item(
  # costs of serious adverse events
  l_costs_sae_weighted <- pick_val_v(
    base = sum(inputs$df_c_sae*inputs$v_weight_sae),
    psa = {
      temp <- inputs$df_c_sae
      for(i_sae in 1:inputs$n_sae) {
        temp[i_sae] <- rgamma(1, ((inputs$df_c_sae[i_sae])^2) / ((inputs$df_c_sae[i_sae]/inputs[["sd_psa"]])^2),
                              inputs$df_c_sae[i_sae] / ((inputs$df_c_sae[i_sae]/inputs[["sd_psa"]])^2))
      }
      temp <- sum(temp*inputs$v_weight_sae)
      temp
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, 1),    
    names_out = "costs",
    deploy_env= FALSE
  )
  
) %>% add_item(
  # disutilities of serious adverse events
   l_utilities_sae_weighted <- pick_val_v(
    base = sum(inputs$df_u_sae*inputs$v_weight_sae),
    psa = {
      temp <- inputs$df_u_sae
      for(i_sae in 1:inputs$n_sae) {
        temp[i_sae] <- pmin(rnorm(1, 
                                  mean =  inputs$df_u_sae[i_sae], 
                                  sd =  -inputs$df_u_sae[i_sae] / inputs[["sd_psa"]]), 0)
      }
      temp <- sum(temp*inputs$v_weight_sae)
      temp
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, 1),    
    names_out = "utility",
    deploy_env= FALSE
  )
  
)  %>% add_item(
  # rates of serious adverse events
  l_sae_events_rate <- pick_val_v(
    base = -log(1-(rbeta(1,inputs$m_saes["AFFIRM Natalizumab IV300","alpha"], 
                         inputs$m_saes["AFFIRM Natalizumab IV300","beta"]))
    )/inputs$m_saes["AFFIRM Natalizumab IV300","follow_up"],
    
    psa = {
      temp <- -log(1-(rbeta(1,inputs$m_saes["AFFIRM Natalizumab IV300","alpha"], 
                            inputs$m_saes["AFFIRM Natalizumab IV300","beta"]))
      )/inputs$m_saes["AFFIRM Natalizumab IV300","follow_up"]
      
      temp
    },
    
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, 1),    
    names_out = "rate",
    deploy_env= FALSE
  )
  
) %>% add_item(
  # list of Hazard ratios for SAEs
  l_hr_saes <- pick_val_v(
    base = c(1, 1, 1, 1, 1, 
             exp(inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"]), 
             exp(inputs$m_hr_rrms_saes_fe["Ofatumumab SC20","mean"]), 
             exp(inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"])),
    psa = {
      temp <- c(rep(NA,n_treatments))
      # serious adverse events relative treatment effects log Hr from the NMA with Natalizumab as the reference
      temp[v_treatment_codes["Natalizumab-IV"]] <- 1 #reference treatment
      temp[v_treatment_codes["Natalizumab-SC"]] <- 1 #reference treatment
      temp[v_treatment_codes["Natalizumab biosimilar-IV"]] <- 1 #assumption
      temp[v_treatment_codes["Natalizumab biosimilar-SC"]] <- 1 #assumption
      temp[v_treatment_codes["Cladribine"]] <- 1 #assumption
      temp[v_treatment_codes["Ublituximab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"],
                                                          sd = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","sd"])) 
      temp[v_treatment_codes["Ofatumumab"]] <- exp(rnorm(1, mean = inputs$m_hr_rrms_saes_fe["Ofatumumab SC20","mean"], 
                                                         sd = inputs$m_hr_rrms_saes_fe["Ofatumumab SC20","sd"])) 
      temp[v_treatment_codes["Ocrelizumab"]] <- exp(rnorm(1,mean = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","mean"], 
                                                          sd = inputs$m_hr_rrms_saes_fe["Ocrelizumab IV600","sd"]))
      temp 
    },
    
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, length(v_treatment_codes)),    
    names_out = names(v_treatment_codes),
    deploy_env= FALSE
  )
  
) %>% add_item(
   l_hr_pbo <- pick_val_v(
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
    names_out = paste0(c("edss_increase", "arr", "discontinuation","saes")),
    deploy_env= FALSE
  )
  
) %>% add_item(
  #proportions on 2nd line treatments
  l_prop_2ndline_trt <- pick_val_v(
    base = inputs[["m_proportion_ontrtline"]]["alpha_2ndline",] /inputs[["m_proportion_ontrtline"]]["beta_2ndline",] ,
    psa = {
      
      temp<- inputs[["m_proportion_ontrtline"]]["alpha_2ndline",] /inputs[["m_proportion_ontrtline"]]["beta_2ndline",]
      
      for(i_trt in 1:ncol(inputs[["m_proportion_ontrtline"]])) {
        temp[i_trt] <- rbeta(1, shape1= inputs[["m_proportion_ontrtline"]]["alpha_2ndline", i_trt], shape2 = inputs[["m_proportion_ontrtline"]]["beta_2ndline", i_trt])
        
      }
      
      temp
      
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, ncol(inputs[["m_proportion_ontrtline"]])),    
    names_out = colnames(inputs$m_proportion_ontrtline),
    deploy_env= FALSE
  )

) %>% add_item(
  #proportions on 3rd line treatments
  l_prop_3rdline_trt <- pick_val_v(
    base = inputs[["m_proportion_ontrtline"]]["alpha_3rdline",] /inputs[["m_proportion_ontrtline"]]["beta_3rdline",] ,
    psa = {
      
      temp<- inputs[["m_proportion_ontrtline"]]["alpha_3rdline",] /inputs[["m_proportion_ontrtline"]]["beta_3rdline",]
      
      for(i_trt in 1:ncol(inputs[["m_proportion_ontrtline"]])) {
        temp[i_trt] <- rbeta(1, shape1= inputs[["m_proportion_ontrtline"]]["alpha_3rdline", i_trt], shape2 = inputs[["m_proportion_ontrtline"]]["beta_3rdline", i_trt])
        
      }
      
      temp
      
    },
    #sens = , 
    psa_ind = psa_bool,
    sens_ind = sensitivity_bool,    
    indicator = rep(indicators, ncol(inputs[["m_proportion_ontrtline"]])),    
    names_out = colnames(inputs$m_proportion_ontrtline),
    deploy_env= FALSE
  )
  
)
  }
