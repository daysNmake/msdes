

#generate model inputs
#function takes names vectors and severity states
gen_inputs <- function(){
  
  ## baseline distribution of EDSS health states (MS Registry)
  m_n_patients_edss <- as.matrix(read.csv("data/m_n_patients_edss.csv", header = T, row.names = 1))
  colnames(m_n_patients_edss) <- names(v_edss_values)
  
  ##  Annual treatment, administration and monitoring costs 
  #public list prices
  df_c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
  df_c_tx <- df_c_tx[,intersect(colnames(df_c_tx),v_treatment_names)]
  df_c_tx[,1:4] <- (df_c_tx[,1:4]*0.6*(round(52/5.55, 2)/13)) + df_c_tx[,1:4]*0.4 # 60% on EID interval dosing
  
  # These are direct medical costs we excluded the indirect medical costs
  df_c_tx_admin <- t(read.csv("data/df_c_drugs_admin_v2.csv", header = T, row.names = 1))
  df_c_tx_admin[df_c_tx_admin==0] <-0.1 # set zero costs to positive number stops generating NaNs
  df_c_tx_admin <- df_c_tx_admin[,intersect(colnames(df_c_tx_admin),v_treatment_names)]
  df_c_tx_admin[,1:4] <- (df_c_tx_admin[,1:4]*0.6*(round(52/5.55, 2)/13)) + df_c_tx_admin[,1:4]*0.4 # 60% on EID interval dosing
  df_c_tx_admin[1,"Natalizumab-SC"] <- (df_c_tx_admin[1,"Natalizumab-SC"] * 0.5) # Scenario for reduced admin cost for Nat SC year 1
  df_c_tx_admin[2:3,"Natalizumab-SC"] <- (df_c_tx_admin[2:3,"Natalizumab-SC"] * 0.5) # Scenario for reduced admin cost for Nat SC year 2

  df_c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
  df_c_tx_monitor <- df_c_tx_monitor[,intersect(colnames(df_c_tx_monitor),v_treatment_names)]
  df_c_tx_monitor[,"Natalizumab-IV"] <- (df_c_tx_monitor[,"Natalizumab-IV"] - 247) #  free JCV test 
  df_c_tx_monitor[,"Natalizumab-SC"] <- (df_c_tx_monitor[,"Natalizumab-SC"] - 247) #  free JCV test 
  df_c_tx_monitor[,"Natalizumab biosimilar-IV"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-IV"] - 247) #  free JCV test 
  df_c_tx_monitor[,"Natalizumab biosimilar-SC"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-SC"] - 247) #  free JCV test
  
  ## Annual treatment scheduled
  df_c_tx_load <-t(read.csv("data/df_c_retreatment.csv", header = T, row.names = 1))
  df_c_tx_load <- df_c_tx_load[,intersect(colnames(df_c_tx_load),v_treatment_names)]
  rownames(df_c_tx_load)<- c(seq(1,5,1))

  ## Excess all cause mortality for MS patients HR by severity edss<4 (Jick et al), edss>=4 (Harding et al)
  v_smr_severity_harding <- as.matrix(read.csv("data/v_smr_severity_Harding.csv", header = T, row.names = 1))
  v_smr_severity_harding[,c(9:10)] <- v_smr_severity_harding[,8] # Assuming SMR on EDSS 8 & 9 are equal to EDSS7 
  dimnames(v_smr_severity_harding) <- list(c("smr", "sd", "logsd"), v_edss_values)
  
  ## proportions on 2nd and 3rd line treatment, assuming Cladribine and Ublituximab are equivalent to Natalizumab
  m_proportion_ontrtline<- read.csv("data/m_proportion_ontrtline.csv", header = T, row.names = 1)
  colnames(m_proportion_ontrtline) <- v_treatment_names
  
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
  
  ## Excess all cause mortality for MS patients Jick 2014 
  v_smr <- read.csv("data/v_smr.csv", header = T, row.names = 1)
  
  ##rates of mortality, ONS
  m_r_mortality <- as.matrix(read.csv("data/df_all_cause_mortality.csv", header = T, row.names = 1))
  m_r_mortality <- m_r_mortality[,c("age", "mx", "mx.1")]
  m_r_mortality[,"age"] <- as.numeric(m_r_mortality[,"age"]) #check if this step is needed?
  dimnames(m_r_mortality) <-  list(NULL, c("time" ,"male", "female"))
  
  ## edss costs direct medical costs Tyas etal. 2005 inflated to 2022/2023 
  m_c_edss <- read.csv("data/m_c_edss.csv", header = T, row.names = 1) 
  m_c_edss_rrms <- m_c_edss[,c("rrms_mean","rrms_se")]
  m_c_edss_spms <- m_c_edss[,c("spms_mean","spms_se")]
  
  # SPMS Costs
  df_c_spms<- read.csv("data/df_c_spms.csv", header = T, row.names = 1)
  colnames(df_c_spms)[1] <- "PegIFN-β-1 SC 125μg"
  
  p_siponimod <-0.556962025 #proportion treated with Siponimod based on MS registry data
  
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
  
  return(list(
    ## baseline distribution of EDSS health states (MS Registry)
    m_n_patients_edss=m_n_patients_edss, 
    ##  Annual treatment, administration and monitoring costs 
    df_c_tx=df_c_tx, df_c_tx_admin=df_c_tx_admin, df_c_tx_monitor=df_c_tx_monitor, 
    ## Annual treatment scheduled
    df_c_tx_load=df_c_tx_load ,
    ## Excess all cause mortality for MS patients
    v_smr_severity_harding=v_smr_severity_harding, 
    ## proportions on 2nd and 3rd line treatment
    m_proportion_ontrtline=m_proportion_ontrtline,
    ## Natural History MS Registry
    v_tto_edss_increase_harrms_mu=v_tto_edss_increase_harrms_mu, m_tto_edss_increase_harrms_sigma=m_tto_edss_increase_harrms_sigma,
    v_tto_edss_increase_rrms_mu=v_tto_edss_increase_rrms_mu, m_tto_edss_increase_rrms_sigma=m_tto_edss_increase_rrms_sigma,
    v_tto_edss_increase_spms_mu=v_tto_edss_increase_spms_mu, m_tto_edss_increase_spms_sigma=m_tto_edss_increase_spms_sigma,
    v_tto_edss_decrease_rrms_mu=v_tto_edss_decrease_rrms_mu, m_tto_edss_decrease_rrms_sigma=m_tto_edss_decrease_rrms_sigma,
    v_tto_edss_decrease_spms_mu=v_tto_edss_decrease_spms_mu, m_tto_edss_decrease_spms_sigma=m_tto_edss_decrease_spms_sigma,
    v_tto_relapse_harrms_mu=v_tto_relapse_harrms_mu, tto_relapse_harrms_sigma=tto_relapse_harrms_sigma,
    v_tto_relapse_spms_mu=v_tto_relapse_spms_mu, m_tto_relapse_spms_sigma=m_tto_relapse_spms_sigma,
    v_tto_sp_conversion_harrms_mu=v_tto_sp_conversion_harrms_mu, m_tto_sp_conversion_harrms_sigma=m_tto_sp_conversion_harrms_sigma,
    ## baseline proportion of patients with SPMS
    initial_spms_prop=initial_spms_prop,
    ## Proportion of patients having relapse leading to hospital_admission
    p_relapse_hospital=p_relapse_hospital, 
    ## Incidence & weights of Serious Adverse events
    sae_names=sae_names, 
    # Number of serious adverse events in the model
    n_sae=n_sae, 
    # The weights are used to calculate SAEs costs and utilities 
    v_weight_sae=v_weight_sae, 
    # % on Natalizumab would have experienced SAEs
    m_saes=m_saes,
    ## Excess all cause mortality for MS patients
    v_smr=v_smr, 
    ##rates of mortality, ONS
    m_r_mortality=m_r_mortality, 
    ## edss costs direct medical costs
    m_c_edss_rrms=m_c_edss_rrms,
    m_c_edss_spms=m_c_edss_spms, 
    # SPMS Costs
    df_c_spms=df_c_spms,
    p_siponimod=p_siponimod, 
    ## Relapse Costs 
    m_c_relapse=m_c_relapse,
    ## Adverse events costs
    df_c_sae=df_c_sae,
    ## RRMS & SPMS edss utilities
    m_u_edss_rrms=m_u_edss_rrms,
    m_u_edss_spms=m_u_edss_spms, 
    ## Relapse utilities
    df_u_relapse=df_u_relapse, 
    ## Carer disutilities
    m_u_carer=m_u_carer, 
    ## Adverse events dis-utilities
    df_u_sae=df_u_sae, 
    ## baseline discontinuation
    m_discontinuation=m_discontinuation,
    ## NMA data
    m_hr_rrms_cdp3_fe=m_hr_rrms_cdp3_fe, 
    m_hr_rrms_cdp6_fe=m_hr_rrms_cdp6_fe, 
    m_hr_rrms_arr_fe=m_hr_rrms_arr_fe, 
    m_hr_rrms_saes_fe=m_hr_rrms_saes_fe,
    m_hr_rrms_discontinuation_fe=m_hr_rrms_discontinuation_fe
  )
  )

  
}











