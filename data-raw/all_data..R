# data-raw/all_data.R

#documented R/in data.R
m_n_patients_edss <- as.matrix(read.csv("data-raw/m_n_patients_edss.csv", header = TRUE, row.names = 1))
df_c_tx <-t(read.csv("data-raw/df_c_drugs.csv", header = T, row.names = 1))
df_c_tx_admin <- t(read.csv("data-raw/df_c_drugs_admin_v2.csv", header = T, row.names = 1))
df_c_tx_monitor <-t(read.csv("data-raw/df_c_monitoring.csv", header = T, row.names = 1))
df_c_tx_load <-t(read.csv("data-raw/df_c_retreatment.csv", header = T, row.names = 1))
v_smr_severity_harding <- as.matrix(read.csv("data-raw/v_smr_severity_Harding.csv", header = T, row.names = 1))
m_proportion_ontrtline<- as.matrix(read.csv("data-raw/m_proportion_ontrtline.csv", header = T, row.names = 1))
v_tto_edss_increase_harrms_mu <- t(as.matrix(read.csv("data-raw/v_tto_edss_increase_harrms_mu.csv", header = T, row.names = 1)))
m_tto_edss_increase_harrms_sigma <- as.matrix(read.csv( file="data-raw/m_tto_edss_increase_harrms_sigma.csv", header = T, row.names = 1))
v_tto_edss_increase_rrms_mu <- t(as.matrix(read.csv("data-raw/v_tto_edss_increase_rrms_mu.csv", header = T, row.names = 1)))
m_tto_edss_increase_rrms_sigma <- as.matrix(read.csv( file="data-raw/m_tto_edss_increase_rrms_sigma.csv", header = T, row.names = 1))
v_tto_edss_increase_spms_mu <- t(as.matrix(read.csv("data-raw/v_tto_edss_increase_spms_mu.csv", header = T, row.names = 1)))
m_tto_edss_increase_spms_sigma <- as.matrix(read.csv("data-raw/m_tto_edss_increase_spms_sigma.csv", header = T, row.names = 1))
v_tto_edss_decrease_rrms_mu <- t(as.matrix(read.csv("data-raw/v_tto_edss_decrease_rrms_mu.csv", header = T, row.names = 1)))
m_tto_edss_decrease_rrms_sigma <- as.matrix(read.csv("data-raw/m_tto_edss_decrease_rrms_sigma.csv", header = T, row.names = 1))
v_tto_edss_decrease_spms_mu <- t(as.matrix(read.csv("data-raw/v_tto_edss_decrease_spms_mu.csv", header = T, row.names = 1)))
m_tto_edss_decrease_spms_sigma <- as.matrix(read.csv("data-raw/m_tto_edss_decrease_spms_sigma.csv", header = T, row.names = 1))
v_tto_relapse_harrms_mu <- as.matrix(read.csv("data-raw/v_tto_relapse_harrms_mu.csv", header = T, row.names = 1))
tto_relapse_harrms_sigma<- as.matrix(read.csv("data-raw/m_tto_relapse_harrms_sigma.csv", header = T, row.names = 1))
v_tto_relapse_spms_mu <- as.matrix(read.csv("data-raw/v_tto_relapse_spms_mu.csv", header = T, row.names = 1))
m_tto_relapse_spms_sigma<- as.matrix(read.csv("data-raw/m_tto_relapse_spms_sigma.csv", header = T, row.names = 1))
v_tto_sp_conversion_harrms_mu <- as.matrix(read.csv("data-raw/v_tto_sp_conversion_harrms_mu.csv", header = T, row.names = 1))
m_tto_sp_conversion_harrms_sigma<- as.matrix(read.csv("data-raw/m_tto_sp_conversion_harrms_sigma.csv", header = T, row.names = 1))
#pending documentation in R/in data.R
v_p_relapse_severity <- read.csv(file="data-raw/v_p_relapse_severity.csv", header = T, row.names =1)
saes_data <-read.csv(file="data-raw/df_saes_data.csv", header = T, row.names =1)
m_saes <- as.matrix(read.csv("data-raw/m_saes.csv", header = T, row.names = 1))
v_smr <- as.matrix(read.csv("data-raw/v_smr.csv", header = T, row.names = 1))
m_r_mortality <- as.matrix(read.csv("data-raw/df_all_cause_mortality.csv", header = T, row.names = 1))
m_c_edss <- as.matrix(read.csv("data-raw/m_c_edss.csv", header = T, row.names = 1))
m_c_relapse <-read.csv("data-raw/m_c_relapse.csv", header = T, row.names = 1)
df_c_spms<- read.csv("data-raw/df_c_spms.csv", header = T, row.names = 1)
u_edss <- as.matrix(read.csv("data-raw/m_u_edss.csv", header = T, row.names = 1))
df_u_relapse <- as.matrix(read.csv("data-raw/df_u_relapse.csv", header = T, row.names = 1))
m_u_carer <- as.matrix(read.csv("data-raw/m_u_carer.csv", header = T, row.names = 1))
m_discontinuation <- as.matrix(read.csv("data-raw/m_discontinuation.csv", header = T, row.names = 1))
hr_rrms_cdp3_fe <- read.csv("data-raw/Releff_RRMS_CDP3_FE.csv", header = TRUE, row.names=1)
hr_rrms_cdp6_fe <- read.csv("data-raw/Releff_RRMS_CDP6_FE.csv", header = TRUE, row.names=1)
hr_rrms_arr_fe <- read.csv("data-raw/Releff_RRMS_ARR_FE.csv", header = TRUE, row.names=1)
hr_rrms_saes_fe <- read.csv("data-raw/Releff_RRMS_SeriousAEs_FE.csv", header = TRUE, row.names=1)
hr_rrms_discontinuation_fe <- read.csv("data-raw/Releff_RRMS_Discontinuation_FE.csv", header = TRUE, row.names=1)

# load into package
usethis::use_data(
  m_n_patients_edss,
  df_c_tx,   df_c_tx_admin,   df_c_tx_monitor,   df_c_tx_load,
  v_smr_severity_harding,
  m_proportion_ontrtline,
  v_tto_edss_increase_harrms_mu,   m_tto_edss_increase_harrms_sigma,
  v_tto_edss_increase_rrms_mu,  m_tto_edss_increase_rrms_sigma,
  v_tto_edss_increase_spms_mu,  m_tto_edss_increase_spms_sigma,
  v_tto_edss_decrease_rrms_mu,  m_tto_edss_decrease_rrms_sigma,
  v_tto_edss_decrease_spms_mu,  m_tto_edss_decrease_spms_sigma,
  v_tto_relapse_harrms_mu,  tto_relapse_harrms_sigma,
  v_tto_relapse_spms_mu,  m_tto_relapse_spms_sigma,
  v_tto_sp_conversion_harrms_mu,  m_tto_sp_conversion_harrms_sigma,
  v_p_relapse_severity,
  saes_data,   m_saes,
  v_smr,
  m_r_mortality,
  m_c_edss, m_c_relapse, df_c_spms, u_edss, df_u_relapse, m_u_carer,
  m_discontinuation, 
  hr_rrms_cdp3_fe, hr_rrms_cdp6_fe, hr_rrms_arr_fe, hr_rrms_saes_fe, hr_rrms_discontinuation_fe,
  
  overwrite = TRUE
)

