# data-raw/all_data.R


m_n_patients_edss <- as.matrix(read.csv("data-raw/m_n_patients_edss.csv", header = TRUE, row.names = 1))
df_c_tx <-t(read.csv("data-raw/df_c_drugs.csv", header = T, row.names = 1))
df_c_tx_admin <- t(read.csv("data/df_c_drugs_admin_v2.csv", header = T, row.names = 1))
df_c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
df_c_tx_load <-t(read.csv("data/df_c_retreatment.csv", header = T, row.names = 1))
v_smr_severity_harding <- as.matrix(read.csv("data/v_smr_severity_Harding.csv", header = T, row.names = 1))

usethis::use_data(
  m_n_patients_edss,
  df_c_tx,
  df_c_tx_admin,
  df_c_tx_monitor,
  df_c_tx_load,
  v_smr_severity_harding,
  overwrite = TRUE
)