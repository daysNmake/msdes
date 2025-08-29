
#rm(list = ls())
#setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/MS-MTA")

library(dplyr)
#ipd
psa_ipd <- bind_rows(map(results$output_psa, "merged_df"))



#seoerate out Nat-IV ipd and recombine once anaysis is complete
nativ_ipd <- psa_ipd[ psa_ipd$trt == "Natalizumab-IV"]
remainder_ipd <- psa_ipd[ psa_ipd$trt != "Natalizumab-IV"]



# treatment, relpase,  health state cost data input ( when not avaible outside descem enviroment)
#---------------------------------------------------------------------------------------------
v_c_trt_admin <- matrix(df_c_tx_admin, nrow = nrow(df_c_tx_admin), ncol = n_treatments,
                        dimnames = list(row.names(df_c_tx_admin), v_treatment_names))

v_c_trt_monitor <- matrix(df_c_tx_monitor, nrow = nrow(df_c_tx_monitor), ncol = n_treatments,
                          dimnames = list(row.names(df_c_tx_monitor), v_treatment_names))

v_c_rrms_edss <- m_c_edss_rrms[,"rrms_mean"] 
v_c_spms_edss <- m_c_edss_spms[,"spms_mean"] 
names(v_c_spms_edss) <- names(v_c_rrms_edss) <- v_edss_values

v_c_relapse <- m_c_relapse[, "mean"]
v_c_sae_events <- t(matrix(df_c_sae, nrow = n_sae, ncol = 1,
                           dimnames = list(sae_names, "sae_cost")))

## addin instant  costs:
#-----------------------
# treatment costs

nativ_ipd<- cbind(nativ_ipd,"trt_cost" = 0, "relapse_cost" =0, "sae_cost"=0, "hs_cost"=0, "disc_trt_cost"=0, "disc_relapse_cost" =0, "disc_sae_cost"=0, "disc_hs_cost"=0, "tot_posthoc"=0)
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nativ_ipd[simulation == i_sample][pat_id == i_patient])) {
  
  
  if(nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment <= 1]) {
    
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,treatment_stop] ) *(
      (1 - nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * (df_c_tx["cost_annual_yr1",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_admin["cost_annual_yr1",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_monitor["cost_annual_yr1",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      ) +
        (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr1","Siponimod"])))
    )
    
  } else if (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 2]) {
    
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"treatment_stop"] ) *(
      (1 - nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * (df_c_tx["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_admin["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_monitor["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      ) +
        (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  } else if (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 3]) {
    
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,treatment_stop] ) *(
      (1 - nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      ) * df_c_tx_load[3,nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]) +
        (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  } else if (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 4]) {
    
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"treatment_stop"] ) *(
      (1 - nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      )  * df_c_tx_load[4,nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]) +
        (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  }
  else if (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" &  years_on_treatment >= 5]) {
    
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"treatment_stop"] ) *(
      (1 - nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      )  * df_c_tx_load[5,nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]) +
        (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  } #else {nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,trt_cost] <- 0}
  
  
  if(nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,evtname == "relapse"]) {
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"relapse_cost"] <- (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,relapse_hospital_flag]* v_c_relapse[6])+
      ((1 - nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,relapse_hospital_flag] ) * v_c_relapse[4]) 
  } #else {nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,relapse_cost] <- 0}
  
  if(nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,evtname == "serious_adverse_event"]) {
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"sae_cost"] <- (1- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,treatment_stop]) * sum(v_c_sae_events * v_weight_sae)
  } #else {nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,sae_cost] <- 0}
  
  nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"hs_cost"] <- (1 - nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * (v_c_rrms_edss[nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,edss_value] == v_edss_values]) +
    (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * v_c_spms_edss[nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,edss_value] == v_edss_values])
  
}
}
}

## discount  costs
#--------------------------------------
# the instantaneous costs

lcldrc=0.035
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nativ_ipd[simulation == i_sample][pat_id == i_patient])) {
  
#  if (nativ_ipd[row_i,trt_cost > 0] ) {
    
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"disc_trt_cost"]<- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,trt_cost] *  ((1+lcldrc)^(-nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
#    } else {nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"disc_trt_cost"]<-0}
  
  nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"disc_relapse_cost"] <- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,relapse_cost] *  ((1+lcldrc)^(-nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
  nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"disc_sae_cost"] <- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,sae_cost] *  ((1+lcldrc)^(-nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
  #nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,disc_hs_cost] <- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,hs_cost] *  ((1+lcldrc)^(-nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,curtime])) # ongoing not instantaious costs
  
}
}}

## discount on going costs

Instantdrc <- log(1+lcldrc)
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nativ_ipd[simulation == i_sample][pat_id == i_patient])) {
  
  #if (nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,hs_cost > 0] ) {
    nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"disc_hs_cost"] <- ((nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,hs_cost])/(0 - Instantdrc)) * (exp(nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,curtime] * ( 0 - Instantdrc)) - exp(nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,prevtime] * (0 - Instantdrc)))
  #} #else {nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,disc_hs_cost] <-0}
}
}}

## do the trt costs tally up? (must fix this by subtracting additional cost ( the health state costs))
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nativ_ipd[simulation == i_sample][pat_id == i_patient])) {
  nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,"tot_posthoc"] <- nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,disc_trt_cost]+ nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,disc_hs_cost] + 
                                                              nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,disc_relapse_cost] + nativ_ipd[simulation == i_sample][pat_id == i_patient][row_i,disc_sae_cost]
}
}}

## do the all costs tally up? 
post_hoc_cost <- model_cost <- delta_cost <-  array(NA, dim = c(n_samples,n_patients),
                       dimnames = list(NULL))
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
  for (row_i in 1:nrow(nativ_ipd[simulation == i_sample][pat_id == i_patient])) {
post_hoc_cost[i_sample,i_patient] <- sum(nativ_ipd[simulation == i_sample][pat_id == i_patient][,"disc_trt_cost"]) 
model_cost[i_sample,i_patient] <- sum(nativ_ipd[simulation == i_sample][pat_id == i_patient][,"cost"])
delta_cost[i_sample,i_patient] <- ((post_hoc_cost[i_sample,i_patient] - model_cost[i_sample,i_patient]) / model_cost[i_sample,i_patient]) * 100
  }
}
}

paste(delta_cost, "%")
mean(delta_cost)
quantile(delta_cost, probs = c(0.025, 0.5, 0.9725))

#           2.5%             50%          97.25% 
#-11.45737268772  -1.47243292601  -0.00001249586 




