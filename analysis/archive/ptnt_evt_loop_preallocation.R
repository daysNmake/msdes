
patient_ipd<- cbind(patient_ipd,"trt_cost" = 0, "relapse_cost" =0, "sae_cost"=0, "hs_cost"=0, "disc_trt_cost"=0, "disc_relapse_cost" =0, "disc_sae_cost"=0, "disc_hs_cost"=0, "tot_posthoc"=0)


#calculate an average number of events per patient to start with, then update with the correct number in the loop
n_events <- c(rep(round(nrow(patient_ipd)/n_patients), n_patients))
x <- c(rep(0, mean(n_events)))
trt_cost <- rep(list(x),n_patients)
# trt_cost <- matrix(rep(
#   c(rep(0, mean(n_events))),
#   n_patients), ncol = n_patients)

for(i_patient in 1:n_patients) {
  n_events[i_patient] <-length(patient_ipd$trt_cost[patient_ipd$pat_id == i_patient])
  trt_cost[[i_patient]] <- rep(0, n_events[i_patient])
  #trt_cost[i_patient] <- patient_ipd$trt_cost[patient_ipd$pat_id == i_patient]
#relapse_cost[i_patient] <- numeric(nrow(patient_ipd[pat_id == i_patient][pat_id == i_patient][,"relapse_cost"]))
#sae_cost[i_patient] <- numeric(nrow(patient_ipd[,"sae_cost"]))
#hs_cost[i_patient] <- numeric(nrow(patient_ipd[pat_id == i_patient][,"hs_cost"]))
}

rm(y)
system.time(
  
  for(i_patient in 1:n_patients) {
    
    y <- trt_cost[[i_patient]]
    # relapse_cost <- relapse_cost[i_patient] 
    # sae_cost <-  sae_cost[i_patient] 
    # hs_cost <-  hs_cost[i_patient] 
    
    for (row_i in 1:nrow(patient_ipd)) {

      
      if(patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment <= 1]) {
        
        y[row_i] <- (1- patient_ipd[row_i,treatment_stop] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * (df_c_tx["cost_annual_yr1",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_admin["cost_annual_yr1",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_monitor["cost_annual_yr1",patient_ipd[row_i,current_treatment_code]]
          ) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr1","Siponimod"])))
        )
        
      } else if (patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment == 2]) {
        
        y[row_i] <- (1- patient_ipd[row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * (df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          ) +/
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } else if (patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment == 3]) {
        
        y[row_i] <- (1- patient_ipd[row_i,treatment_stop] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          ) * df_c_tx_load[3,patient_ipd[row_i,current_treatment_code]]) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } else if (patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment == 4]) {
        
        y[row_i] <- (1- patient_ipd[row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          )  * df_c_tx_load[4,patient_ipd[row_i,current_treatment_code]]) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      }
      else if (patient_ipd[row_i,evtname == "annual_treatment" &  years_on_treatment >= 5]) {
        
        y[row_i] <- (1- patient_ipd[row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          )  * df_c_tx_load[5,patient_ipd[row_i,current_treatment_code]]) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } #else {patient_ipd[row_i,trt_cost] <- 0}
      
      # 
      # if(patient_ipd[row_i,evtname == "relapse"]) {
      #   relapse_cost[row_i] <- (patient_ipd[row_i,relapse_hospital_flag]* v_c_relapse[6])+
      #     ((1 - patient_ipd[row_i,relapse_hospital_flag] ) * v_c_relapse[4]) 
      # } #else {patient_ipd[row_i,relapse_cost] <- 0}
      # 
      # if(patient_ipd[row_i,evtname == "serious_adverse_event"]) {
      #   sae_cost[row_i] <- (1- patient_ipd[row_i,treatment_stop]) * sum(v_c_sae_events * v_weight_sae)
      # } #else {patient_ipd[row_i,sae_cost] <- 0}
      # 
      # hs_cost[row_i] <- (1 - patient_ipd[row_i,spms_flag]) * (v_c_rrms_edss[patient_ipd[row_i,edss_value] == v_edss_values]) +
      #   (patient_ipd[row_i,spms_flag] * v_c_spms_edss[patient_ipd[row_i,edss_value] == v_edss_values])
      # 
      
      
    }})

    patient_ipd$trt_cost <- as.numeric(trt_cost) 
    patient_ipd$relapse_cost <- as.numeric(relapse_cost) 
    patient_ipd$sae_cost <- as.numeric(sae_cost) 
    patient_ipd$hs_cost <- as.numeric(hs_cost) 


sum(patient_ipd[,trt_cost]) #783044.9
sum(patient_ipd$relapse_cost) #0.0002
sum(patient_ipd$sae_cost) #0.0003
sum(patient_ipd$hs_cost) #0.0057
#user  system elapsed 
