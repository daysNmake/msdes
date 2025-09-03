patient_ipd <- psa_ipd[psa_ipd$trt == "Natalizumab-IV" & psa_ipd$simulation == 1 & pat_id == 1]

patient_ipd<- cbind(patient_ipd,"trt_cost" = 0, "relapse_cost" =0, "sae_cost"=0, "hs_cost"=0, "disc_trt_cost"=0, "disc_relapse_cost" =0, "disc_sae_cost"=0, "disc_hs_cost"=0, "tot_posthoc"=0)

trt_cost <- numeric(nrow(patient_ipd[,"trt_cost"]))
relapse_cost <- numeric(nrow(patient_ipd[,"relapse_cost"]))
sae_cost <- numeric(nrow(patient_ipd[,"sae_cost"]))
hs_cost <- numeric(nrow(patient_ipd[,"hs_cost"]))

system.time(

    for (row_i in 1:nrow(patient_ipd)) {
      #trt_cost[row_i] <- relapse_cost[row_i] <-  sae_cost[row_i] <-  hs_cost[row_i] <- row_i
      
      if(patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment <= 1]) {
        
        trt_cost[row_i] <- (1- patient_ipd[row_i,treatment_stop] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * (df_c_tx["cost_annual_yr1",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_admin["cost_annual_yr1",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_monitor["cost_annual_yr1",patient_ipd[row_i,current_treatment_code]]
          ) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr1","Siponimod"])))
        )
        
      } else if (patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment == 2]) {
        
        trt_cost[row_i] <- (1- patient_ipd[row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * (df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                       v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          ) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } else if (patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment == 3]) {
        
        trt_cost[row_i] <- (1- patient_ipd[row_i,treatment_stop] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          ) * df_c_tx_load[3,patient_ipd[row_i,current_treatment_code]]) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } else if (patient_ipd[row_i,evtname == "annual_treatment" & years_on_treatment == 4]) {
        
        trt_cost[row_i]  <- (1- patient_ipd[row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          )  * df_c_tx_load[4,patient_ipd[row_i,current_treatment_code]]) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      }
      else if (patient_ipd[row_i,evtname == "annual_treatment" &  years_on_treatment >= 5]) {
        
        trt_cost[row_i] <- (1- patient_ipd[row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[row_i,current_treatment_code]]
          )  * df_c_tx_load[5,patient_ipd[row_i,current_treatment_code]]) +
            (patient_ipd[row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } #else {patient_ipd[row_i,trt_cost] <- 0}
      
      
      if(patient_ipd[row_i,evtname == "relapse"]) {
        relapse_cost[row_i] <- (patient_ipd[row_i,relapse_hospital_flag]* v_c_relapse[6])+
          ((1 - patient_ipd[row_i,relapse_hospital_flag] ) * v_c_relapse[4]) 
      } #else {patient_ipd[row_i,relapse_cost] <- 0}
      
      if(patient_ipd[row_i,evtname == "serious_adverse_event"]) {
        sae_cost[row_i] <- (1- patient_ipd[row_i,treatment_stop]) * sum(v_c_sae_events * v_weight_sae)
      } #else {patient_ipd[row_i,sae_cost] <- 0}
      
      hs_cost[row_i] <- (1 - patient_ipd[row_i,spms_flag]) * (v_c_rrms_edss[patient_ipd[row_i,edss_value] == v_edss_values]) +
        (patient_ipd[row_i,spms_flag] * v_c_spms_edss[patient_ipd[row_i,edss_value] == v_edss_values])
      
      
      
    })

    patient_ipd$trt_cost <- as.numeric(trt_cost) 
    patient_ipd$relapse_cost <- as.numeric(relapse_cost) 
    patient_ipd$sae_cost <- as.numeric(sae_cost) 
    patient_ipd$hs_cost <- as.numeric(hs_cost) 


sum(patient_ipd[,trt_cost]) #783044.9
sum(patient_ipd$relapse_cost) #0.0002
sum(patient_ipd$sae_cost) #0.0003
sum(patient_ipd$hs_cost) #0.0057
#user  system elapsed 
#  0.43    0.05    1.20
#  0.50    0.06    1.16 
#  0.57    0.01    0.58
#  0.56    0.03    0.61 
#  0.44    0.04    0.48 
#  0.35    0.00    0.34 
#  0.30    0.02    0.35