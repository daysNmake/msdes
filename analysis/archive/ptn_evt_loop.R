patient_ipd<- cbind(patient_ipd,"trt_cost" = 0, "relapse_cost" =0, "sae_cost"=0, "hs_cost"=0, "disc_trt_cost"=0, "disc_relapse_cost" =0, "disc_sae_cost"=0, "disc_hs_cost"=0, "tot_posthoc"=0)
system.time(
  for(i_patient in 1:n_patients) {
    for (row_i in 1:nrow(patient_ipd[pat_id == i_patient])) {
      
      if(patient_ipd[pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment <= 1]) {
        
        patient_ipd[pat_id == i_patient][row_i,"trt_cost"] <- (1- patient_ipd[pat_id == i_patient][row_i,treatment_stop] ) *(
          (1 - patient_ipd[pat_id == i_patient][row_i,spms_flag]) * (df_c_tx["cost_annual_yr1",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                       v_c_trt_admin["cost_annual_yr1",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                       v_c_trt_monitor["cost_annual_yr1",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]
          ) +
            (patient_ipd[pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr1","Siponimod"])))
        )
        
      } else if (patient_ipd[pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 2]) {
        
        patient_ipd[pat_id == i_patient][row_i,"trt_cost"] <- (1- patient_ipd[pat_id == i_patient][row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[pat_id == i_patient][row_i,spms_flag]) * (df_c_tx["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                       v_c_trt_admin["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                       v_c_trt_monitor["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]
          ) +
            (patient_ipd[pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } else if (patient_ipd[pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 3]) {
        
        patient_ipd[pat_id == i_patient][row_i,"trt_cost"] <- (1- patient_ipd[pat_id == i_patient][row_i,treatment_stop] ) *(
          (1 - patient_ipd[pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]
          ) * df_c_tx_load[3,patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]) +
            (patient_ipd[pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } else if (patient_ipd[pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 4]) {
        
        patient_ipd[pat_id == i_patient][row_i,"trt_cost"] <- (1- patient_ipd[pat_id == i_patient][row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]
          )  * df_c_tx_load[4,patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]) +
            (patient_ipd[pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      }
      else if (patient_ipd[pat_id == i_patient][row_i,evtname == "annual_treatment" &  years_on_treatment >= 5]) {
        
        patient_ipd[pat_id == i_patient][row_i,"trt_cost"] <- (1- patient_ipd[pat_id == i_patient][row_i,"treatment_stop"] ) *(
          (1 - patient_ipd[pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                        v_c_trt_admin["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]] +
                                                                        v_c_trt_monitor["cost_annual_yr2",patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]
          )  * df_c_tx_load[5,patient_ipd[pat_id == i_patient][row_i,current_treatment_code]]) +
            (patient_ipd[pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                                                    (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
        )
        
      } #else {patient_ipd[pat_id == i_patient][row_i,trt_cost] <- 0}
      
      
      if(patient_ipd[pat_id == i_patient][row_i,evtname == "relapse"]) {
        patient_ipd[pat_id == i_patient][row_i,"relapse_cost"] <- (patient_ipd[pat_id == i_patient][row_i,relapse_hospital_flag]* v_c_relapse[6])+
          ((1 - patient_ipd[pat_id == i_patient][row_i,relapse_hospital_flag] ) * v_c_relapse[4]) 
      } #else {patient_ipd[pat_id == i_patient][row_i,relapse_cost] <- 0}
      
      if(patient_ipd[pat_id == i_patient][row_i,evtname == "serious_adverse_event"]) {
        patient_ipd[pat_id == i_patient][row_i,"sae_cost"] <- (1- patient_ipd[pat_id == i_patient][row_i,treatment_stop]) * sum(v_c_sae_events * v_weight_sae)
      } #else {patient_ipd[pat_id == i_patient][row_i,sae_cost] <- 0}
      
      patient_ipd[pat_id == i_patient][row_i,"hs_cost"] <- (1 - patient_ipd[pat_id == i_patient][row_i,spms_flag]) * (v_c_rrms_edss[patient_ipd[pat_id == i_patient][row_i,edss_value] == v_edss_values]) +
        (patient_ipd[pat_id == i_patient][row_i,spms_flag] * v_c_spms_edss[patient_ipd[pat_id == i_patient][row_i,edss_value] == v_edss_values])
      
    }
  })