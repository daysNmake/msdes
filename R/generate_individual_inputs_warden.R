# This is called for each new patient so sample 1 from a distribution of choice
# for heterogeneous characteristics


# Objects that depend on treatment
# Note that the current treatment is later initialised to trt
unique_pt_inputs <- add_item(edss_value = sample(size = 1, x = v_edss_values,  
                                                 # rrms larger sample size, option to use harrms
                                                 prob = rdiric(1, inputs[["m_n_patients_edss"]]["harrms",])),
                             #(male=0, female=1)
                             sex = sample(size = 1, x = c(0, 1), 
                                          prob = c(1 - prop_female, prop_female)),
                             n_relapse = 0,
                             n_sae_evts = 0,
                             n_edssinc_evts = 0,
                             n_edssdec_evts = 0,
                             t_progression = 0,
                             t_2ndline = 0, t_3rdline = 0, t_4thline = 0,
                             tto_dp1 = 0, tto_dp2 = 0, tto_dp3 = 0, tto_dp4 = 0, tto_dp5 = 0, tto_dp6 = 0, tto_dp7 = 0,  #time to progression for each event upto 7 events
                             tto_rel1 = 0, tto_rel2 = 0, tto_rel3 = 0, tto_rel4 = 0, tto_rel5 = 0, tto_rel6 = 0, tto_rel7 = 0,  #time to progression for each event upto 7 events
                             yrs_edss_0 = 0, yrs_edss_1 = 0, yrs_edss_2 = 0, yrs_edss_3 = 0, yrs_edss_4 = 0, yrs_edss_5 = 0, yrs_edss_6 = 0, yrs_edss_7 = 0, yrs_edss_8 = 0, yrs_edss_9 = 0, #  annual severity state counter
                             waning = 0, # waning flag set to zero will have no effect on HR x (1-waning)
                             spms_flag = sample(size = 1, 
                                                x = c(0, 1), # Random
                                                prob = c(1 - inputs[["initial_spms_prop"]], inputs[["initial_spms_prop"]])),
                             relapse_hospital_flag = sample(size = 1, 
                                                            x = c(0, 1), # Random
                                                            prob = c(1 - inputs[["p_relapse_hospital"]],inputs[["p_relapse_hospital"]])),
                             #current_treatment = "Natalizumab", # This is later changed to trt
                             current_treatment = sample(x = v_treatment_names, size=1, prob = (pluck_vector_from_env(inputs[["m_proportion_ontrtline"]]["alpha_3rdline",], v_treatment_names) / 
                                                                                                 pluck_vector_from_env(inputs[["m_proportion_ontrtline"]]["beta_3rdline",], v_treatment_names))),
                             #current_treatment_code = 1, # This is later changed to code of trt
                             current_treatment_code = v_treatment_codes[current_treatment],
                             treatment_line = 2, # all patients starting on 2nd line treatment
                             n_switch = 0, # treatment switch counter for sense checking
                             years_on_treatment  = 0, #  annual treatment counter for drug acquisition costs
                             treatment_stop = 0, # all patients start on treatment, flag set to 1 when edss is 7+
                             age = initial_age,
                             utilities_disease = (1 - spms_flag) * pluck_vector_from_env(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                               spms_flag * pluck_vector_from_env(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values])),
                             costs_disease =(1 - spms_flag) * pluck_vector_from_env(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                               spms_flag *  pluck_vector_from_env(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values])),
                             costs_treatment = if(years_on_treatment <= 1) {(1- treatment_stop ) *(
                               (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr1",current_treatment_code] +
                                                    pluck_vector_from_env(l_c_trt_admin_yr1, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                                    pluck_vector_from_env(l_c_trt_monitor_yr1, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) +
                                 spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) +
                                                (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr1","Siponimod"])))
                             } else if(years_on_treatment == 2){(1- treatment_stop ) * (
                               (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                                    pluck_vector_from_env(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                                    pluck_vector_from_env(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])))+
                                 spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                                (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                             } else if(years_on_treatment == 3){(1- treatment_stop ) * (
                               (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                                    pluck_vector_from_env(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                                    pluck_vector_from_env(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) * inputs$df_c_tx_load[3,current_treatment_code])+
                                 spms_flag *(((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                               (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                             } else if(years_on_treatment == 4){ (1- treatment_stop ) *(
                               (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                                    pluck_vector_from_env(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                                    pluck_vector_from_env(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) * inputs$df_c_tx_load[4,current_treatment_code])+
                                 spms_flag *(((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                               (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                             } else {(1- treatment_stop ) *(
                               (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                                    pluck_vector_from_env(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                                    pluck_vector_from_env(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) * inputs$df_c_tx_load[5,current_treatment_code])+
                                 spms_flag *(((1 - inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                               (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                             },
                             utilities_relapse= u_mean_relapse_hospital,
                             costs_relapse = (relapse_hospital_flag * l_c_relapse[["Relapse_hospital_addmision"]])+
                               ((1-relapse_hospital_flag) *l_c_relapse[["Relapse_oral_steroids"]]),
                             utilities_sae= utilities_sae_weighted,
                             costs_sae = (1- treatment_stop ) * costs_sae_weighted
)
