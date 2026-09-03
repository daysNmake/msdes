
## WARDEN implementation of reaction events
# need to ask for JSA for review implementation of death
# reaction events run on treatment codes

gen_react_events <- function(RRMS_events){
  
  
  add_reactevt(name_evt = "initialise",
               input = {
                 # This ensures that the current treatment is the initial treatment
                 # of this strategy.
                 current_treatment <- arm
                 current_treatment_code <- v_treatment_codes[arm]
                 # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                 utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                                      spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                 costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                                    spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                 # modify_event(list(#future death events do not exceed time horizon
                 #   "death"= min(cur_evtlist[["death"]], time_horizon)
                 # ))
                 
                 # annual cost events
                 new_event(list(
                   "annual_treatment" = curtime 
                 ))
               }) %>%
    add_reactevt(name_evt = "age",
                 input = {
                   age <- initial_age + round(curtime)
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                 }) %>%
    
    
    #add annual cost events
    add_reactevt(name_evt = "annual_treatment",
                 input = { 
                   years_on_treatment <- years_on_treatment  + 1
                   age <- initial_age + round(curtime)

                   # time on treatment-line counter 
                   if(treatment_line == 2 & spms_flag == 0 & treatment_stop == 0) {
                     t_2ndline <- years_on_treatment
                   } else if (treatment_line == 3 & spms_flag == 0 & treatment_stop == 0){
                     t_3rdline <- years_on_treatment
                   }else if (treatment_line == 4 & spms_flag == 0 & treatment_stop == 0){
                     t_4thline <- years_on_treatment
                   }
                   

                   if (edss_value <= 6 & treatment_stop == 0) {new_event(list(
                     "annual_treatment" = curtime + 1
                   ))}
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                   # Instantaneous treatment costs 
                   costs_treatment[years_on_treatment <= 1] <- (1- treatment_stop ) *(
                     (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr1",current_treatment_code] +
                                          purrr::pluck(l_c_trt_admin_yr1, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                          purrr::pluck(l_c_trt_monitor_yr1, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) +
                       spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) +
                                      (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr1","Siponimod"])))
                   
                   costs_treatment[years_on_treatment >= 2 & years_on_treatment <= 4] <- (1- treatment_stop ) * (
                     (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                          purrr::pluck(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                          purrr::pluck(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])))+
                       spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                      (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
    
                   costs_treatment[years_on_treatment >= 5] <- (1- treatment_stop ) *(
                     (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                          purrr::pluck(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                                          purrr::pluck(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) * inputs$df_c_tx_load[5,current_treatment_code])+
                       spms_flag *(((1 - inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                     (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                   #  costs_treatment = 
                   # if(years_on_treatment <= 1) {(1- treatment_stop ) *(
                   #   (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr1",current_treatment_code] +
                   #                        purrr::pluck(l_c_trt_admin_yr1, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                   #                        purrr::pluck(l_c_trt_monitor_yr1, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) +
                   #     spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) +
                   #                    (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr1","Siponimod"])))
                   # } else if(years_on_treatment == 2){(1- treatment_stop ) * (
                   #   (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                   #                        purrr::pluck(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                   #                        purrr::pluck(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])))+
                   #     spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                   #                    (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                   # } else if(years_on_treatment == 3){(1- treatment_stop ) * (
                   #   (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                   #                        purrr::pluck(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                   #                        purrr::pluck(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) * inputs$df_c_tx_load[3,current_treatment_code])+
                   #     spms_flag *(((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                   #                   (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                   # } else if(years_on_treatment == 4){ (1- treatment_stop ) *(
                   #   (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                   #                        purrr::pluck(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                   #                        purrr::pluck(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) * inputs$df_c_tx_load[4,current_treatment_code])+
                   #     spms_flag *(((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                   #                   (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                   # } else {(1- treatment_stop ) *(
                   #   (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                   #                        purrr::pluck(l_c_trt_admin_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) +
                   #                        purrr::pluck(l_c_trt_monitor_yr2, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) * inputs$df_c_tx_load[5,current_treatment_code])+
                   #     spms_flag *(((1 - inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                   #                   (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                   # }
                 }
    ) %>%
    add_reactevt(name_evt = "relapse",
                 input = {
                   n_relapse<- n_relapse + 1
                   age <- initial_age + round(curtime)
                   
                   # # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                   # instantaneous costs and utilities
                   utilities_relapse <- l_u_mean_relapse_hospital$utility
                   costs_relapse <- (relapse_hospital_flag * l_c_relapse[["Relapse_hospital_addmision"]])+
                     ((1-relapse_hospital_flag) *l_c_relapse[["Relapse_oral_steroids"]])
                   
                   #  next time to relapse
                   new_event(list(
                     "relapse" = curtime + draw_tte(
                       1, 
                       dist = "exp", 
                       coef1 = (1 - spms_flag) * (purrr::pluck(l_baseline_harrms_larr, "rate")  +
                                                    (purrr::pluck(l_baseline_harrms_larr, "BaselineWebEDSS") * edss_value) +
                                                    purrr::pluck(l_baseline_harrms_larr, "DMTNatalizumab") ) +
                         # MS registry time to relapse SPMS log rate
                         spms_flag * (purrr::pluck(l_baseline_spms_larr, "rate") +
                                        (purrr::pluck(l_baseline_spms_larr, "BaselineWebEDSS") * edss_value)),
                       beta_tx =  (1 - spms_flag) * ((1 - treatment_stop) * (purrr::pluck(l_hr_arr, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))*(1 - waning)) +
                                                 treatment_stop * l_hr_pbo$arr ) + spms_flag
                     )))
                 }) %>%
    add_reactevt(name_evt = "edss_increase",
                 input = {
                   edss_value <- min(edss_value + 1, max(v_edss_values))
                   age <- initial_age + round(curtime)
                   n_edssinc_evts <- n_edssinc_evts + 1
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   
                   if (edss_value > 6 & treatment_stop == 0) {treatment_stop <- 1}
                   
                  
                   # simulate the next time to EDSS increase
                   if (edss_value < 9) {new_event(list(
                     "edss_increase" = curtime + draw_tte(
                       1, 
                       dist = "exp",
                       coef1 = (1 - spms_flag) * (purrr::pluck(l_baseline_harrms_edss_increase, "rate") +
                                                 (purrr::pluck(l_baseline_harrms_edss_increase, "BaselineWebEDSS") * (edss_value)) + 
                                                  purrr::pluck(l_baseline_harrms_edss_increase, "DMTNatalizumab")) +
                         spms_flag * (purrr::pluck(l_spms_edss_increase, "rate") +
                                      purrr::pluck(l_spms_edss_increase, "BaselineWebEDSS") * (edss_value)),  
                       beta_tx =  (1 - spms_flag ) * ((1 - treatment_stop) * (purrr::pluck(l_hr_edss_increase, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) *(1 - waning)) + 
                                                  treatment_stop * l_hr_pbo$edss_increase ) + spms_flag
                     )
                   ))}
                 }) %>%
    add_reactevt(name_evt = "edss_decrease",
                 input = {
                   edss_value <- max(edss_value - 1, min(v_edss_values))
                   age <- initial_age + round(curtime)
                   n_edssdec_evts <- n_edssdec_evts + 1

                   if (edss_value > 6 & treatment_stop == 0) {treatment_stop <- 1}
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                   #  next time to EDSS decrease
                   if (edss_value > 0) {new_event(list(
                     "edss_decrease" = curtime + draw_tte(
                       1, 
                       dist = "exp",
                       # EDSS decrease  
                       coef1 = (1 - spms_flag) * (purrr::pluck(l_rrms_edss_decrease, "rate") +
                                                  purrr::pluck(l_rrms_edss_decrease, "BaselineWebEDSS")*(edss_value) )  + 
                         #setting a very high event time to edss decrease for SPMS patients
                         (spms_flag * -10 )
                     )
                   ) )}
                 }) %>%
    add_reactevt(name_evt = "progression",
                 # Progression can only occur once so just change the SPMS status
                 input = {
                   age <- initial_age + round(curtime)
                   spms_flag <- 1
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                   
                 }) %>%
    add_reactevt(name_evt = "death",
                 # Death only occurs once.
                 # Set current time to Inf so simulation ends.
                 input = {
                   age <- initial_age + round(curtime)
                   curtime <- Inf
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                 }) %>%
    add_reactevt(name_evt = "serious_adverse_event",
                 input = {  
                   age <- initial_age + round(curtime)
                   n_sae_evts <- n_sae_evts + 1
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                   # instantaneous weighted sum of adverse events costs
                   utilities_sae <- l_utilities_sae_weighted[["utility"]]
                   costs_sae <- (1- treatment_stop ) * l_costs_sae_weighted$costs
          
                   
                   # Sample next serious event. T
                   new_event(list(
                     "serious_adverse_event" = curtime + draw_tte(
                       1,
                       dist = "exp",
                       # SAE rates same for all treatments
                       # change to include HR from safety NMA
                       coef1 = log(l_sae_events_rate[["rate"]]),
                       beta_tx = ((1 - treatment_stop) * purrr::pluck(l_hr_saes, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) + (treatment_stop *  l_hr_pbo$saes )
                     )
                   ))
                 }) %>%
    # Patients switch only twice (Starting from 2nd line) so 4th line is final (As in protocol)
    add_reactevt(name_evt = "treatment_switch",
                 input = {
                   treatment_line <- treatment_line + 1
                   # discontinuation_rate$discontinuation_rate <- (treatment_line < 4) *  discontinuation_rate$discontinuation_rate
                   v_prior_treatment_indices[current_treatment_code] <- 1
                   n_switch <- n_switch + 1
                   if(n_switch == 1) {
                     current_treatment <- sample(x=v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]], 
                                                size=1, prob = l_prop_3rdline_trt[names(v_higher_line_treatment_codes[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]])]
                                                                            )
                   } else if (n_switch >= 2) {
                     current_treatment <- sample(x = v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]],
                                                size = 1) 
                   }
                   
                   current_treatment_code <- v_higher_line_treatment_codes[current_treatment]
                   
                   age <- initial_age + round(curtime)
                   years_on_treatment <- 0
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * purrr::pluck(l_u_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag * purrr::pluck(l_u_spms_edss, names(v_edss_values[edss_value == v_edss_values]))
                   costs_disease <- (1 - spms_flag) * purrr::pluck(l_c_rrms_edss, names(v_edss_values[edss_value == v_edss_values])) +
                     spms_flag *  purrr::pluck(l_c_spms_edss, names(v_edss_values[edss_value == v_edss_values]))

                   # Sample next discontinuation event. This will be zero if treatment_line >=4
                   new_event(list(
                     "treatment_switch" = curtime + draw_tte(
                       1,
                       dist = "exp",
                       coef1 = ((1 - treatment_stop)  * log(discontinuation_rate$discontinuation_rate)) + (treatment_stop * log(0.01)),
                       beta_tx =  ((1 - treatment_stop)  * purrr::pluck(l_hr_discontinuation, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) + (treatment_stop * 0.01)
                     )
                   ))
                   
                 })
  
}

