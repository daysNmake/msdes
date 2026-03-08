
## DESCEM implementation of reaction events

gen_react_events <- function(RRMS_events){
  
  
  add_reactevt(name_evt = "initialise",
               input = {
                 # This ensures that the current treatment is the initial treatment
                 # of this strategy.
                 current_treatment = trt
                 current_treatment_code = v_treatment_codes[trt]
                 modify_item(list(current_treatment = current_treatment,
                                  current_treatment_code = current_treatment_code
                 ))
                 modify_event(list(#future death events do not exceed time horizon
                   "death"= min(cur_evtlist[["death"]], time_horizon)
                 ))
                 # annual cost events
                 new_event(list(
                   "annual_treatment" = curtime 
                 ))
               }) %>%
    add_reactevt(name_evt = "age",
                 input = {
                   age = initial_age + round(curtime)
                   modify_item(list(age = age
                   ))
                 }) %>%
    
    
    #add annual cost events
    add_reactevt(name_evt = "annual_treatment",
                 input = { 
                   years_on_treatment = years_on_treatment  + 1
                   age = initial_age + round(curtime)
                   modify_item(list(
                     years_on_treatment = years_on_treatment,
                     age = age
                   ))
                   
                   # time on treatment-line counter 
                   if(treatment_line == 2 & spms_flag == 0 & treatment_stop == 0) {
                     modify_item(list(t_2ndline = years_on_treatment))
                   } else if (treatment_line == 3 & spms_flag == 0 & treatment_stop == 0){
                     modify_item(list(t_3rdline = years_on_treatment))
                   }else if (treatment_line == 4 & spms_flag == 0 & treatment_stop == 0){
                     modify_item(list(t_4thline = years_on_treatment))
                   }
                   
                   # treatment waning flag sets weight to 0.25 at 3>= t <5 or 0.5 at t >=5 otherwise 0
                   #                 if (spms_flag == 0 & treatment_stop == 0) {
                   #                   if (years_on_treatment <= 2){modify_item(list(waning = 0))
                   #                   } else if (years_on_treatment > 2 & years_on_treatment <5 ){modify_item(list(waning = 0.25))
                   #                   } else if (years_on_treatment >= 5){modify_item(list(waning = 0.5))}
                   #                 } else if(spms_flag == 1 | treatment_stop == 1) {modify_item(list(waning = 0))}
                   
                   if (edss_value <= 6 & treatment_stop == 0) {new_event(list(
                     "annual_treatment" = curtime + 1
                   ))}
                   
                 }
    ) %>%
    add_reactevt(name_evt = "relapse",
                 input = {
                   n_relapse= n_relapse + 1
                   age = initial_age + round(curtime)
                   modify_item(list(n_relapse= n_relapse,
                                    age = age))
                   
                   #  next time to relapse
                   new_event(list(
                     "relapse" = curtime + draw_tte(
                       1, 
                       dist = "exp", 
                       coef1 = (1 - spms_flag) * (v_baseline_harrms_larr["rate"] +
                                                    v_baseline_harrms_larr["BaselineWebEDSS"] * (edss_value)  +
                                                    v_baseline_harrms_larr["DMTNatalizumab"]) +
                         spms_flag * (v_baseline_spms_larr["rate"] +
                                        v_baseline_spms_larr["BaselineWebEDSS"] * (edss_value)),
                       beta_tx = (1 - spms_flag) * ((1 - treatment_stop) * ( v_hr_arr[current_treatment_code]*(1 - waning)) +
                                                 treatment_stop * v_hr_pbo[, "arr"]) + spms_flag
                     )))
                 }) %>%
    add_reactevt(name_evt = "edss_increase",
                 input = {
                   edss_value = min(edss_value + 1, max(v_edss_values))
                   age = initial_age + round(curtime)
                   n_edssinc_evts = n_edssinc_evts + 1
                   modify_item(list(edss_value = edss_value,
                                    age = age,
                                    n_edssinc_evts = n_edssinc_evts))
                   if (edss_value > 6 & treatment_stop == 0) {modify_item(list(treatment_stop = 1))}
                   
                   # simulate the next time to EDSS increase
                   if (edss_value < 9) {new_event(list(
                     "edss_increase" = curtime + draw_tte(
                       1, 
                       dist = "exp",
                       coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                                                    v_baseline_harrms_edss_increase["BaselineWebEDSS"] * (edss_value) ) + 
                         v_baseline_harrms_edss_increase["DMTNatalizumab"] +
                         spms_flag * (v_spms_edss_increase["rate"] +
                                        v_spms_edss_increase["BaselineWebEDSS"] * (edss_value)),  
                       beta_tx = (1 - spms_flag ) * ((1 - treatment_stop) * ( v_hr_arr[current_treatment_code]*(1 - waning)) + 
                                                  treatment_stop * v_hr_pbo[, "edss_increase"]) + spms_flag
                     )
                   ))}
                 }) %>%
    add_reactevt(name_evt = "edss_decrease",
                 input = {
                   edss_value = max(edss_value - 1, min(v_edss_values))
                   age = initial_age + round(curtime)
                   n_edssdec_evts = n_edssdec_evts + 1
                   modify_item(list(edss_value = edss_value,
                                    age = age,
                                    n_edssdec_evts = n_edssdec_evts))
                   if (edss_value > 6 & treatment_stop == 0) {modify_item(list(treatment_stop = 1))}
                   
                   #  next time to EDSS decrease
                   if (edss_value > 0) {new_event(list(
                     "edss_decrease" = curtime + draw_tte(
                       1, 
                       dist = "exp",
                       # EDSS decrease  
                       coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                                    v_rrms_edss_decrease["BaselineWebEDSS"]*(edss_value) )  + 
                         #setting a very high event time to edss decrease for SPMS patients
                         (spms_flag * -10 )
                     )
                   ) )}
                 }) %>%
    add_reactevt(name_evt = "progression",
                 # Progression can only occur once so just change the SPMS status
                 input = {
                   age = initial_age + round(curtime)
                   modify_item(list(spms_flag = 1,
                                    age = age))
                 }) %>%
    add_reactevt(name_evt = "death",
                 # Death only occurs once.
                 # Set current time to Inf so simulation ends.
                 input = {
                   age = initial_age + round(curtime)
                   modify_item(list(curtime = Inf ,
                                    age = age
                   )) 
                 }) %>%
    add_reactevt(name_evt = "serious_adverse_event",
                 input = {  
                   age = initial_age + round(curtime)
                   n_sae_evts = n_sae_evts + 1
                   modify_item(list(age = age,
                                    n_sae_evts = n_sae_evts))           
                   
                   # Sample next serious event. T
                   new_event(list(
                     "serious_adverse_event" = curtime + draw_tte(
                       1,
                       dist = "exp",
                       # SAE rates same for all treatments
                       # change to include HR from safety NMA
                       coef1 = log(sae_events_rate),
                       beta_tx= ((1 - treatment_stop) * v_hr_saes) + (treatment_stop *  v_hr_pbo[, "saes"])
                     )
                   ))
                 }) %>%
    # Patients switch only twice (Starting from 2nd line) so 4th line is final (As in protocol)
    add_reactevt(name_evt = "treatment_switch",
                 input = {
                   treatment_line = treatment_line + 1
                   discontinuation_rate = (treatment_line < 4) *  discontinuation_rate
                   v_prior_treatment_indices[current_treatment_code] = 1
                   n_switch = n_switch + 1
                   if(n_switch == 1) {
                     current_treatment = sample(x=v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]], 
                                                size=1, prob = m_prop_2nd_3rd_trt["3",current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]])
                   } else if (n_switch >= 2) {
                     current_treatment = sample(x = v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]],
                                                size = 1) 
                   }
                   
                   current_treatment_code = v_higher_line_treatment_codes[current_treatment]
                   
                   age = initial_age + round(curtime)
                   modify_item(list(current_treatment = current_treatment,
                                    current_treatment_code = current_treatment_code,
                                    v_prior_treatment_indices = v_prior_treatment_indices,
                                    treatment_line = treatment_line,
                                    discontinuation_rate = discontinuation_rate,
                                    n_switch = n_switch,
                                    years_on_treatment = 0,   
                                    age = age))
                   
                   # Sample next discontinuation event. This will be zero if treatment_line >=4
                   new_event(list(
                     "treatment_switch" = curtime + draw_tte(
                       1,
                       dist = "exp",
                       coef1 = ((1 - treatment_stop)  * log(discontinuation_rate)) + (treatment_stop * log(0.01)),
                       beta_tx= ((1 - treatment_stop)  * v_hr_discontinuation[current_treatment_code]) + (treatment_stop * 0.01)
                     )
                   ))
                   
                 })
  
}

##WARDEN FORMAT

# gen_react_events <- function(RRMS_events){
#   
#   add_reactevt(name_evt = RRMS_events[1],
#                input = {}) %>%
#     add_reactevt(name_evt = RRMS_events[2],
#                  input = {
#                    q_default <- util.sicker
#                    c_default <- cost.sicker + if(arm=="int"){cost.int}else{0}
#                    fl.sick   <- 0 
#                  }) %>%
#     add_reactevt(name_evt = RRMS_events[3],
#                  input = {
#                    q_default <- 0
#                    c_default <- 0
#                    curtime   <- Inf
#                  }) 
#   
#   
#   
# }