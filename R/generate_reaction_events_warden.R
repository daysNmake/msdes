
## WARDEN implementation of reaction events
#costs and utilities to move here (not implemented)
# need to ask for help with death

gen_react_events <- function(RRMS_events){
  
  
  add_reactevt(name_evt = "initialise",
               input = {
                 # This ensures that the current treatment is the initial treatment
                 # of this strategy.
                 current_treatment <- arm
                 current_treatment_code <- v_treatment_codes[arm]
                 
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
                   
                 }
    ) %>%
    add_reactevt(name_evt = "relapse",
                 input = {
                   n_relapse<- n_relapse + 1
                   age <- initial_age + round(curtime)
                   
                   #  next time to relapse
                   new_event(list(
                     "relapse" = curtime + draw_tte(
                       1, 
                       dist = "exp", 
                       coef1 = (1 - spms_flag) * (purrr::pluck(l_baseline_harrms_larr, "rate")  +
                                                 (purrr::pluck(l_baseline_harrms_larr, "BaselineWebEDSS")* (edss_value))  +
                                                  purrr::pluck(l_baseline_harrms_larr, "DMTNatalizumab")) +
                         spms_flag * (purrr::pluck(l_baseline_harrms_larr,"rate") +
                                        (purrr::pluck(l_baseline_harrms_larr,"BaselineWebEDSS") * (edss_value))),
                       hr = (1 - spms_flag) * ((1 - treatment_stop) * ( pluck_vector_from_env(l_hr_arr, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))*(1 - waning)) +
                                                 treatment_stop * purrr::pluck(l_hr_pbo, "arr") ) + spms_flag
                     )))
                 }) %>%
    add_reactevt(name_evt = "edss_increase",
                 input = {
                   edss_value <- min(edss_value + 1, max(v_edss_values))
                   age <- initial_age + round(curtime)
                   n_edssinc_evts <- n_edssinc_evts + 1

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
                       hr = (1 - spms_flag ) * ((1 - treatment_stop) * (pluck_vector_from_env(l_hr_edss_increase, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) *(1 - waning)) + 
                                                  treatment_stop * purrr::pluck(l_hr_pbo, "edss_increase") ) + spms_flag
                     )
                   ))}
                 }) %>%
    add_reactevt(name_evt = "edss_decrease",
                 input = {
                   edss_value <- max(edss_value - 1, min(v_edss_values))
                   age <- initial_age + round(curtime)
                   n_edssdec_evts <- n_edssdec_evts + 1

                   if (edss_value > 6 & treatment_stop == 0) {treatment_stop <- 1}
                   
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
                   
                 }) %>%
    # add_reactevt(name_evt = "death",
    #              # Death only occurs once.
    #              # Set current time to Inf so simulation ends.
    #              input = {
    #                age <- initial_age + round(curtime)
    #                curtime <- Inf 
    #                
    #              }) %>%
    add_reactevt(name_evt = "serious_adverse_event",
                 input = {  
                   age <- initial_age + round(curtime)
                   n_sae_evts <- n_sae_evts + 1
          
                   
                   # Sample next serious event. T
                   new_event(list(
                     "serious_adverse_event" = curtime + draw_tte(
                       1,
                       dist = "exp",
                       # SAE rates same for all treatments
                       # change to include HR from safety NMA
                       coef1 = log(purrr::pluck(sae_events_rate, "sae_events_rate")),
                       hr= ((1 - treatment_stop) * pluck_vector_from_env(l_hr_saes, names(v_treatment_codes[ v_treatment_codes]))) + (treatment_stop *  purrr::pluck(l_hr_pbo, "saes") )
                     )
                   ))
                 }) %>%
    # Patients switch only twice (Starting from 2nd line) so 4th line is final (As in protocol)
    add_reactevt(name_evt = "treatment_switch",
                 input = {
                   treatment_line <- treatment_line + 1
                   l_discontinuation_rate[["discontinuation_rate"]] <- (treatment_line < 4) *  purrr::pluck(l_discontinuation_rate, "discontinuation_rate")
                   v_prior_treatment_indices[current_treatment_code] <- 1
                   n_switch <- n_switch + 1
                   if(n_switch == 1) {
                     current_treatment <- sample(x=v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]], 
                                                size=1, prob = pluck_vector_from_env(l_prop_3rdline_trt, names(v_higher_line_treatment_codes[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]])))
                   } else if (n_switch >= 2) {
                     current_treatment <- sample(x = v_higher_line_treatments[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]],
                                                size = 1) 
                   }
                   
                   current_treatment_code <- v_higher_line_treatment_codes[current_treatment]
                   
                   age <- initial_age + round(curtime)
                   years_on_treatment <- 0
                   
                   # Sample next discontinuation event. This will be zero if treatment_line >=4
                   new_event(list(
                     "treatment_switch" = curtime + draw_tte(
                       1,
                       dist = "exp",
                       coef1 = ((1 - treatment_stop)  * log(purrr::pluck(l_discontinuation_rate, "discontinuation_rate"))) + (treatment_stop * log(0.01)),
                       hr= ((1 - treatment_stop)  * pluck_vector_from_env(l_hr_discontinuation, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) + (treatment_stop * 0.01)
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