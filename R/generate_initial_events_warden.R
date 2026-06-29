
## WARDEN implementation of initial events
# need to ask for help with death

gen_init_events<-function(RRMS_events){
  add_tte(arm = v_treatment_names,
          evts = RRMS_events,
          input={ 
            initialise <- 0
            annual_treatment <- 0
            age <-0
            relapse <- draw_tte(
              1, 
              dist = "exp", 
              # Current treatment code allows treatment switching 
              # (i.e., not the trt which is only initial assignment)
              # MS registry time to relapse HA RRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (purrr::pluck(l_baseline_harrms_larr, "rate") +
                                        (purrr::pluck(l_baseline_harrms_larr, "BaselineWebEDSS") * (edss_value) ) +
                                         purrr::pluck(l_baseline_harrms_larr, "DMTNatalizumab")) +
                # MS registry time to relapse SPMS log rate
                spms_flag * (purrr::pluck(l_baseline_spms_larr,"rate") +
                               (purrr::pluck(l_baseline_spms_larr,"BaselineWebEDSS") * (edss_value))),
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              beta_tx = (1 - spms_flag) * ((1 - treatment_stop) * (pluck_vector_from_env(l_hr_arr, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))*(1-waning)) +
                                        treatment_stop * purrr::pluck(l_hr_pbo, "arr") ) + spms_flag
            )
            edss_increase <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss increase HARRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (purrr::pluck(l_baseline_harrms_edss_increase, "rate") +
                                        (purrr::pluck(l_baseline_harrms_edss_increase, "BaselineWebEDSS") * (edss_value)) + 
                                         purrr::pluck(l_baseline_harrms_edss_increase, "DMTNatalizumab")) + 
                # MS registry time to edss increase SPMS log rate
                spms_flag * (purrr::pluck(l_spms_edss_increase, "rate") + 
                             purrr::pluck(l_spms_edss_increase, "BaselineWebEDSS")*(edss_value)),    
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              beta_tx = (1 - spms_flag ) * ((1 - treatment_stop) * (pluck_vector_from_env(l_hr_edss_increase, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) *(1-waning)) + 
                                         treatment_stop * purrr::pluck(l_hr_pbo, "edss_increase") ) + spms_flag
            )
            edss_decrease <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss decrease All RRMS patients log rate 
              coef1 = (1 - spms_flag) * (purrr::pluck(l_rrms_edss_decrease, "rate") +
                                         (purrr::pluck(l_rrms_edss_decrease, "BaselineWebEDSS") * (edss_value))) + 
                #we are setting a very high event time to edss decrease for SPMS patients
                (spms_flag * -10)
            )
            
            progression <- draw_tte(
              1,
              dist = "exp",
              # MS registry time to progression All RRMS patients to SPMS log rate on natalizumab
              # Below is zero if already progressed to SPMS
              coef1 = if (edss_value > 4) {(1 - spms_flag) * (purrr::pluck(l_spms_progression, "rate") +
                                                              purrr::pluck(l_spms_progression, "BaselineWebEDSS") * (edss_value) + 
                                                              purrr::pluck(l_spms_progression, "DMTNatalizumab")) + 
                  #setting a very high event time to progression for SPMS patients
                  (spms_flag * -10)} else {  -10 }
            )
            death <- draw_tte_new_warden(
              1,
              dist = "pwexp",
              # population mortality drawn using piece wise exponential (male=0, female=1)
              coef1 = list(c(log((1 - sex) * inputs[["m_r_mortality"]][(age + 1):nrow(inputs[["m_r_mortality"]]), c("male")] +
                              sex *  inputs[["m_r_mortality"]][(age + 1):nrow(inputs[["m_r_mortality"]]), c("female")],
              ))),
              time = list(c(inputs[["m_r_mortality"]][(age + 1):nrow(inputs[["m_r_mortality"]])])) ,
              # MS specific mortality multiplier
              hr = list(pluck_vector_from_env(l_hr_smr_edss, names(v_edss_values[edss_value == v_edss_values])))
            ) - age
            serious_adverse_event <- draw_tte(
              1,
              dist = "exp",
              # SAE rates same for all treatments
              # change to include HR from safety NMA
              coef1 = log(purrr::pluck(l_sae_events_rate, "sae_events_rate")),
              beta_tx= pluck_vector_from_env(l_hr_saes, names(v_treatment_codes[v_treatment_codes]))
            )
            treatment_switch <- draw_tte(
              1,
              dist = "exp",
              # Discontinuation rates depend on current treatment
              coef1 = ((1 - treatment_stop)  * log(purrr::pluck(l_discontinuation_rate, "discontinuation_rate"))) + (treatment_stop * log(0.01)),
              beta_tx= ((1 - treatment_stop)  * pluck_vector_from_env(l_hr_discontinuation, names(v_treatment_codes[current_treatment_code == v_treatment_codes]))) + (treatment_stop * 0.01)
            )
          })
  
}






