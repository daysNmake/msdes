


gen_init_events<-function(RRMS_events){
  add_tte(trt = v_treatment_names,
          evts = v_event_names,
          input={ 
            initialise <- 0
            relapse <- draw_tte(
              1, 
              dist = "exp", 
              # Current treatment code allows treatment switching 
              # (i.e., not the trt which is only initial assignment)
              # MS registry time to relapse HA RRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (v_baseline_harrms_larr["rate"] +
                                           v_baseline_harrms_larr["BaselineWebEDSS"] * (edss_value)  +
                                           v_baseline_harrms_larr["DMTNatalizumab"]) +
                # MS registry time to relapse SPMS log rate
                spms_flag * (v_baseline_spms_larr["rate"] +
                               v_baseline_spms_larr["BaselineWebEDSS"] * (edss_value)),
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              hr = (1 - spms_flag) * ((1 - treatment_stop) * (v_hr_arr[current_treatment_code]*(1-waning)) +
                                        treatment_stop * v_hr_pbo[, "arr"]) + spms_flag
            )
            edss_increase <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss increase HARRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                                           v_baseline_harrms_edss_increase["BaselineWebEDSS"] * (edss_value) + 
                                           v_baseline_harrms_edss_increase["DMTNatalizumab"]) + 
                # MS registry time to edss increase SPMS log rate
                spms_flag * (v_spms_edss_increase["rate"] + 
                               v_spms_edss_increase["BaselineWebEDSS"]*(edss_value) ),    
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              hr = (1 - spms_flag ) * ((1 - treatment_stop) * (v_hr_edss_increase[current_treatment_code])*(1-waning) + 
                                         treatment_stop * v_hr_pbo[, "edss_increase"]) + spms_flag
            )
            edss_decrease <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss decrease All RRMS patients log rate 
              coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                           v_rrms_edss_decrease["BaselineWebEDSS"] * (edss_value)) + 
                #we are setting a very high event time to edss decrease for SPMS patients
                (spms_flag * -10)
            )
            
            progression <- draw_tte(
              1,
              dist = "exp",
              # MS registry time to progression All RRMS patients to SPMS log rate on natalizumab
              # Below is zero if already progressed to SPMS
              coef1 = if (edss_value > 4) {(1 - spms_flag) * (v_spms_progression["rate"] +
                                                                v_spms_progression["BaselineWebEDSS"] * (edss_value) + 
                                                                v_spms_progression["DMTNatalizumab"]) + 
                  #setting a very high event time to progression for SPMS patients
                  (spms_flag * -10)} else {  -10 }
            )
            death <- draw_tte_new(
              1,
              dist = "pwexp",
              # population mortality drawn using piece wise exponential (male=0, female=1)
              coef1 = c(log((1 - sex) * m_r_mortality[(age + 1):nrow(m_r_mortality), c("male")] +
                              sex *  m_r_mortality[(age + 1):nrow(m_r_mortality), c("female")],
              )),
              time = c(m_r_mortality[(age + 1):nrow(m_r_mortality)]) ,
              # MS specific mortality multiplier
              hr = v_hr_smr_edss[edss_value == v_edss_values]
            ) - age
            serious_adverse_event <- draw_tte(
              1,
              dist = "exp",
              # SAE rates same for all treatments
              # change to include HR from safety NMA
              coef1 = log(sae_events_rate),
              hr=v_hr_saes
            )
            treatment_switch <- draw_tte(
              1,
              dist = "exp",
              # Discontinuation rates depend on current treatment
              coef1 = ((1 - treatment_stop)  * log(discontinuation_rate)) + (treatment_stop * log(0.01)),
              hr= ((1 - treatment_stop)  * v_hr_discontinuation[current_treatment_code]) + (treatment_stop * 0.01)
            )
          })
  
}


##WARDEN format
# gen_init_events<-function(RRMS_events){
#   #init_event_list <- 
#   add_tte(arm=c("noint","int"), evts = RRMS_events ,input={
#     sick <- 0
#     sicker <- draw_tte(1,dist="exp", coef1=coef_noint, beta_tx = ifelse(arm=="int",HR_int,1), seed = random_seed_sicker_i[i]) #this way the value would be the same if it wasn't for the HR, effectively "cloning" patients luck
#     
#   })
# }



