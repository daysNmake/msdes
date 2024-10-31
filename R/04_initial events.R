# Add time to event for each patient for each of the treatments
# Setting trt to all treatments should mean that the event applies to all treatments

init_event_list <- 
  add_tte(trt = v_treatment_names,
          evts = v_event_names,
          input={ 
            initialise <- 0
            
            relapse <- draw_tte(
              1, 
              dist = "exp", 
              # Current treatment code allows treatment switching (i.e., not the trt)
              # MS registry time to relapse HA RRMS
              coef1 = (1 - spms_flag) * (v_baseline_harrms_arr["(Intercept)"] +
                                           v_baseline_harrms_arr["BaselineWebEDSS"]*(edss_value)  +
                                           v_baseline_harrms_arr["DMTNatalizumab"]) +
                # MS registry time to relapse SPMS
                spms_flag * (v_baseline_spms_arr["rate"] +
                               v_baseline_spms_arr["BaselineWebEDSS"]*(edss_value)),
              # Baseline rate modified by HR of current treatment
              hr = ((1 - spms_flag) * v_hr_arr[current_treatment_code]) + spms_flag
            )
            edss_increase <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss increase HARRMS
              coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                                           v_baseline_harrms_edss_increase["BaselineWebEDSS"]*(edss_value)  + 
                                           v_baseline_harrms_edss_increase["DMTNatalizumab"]) + 
                #MS registry time to edss increase SPMS
                spms_flag * (v_spms_edss_increase["rate"] + 
                               v_spms_edss_increase["BaselineWebEDSS"]*(edss_value) ),    
              # Baseline rate modified by HR of current treatment
              hr = ((1 - spms_flag) * v_hr_edss_increase[current_treatment_code])+ spms_flag 
            )
            edss_decrease <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss decrease All RRMS patients
              coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                           v_rrms_edss_decrease["BaselineWebEDSS"]*(edss_value)) + 
                #MS registry time to edss increase SPMS
                spms_flag * (v_spms_edss_decrease["rate"] +
                               v_spms_edss_decrease["BaselineWebEDSS"]*(edss_value) )
            )
            
            progression <- draw_tte(
              1,
              dist = "exp",
              # MS registry time to progression All RRMS patients to SPMS
              # Below is zero if already progressed to SPMS
              coef1 = (1 - spms_flag) * (v_spms_progression["(Intercept)"] +
                                           v_spms_progression["BaselineWebEDSS"]*(edss_value) ) + 
                v_spms_progression["DMTNatalizumab"]
            )
            #source("R/draw_tte_new.R")
            death <- draw_tte_new(
              1,
              dist = "pwexp",
              # population mortality drawn using piece wise exponential
              coef1 = c(log((1 - prop_female) * m_r_mortality[(initial_age+1):nrow(m_r_mortality), c("male")] +
                              prop_female *  m_r_mortality[(initial_age+1):nrow(m_r_mortality), c("female")],
              )),
              time = c(m_r_mortality[(initial_age+1):nrow(m_r_mortality)]) ,
              # MS specific mortality multiplier
              hr=v_smr[1]
            )
            serious_adverse_event <- draw_tte(
              1,
              dist = "exp",
              # SAE rates same for all treatments
              # change to include HR from safety NMA
              coef1 = log(mean(v_r_sae))
            )
            treatment_switch <- draw_tte(
              1,
              dist = "exp",
              # Discontinuation rates can vary by current treatment (Dummy data)
              coef1 = log(v_discontinuation_rate[current_treatment_code])
            )
          })

