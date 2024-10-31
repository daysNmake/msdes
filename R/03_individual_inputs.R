# Objects that depend on treatment
# Note that the current treatment is later initialised to trt
unique_pt_inputs <- add_item(edss_value = sample(size = 1, x = v_edss_values, # Random 
                                                 prob = rdiric(1, v_affirm_edss_distribution)),
                             #edss.i = edss_value,
                             n_relapse = 0,
                             spms_flag = sample(size = 1, x = c(0, 1), # Random
                                                prob = c(1 - initial_spms_prop, initial_spms_prop)),
                             relapse_hospital_flag = sample(size = 1, x = c(0, 1), # Random
                                                            prob = c(1 - relapse_hospital_prop, relapse_hospital_prop)),
                             current_treatment = "Natalizumab",
                             current_treatment_code = 1,
                             treatment_line = 2,
                             n_switch = 0,
                             age = initial_age
)


