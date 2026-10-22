#' Generate Individual inputs
#' 
#' `gen_unique_pt_inputs()` Individual patient parameters of the model as they are unique patients and history including treatments
#' 
#' These are called for each new patient so sampled from distribution specified
#' for heterogeneous characteristics and may depend on treatment
#' Note that the current treatment is later initialised to trt
#' 
#' @param sex individual patient sex
#' @param n_relapse number of relapses  counter
#' @param n_edssinc_evts number of edss increases counter
#' @export
gen_unique_pt_inputs <- function(){
  add_item(edss_value = sample(size = 1, x = v_edss_values,  
                                                 # rrms larger sample size, option to use harrms
                                                 prob = rdiric(1, inputs[["m_n_patients_edss"]]["harrms",])),
                             #(male=0, female=1)
                             sex = sample(size = 1, x = c(0, 1), 
                                          prob = c(1 - inputs[["prop_female"]], inputs[["prop_female"]])),
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
                             current_treatment = sample(x = v_treatment_names, size=1, prob = l_prop_3rdline_trt),
                             #current_treatment_code = 1, # This is later changed to code of trt
                             current_treatment_code = v_treatment_codes[current_treatment],
                             treatment_line = 2, # all patients starting on 2nd line treatment
                             n_switch = 0, # treatment switch counter for sense checking
                             years_on_treatment  = 0, #  annual treatment counter for drug acquisition costs
                             treatment_stop = 0, # all patients start on treatment, flag set to 1 when edss is 7+
                             age = inputs[["initial_age"]],
                             utilities_disease = 0,
                             costs_disease = 0,
                             costs_treatment = 0,
                             utilities_relapse= 0,
                             costs_relapse = 0,
                             utilities_sae = 0,
                             costs_sae = 0
)
}
