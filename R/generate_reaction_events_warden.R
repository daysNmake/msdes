#' Generate Reaction Events for the RRMS DES Model
#'
#' Defines the model's response to each event, specifying how patient state
#' variables, costs, utilities, and future events are updated when an event
#' occurs. Returns a WARDEN \code{add_reactevt()} chain consumed by
#' \code{\link[msdes]{run_sim_new}} via the \code{evt_react_list} argument
#' in \code{\link[msdes]{rrmsdesmodel}}.
#'
#' @param RRMS_events Character vector of model event names, passed from
#'   \code{v_event_names} set by \code{\link[msdes]{rrmsdessetup}}. Must
#'   match the events initialised in \code{\link[msdes]{gen_init_events}}.
#'
#' @details
#' Each event reaction is defined using \code{\link[WARDEN]{add_reactevt}}. 
#' All reaction events update \code{utilities_disease} and \code{costs_disease} 
#' at each event time using the EDSS-specific utility and cost.
#'
#' The following reaction events are defined:
#'
#' \strong{\code{initialise}}
#'
#' Sets \code{current_treatment} and \code{current_treatment_code} to the
#' arm being simulated. Adjusts the \code{death} event
#' time to be relative to model entry and
#' caps it at \code{time_horizon}. Schedules the first
#' \code{annual_treatment} event.
#'
#' \strong{\code{age}}
#'
#' Updates \code{age} and refreshes \code{utilities_disease} and
#' \code{costs_disease} for the current EDSS state.
#'
#' \strong{\code{annual_treatment}}
#'
#' Increments \code{years_on_treatment} by 1 and updates the treatment-line
#' time counter (\code{t_2ndline}, \code{t_3rdline}, or \code{t_4thline})
#' for RRMS patients on treatment. Schedules the next \code{annual_treatment}
#' event one year ahead if \code{edss_value <= 6}.
#'
#' Applies instantaneous treatment acquisition, administration, and monitoring
#' costs (\code{costs_treatment}) conditional on treatment year and SPMS
#' status,costs are scaled by the treatment load factor.
#
#' \strong{\code{relapse}}
#'
#' Increments \code{n_relapse} by 1. Applies instantaneous relapse disutilities
#' and relapse costs.
#' Schedules the next relapse using the same exponential draw from an exponential 
#' distribution using baseline log-rate for HARRMS patients modified by the 
#' treatment-specific ARR rate ratio from the NMA.
#'
#' \strong{\code{edss_increase}}
#'
#' Increments \code{edss_value} by 1, capped at (EDSS 9). 
#' Increments \code{n_edssinc_evts} by 1. 
#' Sets \code{treatment_stop <- 1} if \code{edss_value > 6} and treatment has not
#' already stopped — this triggers the cessation of treatment cost accrual and
#' applies placebo hazard ratios to future events.
#' Schedules the next \code{edss_increase} event drawn from an exponential 
#' distribution using baseline log-rate for HARRMS patients modified by the 
#' treatment-specific CDP6 hazard ratio from the NMA. 
#'
#' \strong{\code{edss_decrease}}
#'
#' Decrements \code{edss_value} by 1, floored at (EDSS 0). 
#' Increments \code{n_edssdec_evts} by 1. 
#' Checks and sets \code{treatment_stop} as in \code{edss_increase}. 
#' Schedules the next \code{edss_decrease} event drawn from an exponential 
#' distribution for patients.
#'
#' \strong{\code{progression}}
#'
#' Sets \code{spms_flag <- 1}, converting the patient from RRMS to SPMS.
#' Progression can only occur once; no new progression event is scheduled.
#' Immediately updates \code{utilities_disease} and \code{costs_disease} to
#' SPMS values for the current EDSS state.
#' Only applies to patients with \code{edss_value > 4}.
#'
#' \strong{\code{death}}
#'
#' Sets \code{curtime <- Inf}, ending the patient's simulation. Updates
#' \code{utilities_disease} and \code{costs_disease} for the final EDSS
#' state. No new events are scheduled. Death can only occur once.
#'
#' \strong{\code{serious_adverse_event}}
#'
#' Increments \code{n_sae_evts} by 1. Applies the SAE
#' disutility and SAE costs. 
#' Schedules the next SAE' Drawn from an exponential distribution using the 
#' baseline SAE rate modified by the treatment-specific SAE hazard ratio from 
#' the NMA .
#'
#' \strong{\code{treatment_switch}}
#' 
#' Increments \code{treatment_line} by 1 and \code{n_switch} by 1. Marks the
#' current treatment as used in \code{v_prior_treatment_indices}. Resets
#' \code{years_on_treatment} to 0.
#'
#' New treatment selection logic:
#' \itemize{
#'   \item First switch (\code{n_switch == 1}): samples from available
#'     higher-line treatments (excluding current and prior treatments),
#'     weighted by proportion of patients on 3rd line from the registry.
#'   \item Second or later switch (\code{n_switch >= 2}): samples uniformly
#'     from remaining available treatments.
#' }
#' The discontinuation rate is updated to reflect the protocol
#' assumption that patients switch at most twice (starting from 2nd line,
#' reaching 4th line as the final option). 
#' Schedules the next \code{treatment_switch} event drawn from an exponential 
#' distribution using the baseline discontinuation hazard modified by the 
#' treatment-specific discontinuation hazard ratios.
#'
#' @return A WARDEN \code{add_reactevt()} chain object containing reaction
#'   logic for all events in \code{RRMS_events}, passed to
#'   \code{\link[msdes]{run_sim_new}} via the \code{evt_react_list} argument
#'   in \code{\link[msdes]{rrmsdesmodel}}.
#'
#' @seealso
#'   \code{\link[msdes]{rrmsdessetup}},
#'   \code{\link[msdes]{rrmsdesmodel}},
#'   \code{\link[msdes]{gen_inputs}},
#'   \code{\link[msdes]{gen_common_all_inputs}},
#'   \code{\link[msdes]{gen_unique_pt_inputs}},
#'   \code{\link[msdes]{gen_init_events}},
#'   \code{\link[WARDEN]{add_reactevt}},
#'   \code{\link[msdes]{run_sim_new}}
#'
#' @export
gen_react_events <- function(RRMS_events){
  
  
  add_reactevt(name_evt = "initialise",
               input = {
                 # This ensures that the current treatment is the initial treatment
                 # of this strategy.
                 current_treatment <- arm
                 current_treatment_code <- v_treatment_codes[arm]
                 # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                 utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                                      spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                 costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                                    spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

                 modify_event(
                   c("death"= 
                       ifelse(get_event("death") - inputs[["initial_age"]] > 0 ,
                              min(get_event("death") - inputs[["initial_age"]], inputs[["time_horizon"]])      ,
                              min(get_event("death"), inputs[["time_horizon"]])
                              )
                     ) 
                 )
                 
                 # annual cost events
                 new_event(list(
                   "annual_treatment" = curtime 
                 ))
               }) %>%
    add_reactevt(name_evt = "age",
                 input = {
                   age <- initial_age + round(curtime)
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

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
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

                   # Instantaneous treatment costs 
                   costs_treatment[years_on_treatment <= 1] <- (1- treatment_stop ) *(
                     (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr1",current_treatment_code] +
                                          l_c_trt_admin_yr1[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]] +
                                          l_c_trt_monitor_yr1[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]) +
                       spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) +
                                      (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr1","Siponimod"])))
                   
                   costs_treatment[years_on_treatment >= 2 & years_on_treatment <= 4] <- (1- treatment_stop ) * (
                     (1 - spms_flag) * (inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                          l_c_trt_admin_yr2[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]] +
                                          l_c_trt_monitor_yr2[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]])+
                       spms_flag * (((1-inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                      (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
    
                   costs_treatment[years_on_treatment >= 5] <- (1- treatment_stop ) *(
                     (1 - spms_flag) *((inputs$df_c_tx["cost_annual_yr2",current_treatment_code] +
                                          l_c_trt_admin_yr2[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]] +
                                          l_c_trt_monitor_yr2[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]) * inputs$df_c_tx_load[5,current_treatment_code])+
                       spms_flag *(((1 - inputs$p_siponimod) * inputs$df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) +
                                     (inputs$p_siponimod * inputs$df_c_spms["cost_annual_yr2","Siponimod"])))
                   
                 }
    ) %>%
    add_reactevt(name_evt = "relapse",
                 input = {
                   n_relapse<- n_relapse + 1
                   age <- initial_age + round(curtime)
                   
                   # # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

                   # instantaneous costs and utilities
                   utilities_relapse <- l_u_mean_relapse_hospital$utility
                   costs_relapse <- (relapse_hospital_flag * l_c_relapse[["Relapse_hospital_addmision"]])+
                     ((1-relapse_hospital_flag) *l_c_relapse[["Relapse_oral_steroids"]])
                   
                   #  next time to relapse
                   new_event(list(
                     "relapse" = curtime + draw_tte(
                       1, 
                       dist = "exp", 
                       coef1 = (1 - spms_flag) * (l_baseline_harrms_larr[["rate"]]  +
                                                    (l_baseline_harrms_larr[["BaselineWebEDSS"]] * edss_value) +
                                                    l_baseline_harrms_larr[["DMTNatalizumab"]] ) +
                         # MS registry time to relapse SPMS log rate
                         spms_flag * (l_baseline_spms_larr[["rate"]] +
                                        (l_baseline_spms_larr[["BaselineWebEDSS"]] * edss_value)),
                       beta_tx =  (1 - spms_flag) * ((1 - treatment_stop) * (l_hr_arr[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]*(1 - waning)) +
                                                 treatment_stop * l_hr_pbo$arr ) + spms_flag
                     )))
                 }) %>%
    add_reactevt(name_evt = "edss_increase",
                 input = {
                   edss_value <- min(edss_value + 1, max(v_edss_values))
                   age <- initial_age + round(curtime)
                   n_edssinc_evts <- n_edssinc_evts + 1
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   
                   if (edss_value > 6 & treatment_stop == 0) {treatment_stop <- 1}
                   
                  
                   # simulate the next time to EDSS increase
                   if (edss_value < 9) {new_event(list(
                     "edss_increase" = curtime + draw_tte(
                       1, 
                       dist = "exp",
                       coef1 = (1 - spms_flag) * (l_baseline_harrms_edss_increase[["rate"]] +
                                                 (l_baseline_harrms_edss_increase[["BaselineWebEDSS"]] * (edss_value)) + 
                                                  l_baseline_harrms_edss_increase[["DMTNatalizumab"]]) +
                         spms_flag * (l_spms_edss_increase[["rate"]] +
                                      l_spms_edss_increase[["BaselineWebEDSS"]] * (edss_value)),  
                       beta_tx =  (1 - spms_flag ) * ((1 - treatment_stop) * (l_hr_edss_increase[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]] *(1 - waning)) + 
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
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

                   #  next time to EDSS decrease
                   if (edss_value > 0) {new_event(list(
                     "edss_decrease" = curtime + draw_tte(
                       1, 
                       dist = "exp",
                       # EDSS decrease  
                       coef1 = (1 - spms_flag) * (l_rrms_edss_decrease[["rate"]] +
                                                  l_rrms_edss_decrease[["BaselineWebEDSS"]]*(edss_value) )  + 
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
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

                   
                 }) %>%
    add_reactevt(name_evt = "death",
                 # Death only occurs once.
                 # Set current time to Inf so simulation ends.
                 input = {
                   age <- initial_age + round(curtime)
                   curtime <- Inf
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

                 }) %>%
    add_reactevt(name_evt = "serious_adverse_event",
                 input = {  
                   age <- initial_age + round(curtime)
                   n_sae_evts <- n_sae_evts + 1
                   
                   # Ongoing costs and utilities added to all reaction events as per vignette, but not clear if this is necessary
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

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
                       beta_tx = ((1 - treatment_stop) * l_hr_saes[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]) + (treatment_stop *  l_hr_pbo$saes )
                     )
                   ))
                 }) %>%
    # Patients switch only twice (Starting from 2nd line) so 4th line is final (As in protocol)
    add_reactevt(name_evt = "treatment_switch",
                 input = {
                   treatment_line <- treatment_line + 1
                   discontinuation <- ((treatment_line < 4) *  discontinuation_rate[["rate"]])+0.01
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
                   utilities_disease <-  (1 - spms_flag) * l_u_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag * l_u_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]
                   costs_disease <- (1 - spms_flag) * l_c_rrms_edss[[names(v_edss_values[edss_value == v_edss_values])]] +
                     spms_flag *  l_c_spms_edss[[names(v_edss_values[edss_value == v_edss_values])]]

                   # Sample next discontinuation event. This will be zero if treatment_line >=4
                   new_event(list(
                     "treatment_switch" = curtime + draw_tte(
                       1,
                       dist = "exp",
                       coef1 = ((1 - treatment_stop)  * log(discontinuation)) + (treatment_stop * log(0.01)),
                       beta_tx =  ((1 - treatment_stop)  * l_hr_discontinuation[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]) + (treatment_stop * 0.01)
                     )
                   ))
                   
                 })
  
}

