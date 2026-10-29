#' Generate Initial Events for the RRMS DES Model
#'
#' Generates the initial time-to-event draws for all model events at the start
#' of each patient's simulation, for all treatment arms. Returns a WARDEN
#' \code{add_tte()} object consumed by \code{\link[msdes]{run_sim_new}} via
#' the \code{init_event_list} argument in \code{\link[msdes]{rrmsdesmodel}}.
#'
#' @param RRMS_events Character vector of model event names, passed from
#'   \code{v_event_names} set by \code{\link[msdes]{rrmsdessetup}}. Defines
#'   which events receive an initial time-to-event draw.
#'
#' @details
#' Initial event times are drawn once per patient at simulation entry using
#' \code{\link[WARDEN]{add_tte}}. Events are generated across all arms in
#' \code{v_treatment_names}. Each event uses parameters from the common
#' global inputs (\code{\link[msdes]{gen_common_all_inputs}}) and individual
#' patient inputs (\code{\link[msdes]{gen_unique_pt_inputs}}) available in
#' the WARDEN simulation environment.
#'
#' The following events are initialised:
#'
#' \strong{\code{initialise}, \code{annual_treatment}, \code{age}}
#'
#' Set to 0. These are structural trigger events handled by the reaction event
#' and updated annually.
#'#'
#' \strong{\code{relapse}}
#'
#' Drawn from an exponential distribution using baseline log-rate for HARRMS 
#' patients modified by the treatment-specific ARR rate ratio from the NMA.
#'
#' \strong{\code{edss_increase}}
#'
#' Drawn from an exponential distribution using baseline log-rate for HARRMS 
#' patients modified by the treatment-specific CDP6 hazard ratio from the NMA, 
#'
#' \strong{\code{edss_decrease}}
#'
#' Drawn from an exponential distribution for patients.
#'
#' \strong{\code{progression}}
#'
#' Drawn from an exponential distribution. Only applies to patients with
#' \code{edss_value > 4} and \code{spms_flag = 0}. Uses the MS Registry
#' HARRMS-to-SPMS conversion rate.
#'
#' \strong{\code{death}}
#'
#' Drawn from a piecewise exponential distribution using age-sex-specific
#' all-cause mortality rates modified by MS-specific mortality multiplier 
#' the EDSS-severity.
#'
#' \strong{\code{serious_adverse_event}}
#'
#' Drawn from an exponential distribution using the baseline SAE rate
#' modified by the treatment-specific SAE hazard ratio from the NMA .
#'
#' \strong{\code{treatment_switch}}
#'
#' Drawn from an exponential distribution using the baseline discontinuation
#' hazard modified by the treatment-specific discontinuation hazard ratios.
#'
#' @return A WARDEN \code{add_tte()} object containing initial time-to-event
#'   draws for all events in \code{RRMS_events} across all arms in
#'   \code{v_treatment_names}, passed to \code{\link[msdes]{run_sim_new}}
#'   via the \code{init_event_list} argument in
#'   \code{\link[msdes]{rrmsdesmodel}}.
#'
#' @seealso
#'   \code{\link[msdes]{rrmsdessetup}},
#'   \code{\link[msdes]{rrmsdesmodel}},
#'   \code{\link[msdes]{gen_inputs}},
#'   \code{\link[msdes]{gen_common_all_inputs}},
#'   \code{\link[msdes]{gen_unique_pt_inputs}},
#'   \code{\link[msdes]{gen_react_events}},
#'   \code{\link[WARDEN]{add_tte}},
#'   \code{\link[msdes]{run_sim_new}}
#'
#' @export
gen_init_events<-function(RRMS_events){
  add_tte(arm = v_treatment_names,
          evts = RRMS_events,
          input={ 
            initialise <- 0
            annual_treatment <- 0
            age <-0
            discontinuation <- discontinuation_rate[["rate"]]
            relapse <- draw_tte(
              1, 
              dist = "exp", 
              # Current treatment code allows treatment switching 
              # (i.e., not the trt which is only initial assignment)
              # MS registry time to relapse HA RRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (l_baseline_harrms_larr[["rate"]]  +
                                           (l_baseline_harrms_larr[["BaselineWebEDSS"]] * edss_value) +
                                           l_baseline_harrms_larr[["DMTNatalizumab"]] ) +
                # MS registry time to relapse SPMS log rate
                spms_flag * (l_baseline_spms_larr[["rate"]] +
                               (l_baseline_spms_larr[["BaselineWebEDSS"]] * edss_value)),
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              beta_tx = (1 - spms_flag) * ((1 - treatment_stop) * (l_hr_arr[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]*(1-waning)) +
                                        treatment_stop * l_hr_pbo$arr ) + spms_flag
            )
            edss_increase <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss increase HARRMS log rate on natalizumab
              coef1 = (1 - spms_flag) * (l_baseline_harrms_edss_increase[["rate"]] +
                                        (l_baseline_harrms_edss_increase[["BaselineWebEDSS"]] * (edss_value)) + 
                                         l_baseline_harrms_edss_increase[["DMTNatalizumab"]]) + 
                # MS registry time to edss increase SPMS log rate
                spms_flag * (l_spms_edss_increase[["rate"]] + 
                             l_spms_edss_increase[["BaselineWebEDSS"]]*(edss_value)),    
              # Baseline rate modified by HR of current treatment
              # No treatment effect if SPMS
              # placebo ARR HR from NMA applied when treatment stopped 
              beta_tx = (1 - spms_flag ) * ((1 - treatment_stop) * (l_hr_edss_increase[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]] *(1-waning)) + 
                                         treatment_stop * l_hr_pbo$edss_increase ) + spms_flag
            )
            edss_decrease <- draw_tte(
              1, 
              dist = "exp",
              # MS registry time to edss decrease All RRMS patients log rate 
              coef1 = (1 - spms_flag) * (l_rrms_edss_decrease[["rate"]] +
                                         (l_rrms_edss_decrease[["BaselineWebEDSS"]] * (edss_value))) + 
                #we are setting a very high event time to edss decrease for SPMS patients
                (spms_flag * -10)
            )
            
            progression <- draw_tte(
              1,
              dist = "exp",
              # MS registry time to progression All RRMS patients to SPMS log rate on natalizumab
              # Below is zero if already progressed to SPMS
              coef1 = if (edss_value > 4) {(1 - spms_flag) * (l_spms_progression[["rate"]] +
                                                              l_spms_progression[["BaselineWebEDSS"]] * (edss_value) + 
                                                              l_spms_progression[["DMTNatalizumab"]]) + 
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
              hr = l_hr_smr_edss[[names(v_edss_values[edss_value == v_edss_values])]]
            ) - age
            serious_adverse_event <- draw_tte(
              1,
              dist = "exp",
              # SAE rates same for all treatments
              # change to include HR from safety NMA
              coef1 = log(l_sae_events_rate[["rate"]]),
              beta_tx= l_hr_saes[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]
            )
            treatment_switch <- draw_tte(
              1,
              dist = "exp",
              # Discontinuation rates depend on current treatment
              coef1 = ((1 - treatment_stop)  * log(discontinuation)) + (treatment_stop * log(0.01)),
              beta_tx= ((1 - treatment_stop)  * l_hr_discontinuation[[names(v_treatment_codes[current_treatment_code == v_treatment_codes])]]) + (treatment_stop * 0.01)
            )
          })
  
}






