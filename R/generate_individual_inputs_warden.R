#' Generate Unique Individual Patient Inputs
#'
#' Generates individual patient-level parameters that are heterogeneous across
#' patients and may depend on treatment history. Called once per new patient at
#' the start of their simulation. Returns a WARDEN \code{add_item()} object
#' consumed by \code{\link[msdes]{run_sim_new}} via the
#' \code{unique_pt_inputs} argument in \code{\link[msdes]{rrmsdesmodel}}.
#'
#' @details
#' Parameters are sampled or initialised for each new patient. The current
#' treatment arm is initialised here but is later overwritten by WARDEN with
#' the arm being simulated (\code{arm}).
#'
#' The following parameters are generated:
#'
#' \strong{Sampled at patient entry}
#' \itemize{
#'   \item \code{edss_value} — baseline EDSS score (0–9), sampled from a
#'     Dirichlet-weighted discrete distribution using the HARRMS row of
#'     \code{m_n_patients_edss} (MS Registry).
#'   \item \code{sex} — patient sex (0 = male, 1 = female), sampled using
#'     \code{prop_female} (0.70, AFFIRM).
#'   \item \code{spms_flag} — indicator for baseline SPMS status (0 = RRMS,
#'     1 = SPMS), sampled using \code{initial_spms_prop} (default 0).
#'   \item \code{relapse_hospital_flag} — indicator for whether a relapse
#'     leads to hospital admission, sampled using \code{p_relapse_hospital}
#'     (Hawton et al. 2026).
#'   \item \code{current_treatment} — initial treatment arm, sampled from
#'     \code{v_treatment_names} weighted by \code{l_prop_3rdline_trt}. Note:
#'     overwritten by WARDEN with the arm under simulation (\code{trt}).
#'   \item \code{current_treatment_code} — numeric code of
#'     \code{current_treatment}, looked up from \code{v_treatment_codes}.
#' }
#'
#' \strong{Event history counters (initialised to 0)}
#' \itemize{
#'   \item \code{n_relapse} — cumulative number of relapses.
#'   \item \code{n_sae_evts} — cumulative number of serious adverse events.
#'   \item \code{n_edssinc_evts} — cumulative number of EDSS increases.
#'   \item \code{n_edssdec_evts} — cumulative number of EDSS decreases.
#'   \item \code{n_switch} — cumulative number of treatment switches (sense
#'     check counter).
#' }
#'
#' \strong{Time trackers (initialised to 0)}
#' \itemize{
#'   \item \code{t_progression} — time spent in progression.
#'   \item \code{t_2ndline}, \code{t_3rdline}, \code{t_4thline} — time spent
#'     on 2nd, 3rd, and 4th line treatments respectively.
#'   \item \code{tto_dp1} to \code{tto_dp7} — time to each disability
#'     progression event, up to 7 events.
#'   \item \code{tto_rel1} to \code{tto_rel7} — time to each relapse event,
#'     up to 7 events.
#'   \item \code{yrs_edss_0} to \code{yrs_edss_9} — annual time spent in each
#'     EDSS severity state (0–9).
#'   \item \code{years_on_treatment} — cumulative years on current treatment,
#'     used for drug acquisition cost calculation.
#' }
#'
#' \strong{State flags (initialised to 0)}
#' \itemize{
#'   \item \code{waning} — waning of treatment effect flag. Set to 0 by
#'     default (no waning); multiplied into the hazard ratio as
#'     \code{HR x (1 - waning)}.
#'   \item \code{treatment_stop} — flag set to 1 when EDSS reaches 7 or
#'     above, stopping treatment acquisition costs.
#' }
#'
#' \strong{Treatment and patient state (initialised)}
#' \itemize{
#'   \item \code{treatment_line} — current treatment line, initialised to 2
#'     (all patients start on second-line therapy).
#'   \item \code{age} — baseline age, set to \code{inputs[["initial_age"]]}.
#' }
#'
#' \strong{Cost and utility accumulators (initialised to 0)}
#' \itemize{
#'   \item \code{utilities_disease} — accumulated disease utility.
#'   \item \code{costs_disease} — accumulated disease costs.
#'   \item \code{costs_treatment} — accumulated treatment costs.
#'   \item \code{utilities_relapse} — accumulated relapse utility.
#'   \item \code{costs_relapse} — accumulated relapse costs.
#'   \item \code{utilities_sae} — accumulated SAE utility.
#'   \item \code{costs_sae} — accumulated SAE costs.
#' }
#'
#' @return A WARDEN \code{add_item()} object containing all individual patient
#'   inputs, passed directly to \code{\link[WARDEN]{run_sim_new}} via the
#'   \code{unique_pt_inputs} argument in \code{\link[msdes]{rrmsdesmodel}}.
#'
#' @seealso
#'   \code{\link[msdes]{rrmsdessetup}},
#'   \code{\link[msdes]{rrmsdesmodel}},
#'   \code{\link[msdes]{gen_inputs}},
#'   \code{\link[WARDEN]{add_item}},
#'   \code{\link[msdes]{run_sim_new}}
#'
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
