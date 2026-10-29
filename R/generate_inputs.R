#' Generate RRMS DES Model Inputs
#'
#' Generates all model inputs from package data and user-specified assumptions,
#' returning a named list consumed by \code{\link[msdes]{rrmsdessetup}} and
#' subsequently passed to \code{\link[msdes]{rrmsdesmodel}} via the global
#' \code{inputs} object.
#'
#' @param drc          Numeric. Annual discount rate for costs, expressed as a
#'   proportion (e.g. \code{0.035} for 3.5\%).
#' @param drq          Numeric. Annual discount rate for QALYs, expressed as a
#'   proportion (e.g. \code{0.035} for 3.5\%).
#' @param time_horizon Integer. Model time horizon in years.
#' @param p_eid        Numeric. Proportion of patients receiving extended
#'   interval dosing (EID) of Natalizumab, expressed as a proportion
#'   (e.g. \code{0.6} for 60\%). Set to \code{0} to assume standard dosing
#'   for all patients.
#' @param jcvtest_cost Numeric. Unit cost (£) of a JC virus (JCV) antibody
#'   test. Set to \code{0} if the test is assumed to be provided free of
#'   charge (e.g. via manufacturer home service). The private cost is £247.
#'
#' @details
#' All default parameter values are stored within this function. The following
#' processing steps are performed before inputs are returned:
#'
#' \strong{Population}
#' \itemize{
#'   \item Baseline age is drawn from \code{Normal(36, 8/36)} reflecting both
#'     arms of the AFFIRM trial (N = 942).
#'   \item Proportion female set to 0.70 (n = 660, AFFIRM).
#'   \item PSA standard deviation set to 1/10 of the mean (\code{sd_psa = 10})
#'     where uncertainty is not reported.
#' }
#'
#' \strong{Treatment costs (\code{df_c_tx}, \code{df_c_tx_admin},
#' \code{df_c_tx_monitor})}
#' \itemize{
#'   \item Row 3 is dropped from each cost matrix and columns are restricted to
#'     active treatment names in \code{v_treatment_names}.
#'   \item Natalizumab IV and SC (columns 1–4) are adjusted for the proportion
#'     on EID using a 5.55-week interval dosing schedule
#'     (\code{round(52/5.55, 2)/13}).
#'   \item Administration costs for Natalizumab SC in year 1 and year 2 are
#'     scaled by \code{s_c_tx_adminyr1_natsc} and \code{s_c_tx_adminyr2_natsc}
#'     respectively (both defaulting to 0.5).
#'   \item Zero administration costs are set to 0.1 to prevent \code{NaN}
#'     values in downstream calculations.
#'   \item Monitoring costs for all four Natalizumab formulations are reduced
#'     by £247 and replaced with \code{jcvtest_cost}.
#' }
#'
#' \strong{Mortality}
#' \itemize{
#'   \item  standardised mortality ratios (SMR) EDSS 8 and 9 are assumed equal
#'     to EDSS 7 (Harding et al.).
#'   \item ONS all-cause mortality rates by age, male, ands ex.
#' }
#'
#' \strong{Relapse costs}
#' \itemize{
#'   \item Annualisedrelapse costs (Hawton 2016, inflated to 2023) 
#' }
#'
#' \strong{NMA data}
#' \itemize{
#'   \item Mean and standard deviation columns are extracted from each NMA
#'     result object (CDP3, CDP6, ARR, SAEs, discontinuation) 
#' }
#'
#' @return A named list of model inputs. Key elements include:
#' \describe{
#'   \item{\code{initial_age}}{Numeric. Baseline age in years.}
#'   \item{\code{time_horizon}}{Integer. Model time horizon in years.}
#'   \item{\code{prop_female}}{Numeric. Proportion of patients who are female
#'     (0.70).}
#'   \item{\code{sd_psa}}{Numeric. Default PSA standard deviation (10).}
#'   \item{\code{p_eid}}{Numeric. Proportion on extended interval dosing.}
#'   \item{\code{drc}, \code{drq}}{Numeric. Discount rates for costs and
#'     QALYs.}
#'   \item{\code{jcvtest_cost}}{Numeric. JCV test unit cost (£).}
#'   \item{\code{m_n_patients_edss}}{Matrix. Baseline EDSS distribution from
#'     the MS Registry.}
#'   \item{\code{df_c_tx}, \code{df_c_tx_admin},
#'     \code{df_c_tx_monitor}}{Data frames of annual treatment, administration,
#'     and monitoring costs per DMT, adjusted for EID and JCV test assumptions.}
#'   \item{\code{df_c_tx_load}}{Data frame of annual treatment scheduled load
#'     factors for years 1–5.}
#'   \item{\code{v_smr_severity_harding}}{Matrix. Excess all-cause mortality
#'     SMRs by EDSS severity (Harding et al.).}
#'   \item{\code{m_proportion_ontrtline}}{Matrix. Proportions of patients on
#'     2nd and 3rd line treatment.}
#'   \item{\code{v_tto_edss_increase_harrms_mu},
#'     \code{m_tto_edss_increase_harrms_sigma}}{Time-to-event parameters for
#'     EDSS increase in HARRMS patients (MS Registry).}
#'   \item{\code{v_tto_edss_increase_rrms_mu},
#'     \code{m_tto_edss_increase_rrms_sigma}}{Time-to-event parameters for
#'     EDSS increase in RRMS patients.}
#'   \item{\code{v_tto_edss_increase_spms_mu},
#'     \code{m_tto_edss_increase_spms_sigma}}{Time-to-event parameters for
#'     EDSS increase in SPMS patients.}
#'   \item{\code{v_tto_edss_decrease_rrms_mu},
#'     \code{m_tto_edss_decrease_rrms_sigma}}{Time-to-event parameters for
#'     EDSS decrease in RRMS patients.}
#'   \item{\code{v_tto_edss_decrease_spms_mu},
#'     \code{m_tto_edss_decrease_spms_sigma}}{Time-to-event parameters for
#'     EDSS decrease in SPMS patients.}
#'   \item{\code{v_tto_relapse_harrms_mu},
#'     \code{tto_relapse_harrms_sigma}}{Time-to-event parameters for relapse
#'     in HARRMS patients.}
#'   \item{\code{v_tto_relapse_spms_mu},
#'     \code{m_tto_relapse_spms_sigma}}{Time-to-event parameters for relapse
#'     in SPMS patients.}
#'   \item{\code{v_tto_sp_conversion_harrms_mu},
#'     \code{m_tto_sp_conversion_harrms_sigma}}{Time-to-event parameters for
#'     secondary progressive conversion in HARRMS patients.}
#'   \item{\code{initial_spms_prop}}{Numeric. Baseline proportion of patients
#'     with SPMS (0).}
#'   \item{\code{p_relapse_hospital}}{Numeric. Proportion of relapses leading
#'     to hospital admission (Hawton et al. 2026).}
#'   \item{\code{sae_names}}{Character vector of serious adverse event names
#'     from AFFIRM.}
#'   \item{\code{n_sae}}{Integer. Number of serious adverse events in the
#'     model.}
#'   \item{\code{v_weight_sae}}{Numeric vector. Weights for SAE cost and
#'     utility calculations.}
#'   \item{\code{m_saes}}{Matrix. SAE incidence on Natalizumab from AFFIRM
#'     (Polman 2006).}
#'   \item{\code{v_smr}}{Numeric vector. Excess all-cause mortality SMRs
#'     (Jick 2014).}
#'   \item{\code{m_r_mortality}}{Matrix. ONS all-cause mortality rates by age,
#'     male, and female.}
#'   \item{\code{m_c_edss_rrms}, \code{m_c_edss_spms}}{Matrices of direct
#'     medical costs by EDSS state for RRMS and SPMS (Tyas et al. 2005,
#'     inflated to 2022/23).}
#'   \item{\code{df_c_spms}}{Matrix of SPMS treatment costs.}
#'   \item{\code{p_siponimod}}{Numeric. Proportion treated with Siponimod
#'     based on MS Registry data (0.557).}
#'   \item{\code{m_c_relapse}}{Matrix of annualised relapse costs (Hawton
#'     2016, inflated to 2023).}
#'   \item{\code{df_c_sae}}{Matrix of serious adverse event costs (MS-MTA
#'     report).}
#'   \item{\code{m_u_edss_rrms}, \code{m_u_edss_spms}}{Matrices of EDSS
#'     utilities for RRMS and SPMS (Orme).}
#'   \item{\code{df_u_relapse}}{Data frame of relapse utilities.}
#'   \item{\code{m_u_carer}}{Matrix of carer disutilities.}
#'   \item{\code{df_u_sae}}{Matrix of serious adverse event disutilities
#'     (MS-MTA report).}
#'   \item{\code{m_discontinuation}}{Matrix of baseline discontinuation rates
#'     (AFFIRM and Antilope).}
#'   \item{\code{m_hr_rrms_cdp3_fe}}{Matrix. CDP3 NMA hazard ratios (mean,
#'     SD).}
#'   \item{\code{m_hr_rrms_cdp6_fe}}{Matrix. CDP6 NMA hazard ratios (mean,
#'     SD).}
#'   \item{\code{m_hr_rrms_arr_fe}}{Matrix. ARR NMA rate ratios (mean, SD).}
#'   \item{\code{m_hr_rrms_saes_fe}}{Matrix. SAE NMA rate ratios (mean, SD).}
#'   \item{\code{m_hr_rrms_discontinuation_fe}}{Matrix. Discontinuation NMA
#'     hazard ratios (mean, SD).}
#' }
#'
#' @examples
#' \dontrun{
#' inputs <- gen_inputs(
#'   drc          = 0.035,
#'   drq          = 0.035,
#'   time_horizon = 70,
#'   p_eid        = 0,
#'   jcvtest_cost = 0
#' )
#'
#' # Access individual inputs
#' inputs[["df_c_tx"]]
#' inputs[["time_horizon"]]
#' }
#'
#' @export
gen_inputs <- function(drc=drc, drq=drq,
                       time_horizon=time_horizon, p_eid=p_eid, jcvtest_cost=jcvtest_cost){
  ## Standard Deviation for probabilistic analysis when uncertainty not reported
  # assumed 1/10 of mean
  sd_psa <- 10
  
  
  drc<-drc #discount rate costs
  drq<-drq #discount rate qalys
  
  ## Baseline population age sex and model time horizon
  initial_age <- rnorm(1,36, 8/36)  # 36 ± 8 both arms of AFFIRM (N = 942)
  time_horizon <- time_horizon # Years so lifetime
  prop_female <- 0.70 #  (n=660) both arms of AFFIRM (N = 942)
  p_eid <- p_eid # proportion on extended interval dosing
  s_c_tx_adminyr1_natsc <-0.5 # scenario reducing admin cost of natalizumab sc year 1, default is 0.5
  s_c_tx_adminyr2_natsc <-0.5 # scenario reducing admin cost of natalizumab sc year 2, default is 0?
  jcvtest_cost <- jcvtest_cost #scenario reducing the JCV testing cost to zero
  
  #add sensitivities for NMA here or in generate_global_inputs_warden.R
  
  ## baseline distribution of EDSS health states (MS Registry)
  colnames(m_n_patients_edss) <- names(v_edss_values)
  
  ##  Annual treatment, administration and monitoring costs 
  #public list prices
  df_c_tx <- df_c_tx[-3,intersect(colnames(df_c_tx),v_treatment_names)]
  df_c_tx[,1:4] <- (df_c_tx[,1:4]*p_eid*(round(52/5.55, 2)/13)) + df_c_tx[,1:4]*(1-p_eid) # 60% on EID interval dosing
  
  # These are direct medical costs we excluded the indirect medical costs
  df_c_tx_admin[df_c_tx_admin==0] <-0.1 # set zero costs to positive number stops generating NaNs
  df_c_tx_admin <- df_c_tx_admin[-3,intersect(colnames(df_c_tx_admin),v_treatment_names)]
  df_c_tx_admin[,1:4] <- (df_c_tx_admin[,1:4]*p_eid*(round(52/5.55, 2)/13)) + df_c_tx_admin[,1:4]*(1-p_eid) # 60% on EID interval dosing
  df_c_tx_admin[1,"Natalizumab-SC"] <- (df_c_tx_admin[1,"Natalizumab-SC"] * s_c_tx_adminyr1_natsc) # Scenario for reduced admin cost for Nat SC year 1
  df_c_tx_admin[2,"Natalizumab-SC"] <- (df_c_tx_admin[2,"Natalizumab-SC"] * s_c_tx_adminyr2_natsc) # Scenario for reduced admin cost for Nat SC year 2

  df_c_tx_monitor <- df_c_tx_monitor[-3,intersect(colnames(df_c_tx_monitor),v_treatment_names)]
  df_c_tx_monitor[,"Natalizumab-IV"] <- (df_c_tx_monitor[,"Natalizumab-IV"] -247 + jcvtest_cost) #  free JCV test 
  df_c_tx_monitor[,"Natalizumab-SC"] <- (df_c_tx_monitor[,"Natalizumab-SC"] -247+ jcvtest_cost) #  free JCV test 
  df_c_tx_monitor[,"Natalizumab biosimilar-IV"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-IV"] -247+ jcvtest_cost) #  free JCV test 
  df_c_tx_monitor[,"Natalizumab biosimilar-SC"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-SC"] -247+ jcvtest_cost) #  free JCV test
  
  ## Annual treatment scheduled load factors years 1-5
  df_c_tx_load <- df_c_tx_load[,intersect(colnames(df_c_tx_load),v_treatment_names)]
  rownames(df_c_tx_load)<- c(seq(1,5,1))

  ## Excess all cause mortality for MS patients HR by severity edss<4 (Jick et al), edss>=4 (Harding et al)
  v_smr_severity_harding[,c(9:10)] <- v_smr_severity_harding[,8] # Assuming SMR on EDSS 8 & 9 are equal to EDSS7 
  dimnames(v_smr_severity_harding) <- list(c("smr", "sd", "logsd"), v_edss_values)
  
  ## proportions on 2nd and 3rd line treatment, assuming Cladribine and Natalizumab, Ublituximab and Ocrelizumab are equivalent to 
  colnames(m_proportion_ontrtline) <- v_treatment_names
  m_proportion_ontrtline <- as.matrix(m_proportion_ontrtline)
  
  ## Natural History MS Registry
  ## Vectors of coefficients and matrices of variance and covariance
  v_tto_edss_increase_harrms_mu
  m_tto_edss_increase_harrms_sigma 
  v_tto_edss_increase_rrms_mu 
  m_tto_edss_increase_rrms_sigma
  v_tto_edss_increase_spms_mu
  m_tto_edss_increase_spms_sigma
  
  v_tto_edss_decrease_rrms_mu 
  m_tto_edss_decrease_rrms_sigma
  v_tto_edss_decrease_spms_mu 
  m_tto_edss_decrease_spms_sigma 
  
  v_tto_relapse_harrms_mu 
  tto_relapse_harrms_sigma
  v_tto_relapse_spms_mu 
  m_tto_relapse_spms_sigma
  
  v_tto_sp_conversion_harrms_mu 
  m_tto_sp_conversion_harrms_sigma
  
  ## baseline proportion of patients with SPMS
  initial_spms_prop <- 0
  
  ## Proportion of patients having relapse leading to hospital_admission Hawton et al 2026
  p_relapse_hospital<- v_p_relapse_severity[,"hospital_admission"]
  
  ## Incidence & weights of Serious Adverse events in AFFIRM
  sae_names <- saes_data[,"Serious.Adverse.events"]
  
  # Number of serious adverse events in the model
  n_sae <- length(sae_names)
  
  # The weights are used to calculate SAEs costs and utilities 
  v_weight_sae <- saes_data[, "Weight"] 
  
  # % on Natalizumab would have experienced SAEs AFFIRM (Polman 2006)
  m_saes
  
  ## Excess all cause mortality for MS patients Jick 2014 
  v_smr 
  
  ##rates of mortality, ONS
  m_r_mortality <- m_r_mortality[,c("age", "mx", "mx.1")]
  m_r_mortality[,"age"] <- as.numeric(m_r_mortality[,"age"]) #check if this step is needed?
  dimnames(m_r_mortality) <-  list(NULL, c("time" ,"male", "female"))
  
  ## edss costs direct medical costs Tyas etal. 2005 inflated to 2022/2023 
  m_c_edss_rrms <- m_c_edss[,c("rrms_mean","rrms_se")]
  m_c_edss_spms <- m_c_edss[,c("spms_mean","spms_se")]
  
  # SPMS Costs
  colnames(df_c_spms)[1] <- "PegIFN-β-1 SC 125μg"
  df_c_spms<- as.matrix(df_c_spms)
  
  p_siponimod <-0.556962025 #proportion treated with Siponimod based on MS registry data
  
  ## Relapse Costs inflated Hawton 2016 to 2023
  m_c_relapse <- m_c_relapse * 2 # Annualised these 6 months costs
  m_c_relapse <- as.matrix(m_c_relapse)
  
  ## Adverse events costs MS-MTA report
  df_c_sae <- saes_data[,4]
  names(df_c_sae)<-sae_names
  df_c_sae <- as.matrix(df_c_sae)
  
  ## RRMS & SPMS edss utilities Orme 
  m_u_edss_rrms <- u_edss[,c("rrms_mean","rrms_sd")]
  m_u_edss_spms <- u_edss[,c("spms_mean","spms_sd")]
  
  ## Relapse utilities
  df_u_relapse
  
  ## Carer disutilities # add to costs calculation
  m_u_carer
  
  ## Adverse events dis-utilities MS-MTA report
  df_u_sae <- saes_data[,5]
  names(df_u_sae)<-sae_names
  df_u_sae <- as.matrix(df_u_sae)
  
  ## baseline discontinuation AFFIRM and Antilope
  m_discontinuation
  
  ## NMA data
  # CDP3 NMA
  m_hr_rrms_cdp3_fe  <- hr_rrms_cdp3_fe[,c("mean", "sd")]
  m_hr_rrms_cdp3_fe  <- as.matrix(m_hr_rrms_cdp3_fe)
  
  # CDP6 NMA
  m_hr_rrms_cdp6_fe  <- hr_rrms_cdp6_fe[,c("mean", "sd")] 
  m_hr_rrms_cdp6_fe <- as.matrix(m_hr_rrms_cdp6_fe)
  
  # ARR NMA
  m_hr_rrms_arr_fe  <- hr_rrms_arr_fe[,c("mean", "sd")] 
  m_hr_rrms_arr_fe <- as.matrix(m_hr_rrms_arr_fe)
  
  # Serious Adverse Events NMA
  m_hr_rrms_saes_fe  <- as.matrix(hr_rrms_saes_fe[,c("mean", "sd")] )
  
  # Discontinuation NMA
  m_hr_rrms_discontinuation_fe <- as.matrix(hr_rrms_discontinuation_fe[,c("mean", "sd")]) 
  
  return(list(
    ## Baseline population age sex and model time horizon
    initial_age =initial_age,
    time_horizon = time_horizon,
    prop_female = prop_female,
    sd_psa =sd_psa,
    p_eid=p_eid,
    drc=drc,
    drq=drq,
    time_horizon=time_horizon,
    jcvtest_cost=jcvtest_cost,
    ## baseline distribution of EDSS health states (MS Registry)
    m_n_patients_edss=m_n_patients_edss, 
    ##  Annual treatment, administration and monitoring costs 
    df_c_tx=df_c_tx, df_c_tx_admin=df_c_tx_admin, df_c_tx_monitor=df_c_tx_monitor, 
    ## Annual treatment scheduled
    df_c_tx_load=df_c_tx_load ,
    ## Excess all cause mortality for MS patients
    v_smr_severity_harding=v_smr_severity_harding, 
    ## proportions on 2nd and 3rd line treatment
    m_proportion_ontrtline=m_proportion_ontrtline,
    ## Natural History MS Registry
    v_tto_edss_increase_harrms_mu=v_tto_edss_increase_harrms_mu, m_tto_edss_increase_harrms_sigma=m_tto_edss_increase_harrms_sigma,
    v_tto_edss_increase_rrms_mu=v_tto_edss_increase_rrms_mu, m_tto_edss_increase_rrms_sigma=m_tto_edss_increase_rrms_sigma,
    v_tto_edss_increase_spms_mu=v_tto_edss_increase_spms_mu, m_tto_edss_increase_spms_sigma=m_tto_edss_increase_spms_sigma,
    v_tto_edss_decrease_rrms_mu=v_tto_edss_decrease_rrms_mu, m_tto_edss_decrease_rrms_sigma=m_tto_edss_decrease_rrms_sigma,
    v_tto_edss_decrease_spms_mu=v_tto_edss_decrease_spms_mu, m_tto_edss_decrease_spms_sigma=m_tto_edss_decrease_spms_sigma,
    v_tto_relapse_harrms_mu=v_tto_relapse_harrms_mu, tto_relapse_harrms_sigma=tto_relapse_harrms_sigma,
    v_tto_relapse_spms_mu=v_tto_relapse_spms_mu, m_tto_relapse_spms_sigma=m_tto_relapse_spms_sigma,
    v_tto_sp_conversion_harrms_mu=v_tto_sp_conversion_harrms_mu, m_tto_sp_conversion_harrms_sigma=m_tto_sp_conversion_harrms_sigma,
    ## baseline proportion of patients with SPMS
    initial_spms_prop=initial_spms_prop,
    ## Proportion of patients having relapse leading to hospital_admission
    p_relapse_hospital=p_relapse_hospital, 
    ## Incidence & weights of Serious Adverse events
    sae_names=sae_names, 
    # Number of serious adverse events in the model
    n_sae=n_sae, 
    # The weights are used to calculate SAEs costs and utilities 
    v_weight_sae=v_weight_sae, 
    # % on Natalizumab would have experienced SAEs
    m_saes=m_saes,
    ## Excess all cause mortality for MS patients
    v_smr=v_smr, 
    ##rates of mortality, ONS
    m_r_mortality=m_r_mortality, 
    ## edss costs direct medical costs
    m_c_edss_rrms=m_c_edss_rrms,
    m_c_edss_spms=m_c_edss_spms, 
    # SPMS Costs
    df_c_spms=df_c_spms,
    p_siponimod=p_siponimod, 
    ## Relapse Costs 
    m_c_relapse=m_c_relapse,
    ## Adverse events costs
    df_c_sae=df_c_sae,
    ## RRMS & SPMS edss utilities
    m_u_edss_rrms=m_u_edss_rrms,
    m_u_edss_spms=m_u_edss_spms, 
    ## Relapse utilities
    df_u_relapse=df_u_relapse, 
    ## Carer disutilities
    m_u_carer=m_u_carer, 
    ## Adverse events dis-utilities
    df_u_sae=df_u_sae, 
    ## baseline discontinuation
    m_discontinuation=m_discontinuation,
    ## NMA data
    m_hr_rrms_cdp3_fe=m_hr_rrms_cdp3_fe, 
    m_hr_rrms_cdp6_fe=m_hr_rrms_cdp6_fe, 
    m_hr_rrms_arr_fe=m_hr_rrms_arr_fe, 
    m_hr_rrms_saes_fe=m_hr_rrms_saes_fe,
    m_hr_rrms_discontinuation_fe=m_hr_rrms_discontinuation_fe
  )
  )

  
}











