#' Patient counts by EDSS score
#'
#' A matrix of patient counts stratified by EDSS score.
#'
#' @keywords data
#' @format A matrix with \code{2} rows and \code{10} columns.
#' Each column contains the number of patients at that EDSS score:
#' \describe{
#'   \item{harrms}{distribution of patients}
#'   \item{rrms}{distribution of patients}
#' }
#' @source AFFIRM baseline
"m_n_patients_edss"


#' DMT costs by year
#'
#' A matrix of annual Disease Modifying Therapy DMT costs by year
#'
#' @keywords data
#' @format A matrix with \code{3} rows and \code{17} columns.
#' Each column contains the costs by treatment:
#' \describe{
#'   \item{cost_annual_yr1}{Annual DMT costs during 1st year of treatment}
#'   \item{cost_annual_yr2}{Annual DMT costs during 2nd year of treatment}
#' }
#' @source British National Formulary
"df_c_tx"


#' Treatment administration costs by year
#'
#' A matrix of annual treatment administration costs by year
#'
#' @keywords data
#' @format A matrix with \code{3} rows and \code{17} columns.
#' Each column contains the costs by treatment:
#' \describe{
#'   \item{cost_annual_yr1}{Annual treatment administration costs during 1st year of treatment}
#'   \item{cost_annual_yr2}{Annual treatment administration costs during 2nd year of treatment}
#' }
#' @source UoB_TAG analysis (refer to report)
"df_c_tx_admin"


#' Treatment monitoring costs by year
#'
#' A matrix of annual treatment monitoring costs by year
#'
#' @keywords data
#' @format A matrix with \code{3} rows and \code{17} columns.
#' Each column contains the costs by treatment:
#' \describe{
#'   \item{cost_annual_yr1}{Annual treatment monitoring costs during 1st year of treatment}
#'   \item{cost_annual_yr2}{Annual treatment monitoring costs during 2nd year of treatment}
#' }
#' @source UoB_TAG analysis (refer to report)
"df_c_tx_monitor"


#' Annual treatment scheduled load factors years 1-5
#'
#' A matrix of annual treatment scheduled load factors years 1-5
#' 
#' @keywords data
#' @format A matrix with \code{5} rows and \code{8} columns.
#' Each column contains the percentage of patients treated/re-treated by by DMT, 
#' with the 5 rows repenting the load factors during the first years.
#' 
#' @source UoB_TAG analysis (refer to report)
"df_c_tx_load"


#' A matrix of excess all cause mortality for MS patients by severity 
#' 
#' @keywords data
#' @format A matrix with \code{3} rows and \code{10} columns.
#' Each column contains the data by EDSS severity state:
#' \describe{
#'   \item{smr}{average standard mortality ratio by severity state}
#'   \item{sd}{standard deviationby severity state}
#'   \item{logsd}{log standard deviation by severity state}
#' }
#' @source edss<4 (Jick et al), edss>=4 (Harding et al)
"v_smr_severity_harding"


#' A matrix of patient counts stratified by treatment
#' 
#' @keywords data
#' @format A matrix with \code{4} rows and \code{8} columns.
#' Each column contains the number of patients on treatment at line of treatment :
#' \describe{
#'   \item{alpha_2ndline}{distribution of patients on 2nd line treatment}
#'   \item{beta_2ndline}{distribution of patients on 2nd line treatment}
#'   \item{alpha_3rdline}{distribution of patients on 3rd line treatment}
#'   \item{beta_3rdline}{distribution of patients on 3rd line treatment}
#' }
#' @source MS Registry analysis
"m_proportion_ontrtline"

#' Natural history treatment dependent coefficients.
#' 
#' @keywords data
#' @format A named vector with 3 elements containing the formula coefficients:
#' #' \describe{
#'   \item{rate}{intercept coefficient}
#'   \item{BaselineWebEDSS}{EDSS coefficient}
#'   \item{DMTNatalizumab}{on treatment with Natalizumab coefficient}
#' }
#' @source MS Registry analysis
#' @name v_tto_edss_increase_harrms_mu
#' @aliases v_tto_edss_increase_rrms_mu v_tto_relapse_harrms_mu v_tto_sp_conversion_harrms_mu
NULL

#' Natural history treatment dependent variance covariance.
#' 
#' @keywords data
#' @format A matrix with \code{3} rows and \code{3} columns
#' #' \describe{
#'   \item{rate}{intercept VCV}
#'   \item{BaselineWebEDSS}{EDSS VCV}
#'   \item{DMTNatalizumab}{on treatment with Natalizumab VCV}
#' }
#' @source MS Registry analysis
#' @name m_tto_edss_increase_harrms_sigma 
#' @aliases m_tto_edss_increase_rrms_sigma tto_relapse_harrms_sigma m_tto_sp_conversion_harrms_sigma
NULL

#' Natural history coefficients.
#' 
#' @keywords data
#' @format A named vector with 2 elements containing the formula coefficients:
#' #' \describe{
#'   \item{rate}{intercept coefficient}
#'   \item{BaselineWebEDSS}{EDSS coefficient}
#' }
#' @source MS Registry analysis
#' @name v_tto_edss_increase_spms_mu
#' @aliases v_tto_edss_decrease_rrms_mu v_tto_edss_decrease_spms_mu v_tto_relapse_spms_mu
NULL

#' Natural history variance covariance.
#' 
#' @keywords data
#' @format A matrix with \code{2} rows and \code{2} columns
#' #' \describe{
#'   \item{rate}{intercept VCV}
#'   \item{BaselineWebEDSS}{EDSS VCV}
#' }
#' @source MS Registry analysis
#' @name m_tto_edss_increase_spms_sigma
#' @aliases m_tto_edss_decrease_rrms_sigma m_tto_edss_decrease_spms_sigma m_tto_relapse_spms_sigma
NULL
