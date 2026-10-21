#' Patient counts by EDSS score
#'
#' A matrix of patient counts stratified by EDSS score.
#'
#' @format A matrix with \code{2} rows and \code{10} columns.
#' Each column contains the number of patients at that EDSS score:
#' \describe{
#'   \item{harrms}{distribution of patietients}
#'   \item{rrms}{distribution of patietients}
#' }
#' @source AFFIRM baseline
"m_n_patients_edss"


#' DMT costs by year
#'
#' A matrix of annual Disease Modifying Therapy DMT costs by year
#'
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
#' @format A matrix with \code{5} rows and \code{8} columns.
#' Each column contains the percentage of patients treated/re-treated by by DMT, 
#' with the 5 rows repenting the load factors during the first years.
#' 
#' @source UoB_TAG analysis (refer to report)
"df_c_tx_load"


#' A matrix of excess all cause mortality for MS patients by severity 

#'
#' @format A matrix with \code{3} rows and \code{10} columns.
#' Each column contains the data by EDSS severity state:
#' \describe{
#'   \item{smr}{average standard mortality ratio by severity state}
#'   \item{sd}{standard deviationby severity state}
#'   \item{logsd}{log standard deviation by severity state}
#' }
#' @source edss<4 (Jick et al), edss>=4 (Harding et al)
"v_smr_severity_harding"

