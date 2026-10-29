#' Summarise Individual Patient Data from DES Results
#'
#' Extracts total costs and QALYs from raw WARDEN DES simulation output,
#' organises them into a \[samples x treatments x patients\] array, and
#' returns the mean across patients for each sample and treatment arm.
#'
#' @param res_list  List of length \code{n_samples}. Each element is a WARDEN
#'   simulation result containing a \code{merged_df} data frame with columns
#'   \code{evtname}, \code{arm}, \code{pat_id}, \code{total_costs}, and
#'   \code{total_qalys}.
#' @param n_samples  Integer. Number of PSA samples (outer loop iterations).
#' @param n_patients Integer. Number of simulated patients per sample.
#'
#' @details
#' Only rows where \code{evtname == "death"} are used, as these carry each
#' patient's cumulative costs and QALYs at the end of their simulated
#' lifetime. The function relies on two objects from the global environment
#' set by \code{msdes::rrmsdessetup()}:
#' \itemize{
#'   \item \code{v_treatment_names} — character vector of treatment arm names,
#'     used to name array dimensions and build the treatment index.
#'   \item \code{n_treatments} — integer count of treatment arms, used to
#'     size the array.
#' }
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{\code{costs}}{Numeric matrix \[n_samples x n_treatments\] of mean
#'     total discounted costs per patient, averaged across \code{n_patients}.}
#'   \item{\code{qalys}}{Numeric matrix \[n_samples x n_treatments\] of mean
#'     total discounted QALYs per patient, averaged across \code{n_patients}.}
#' }
#'
#' @seealso \code{\link[msdes]{rrmsdessetup}}, \code{\link[msdes]{rrmsdesmodel}}
#'
#' @examples
#' \dontrun{
#' rrmsdessetup(drc = 0.035, drq = 0.035, time_horizon = 70,
#'                     p_eid = 0, jcvtest_cost = 0)
#' rrmsdesmodel(n_patients = 10, n_samples = 10)
#'
#' ipdcesummary <- summarise_ipd(
#'   res_list   = results[[1]],
#'   n_samples  = 10,
#'   n_patients = 10
#' )
#'
#' }
#'
#' @export

summarise_ipd <- function(res_list, n_samples, n_patients) {
  dim_res   <- c(n_samples, n_treatments, n_patients)
  trt_idx   <- setNames(seq_along(v_treatment_names), v_treatment_names)
  
  costs_arr  <- array(NA_real_, dim = dim_res,
                      dimnames = list(NULL, v_treatment_names, NULL))
  qalys_arr  <- array(NA_real_, dim = dim_res,
                      dimnames = list(NULL, v_treatment_names, NULL))
  
  for (i_sample in seq_len(n_samples)) {
    df  <- res_list[[i_sample]]$merged_df
    df  <- df[df$evtname == "death", ]
    idx <- cbind(i_sample, trt_idx[df$arm], df$pat_id)
    costs_arr[idx] <- df$total_costs
    qalys_arr[idx] <- df$total_qalys
  }
  
  
  list(
    costs  = apply(costs_arr,  c(1, 2), mean),
    qalys  = apply(qalys_arr,  c(1, 2), mean)
  )
}
