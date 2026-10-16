#'
#' @description
#' Functions to tabulate the PSA results at willingness to pay (WTP) thresholds
#' 
#' @details
#' 'summarise_ce_results()'returns the incremental cost and qalys
#' calculates the expected incremental cost effectiveness ratio 
#' calculates the expected incremental net benefit ratio 
#' returns the probability of treatment arms being cost effective
#' 
#' @param wtp1 set the willingness to pay threshold, NICE default is £25,000
#' @param wtp2 set the willingness to pay threshold, NICE default is £35,000
#' @param total_qalys input from the model results: the average total quality adjusted life years per treatment arm
#' @param total_costs input from the model results: the average total costs per treatment arm
#' @param reference The treatment arm specified as the reference for the comparison
#' @export
summarise_ce_results <- function(wtp1=wtp1, wtp2=wtp2,
                                total_qalys, total_costs,
                                 reference = "Natalizumab-IV") {
  
  # Calculate net benefits so as to calculate CEAC
  
  net_benefit_wtp1 <- total_qalys * wtp1 - total_costs
  net_benefit_wtp2 <- total_qalys * wtp2 - total_costs
  
  results_table <- matrix(nrow = dim(total_qalys)[2], ncol = 10)
  rownames(results_table) <- colnames(total_qalys)
  colnames(results_table) <- c("Total costs (95% CrI)",
                               "Total QALYs (95% CrI)",
                               paste0("Net benefit at £",{wtp1} , "/QALY (95% CrI)"),
                               paste0("Net benefit at £",{wtp2} , "/QALY (95% CrI)"),
                               "Incremental costs (95% CrI)",
                               "Incremental QALYs (95% CrI)",
                               paste0("INB at £",{wtp1} , "/QALY (95% CrI)"),
                               paste0("INB at £",{wtp2} , "/QALY (95% CrI)"),
                               paste0("CEAC at £",{wtp1} , "/QALY (95% CrI)"),
                               paste0("CEAC at £",{wtp2} , "/QALY (95% CrI)")
                               )
  for(i_treatment in 1:dim(results_table)[1]) {
    results_table[i_treatment, "Total costs (95% CrI)"] <- 
      summarise_samples(total_costs[, i_treatment], n_digits = 2)
    
    results_table[i_treatment, "Total QALYs (95% CrI)"] <- 
      summarise_samples(total_qalys[, i_treatment], n_digits = 2)
    
    results_table[i_treatment, paste0("Net benefit at £",{wtp1} , "/QALY (95% CrI)")] <- 
      summarise_samples(total_qalys[, i_treatment] * wtp1 -
                      total_costs[, i_treatment], n_digits = 2)
    
    results_table[i_treatment, paste0("Net benefit at £",{wtp2} , "/QALY (95% CrI)")] <- 
      summarise_samples(total_qalys[, i_treatment] * wtp2 -
                      total_costs[, i_treatment], n_digits = 2)
    
    results_table[i_treatment, "Incremental costs (95% CrI)"] <- 
      summarise_samples(total_costs[, i_treatment] -
                      total_costs[, reference], n_digits = 2)
    
    results_table[i_treatment, "Incremental QALYs (95% CrI)"] <- 
      summarise_samples(total_qalys[, i_treatment] -
                      total_qalys[, reference], n_digits = 2)
    
    results_table[i_treatment, paste0("INB at £",{wtp1} , "/QALY (95% CrI)")] <- 
      summarise_samples((total_qalys[, i_treatment] - total_qalys[, reference]) * wtp1 -
                      (total_costs[, i_treatment] - total_costs[, reference]), n_digits = 2)
    
    results_table[i_treatment, paste0("INB at £",{wtp2} , "/QALY (95% CrI)")] <- 
      summarise_samples((total_qalys[, i_treatment] - total_qalys[, reference]) * wtp2 -
                      (total_costs[, i_treatment] - total_costs[, reference]), n_digits = 2)
    
    results_table[i_treatment, paste0("CEAC at £",{wtp1} , "/QALY (95% CrI)")] <- 
      sum(apply(net_benefit_wtp1, c(1), which.max) == i_treatment) /
      dim(net_benefit_wtp1)[1]
    
    results_table[i_treatment, paste0("CEAC at £",{wtp2} , "/QALY (95% CrI)")] <- 
      sum(apply(net_benefit_wtp2, c(1), which.max) == i_treatment) /
      dim(net_benefit_wtp2)[1]
  }
  
  return(results_table)
}


