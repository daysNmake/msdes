#'
#' @description
#' Functions to tabulate the PSA results at WTP25k
#' 
#' @details
#' 'summarise_tha_results()'returns the assessment of a treatment incremental net benefit 
#' calculates the expected incremental net benefit ratio 
#' returns the probability of treatment arms providing positive net benefits
#' 
#' @param total_qalys the average total quality adjusted life years per treatment arm
#' @param total_costs the average total costs per treatment arm
#' @param reference The treatment arm specified as the reference for the comparison
#' @export
summarise_tha_results <- function(total_qalys, total_costs,
                                 reference) {
  
  # Calculate net benefits so as to calculate CEAC
  net_benefit_25k <- total_qalys * 25000 - total_costs
  #net_benefit_30k <- total_qalys * 30000 - total_costs
  
  threshold_table <- matrix(nrow = dim(total_qalys)[2], ncol = 3)
  rownames(threshold_table) <- colnames(total_qalys)
  colnames(threshold_table) <- c("Net benefit at £25,000/QALY (95% CrI)",
                                "INB at £25,000/QALY (95% CrI)",
                               "PINB>0 at £25,000/QALY"
                               )
  for(i_treatment in 1:dim(threshold_table)[1]) {
 
     threshold_table[i_treatment, "Net benefit at £25,000/QALY (95% CrI)"] <- 
       summarise_samples(total_qalys[, i_treatment] * 25000 -
                       total_costs[, i_treatment], n_digits = 2)


    threshold_table[i_treatment, "INB at £25,000/QALY (95% CrI)"] <- 
      summarise_samples((total_qalys[, i_treatment] - total_qalys[, reference]) * 25000 -
                      (total_costs[, i_treatment] - total_costs[, reference]), n_digits = 2)

    
    threshold_table[i_treatment, "PINB>0 at £25,000/QALY"] <- 

        sum(
          ((total_qalys[, i_treatment] - total_qalys[, reference]) * 25000 -
                          (total_costs[, i_treatment] - total_costs[, reference]))
          > 0, na.rm = TRUE) /
          length(
            ((total_qalys[, i_treatment] - total_qalys[, reference]) * 25000 -
               (total_costs[, i_treatment] - total_costs[, reference]))
          )
       
  }
  
  return(threshold_table)
}
