# Script to analyse results

# Generate mean and 95% CrI for PSA samples
summarise_psa <- function(x, n_digits = 3) {
  return(paste0(format(mean(x), digits = n_digits, nsmall = n_digits),
                " (",
                format(quantile(x, probs = 0.025), digits = n_digits, nsmall = n_digits),
                ", ",
                format(quantile(x, probs = 0.975), digits = n_digits, nsmall = n_digits),
                ")"))
}

# Generate tabular summary of results
summarise_ce_results <- function(total_qalys, total_costs,
                                 reference = "Natalizumab-IV") {
  results_table <- matrix(nrow = dim(total_qalys)[2], ncol = 8)
  rownames(results_table) <- colnames(total_qalys)
  colnames(results_table) <- c("Total costs (95% CrI)",
                              "Total QALYs (95% CrI)",
                              "Net benefit at £20,000/QALY (95% CrI)",
                              "Net benefit at £30,000/QALY (95% CrI)",
                              "Incremental costs (95% CrI)",
                              "Incremental QALYs (95% CrI)",
                              "INB at £20,000/QALY (95% CrI)",
                              "INB at £30,000/QALY (95% CrI)")
  for(i_treatment in 1:dim(results_table)[1]) {
    results_table[i_treatment, "Total costs (95% CrI)"] <- 
      summarise_psa(total_costs[, i_treatment])
    
    results_table[i_treatment, "Total QALYs (95% CrI)"] <- 
      summarise_psa(total_qalys[, i_treatment])
    
    results_table[i_treatment, "Net benefit at £20,000/QALY (95% CrI)"] <- 
      summarise_psa(total_qalys[, i_treatment] * 20000 -
                      total_costs[, i_treatment])
    
    results_table[i_treatment, "Net benefit at £30,000/QALY (95% CrI)"] <- 
      summarise_psa(total_qalys[, i_treatment] * 30000 -
                      total_costs[, i_treatment])
    
    results_table[i_treatment, "Incremental costs (95% CrI)"] <- 
      summarise_psa(total_costs[, i_treatment] -
                      total_costs[, reference])
    
    results_table[i_treatment, "Incremental QALYs (95% CrI)"] <- 
      summarise_psa(total_qalys[, i_treatment] -
                      total_qalys[, reference])
    
    results_table[i_treatment, "INB at £20,000/QALY (95% CrI)"] <- 
      summarise_psa((total_qalys[, i_treatment] - total_qalys[, reference]) * 20000 -
                      (total_costs[, i_treatment] - total_costs[, reference]))
    
    results_table[i_treatment, "INB at £30,000/QALY (95% CrI)"] <- 
      summarise_psa((total_qalys[, i_treatment] - total_qalys[, reference]) * 30000 -
                      (total_costs[, i_treatment] - total_costs[, reference]))
    
  }
  
  return(results_table)
}

results_table <- summarise_ce_results(total_qalys = total_qalys, 
                     total_costs = total_costs,
                     reference = "Natalizumab-IV")

View(results_table)