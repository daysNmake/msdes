# Functions to tabulate the PSA results
summarise_tha_results <- function(total_qalys, total_costs,
                                 reference) {
  
  # Calculate net benefits so as to calculate CEAC
  net_benefit_25k <- total_qalys * 25000 - total_costs
  #net_benefit_30k <- total_qalys * 30000 - total_costs
  
  threshold_table <- matrix(nrow = dim(total_qalys)[2], ncol = 3)
  rownames(threshold_table) <- colnames(total_qalys)
  colnames(threshold_table) <- c( #"Total costs (95% CrI)",
                               # "Total QALYs (95% CrI)",
                                "Net benefit at £25,000/QALY (95% CrI)",
                               # #"Net benefit at £30,000/QALY (95% CrI)",
                               # "Incremental costs (95% CrI)",
                               # "Incremental QALYs (95% CrI)",
                                "INB at £25,000/QALY (95% CrI)",
                               # #"INB at £30,000/QALY (95% CrI)",
                               # "CEAC at £25,000/QALY",
                               "PINB>0 at £25,000/QALY"
                               #"CEAC at £30,000/QALY"
                               )
  for(i_treatment in 1:dim(threshold_table)[1]) {
    # threshold_table[i_treatment, "Total costs (95% CrI)"] <- 
    #   summarise_samples(total_costs[, i_treatment], n_digits = 2)
    # 
    # threshold_table[i_treatment, "Total QALYs (95% CrI)"] <- 
    #   summarise_samples(total_qalys[, i_treatment], n_digits = 2)
    # 
     threshold_table[i_treatment, "Net benefit at £25,000/QALY (95% CrI)"] <- 
       summarise_samples(total_qalys[, i_treatment] * 25000 -
                       total_costs[, i_treatment], n_digits = 2)
    # 
    #threshold_table[i_treatment, "Net benefit at £30,000/QALY (95% CrI)"] <- 
    #  summarise_samples(total_qalys[, i_treatment] * 30000 -
    #                  total_costs[, i_treatment], n_digits = 2)
    
    # threshold_table[i_treatment, "Incremental costs (95% CrI)"] <- 
    #   summarise_samples(total_costs[, i_treatment] -
    #                   total_costs[, reference], n_digits = 2)
    # 
    # threshold_table[i_treatment, "Incremental QALYs (95% CrI)"] <- 
    #   summarise_samples(total_qalys[, i_treatment] -
    #                   total_qalys[, reference], n_digits = 2)
    # 
    threshold_table[i_treatment, "INB at £25,000/QALY (95% CrI)"] <- 
      summarise_samples((total_qalys[, i_treatment] - total_qalys[, reference]) * 25000 -
                      (total_costs[, i_treatment] - total_costs[, reference]), n_digits = 2)
    
   # threshold_table[i_treatment, "INB at £30,000/QALY (95% CrI)"] <- 
  #    summarise_samples((total_qalys[, i_treatment] - total_qalys[, reference]) * 30000 -
   #                   (total_costs[, i_treatment] - total_costs[, reference]), n_digits = 2)
    # 
    # threshold_table[i_treatment, "CEAC at £25,000/QALY"] <- 
    #   sum(apply(net_benefit_25k, c(1), which.max) == i_treatment) /
    #   dim(net_benefit_25k)[1]
    
    threshold_table[i_treatment, "PINB>0 at £25,000/QALY"] <- 
      #summarise_samples(
        sum(
          ((total_qalys[, i_treatment] - total_qalys[, reference]) * 25000 -
                          (total_costs[, i_treatment] - total_costs[, reference]))
          > 0, na.rm = TRUE) /
          length(
            ((total_qalys[, i_treatment] - total_qalys[, reference]) * 25000 -
               (total_costs[, i_treatment] - total_costs[, reference]))
          )
        #, n_digits = 2)
    
   # threshold_table[i_treatment, "CEAC at £30,000/QALY"] <- 
   #   sum(apply(net_benefit_30k, c(1), which.max) == i_treatment) /
   #   dim(net_benefit_30k)[1]
  }
  
  return(threshold_table)
}
