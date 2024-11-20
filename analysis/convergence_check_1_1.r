# Script to check convergence of results

source("R/summarise_samples.R")

# Load results from previous run
input_matrix <- readRDS("results/input_matrix_17_nov.RDS")
total_costs_ipd <- readRDS("results/total_costs_ipd_17_nov.RDS")
total_qalys_ipd <- readRDS("results/total_qalys_ipd_17_nov.RDS")

convergence_tables <- array(dim = c(4, 4, 4),
                            dimnames = list(c("Total costs", "Total QALYs", 
                                            "Net benefit at £20,000/QALY", "Net benefit at £30,000/QALY"),
                                            c("100 patients", "250 patients", "500 patients", "1000 patients"),
                                            c("100 samples", "250 samples", "500 samples", "1000 samples")))

convergence_subsets <- c(100, 250, 500, 1000)
for(i_patients_con in c(1:4)) {
  for(i_samples_con in c(1:4)) {
    # Use a subset of the IPD 
    # Calculate the mean across patients aligning with how they are used for main results
    total_costs_temp <- rowMeans(total_costs_ipd[1:convergence_subsets[i_samples_con], "Natalizumab-IV", 1:convergence_subsets[i_patients_con]])
    total_qalys_temp <- rowMeans(total_qalys_ipd[1:convergence_subsets[i_samples_con], "Natalizumab-IV", 1:convergence_subsets[i_patients_con]])
    total_nb_20k_temp <- total_qalys_temp * 20000 - total_costs_temp
    total_nb_30k_temp <- total_qalys_temp * 30000 - total_costs_temp
    convergence_tables["Total costs", i_patients_con, i_samples_con] <- 
      summarise_samples(total_costs_temp)
    convergence_tables["Total QALYs", i_patients_con, i_samples_con] <- 
      summarise_samples(total_qalys_temp)
    convergence_tables["Net benefit at £20,000/QALY", i_patients_con, i_samples_con] <- 
      summarise_samples(total_nb_20k_temp)
    convergence_tables["Net benefit at £30,000/QALY", i_patients_con, i_samples_con] <- 
      summarise_samples(total_nb_30k_temp)
  }
}

# Put convergence checks in a single table
convergence_results <- rbind(convergence_tables["Total costs", , ],
                             convergence_tables["Total QALYs", , ])