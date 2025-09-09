# Script to check convergence of results
setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/")


# Load results from previous run
# input_matrix <- readRDS("too large for git/results/results_public/nat qaly aligned 04 june 25/input_matrix_ACM2_CPA_ACU_NAT_ALIGNED_public.RDS")
# total_qalys_ipd <- readRDS("too large for git/results/results_public/nat qaly aligned 04 june 25/total_qalys_ipd_ACM2_CPA_ACU_NAT_ALIGNED_public.RDS")
# total_costs_ipd <- readRDS("too large for git/results/results_public/nat qaly aligned 04 june 25/total_costs_ipd_ACM2_CPA_ACU_NAT_ALIGNED_public.RDS")

input_matrix <- readRDS("too large for git/results/results_conf/nat qaly aligned 04 june 25/input_matrix_ipd_ACM2_CPA_ACU_NAT_ALIGNED_conf.RDS")
total_qalys_ipd <- readRDS("too large for git/results/results_conf/nat qaly aligned 04 june 25/total_qalys_ipd_ACM2_CPA_ACU_NAT_ALIGNED_conf.RDS")
total_costs_ipd <- readRDS("too large for git/results/results_conf/nat qaly aligned 04 june 25/total_costs_ipd_ACM2_CPA_ACU_NAT_ALIGNED_conf.RDS")

source("MS-MTA/R/summarise_samples.R")

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
    total_costs_temp <- rowMeans(total_costs_ipd[1:convergence_subsets[i_samples_con], "Natalizumab biosimilar-IV", 1:convergence_subsets[i_patients_con]])
    total_qalys_temp <- rowMeans(total_qalys_ipd[1:convergence_subsets[i_samples_con], "Natalizumab biosimilar-IV", 1:convergence_subsets[i_patients_con]])
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
                             convergence_tables["Total QALYs", , ],
                             convergence_tables["Net benefit at £20,000/QALY", , ],
                             convergence_tables["Net benefit at £30,000/QALY", , ])

convergence_results <- cbind(row.names(convergence_results),convergence_results)
row.names(convergence_results)[1:4] <- "Total costs"
row.names(convergence_results)[5:8] <- "Total QALYs"
row.names(convergence_results)[9:12] <- "Net benefit at £20,000/QALY"
row.names(convergence_results)[13:16] <- "Net benefit at £30,000/QALY"

write.csv(convergence_results, file = "too large for git/results/results_conf/nat qaly aligned 04 june 25/convergencetable_cic_nat_qaly_aligned_nat_biosim_iv_conf.csv")
# write.csv(convergence_results, file = "too large for git/results/results_public/post ACM2/convergencetable_postacm2_public.csv")

