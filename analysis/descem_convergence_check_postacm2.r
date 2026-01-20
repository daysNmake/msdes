# Script to check convergence of results
setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/")


# Load results from previous run
# input_matrix <- readRDS("too large for git/results/results_public/post ACM2/input_matrix_ACM2_CPA_ACU_NAT_ALIGNED_public.RDS")
# total_qalys_ipd <- readRDS("too large for git/results/results_public/post ACM2/total_qalys_ipd_ACM2_CPA_ACU_NAT_ALIGNED_public.RDS")
# total_costs_ipd <- readRDS("too large for git/results/results_public/post ACM2/total_costs_ipd_ACM2_CPA_ACU_NAT_ALIGNED_public.RDS")

input_matrix <- readRDS("too large for git/results/results_conf/post ACM2/input_matrix_ipd_ACM2_CPA_ACU_conf.RDS")
total_qalys_ipd <- readRDS("too large for git/results/results_conf/post ACM2/total_qalys_ipd_ACM2_CPA_ACU_conf.RDS")
total_costs_ipd <- readRDS("too large for git/results/results_conf/post ACM2/total_costs_ipd_ACM2_CPA_ACU_conf.RDS")

source("MS-MTA/R/summarise_samples.R")
options(scipen = 999)
reference <- "Natalizumab-IV"
comparator <- names(total_costs_ipd[1,6,1])
convergence_tables <- array(dim = c(8, 4, 4),
                            dimnames = list(c(paste0("Total costs ", comparator), paste0("Total QALYs ", comparator), 
                                              paste0("NMB £20k/QALY ", comparator), paste0("NMB £30k/QALY ", comparator) , 
                                              paste0("Incremental costs ", comparator), paste0("Incremental QALYs ", comparator) ,
                                              paste0("INB £20k/QALY ", comparator), paste0("INB £30k/QALY ", comparator)
                                              ),
                                            c("100 patients", "250 patients", "500 patients", "1000 patients"),
                                            c("100 samples", "250 samples", "500 samples", "1000 samples")))

convergence_subsets <- c(100, 250, 500, 1000)

for(i_patients_con in c(1:4)) {
  for(i_samples_con in c(1:4)) {
    # Use a subset of the IPD 
    # Calculate the mean across patients aligning with how they are used for main results
    total_costs_ref_temp <- rowMeans(total_costs_ipd[1:convergence_subsets[i_samples_con], reference, 1:convergence_subsets[i_patients_con]])
    total_qalys_ref_temp <- rowMeans(total_qalys_ipd[1:convergence_subsets[i_samples_con], reference, 1:convergence_subsets[i_patients_con]])
    total_costs_comp_temp <- rowMeans(total_costs_ipd[1:convergence_subsets[i_samples_con], comparator, 1:convergence_subsets[i_patients_con]])
    total_qalys_comp_temp <- rowMeans(total_qalys_ipd[1:convergence_subsets[i_samples_con], comparator, 1:convergence_subsets[i_patients_con]])
    total_nb_ref_20k_temp <- total_qalys_ref_temp * 20000 - total_costs_ref_temp
    total_nb_ref_30k_temp <- total_qalys_ref_temp * 30000 - total_costs_ref_temp
    total_nb_comp_20k_temp <- total_qalys_comp_temp * 20000 - total_costs_comp_temp
    total_nb_comp_30k_temp <- total_qalys_comp_temp * 30000 - total_costs_comp_temp
    convergence_tables[paste0("Total costs ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples(total_costs_comp_temp)
    convergence_tables[paste0("Total QALYs ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples(total_qalys_comp_temp)
    convergence_tables[paste0("NMB £20k/QALY ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples(total_nb_comp_20k_temp)
    convergence_tables[paste0("NMB £30k/QALY ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples(total_nb_comp_30k_temp)
    convergence_tables[paste0("Incremental costs ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples(total_costs_comp_temp - total_costs_ref_temp, n_digits = 2)
    convergence_tables[paste0("Incremental QALYs ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples(total_qalys_comp_temp - total_qalys_ref_temp, n_digits = 2)
    convergence_tables[paste0("INB £20k/QALY ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples( (total_qalys_comp_temp - total_qalys_ref_temp) * 20000 -
        (total_costs_comp_temp - total_costs_ref_temp) , n_digits = )
    convergence_tables[paste0("INB £30k/QALY ", comparator), i_patients_con, i_samples_con] <- 
      summarise_samples( (total_qalys_comp_temp - total_qalys_ref_temp) * 30000 -
        (total_costs_comp_temp - total_costs_ref_temp) ,n_digits = 1)
  }
}

# Put convergence checks in a single table
convergence_results <- rbind(convergence_tables[paste0("Total costs ", comparator), , ],
                             convergence_tables[paste0("Total QALYs ", comparator), , ],
                             convergence_tables[paste0("NMB £20k/QALY ", comparator), , ],
                             convergence_tables[paste0("NMB £30k/QALY ", comparator), , ],
                             convergence_tables[paste0("Incremental costs ", comparator), , ],
                             convergence_tables[paste0("Incremental QALYs ", comparator), , ],
                             convergence_tables[paste0("INB £20k/QALY ", comparator), , ],
                             convergence_tables[paste0("INB £30k/QALY ", comparator), , ]
                             )

convergence_results <- cbind(row.names(convergence_results),convergence_results)
row.names(convergence_results)[1:4] <- paste0("Total costs ", comparator)
row.names(convergence_results)[5:8] <- paste0("Total QALYs ", comparator)
row.names(convergence_results)[9:12] <- paste0("NMB £20k/QALY ", comparator)
row.names(convergence_results)[13:16] <- paste0("NMB £30k/QALY ", comparator)
row.names(convergence_results)[17:20] <- paste0("Incremental costs ", comparator)
row.names(convergence_results)[21:24] <- paste0("Incremental QALYs ", comparator)
row.names(convergence_results)[25:28] <- paste0("INB £20k/QALY ", comparator)
row.names(convergence_results)[29:32] <- paste0("INB £30k/QALY ", comparator)

#View(convergence_results)

write.csv(convergence_results, file = paste0("too large for git/results/results_conf/post ACM2/convergencetable_cic_acm2_cpa_acu_", comparator, "_conf.csv"))
# write.csv(convergence_results, file = "too large for git/results/results_public/post ACM2/convergencetable_postacm2_public.csv")

