
# Script to THA

setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/")


# Load results from previous run

total_qalys_ipd <- readRDS("too large for git/results/results_conf/post ACM2/total_qalys_ipd_ACM2_CPA_ACU_THA5_conf.RDS")
total_costs_ipd <- readRDS("too large for git/results/results_conf/post ACM2/total_costs_ipd_ACM2_CPA_ACU_THA5_conf.RDS")

source("MS-MTA/R/summarise_tha_results.R")
source("MS-MTA/R/summarise_samples.R")

##theshold analysis
options(scipen = 999)
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)

#drop natalizumab-biosimilar-SC and cladrebiene
total_costs <- total_costs[,-c(4,5)]
total_qalys <- total_qalys[,-c(4,5)]

# reference treatment is all the three Natalizumabs and all the three compactors

## Treatments at second line 
v_treatment_names <- c("Natalizumab-IV","Natalizumab-SC","Natalizumab biosimilar-IV",
                       "Natalizumab biosimilar-SC","Cladribine","Ublituximab" ,
                       "Ofatumumab","Ocrelizumab" )
reference <- v_treatment_names[-c(4,5)]
acm2_th_dic <- 30 # (12, 13, 30, 31, 32)

for (i in 1:length(reference)) {
  
  threshold_table <- summarise_tha_results(total_qalys = total_qalys, 
                                           total_costs = total_costs,
                                           reference = reference[i])

  #setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo")
  write.csv(threshold_table, paste0("too large for git/results/results_conf/post ACM2/threshold_analysis_", reference[i], "_", acm2_th_dic,"%", "_.csv"))
 # setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/MS-MTA")
  
}
  