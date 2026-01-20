
# Script to THA

setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo")


# Load results from previous run

total_qalys_ipd <- readRDS("too large for git/results/results_conf/nat qaly aligned 04 june 25/total_qalys_ipd_ACM2_CPA_ACU_NAT_ALIGNED_THA4_conf.RDS")
total_costs_ipd <- readRDS("too large for git/results/results_conf/nat qaly aligned 04 june 25/total_costs_ipd_ACM2_CPA_ACU_NAT_ALIGNED_THA4_conf.RDS")

source("MS-MTA/R/summarise_tha_results.R")
source("MS-MTA/R/summarise_samples.R")

##theshold analysis
options(scipen = 999)
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)

#drop natalizumab-biosimilar-SC and cladrebiene
# total_costs <- total_costs[,-c(4,5)]
# total_qalys <- total_qalys[,-c(4,5)]

# reference treatment is all the three Natalizumabs and all the three compactors

## corrected for new model Treatments at second line 
v_treatment_names <- c("Natalizumab-IV",
                       "Cladribine","Ublituximab" ,
                       "Ofatumumab","Ocrelizumab","Natalizumab-SC","Natalizumab biosimilar-IV" )
reference <- v_treatment_names
acm2_th_dic <- 50 # (30, 35, 40, 45, )

for (i in 1:length(reference)) {
  
  threshold_table <- summarise_tha_results(total_qalys = total_qalys, 
                                           total_costs = total_costs,
                                           reference = reference[i])

  #setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo")
  write.csv(threshold_table, paste0("too large for git/results/results_conf/nat qaly aligned 04 june 25/threshold_analysis_", reference[i], "_", acm2_th_dic,"%", "_.csv"))
 # setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/MS-MTA")
  
}
  