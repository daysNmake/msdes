# Script to perform value of information analysis on output
# of the DESCEM RRMS model
# Howard Thom 17-Nov-2024

# This script needs the total_costs, total_qalys, and input_matrix from
# descem_rrms_x_x.R 

library(voi)
library(BCEA)

total_qalys_ipd <- readRDS(file ="results/total_qalys_ipd_100x100_test_16_Jan_2025.RDS")
total_costs_ipd <- readRDS( file ="results/total_costs_ipd__100x100_test_16_Jan_2025.RDS")
input_matrix  <- readRDS(file ="results/input_matrix_100x100_test_16_Jan_2025.RDS")

## Treatments at second line 
v_treatment_names <- c("Natalizumab-IV",
                       "Natalizumab-SC",
                       "Natalizumab biosimilar-IV",
                       "Natalizumab biosimilar-SC",
                       "Fingolimod",
                       "Alemtuzumab",
                       "Cladribine", 
                       "Ponesimod" ,
                       "Ofatumumab",
                       "Ocrelizumab",
                       "PegIFN-β-1 SC 125μg", 
                       "IFNβ-1a SC 22μg" , 
                       "IFNβ-1a SC 44μg" ,
                       "IFNβ-1a IM 30μg", 
                       "IFNβ-1b SC 250μg", 
                       "Glatiramer Acetate 20mg",
                       "Glatiramer Acetate 40mg")


# Take mean over patients to remove individual variation
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)


# The VOI package uses data frames
df_inputs <- as.data.frame(input_matrix)

total_evpi <- voi::evpi(outputs = list(c = total_costs,
                         e = total_qalys,
                         k = c(20000, 30000)))


utility_parameter_names <- c(
  paste0("u_rrms_edss_", c(0:9)),
  paste0("u_spms_edss_", c(0:9)),
  "u_relapse",
  paste0("u_sae_", c("Cholelithiasis", "Need for rehabilitation therapy",
                     "Urinary tract infection", "Depression",                     
                     "Anaphylactic reaction", "Hypersensitivity reaction",      
                     "Breast cancer", "Gastritis",                   
                     "PML")))

cost_parameter_names <- c(paste0("c_rrms_edss_", c(0:9)),
                          paste0("c_spms_edss_", c(0:9)),
                          paste0("admin_cost_annual_yr1_", v_treatment_names),
                          paste0("admin_cost_annual_yr2_", v_treatment_names),
                          paste0("admin_average_annual_cost_", v_treatment_names),
                          paste0("monitor_cost_annual_yr1_", v_treatment_names),
                          paste0("monitor_cost_annual_yr2_", v_treatment_names),
                          paste0("monitor_average_annual_cost_", v_treatment_names),
                          "c_relapse_none", "c_relapse_untreated",
                          "c_relapse_limit_daily_activity",
                          "c_relapse_oral_steroids",
                          "c_relapse_intravenous_steroids",
                          "c_relapse_hospital_admission",
                          paste0("c_sae_", c("Cholelithiasis", "Need for rehabilitation therapy",
                                             "Urinary tract infection", "Depression",                     
                                             "Anaphylactic reaction", "Hypersensitivity reaction",      
                                             "Breast cancer", "Gastritis",                   
                                             "PML")))

nma_efficacy_parameter_names <- c(paste0("v_hr_edss_increase_", v_treatment_names),
                         paste0("v_hr_arr_", v_treatment_names))

nma_safety_parameter_names <-c(paste0("v_hr_discontinuation_", v_treatment_names),
                               paste0("v_hr_saes_", v_treatment_names))

msregistry_baseline_parameter_names <- c(paste0("v_baseline_harrms_edss_increase_", 
                                     c("rate", "EDSS","Natalizumab")),
                              paste0("v_rrms_edss_decrease_", 
                                     c("rate", "EDSS")),
                              paste0("v_spms_edss_increase_", 
                                     c("rate", "EDSS")),
                              paste0("v_spms_edss_decrease_", 
                                     c("rate", "EDSS")),
                              paste0("v_spms_progression_", 
                                     c("rate", "EDSS","Natalizumab")),
                              paste0("v_baseline_harrms_larr_", 
                                     c("rate", "EDSS","Natalizumab")),
                              paste0("v_baseline_spms_larr_", 
                                     c("rate", "EDSS")))
                         

# All utility parameters
evppi_utilities <- voi::evppi(outputs = list(c = total_costs,
                                            e = total_qalys,
                                            k = c(20000, 30000)),
                             inputs = df_inputs,
                             pars = utility_parameter_names,
                             method = "bart")

# All cost parameters
evppi_costs <- voi::evppi(outputs = list(c = total_costs,
                                             e = total_qalys,
                                             k = c(20000, 30000)),
                              inputs = df_inputs,
                              pars = cost_parameter_names,
                              method = "bart")

# EVPPI for NMA on efficacy
evppi_nma_efficacy <- voi::evppi(outputs = list(c = total_costs,
                                         e = total_qalys,
                                         k = c(20000, 30000)),
                          inputs = df_inputs,
                          pars = nma_efficacy_parameter_names,
                          method = "bart")

# EVPPI for NMA on safety
evppi_nma_safety <- voi::evppi(outputs = list(c = total_costs,
                                                e = total_qalys,
                                                k = c(20000, 30000)),
                                 inputs = df_inputs,
                                 pars = nma_safety_parameter_names,
                                 method = "bart")

# EVPPI for MS registry baseline estimates
evppi_baseline <- voi::evppi(outputs = list(c = total_costs,
                                              e = total_qalys,
                                              k = c(20000, 30000)),
                               inputs = df_inputs,
                               pars = msregistry_baseline_parameter_names,
                               method = "bart")

evppi_discontinuation_rate <- voi::evppi(outputs = list(c = total_costs,
                                                        e = total_qalys,
                                                        k = c(20000, 30000)),
                                         inputs = df_inputs,
                                         pars = "discontinuation_rate",
                                         method = "gam")

evppi_sae_rate <- voi::evppi(outputs = list(c = total_costs,
                                            e = total_qalys,
                                            k = c(20000, 30000)),
                             inputs = df_inputs,
                             pars = "sae_events_rate",
                             method = "gam")


# Create a tabular summary
evppi_table <- cbind(total_evpi$evpi,
                     evppi_nma_efficacy$evppi,
                     evppi_nma_safety$evppi,
                     evppi_costs$evppi,
                     evppi_utilities$evppi,
                     evppi_baseline$evppi,
                     evppi_discontinuation_rate$evppi,
                     evppi_sae_rate$evppi)
rownames(evppi_table) <- c("EVPPI at £20,000/QALY",
                           "EVPPI at £30,000/QALY")
colnames(evppi_table) <- c("Total EVPI",
                           "NMA efficacy",
                           "NMA safety",
                           "Costs",
                           "Utilities",
                           "MS registry baseline",
                           "Discontinuation rate",
                           "SAEs rate")

View(evppi_table)

write.csv(evppi_table, "results/evppi_table.csv")
