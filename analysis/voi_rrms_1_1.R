# Script to perform value of information analysis on output
# of the DESCEM RRMS model
# Howard Thom 17-Nov-2024

# This script needs the total_costs, total_qalys, and input_matrix from
# descem_rrms_x_x.R 

library(voi)
library(BCEA)

# Load public results
total_qalys_ipd <- readRDS(file ="results/total_qalys_ipd_1000x1000_19_jan_public.RDS")
total_costs_ipd <- readRDS( file ="results/total_costs_ipd_1000x1000_19_jan_public.RDS")
input_matrix  <- readRDS(file ="results/input_matrix_1000x1000_19_jan_public.RDS")

# Load confidential results?
#total_qalys_ipd <- readRDS(file ="results/total_qalys_ipd_1000x1000_19_jan_conf.RDS")
#total_costs_ipd <- readRDS( file ="results/total_costs_ipd_1000x1000_19_jan_conf.RDS")
#input_matrix  <- readRDS(file ="results/input_matrix_1000x1000_19_jan_conf.RDS")

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

# Changing names to replacements used in main model
v_treatment_names[11:15] <- c("PegIFN-N2-1 SC 125N<g",
                              "IFNN2-1a SC 22N<g",       
                              "IFNN2-1a SC 44N<g",
                              "IFNN2-1a IM 30N<g",
                              "IFNN2-1b SC 250N<g")
  

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

nma_cdp6_names <- c(paste0("v_hr_edss_increase_", v_treatment_names))
nma_arr_names <- paste0("v_hr_arr_", v_treatment_names)

nma_discontinuation_names <- c(paste0("v_hr_discontinuation_", v_treatment_names))
nma_saes_names <- paste0("v_hr_saes_", v_treatment_names)

msregistry_edss_names <- c(paste0("v_baseline_harrms_edss_increase_", 
                                  c("rate", "EDSS","Natalizumab")),
                           paste0("v_rrms_edss_decrease_", 
                                  c("rate", "EDSS")),
                           paste0("v_spms_edss_increase_", 
                                  c("rate", "EDSS")),
                           paste0("v_spms_edss_decrease_", 
                                  c("rate", "EDSS")))

msregistry_progression_names <- paste0("v_spms_progression_", 
                                       c("rate", "EDSS","Natalizumab"))

msregistry_arr_names <- c(paste0("v_baseline_harrms_larr_", 
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
evppi_nma_cdp6 <- voi::evppi(outputs = list(c = total_costs,
                                            e = total_qalys,
                                            k = c(20000, 30000)),
                             inputs = df_inputs,
                             pars = nma_cdp6_names,
                             method = "bart")

evppi_nma_cdp6_alt <- voi::evppi(outputs = list(c = total_costs,
                                                e = total_qalys,
                                                k = c(20000, 30000)),
                                 inputs = df_inputs,
                                 pars = nma_cdp6_names,
                                 method = "earth")

evppi_nma_arr <- voi::evppi(outputs = list(c = total_costs,
                                           e = total_qalys,
                                           k = c(20000, 30000)),
                            inputs = df_inputs,
                            pars = nma_arr_names,
                            method = "bart")


evppi_nma_arr_alt <- voi::evppi(outputs = list(c = total_costs,
                                               e = total_qalys,
                                               k = c(20000, 30000)),
                                inputs = df_inputs,
                                pars = nma_arr_names,
                                method = "earth")

# EVPPI for NMA on safety
evppi_nma_saes <- voi::evppi(outputs = list(c = total_costs,
                                            e = total_qalys,
                                            k = c(20000, 30000)),
                             inputs = df_inputs,
                             pars = nma_saes_names,
                             method = "bart")

evppi_nma_discontinuation <- voi::evppi(outputs = list(c = total_costs,
                                                       e = total_qalys,
                                                       k = c(20000, 30000)),
                                        inputs = df_inputs,
                                        pars = nma_discontinuation_names,
                                        method = "bart")

# EVPPI for MS registry baseline estimates
evppi_msregistry_edss <- voi::evppi(outputs = list(c = total_costs,
                                                   e = total_qalys,
                                                   k = c(20000, 30000)),
                                    inputs = df_inputs,
                                    pars = msregistry_edss_names,
                                    method = "bart")

evppi_msregistry_progression <- voi::evppi(outputs = list(c = total_costs,
                                                          e = total_qalys,
                                                          k = c(20000, 30000)),
                                           inputs = df_inputs,
                                           pars = msregistry_progression_names,
                                           method = "bart")

evppi_msregistry_arr <- voi::evppi(outputs = list(c = total_costs,
                                                  e = total_qalys,
                                                  k = c(20000, 30000)),
                                   inputs = df_inputs,
                                   pars = msregistry_arr_names,
                                   method = "bart")

# Misc from AFFIRM
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
evppi_table <- rbind(total_evpi$evpi,
                     evppi_nma_cdp6$evppi,
                     evppi_nma_arr$evppi,
                     evppi_nma_saes$evppi,
                     evppi_nma_discontinuation$evppi,
                     evppi_costs$evppi,
                     evppi_utilities$evppi,
                     evppi_msregistry_edss$evppi,
                     evppi_msregistry_progression$evppi,
                     evppi_msregistry_arr$evppi,
                     evppi_discontinuation_rate$evppi,
                     evppi_sae_rate$evppi)
colnames(evppi_table) <- c("EVPPI at £20,000/QALY",
                           "EVPPI at £30,000/QALY")
rownames(evppi_table) <- c("Total EVPI",
                           "NMA on CDP6",
                           "NMA on ARR",
                           "NMA on SAEs",
                           "NMA on discontinuation",
                           "Costs",
                           "Utilities",
                           "MS registry EDSS increase/decrease",
                           "MS registry SPMS progression",
                           "MS registry ARR",
                           "Discontinuation rate",
                           "SAEs rate")

View(evppi_table)

write.csv(evppi_table, "results/evppi_table.csv")
