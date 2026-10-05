# Script to perform value of information analysis on output
# of the DESCEM RRMS model
# Howard Thom 17-Nov-2024

# This script needs the total_costs, total_qalys, and input_matrix from
# descem_rrms_x_x.R 

library(voi)




# The VOI package uses data frames
df_inputs <- as.data.frame(results[["input_matrix"]])

total_evpi <- voi::evpi(outputs = list(c = total_costs,
                         e = total_qalys,
                         k = c(20000, 30000)))



# All  parameters
total_evppi <- voi::evppi(outputs = list(c = total_costs,
                                             e = total_qalys,
                                             k = c(20000, 30000)),
                              inputs = df_inputs,
                              pars = colnames(df_inputs),
                              method = "bart")

View(total_evppi)

# # All cost parameters
# evppi_costs <- voi::evppi(outputs = list(c = total_costs,
#                                          e = total_qalys,
#                                          k = c(20000, 30000)),
#                           inputs = df_inputs,
#                           pars = cost_parameter_names,
#                           method = "bart")
# 
# 
# # EVPPI for NMA on efficacy
# evppi_nma_cdp6 <- voi::evppi(outputs = list(c = total_costs,
#                                             e = total_qalys,
#                                             k = c(20000, 30000)),
#                              inputs = df_inputs,
#                              pars = nma_cdp6_names,
#                              method = "bart")
# 
# evppi_nma_cdp6_alt <- voi::evppi(outputs = list(c = total_costs,
#                                                 e = total_qalys,
#                                                 k = c(20000, 30000)),
#                                  inputs = df_inputs,
#                                  pars = nma_cdp6_names,
#                                  method = "earth")
# 
# evppi_nma_arr <- voi::evppi(outputs = list(c = total_costs,
#                                            e = total_qalys,
#                                            k = c(20000, 30000)),
#                             inputs = df_inputs,
#                             pars = nma_arr_names,
#                             method = "bart")
# 
# 
# evppi_nma_arr_alt <- voi::evppi(outputs = list(c = total_costs,
#                                                e = total_qalys,
#                                                k = c(20000, 30000)),
#                                 inputs = df_inputs,
#                                 pars = nma_arr_names,
#                                 method = "earth")
# 
# # EVPPI for NMA on safety
# evppi_nma_saes <- voi::evppi(outputs = list(c = total_costs,
#                                             e = total_qalys,
#                                             k = c(20000, 30000)),
#                              inputs = df_inputs,
#                              pars = nma_saes_names,
#                              method = "bart")
# 
# evppi_nma_discontinuation <- voi::evppi(outputs = list(c = total_costs,
#                                                        e = total_qalys,
#                                                        k = c(20000, 30000)),
#                                         inputs = df_inputs,
#                                         pars = nma_discontinuation_names,
#                                         method = "bart")
# 
# # EVPPI for MS registry baseline estimates
# evppi_msregistry_edss <- voi::evppi(outputs = list(c = total_costs,
#                                                    e = total_qalys,
#                                                    k = c(20000, 30000)),
#                                     inputs = df_inputs,
#                                     pars = msregistry_edss_names,
#                                     method = "bart")
# 
# evppi_msregistry_progression <- voi::evppi(outputs = list(c = total_costs,
#                                                           e = total_qalys,
#                                                           k = c(20000, 30000)),
#                                            inputs = df_inputs,
#                                            pars = msregistry_progression_names,
#                                            method = "bart")
# 
# evppi_msregistry_arr <- voi::evppi(outputs = list(c = total_costs,
#                                                   e = total_qalys,
#                                                   k = c(20000, 30000)),
#                                    inputs = df_inputs,
#                                    pars = msregistry_arr_names,
#                                    method = "bart")
# 
# # Misc from AFFIRM
# evppi_discontinuation_rate <- voi::evppi(outputs = list(c = total_costs,
#                                                         e = total_qalys,
#                                                         k = c(20000, 30000)),
#                                          inputs = df_inputs,
#                                          pars = "discontinuation_rate",
#                                          method = "gam")
# 
# evppi_sae_rate <- voi::evppi(outputs = list(c = total_costs,
#                                             e = total_qalys,
#                                             k = c(20000, 30000)),
#                              inputs = df_inputs,
#                              pars = "sae_events_rate",
#                              method = "gam")
# 
# 
# # Create a tabular summary
# evppi_table <- rbind(total_evpi$evpi,
#                      evppi_nma_cdp6$evppi,
#                      evppi_nma_arr$evppi,
#                      evppi_nma_saes$evppi,
#                      evppi_nma_discontinuation$evppi,
#                      evppi_costs$evppi,
#                      evppi_utilities$evppi,
#                      evppi_msregistry_edss$evppi,
#                      evppi_msregistry_progression$evppi,
#                      evppi_msregistry_arr$evppi,
#                      evppi_discontinuation_rate$evppi,
#                      evppi_sae_rate$evppi)
# colnames(evppi_table) <- c("EVPPI at £20,000/QALY",
#                            "EVPPI at £30,000/QALY")
# rownames(evppi_table) <- c("Total EVPI",
#                            "NMA on CDP6",
#                            "NMA on ARR",
#                            "NMA on SAEs",
#                            "NMA on discontinuation",
#                            "Costs",
#                            "Utilities",
#                            "MS registry EDSS increase/decrease",
#                            "MS registry SPMS progression",
#                            "MS registry ARR",
#                            "Discontinuation rate",
#                            "SAEs rate")
# 
# View(evppi_table)
# 
# write.csv(evppi_table, "results/evppi_table.csv")
