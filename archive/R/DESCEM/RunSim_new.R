# Update of the RunSim function to allow users to specify the random seed
# This was necessary to run the PSA in batches on HPC facilities
# Howard Thom 17-Nov-2024

# Usage is same as RunSim but the v_random_seed_sequence can be provided
# and is a vector of random seeds with number of elements n_sim

require(doParallel)
require(data.table)


RunSim_new <- function (trt_list = c("int", "noint"), common_all_inputs = NULL, 
                        common_pt_inputs = NULL, unique_pt_inputs = NULL, init_event_list = NULL, 
                        evt_react_list = evt_react_list, util_ongoing_list = NULL, 
                        util_instant_list = NULL, util_cycle_list = NULL, cost_ongoing_list = NULL, 
                        cost_instant_list = NULL, cost_cycle_list = NULL, npats = 500, 
                        n_sim = 1, psa_bool = NULL, ncores = 1, drc = 0.035, drq = 0.035, 
                        input_out = NULL, ipd = TRUE, debug = FALSE,
                        v_random_seed_sequence = NULL,
                        save_input_matrix = FALSE) 
{
  registerDoParallel(ncores)
  
  # Create an input parameter matrix
  # Count up the number of random input parameters
  input_matrix <- matrix(nrow = n_sim,
                         ncol = 2 * 10 + 1 + 9 + # Utilities
                           2 * 10 + 6 * length(trt_list) + 6 + 9 + # Costs
                           4 * length(trt_list) + # NMA HRs
                           3 * 3 + 4 * 2 + # MS Registry
                           2) # Misc
  colnames(input_matrix) <- c(paste0("u_rrms_edss_", c(0:9)),
                              paste0("u_spms_edss_", c(0:9)),
                              "u_relapse",
                              paste0("u_sae_", c("Cholelithiasis", "Need for rehabilitation therapy",
                                                 "Urinary tract infection", "Depression",                     
                                                 "Anaphylactic reaction", "Hypersensitivity reaction",      
                                                 "Breast cancer", "Gastritis",                   
                                                 "PML")),
                              paste0("c_rrms_edss_", c(0:9)),
                              paste0("c_spms_edss_", c(0:9)),
                              paste0("admin_cost_annual_yr1_", trt_list),
                              paste0("admin_cost_annual_yr2_", trt_list),
                              paste0("admin_average_annual_cost_", trt_list),
                              paste0("monitor_cost_annual_yr1_", trt_list),
                              paste0("monitor_cost_annual_yr2_", trt_list),
                              paste0("monitor_average_annual_cost_", trt_list),
                              "c_relapse_none", "c_relapse_untreated",
                              "c_relapse_limit_daily_activity",
                              "c_relapse_oral_steroids",
                              "c_relapse_intravenous_steroids",
                              "c_relapse_hospital_admission",
                              paste0("c_sae_", c("Cholelithiasis", "Need for rehabilitation therapy",
                                                 "Urinary tract infection", "Depression",                     
                                                 "Anaphylactic reaction", "Hypersensitivity reaction",      
                                                 "Breast cancer", "Gastritis",                   
                                                 "PML")),
                              paste0("v_hr_edss_increase_", trt_list),
                              paste0("v_hr_arr_", trt_list),
                              paste0("v_hr_discontinuation_", trt_list),
                              paste0("v_hr_saes_", trt_list),
                              paste0("v_baseline_harrms_edss_increase_", 
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
                                     c("rate", "EDSS")),
                              "discontinuation_rate",
                              "sae_events_rate")
  
  # If no random seed sequence is provided set equal to index for simulations
  if(is.null(v_random_seed_sequence)) {
    v_random_seed_sequence <- 1:n_sim
  } else {
    if(length(v_random_seed_sequence) < n_sim) {
      stop("v_random_seed_sequence must have n_sim elements or be NULL")
    }
  }
  
  trt_list <- trt_list
  output_psa <- list()
  start_time_simulations <- proc.time()
  for (simulation in 1:n_sim) {
    print(paste0("Simulation number: ", simulation))
    start_time <- proc.time()
    input_list <- list(drc = drc, drq = drq, psa_bool = psa_bool, 
                       init_event_list = init_event_list, evt_react_list = evt_react_list, 
                       uc_lists = list(util_ongoing_list = util_ongoing_list, 
                                       util_instant_list = util_instant_list, util_cycle_list = util_cycle_list, 
                                       cost_ongoing_list = cost_ongoing_list, cost_instant_list = cost_instant_list, 
                                       cost_cycle_list = cost_cycle_list), input_out = input_out, 
                       ipd = ipd, trt_list = trt_list, simulation = simulation, 
                       npats = npats, n_sim = n_sim)
    if (!is.null(common_all_inputs)) {
      for (inp in 1:length(common_all_inputs)) {
        set.seed(v_random_seed_sequence[simulation])
        list.common_all_inputs <- lapply(common_all_inputs[inp], 
                                         function(x) eval(x, input_list))
        if (!is.null(names(list.common_all_inputs[[1]]))) {
          warning("Item ", names(list.common_all_inputs), 
                  " is named. It is strongly advised to assign unnamed objects if they are going to be processed in the model, as they can create errors depending on how they are used within the model")
        }
        input_list <- c(input_list, list.common_all_inputs)
      }
    }
    duplic <- duplicated(names(input_list), fromLast = T)
    if (sum(duplic) > 0) {
      warning("Duplicated items detected, using the last one added")
    }
    input_list <- input_list[!duplic]
    
    
    # Save the random parameter inputs for use in EVPPI

    # Utilities
    input_matrix[simulation, 
                 paste0("u_rrms_edss_", c(0:9))] <- input_list$v_u_rrms_edss
    input_matrix[simulation, 
                 paste0("u_spms_edss_", c(0:9))] <- input_list$v_u_spms_edss
    input_matrix[simulation,
                 "u_relapse"] <- input_list$u_relapse
    input_matrix[simulation,
                 paste0("u_sae_", c("Cholelithiasis", "Need for rehabilitation therapy",
                                    "Urinary tract infection", "Depression",                     
                                    "Anaphylactic reaction", "Hypersensitivity reaction",      
                                    "Breast cancer", "Gastritis",                   
                                    "PML"))] <- input_list$v_u_sae_events
    # Costs
    input_matrix[simulation, 
                 paste0("c_rrms_edss_", c(0:9))] <- input_list$v_c_rrms_edss
    input_matrix[simulation, 
                 paste0("c_spms_edss_", c(0:9))] <- input_list$v_c_spms_edss
    input_matrix[simulation, 
                 paste0("admin_cost_annual_yr1_", trt_list)] <- input_list$v_c_trt_admin["cost_annual_yr1", ]
    input_matrix[simulation, 
                 paste0("admin_cost_annual_yr2_", trt_list)] <- input_list$v_c_trt_admin["cost_annual_yr2", ]
    input_matrix[simulation, 
                 paste0("admin_average_annual_cost_", trt_list)] <- input_list$v_c_trt_admin["average_annual_cost", ]
    input_matrix[simulation, 
                 paste0("monitor_cost_annual_yr1_", trt_list)] <- input_list$v_c_trt_monitor["cost_annual_yr1", ]
    input_matrix[simulation, 
                 paste0("monitor_cost_annual_yr2_", trt_list)] <- input_list$v_c_trt_monitor["cost_annual_yr2", ]
    input_matrix[simulation, 
                 paste0("monitor_average_annual_cost_", trt_list)] <- input_list$v_c_trt_monitor["average_annual_cost", ]
    input_matrix[simulation,
                 c("c_relapse_none", "c_relapse_untreated",
                   "c_relapse_limit_daily_activity",
                   "c_relapse_oral_steroids",
                   "c_relapse_intravenous_steroids",
                   "c_relapse_hospital_admission")] <- input_list$v_c_relapse
    input_matrix[simulation,
                 paste0("c_sae_", c("Cholelithiasis", "Need for rehabilitation therapy",
                                    "Urinary tract infection", "Depression",                     
                                    "Anaphylactic reaction", "Hypersensitivity reaction",      
                                    "Breast cancer", "Gastritis",                   
                                    "PML"))] <- input_list$v_c_sae_events
    
    # NMA hazard ratios
    input_matrix[simulation, 
                 paste0("v_hr_edss_increase_", trt_list)] <- input_list$v_hr_edss_increase
    input_matrix[simulation, 
                 paste0("v_hr_arr_", trt_list)] <- input_list$v_hr_arr
    input_matrix[simulation, 
                 paste0("v_hr_discontinuation_", trt_list)] <- input_list$v_hr_discontinuation
    input_matrix[simulation, 
                 paste0("v_hr_saes_", trt_list)] <- input_list$v_hr_saes
    
    
    # Rates from the MS Registry
    input_matrix[simulation,
                 paste0("v_baseline_harrms_edss_increase_", 
                        c("rate", "EDSS","Natalizumab"))] <- input_list$v_baseline_harrms_edss_increase
    input_matrix[simulation,
                 paste0("v_rrms_edss_decrease_", 
                        c("rate", "EDSS"))] <- input_list$v_rrms_edss_decrease
    input_matrix[simulation,
                 paste0("v_spms_edss_increase_", 
                        c("rate", "EDSS"))] <- input_list$v_spms_edss_increase
    input_matrix[simulation,
                 paste0("v_spms_edss_decrease_", 
                        c("rate", "EDSS"))] <- input_list$v_spms_edss_decrease
    input_matrix[simulation,
                 paste0("v_spms_progression_", 
                        c("rate", "EDSS","Natalizumab"))] <- input_list$v_spms_progression
    input_matrix[simulation,
                 paste0("v_baseline_harrms_larr_", 
                        c("rate", "EDSS","Natalizumab"))] <- input_list$v_baseline_harrms_larr
    input_matrix[simulation,
                 paste0("v_baseline_spms_larr_", 
                        c("rate", "EDSS"))] <- input_list$v_baseline_spms_larr
    
    # Discontinuation and SAE rates from AFFIRM
    input_matrix[simulation, "discontinuation_rate"] <- input_list$discontinuation_rate
    
    input_matrix[simulation, "sae_events_rate"] <- input_list$sae_events_rate
    
    
    if (debug == TRUE) {
      final_output <- descem:::RunEngine_debug(trt_list = trt_list, 
                                      common_pt_inputs = common_pt_inputs, unique_pt_inputs = unique_pt_inputs, 
                                      input_list = input_list)
    }
    else {
      final_output <- descem:::RunEngine(trt_list = trt_list, common_pt_inputs = common_pt_inputs, 
                                unique_pt_inputs = unique_pt_inputs, input_list = input_list)
    }
    if (input_list$ipd == TRUE) {
      final_output$merged_df$simulation <- simulation
    }
    output_psa[[simulation]] <- final_output
    print(paste0("Time to run iteration ", simulation, ": ", 
                 round(proc.time()[3] - start_time[3], 2), "s"))
  }
  print(paste0("Total time to run: ", round(proc.time()[3] - 
                                              start_time_simulations[3], 2), "s"))
  results <- list(final_output = final_output, output_psa = output_psa)
  if(save_input_matrix) results$input_matrix <- input_matrix
  return(results)
}