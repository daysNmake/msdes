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
                        v_random_seed_sequence = NULL) 
{
  registerDoParallel(ncores)
  
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
  return(results)
}