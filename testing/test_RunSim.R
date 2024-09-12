# Tests of RunSim

library(doParallel)
library(data.table)

source("testing/test_RunEngine.R")
source("testing/test_engine_helper_f.R")

test_RunSim <- function (trt_list = c("int", "noint"), common_all_inputs = NULL, 
                         common_pt_inputs = NULL, unique_pt_inputs = NULL, init_event_list = NULL, 
                         evt_react_list = evt_react_list, util_ongoing_list = NULL, 
                         util_instant_list = NULL, util_cycle_list = NULL, cost_ongoing_list = NULL, 
                         cost_instant_list = NULL, cost_cycle_list = NULL, npats = 500, 
                         n_sim = 1, psa_bool = NULL, ncores = 1, drc = 0.035, drq = 0.035, 
                         input_out = NULL, ipd = TRUE, debug = FALSE) 
{
  # Omitted call to parallel while doing tests
  registerDoParallel(ncores)
  # Below is redundant?
  trt_list <- trt_list
  output_psa <- list()
  start_time_simulations <- proc.time()
  for (simulation in 1:n_sim) {
    print(paste0("Simulation number: ", simulation))
    start_time <- proc.time()
    input_list <- list(drc = drc, drq = drq, psa_bool = psa_bool, 
                       init_event_list = init_event_list, 
                       evt_react_list = evt_react_list, 
                       uc_lists = list(util_ongoing_list = util_ongoing_list, 
                                       util_instant_list = util_instant_list, 
                                       util_cycle_list = util_cycle_list, 
                                       cost_ongoing_list = cost_ongoing_list, 
                                       cost_instant_list = cost_instant_list, 
                                       cost_cycle_list = cost_cycle_list), 
                       input_out = input_out, 
                       ipd = ipd, trt_list = trt_list, simulation = simulation, 
                       npats = npats, n_sim = n_sim)
    if (!is.null(common_all_inputs)) {
    
      for (inp in 1:length(common_all_inputs)) {
        # Dangerous call below as means the simulation isn't reproducible
        set.seed(simulation)
        # But does run this fr all treatments and patients
        list.common_all_inputs <- lapply(common_all_inputs[inp], 
                                         function(x) eval(x, input_list))
        # At this point unclear why we don't want to name the lists
        if (!is.null(names(list.common_all_inputs[[1]]))) {
          warning("Item ", names(list.common_all_inputs), 
                  " is named. It is strongly advised to assign unnamed objects if they are going to be processed in the model, as they can create errors depending on how they are used within the model")
        }
        # Append the common inputs to the general input list
        input_list <- c(input_list, list.common_all_inputs)
      }
    }
    # Removes duplicated elements
    duplic <- duplicated(names(input_list), fromLast = T)
    if (sum(duplic) > 0) {
      warning("Duplicated items detected, using the last one added")
    }
    input_list <- input_list[!duplic]
    # The RunEngine functions are hidden within descem
    if (debug == TRUE) {
      final_output <- RunEngine_debug(trt_list = trt_list, 
                                      common_pt_inputs = common_pt_inputs, 
                                      unique_pt_inputs = unique_pt_inputs, 
                                      input_list = input_list)
    }
    else {
      final_output <- test_RunEngine(trt_list = trt_list, 
                                common_pt_inputs = common_pt_inputs, 
                                unique_pt_inputs = unique_pt_inputs, 
                                input_list = input_list)
    }
    # Creates a column with the simulation number
    # Unclear how this avoids the IPD being exported
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


# Test call 
test_RunSim(trt_list = c("int", "noint"), 
            common_all_inputs = add_item(treatment_names = c("int", "noint"),
                                         event_names = "stroke"), 
             common_pt_inputs = NULL, 
            unique_pt_inputs = NULL, 
            init_event_list = add_tte(trt = c("int", "noint"),
                                      evts = c("stroke", "initialise"),
                                      input = {
                                        initialise = 0
                                      stroke = 1
                                      }),
       evt_react_list = add_reactevt(name_evt = "initialise", 
                                     input = {}) %>%
         add_reactevt(name_evt = "stroke", 
                      input = {}), 
            util_ongoing_list = add_util(util = 0.4, evt = c("stroke"), trt = c("int", "noint")),
             util_instant_list = NULL, 
              util_cycle_list = NULL, 
            cost_ongoing_list = add_cost(cost = 5, evt = c("stroke"), trt = c("int", "noint")), 
             cost_instant_list = NULL, 
            cost_cycle_list = NULL, 
            npats = 1, 
             n_sim = 1, psa_bool = NULL, 
            ncores = 1, drc = 0.0, drq = 0.0, 
             input_out = NULL, ipd = TRUE, debug = FALSE)