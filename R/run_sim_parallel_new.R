#'
#' @description
#' modified run_sim_parallel() function 
#' 
#' @details
#' 'this runs the model to allow for extra functionality
#' 
#' @param x data to be supplied
#' @export
run_sim_parallel_new<- function (arm_list = c("int", "noint"), sensitivity_inputs = NULL, 
          common_all_inputs = NULL, common_pt_inputs = NULL, unique_pt_inputs = NULL, 
          init_event_list = NULL, evt_react_list = evt_react_list, 
          util_ongoing_list = NULL, util_instant_list = NULL, util_cycle_list = NULL, 
          cost_ongoing_list = NULL, cost_instant_list = NULL, cost_cycle_list = NULL, 
          other_ongoing_list = NULL, other_instant_list = NULL, npats = 500, 
          n_sim = 1, psa_bool = NULL, sensitivity_bool = FALSE, sensitivity_names = NULL, 
          n_sensitivity = 1, ncores = 1, input_out = character(), 
          ipd = 1, constrained = FALSE, timed_freq = NULL, debug = FALSE, 
          accum_backwards = FALSE, continue_on_error = FALSE, seed = NULL,
          save_input_matrix = FALSE) 
{
  .warden_ctx <- new.env(parent = emptyenv())
  .warden_ctx$last <- NULL
  .set_last_ctx(stage = "Error in setup:initial code setup", 
                .warden_ctx = .warden_ctx)
  log_sink <- new.env(parent = emptyenv())
  log_sink$entries <- list()
  on.exit({
    do.call(RNGkind, as.list(rng_kind_store))
    if (debug) export_log(log_sink$entries, paste0("log_model_", 
                                                   format(Sys.time(), "%Y_%m_%d_%Hh_%mm_%Ss"), ".txt"))
    ctx <- .warden_ctx$last
    message(paste0(ctx$stage, "; ", ifelse(!is.na(ctx$sens), 
                                           paste0("Analysis ", ctx$sens, "; "), ""), ifelse(!is.na(ctx$simulation), 
                                                                                            paste0("Simulation ", ctx$simulation, "; "), ""), 
                   ifelse(!is.na(ctx$arm), paste0("Arm ", ctx$arm, 
                                                  "; "), ""), ifelse(!is.na(ctx$patient_id), paste0("Patient ", 
                                                                                                    ctx$patient_id, "; "), ""), ifelse(!is.na(ctx$event), 
                                                                                                                                       paste0("Event ", ctx$event, "; "), ""), ifelse(!is.na(ctx$time), 
                                                                                                                                                                                      paste0("Time ", ctx$time, "; "), "")))
    if (ctx$stage != "Simulation finalized") {
      message("An error occurred somewhere. Traceback available through traceback()")
    }
  }, add = TRUE)
  rng_kind_store <- RNGkind()
  RNGkind("L'Ecuyer-CMRG")
  list_forbidden_names <- c("arm", "arm_list", "categories_for_export", 
                            "cur_evtlist", "curtime", "evt", "i", "prevtime", "sens", 
                            "simulation", "sens_name_used", "list_env", "uc_lists", 
                            "npats", "ipd")
  matched_list_forbidden <- which(list_forbidden_names %in% 
                                    c(ls(parent.frame()), names(common_all_inputs), names(sensitivity_inputs), 
                                      names(common_pt_inputs), names(unique_pt_inputs), 
                                      names(arm_list), names(evt_react_list)))
  if (length(matched_list_forbidden) > 0) {
    stop(paste0("Name(s) or object: `", list_forbidden_names[matched_list_forbidden], 
                "` defined belong to the list of forbidden names, which can cause issues in the model.\n    This is likely caused by an object being declared in the parent/global environment (e.g., `i`) or in the inputs. \n    You can use functions like find('i') to help you locate them. \n    Please remove or rename those objects. See run_sim or run_sim_parallel for a full list of these names.\n"))
  }
  if (length(names(evt_react_list)) != length(init_event_list[[1]][["evts"]])) {
    dif <- setdiff(names(evt_react_list), init_event_list[[1]][["evts"]])
    stop(paste0("Number of events defined in init_event_list first sublist `evts` is not equal to the number of events defined in evt_react_list.\n         Please ensure both have equal length, ", 
                dif, " is/are missing from one of the lists."))
  }
  if (anyDuplicated(names(evt_react_list)) > 0L) {
    dupes <- names(evt_react_list)[duplicated(names(evt_react_list))]
    stop("Duplicate reaction(s) found for event(s): ", paste(dupes, 
                                                             collapse = ", "), ". Make sure that only one reaction has been defined for each event")
  }
  plan(multisession, workers = ncores)
  arm_list <- arm_list
  categories_costs_ongoing <- cost_ongoing_list
  categories_costs_instant <- cost_instant_list
  categories_costs_cycle <- cost_cycle_list
  categories_utilities_ongoing <- util_ongoing_list
  categories_utilities_instant <- util_instant_list
  categories_utilities_cycle <- util_cycle_list
  categories_other_ongoing <- other_ongoing_list
  categories_other_instant <- other_instant_list
  
  # Create an input parameter matrix
  # Count up the number of random input parameters
  input_matrix <- matrix(nrow = n_sim,
                         ncol = 2 * 10 + 1 + 9 + # Utilities
                           2 * 10 + 6 * length(arm_list) + 6 + 9 + # Costs
                           4 * length(arm_list) + # NMA HRs
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
                              paste0("admin_cost_annual_yr1_", arm_list),
                              paste0("admin_cost_annual_yr2_", arm_list),
                              paste0("admin_average_annual_cost_", arm_list),
                              paste0("monitor_cost_annual_yr1_", arm_list),
                              paste0("monitor_cost_annual_yr2_", arm_list),
                              paste0("monitor_average_annual_cost_", arm_list),
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
                              paste0("v_hr_edss_increase_", arm_list),
                              paste0("v_hr_arr_", arm_list),
                              paste0("v_hr_discontinuation_", arm_list),
                              paste0("v_hr_saes_", arm_list),
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
  
  categories_for_export <- unique(c(if (!is.null(categories_costs_ongoing)) {
    categories_costs_ongoing
  }, if (!is.null(categories_costs_instant)) {
    categories_costs_instant
  }, if (!is.null(categories_costs_cycle)) {
    c(categories_costs_cycle, paste0(categories_costs_cycle, 
                                     "_cycle_l"), paste0(categories_costs_cycle, "_cycle_starttime"), 
      paste0(categories_costs_cycle, "_max_cycles"))
  }, if (!is.null(categories_utilities_ongoing)) {
    categories_utilities_ongoing
  }, if (!is.null(categories_utilities_instant)) {
    categories_utilities_instant
  }, if (!is.null(categories_utilities_cycle)) {
    c(categories_utilities_cycle, paste0(categories_utilities_cycle, 
                                         "_cycle_l"), paste0(categories_utilities_cycle, 
                                                             "_cycle_starttime"), paste0(categories_utilities_cycle, 
                                                                                         "_max_cycles"))
  }, if (!is.null(categories_other_ongoing)) {
    categories_other_ongoing
  }, if (!is.null(categories_other_instant)) {
    categories_other_instant
  }))
  output_sim <- list()
  final_log <- list()
  log_list <- list()
  .skip_to_next <- FALSE
  flag_noerr_sim <- TRUE
  start_time <- proc.time()
  all_inputs_args <- list(sensitivity_inputs, common_all_inputs, 
                          common_pt_inputs, unique_pt_inputs)
  .all_dsa_names <- NULL
  .meta_found <- FALSE
  .n_sensitivity_auto <- 0L
  .n_before_offsets <- integer(4L)
  .running_n <- 0L
  for (.k in seq_along(all_inputs_args)) {
    .n_before_offsets[.k] <- .running_n
    .meta_k <- attr(all_inputs_args[[.k]], "warden_block_meta")
    if (!is.null(.meta_k) && !is.null(.meta_k$dsa_names)) {
      if (isTRUE(sensitivity_bool)) {
        .meta_found <- TRUE
        .all_dsa_names <- union(.all_dsa_names, .meta_k$dsa_names)
        .n_sensitivity_auto <- .n_sensitivity_auto + 
          .meta_k$n_groups
      }
      .running_n <- .running_n + .meta_k$n_groups
    }
  }
  if (isTRUE(sensitivity_bool) && .n_sensitivity_auto > 0L) {
    n_sensitivity <- .n_sensitivity_auto
    message("n_sensitivity auto-detected as ", n_sensitivity, 
            " from input_block metadata")
  }
  if (.meta_found && is.null(sensitivity_names) && !is.null(.all_dsa_names)) {
    sensitivity_names <- .all_dsa_names
    message("sensitivity_names auto-set to c(", paste0("\"", 
                                                       sensitivity_names, "\"", collapse = ", "), ") from input_block metadata")
  }
  if (!is.null(sensitivity_names) && !is.null(.all_dsa_names)) {
    .lens <- ifelse(sensitivity_names %in% .all_dsa_names, 
                    n_sensitivity, 1L)
    .sched_name <- rep(sensitivity_names, .lens)
    .sched_iter <- unlist(lapply(.lens, seq_len))
    length_sensitivities <- length(.sched_name)
  }
  else if (.meta_found && !is.null(sensitivity_names)) {
    .sched_name <- sensitivity_names
    .sched_iter <- rep(1L, length(sensitivity_names))
    length_sensitivities <- length(sensitivity_names)
  }
  else {
    .sched_name <- NULL
    .sched_iter <- NULL
    if (is.null(sensitivity_names)) {
      length_sensitivities <- n_sensitivity
    }
    else {
      length_sensitivities <- n_sensitivity * length(sensitivity_names)
    }
  }
  progressr::handlers(progressr::handler_txtprogressbar(width = 100))
  options(progressr.interrupts = FALSE)
  progressr::with_progress({
    pb <- progressr::progressor(min(npats * length_sensitivities * 
                                      n_sim, 50))
    for (sens in 1:length_sensitivities) {
      .skip_to_next <- FALSE
      message(paste0("Analysis number: ", sens))
      start_time_analysis <- proc.time()
      output_sim[[sens]] <- list()
      if (!is.null(.sched_name)) {
        sens_name_used <- .sched_name[sens]
        sens_iter_local <- .sched_iter[sens]
      }
      else if (!is.null(sensitivity_names)) {
        sens_name_used <- sensitivity_names[ceiling(sens/n_sensitivity)]
        sens_iter_local <- sens_iterator(sens, n_sensitivity)
      }
      else {
        sens_name_used <- ""
        sens_iter_local <- sens_iterator(sens, n_sensitivity)
      }
      if (debug) {
        for (evt in names(evt_react_list)) {
          evt_react_list[[evt]]$debug_vars <- extract_assignment_targets(evt_react_list[[evt]]$react)
        }
      }
      input_list_sens <- list(psa_bool = psa_bool, init_event_list = init_event_list, 
                              precision_times = if (!is.null(init_event_list)) {
                                setNames((seq_along(init_event_list[[1]][["evts"]]) - 
                                            1) * 0.0000000001, init_event_list[[1]][["evts"]])
                              }, evt_react_list = evt_react_list, uc_lists = list(util_ongoing_list = util_ongoing_list, 
                                                                                  util_instant_list = util_instant_list, util_cycle_list = util_cycle_list, 
                                                                                  cost_ongoing_list = cost_ongoing_list, cost_instant_list = cost_instant_list, 
                                                                                  cost_cycle_list = cost_cycle_list, other_ongoing_list = other_ongoing_list, 
                                                                                  other_instant_list = other_instant_list, ongoing_inputs = c(if (!is.null(categories_costs_ongoing)) {
                                                                                    categories_costs_ongoing
                                                                                  }, if (!is.null(categories_utilities_ongoing)) {
                                                                                    categories_utilities_ongoing
                                                                                  }, if (!is.null(categories_other_ongoing)) {
                                                                                    categories_other_ongoing
                                                                                  }), instant_inputs = c(if (!is.null(categories_costs_instant)) {
                                                                                    categories_costs_instant
                                                                                  }, if (!is.null(categories_utilities_instant)) {
                                                                                    categories_utilities_instant
                                                                                  }, if (!is.null(categories_other_instant)) {
                                                                                    categories_other_instant
                                                                                  }), cycle_inputs = c(if (!is.null(categories_costs_cycle)) {
                                                                                    categories_costs_cycle
                                                                                  }, if (!is.null(categories_utilities_cycle)) {
                                                                                    categories_utilities_cycle
                                                                                  }), cost_categories_ongoing = categories_costs_ongoing, 
                                                                                  l_cost_categories_ongoing = length(categories_costs_ongoing), 
                                                                                  cost_categories_instant = categories_costs_instant, 
                                                                                  l_cost_categories_instant = length(categories_costs_instant), 
                                                                                  cost_categories_cycle = categories_costs_cycle, 
                                                                                  l_cost_categories_cycle = length(categories_costs_cycle), 
                                                                                  util_categories_ongoing = categories_utilities_ongoing, 
                                                                                  l_util_categories_ongoing = length(categories_utilities_ongoing), 
                                                                                  util_categories_instant = categories_utilities_instant, 
                                                                                  l_util_categories_instant = length(categories_utilities_instant), 
                                                                                  util_categories_cycle = categories_utilities_cycle, 
                                                                                  l_util_categories_cycle = length(categories_utilities_cycle), 
                                                                                  other_categories_ongoing = categories_other_ongoing, 
                                                                                  l_other_categories_ongoing = length(categories_other_ongoing), 
                                                                                  other_categories_instant = categories_other_instant, 
                                                                                  l_other_categories_instant = length(categories_other_instant)), 
                              input_out = unique(c(input_out, categories_for_export)), 
                              categories_for_export = categories_for_export, 
                              ipd = ipd, arm_list = arm_list, npats = npats, 
                              n_sim = n_sim, sensitivity_bool = sensitivity_bool, 
                              n_sensitivity = n_sensitivity, length_sensitivities = length_sensitivities, 
                              sens = sens, sensitivity_names = sensitivity_names, 
                              sens_name_used = sens_name_used, sens_iter_local = sens_iter_local, 
                              timed_freq = timed_freq, debug = debug, accum_backwards = accum_backwards, 
                              continue_on_error = continue_on_error, log_list = list())
      if (is.null(seed)) {
        seed <- 1
      }
      max_seed_product <- as.double(n_sim) * 1007 * as.double(seed) + 
        as.double(npats) * 349 * as.double(seed)
      if (max_seed_product > .Machine$integer.max) {
        safe_max <- floor(.Machine$integer.max/(as.double(n_sim) * 
                                                  1007 + as.double(npats) * 349))
        stop("run_sim(): seed = ", seed, " will cause integer overflow when combined ", 
             "with n_sim = ", n_sim, " and npats = ", npats, 
             ". Use a smaller seed (max safe value: ", 
             safe_max, ").", call. = FALSE)
      }
      set.seed(seed)
      input_list_sens <- as.environment(input_list_sens)
      parent.env(input_list_sens) <- environment()
      input_list_sens$n_elem_before <- 0L
      input_list_sens$n_sens_before <- .n_before_offsets[1L]
      .set_last_ctx(stage = "Error in setup:sensitivity_inputs", 
                    sens = sens, .warden_ctx = .warden_ctx)
      if (!is.null(sensitivity_inputs)) {
        on_error_check({
          load_inputs(inputs = input_list_sens, list_uneval_inputs = sensitivity_inputs)
        }, continue_on_error = continue_on_error)
        if (.skip_to_next) {
          next
        }
        if (input_list_sens$debug) {
          dump_info <- debug_inputs(NULL, input_list_sens)
          names(dump_info) <- paste0("Analysis: ", input_list_sens$sens, 
                                     " ", input_list_sens$sens_name_used, "; Structural")
          log_list <- c(log_list, dump_info)
          log_add(dump_info)
        }
      }
      exported_items <- unique(c("input_list_sens", ls(.GlobalEnv), 
                                 ls(parent.env(environment()), all.names = TRUE), 
                                 ls(environment(), all.names = TRUE)))
      options(future.rng.onMisuse = "ignore")
      n_chunks <- min(n_sim, ncores * 2L)
      sim_chunks <- split(seq_len(n_sim), ceiling(seq_len(n_sim)/ceiling(n_sim/n_chunks)))
      output_sim[[sens]] <- foreach(chunk = sim_chunks, 
                                    .options.future = list(packages = .packages()), 
                                    .options.future = list(globals = structure(TRUE, 
                                                                               add = exported_items)), .combine = "c") %dofuture% 
        {
          prev_dt_threads <- data.table::getDTthreads()
          data.table::setDTthreads(1L)
          on.exit(data.table::setDTthreads(prev_dt_threads), 
                  add = TRUE)
          RNGkind("L'Ecuyer-CMRG")
          chunk_results <- vector("list", length(chunk))
          for (idx in seq_along(chunk)) {
            simulation <- chunk[idx]
            .skip_to_next <- FALSE
            .set_last_ctx(stage = "Error in setup:simulation_start", 
                          sens = sens, simulation = simulation, 
                          .warden_ctx = .warden_ctx)
            message(paste0("Simulation number: ", simulation))
            start_time_sim <- proc.time()
            input_list <- new.env(parent = input_list_sens)
            list2env(as.list(input_list_sens), input_list)
            input_list$simulation <- simulation
            input_list$n_sens_before <- .n_before_offsets[2L]
            set.seed(simulation * 1007 * seed)
            .set_last_ctx(stage = "setup:common_all_inputs", 
                          sens = sens, simulation = simulation, 
                          .warden_ctx = .warden_ctx)
            if (!is.null(common_all_inputs)) {
              on_error_check({
                load_inputs(inputs = input_list, list_uneval_inputs = common_all_inputs)
              }, continue_on_error = continue_on_error)
              if (.skip_to_next) {
                next
              }
              if (input_list_sens$debug) {
                dump_info <- debug_inputs(input_list_sens, 
                                          input_list)
                names(dump_info) <- paste0("Analysis: ", 
                                           input_list$sens, " ", input_list$sens_name_used, 
                                           "; Sim: ", input_list$simulation, 
                                           "; Statics")
                log_list <- c(log_list, dump_info)
                log_add(dump_info)
              }
            }
            if (is.null(input_list$drc)) {
              input_list$drc <- 0.03
            }
            if (is.null(input_list$drq)) {
              input_list$drq <- 0.03
            }
            .set_last_ctx(stage = "Error in engine:start", 
                          sens = sens, simulation = simulation, 
                          .warden_ctx = .warden_ctx)
            on_error_check({
              if (constrained) {
                final_output <- run_engine_constrained(arm_list = arm_list, 
                                                       common_pt_inputs = common_pt_inputs, 
                                                       unique_pt_inputs = unique_pt_inputs, 
                                                       input_list = input_list, pb = pb, 
                                                       seed = seed, .warden_ctx = .warden_ctx)
              }
              else {
                final_output <- run_engine(arm_list = arm_list, 
                                           common_pt_inputs = common_pt_inputs, 
                                           unique_pt_inputs = unique_pt_inputs, 
                                           input_list = input_list, pb = pb, 
                                           seed = seed, n_sens_before_common_pt = .n_before_offsets[3L], 
                                           n_sens_before_unique_pt = .n_before_offsets[4L], 
                                           .warden_ctx = .warden_ctx)
              }
            }, continue_on_error = continue_on_error)
            if (.skip_to_next) {
              next
            }
            if (input_list$ipd > 0) {
              final_output$merged_df$simulation <- simulation
              final_output$merged_df$sensitivity <- sens
            }
            final_output <- c(list(sensitivity_name = sens_name_used), 
                              final_output)
            if (debug) 
              final_output$log_list <- log_sink$entries
            chunk_results[[idx]] <- final_output
            message(paste0("Time to run simulation ", 
                           simulation, ": ", round(proc.time()[3] - 
                                                     start_time_sim[3], 2), "s"))
          }
          Filter(Negate(is.null), chunk_results)
        }
      message(paste0("Time to run analysis ", sens, ": ", 
                     round(proc.time()[3] - start_time_analysis[3], 
                           2), "s"))
    }
    message(paste0("Total time to run: ", round(proc.time()[3] - 
                                                  start_time[3], 2), "s"))
    results <- output_sim
    class(results) <- c("warden_results", "list")
  }, enable = FALSE, cleanup = FALSE)
  .set_last_ctx(stage = "Simulation finalized", .warden_ctx = .warden_ctx)
  if(save_input_matrix) results$input_matrix <- input_matrix
  return(results)
}
