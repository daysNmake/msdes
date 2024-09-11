# Tests of add_reactevnt()

test_add_reactevt <- function (.data = NULL, name_evt, input) 
{
  # Unclear what the out_calc is doing and compute_outputs is hidden
  out_calc <- list(react = substitute(compute_outputs(cost_ongoing_i = cost_ongoing, 
                                                      cost_instant_i = cost_instant, cost_cycle_i = cost_cycle, 
                                                      util_cycle_i = util_cycle, util_instant_i = util_instant, 
                                                      utilmlt_i = utilmlt, cycle_l_i = cycle_l, util_cycle_l_i = util_cycle_l, 
                                                      prevtime_i = prevtime, curtime_i = curtime, starttime_i = cycle_starttime, 
                                                      util_starttime_i = util_cycle_starttime, input_list_trt = input_list_trt)))[[1]]
  data_list <- .data
  # List of single list named after name_evt
  # The interior list is the default reaction and the other is what
  # happens when calculating costs and utilities (probably explained in a later
  # function)
  evt_r <- list(list(react = substitute(input), comp_out = out_calc))
  names(evt_r) <- paste(name_evt)
  # Create if new otherwise append
  if (is.null(data_list)) {
    data_list <- evt_r
  }
  else {
    data_list <- append(data_list, evt_r)
  }
  return(data_list)
}

# Creates an extression for reaction to event named "stroke"
x <- test_add_reactevt(name_evt = "stroke", 
                  input = runif(1, 0 , 1))
eval(x$stroke$react)

# Allows you to define the event to be a number, even though this doesn't run.
x <- test_add_reactevt(name_evt = 2, 
                       input = runif(1, 0 , 1))
x$2

# Code below runs despite events elsewhere being restricted from being single characters
x <- test_add_reactevt(name_evt = "a", 
                       input = runif(1, 0 , 1))
eval(x$a$react)

# Gives an error if multiple events passed
x <- test_add_reactevt(name_evt = c("stroke", "bleed"), 
                       input = runif(1, 0 , 1))


