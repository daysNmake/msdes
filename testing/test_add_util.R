# Tests of add_util()

test_add_util <- function (.data = NULL, util, evt, trt, 
                           cycle_l = NULL, cycle_starttime = 0) 
{
  # Previous utility data (can be NULL if no other utility data)
  data_list <- .data
  # Adds to all treatments and events in the trt and evt vectors
  for (trt_item in trt) {
    for (event_item in evt) {
      # The utilities are a list of lists with the utility, cycle length, 
      # and start time of the utility
      # All three elements can be functions
      util_item <- list(list(util = substitute(util), 
                             cycle_l = substitute(cycle_l), 
                             cycle_starttime = substitute(cycle_starttime)))
      # Name is combined event and treatment to which it applies
      names(util_item) <- paste(event_item, trt_item, 
                                sep = "_")
      if (is.null(data_list)) {
        data_list <- util_item
      }
      else {
        data_list <- append(data_list, util_item)
      }
    }
  }
  return(data_list)
}
