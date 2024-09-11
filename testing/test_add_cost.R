# Tests of add_cost()

# Functionally identical to add_util()
# Unclear why two functions are needed

test_add_cost <- function (.data = NULL, cost, evt, trt, cycle_l = NULL, cycle_starttime = 0) 
{
  data_list <- .data
  for (trt_item in trt) {
    for (event_item in evt) {
      cost_item <- list(list(cost = substitute(cost), 
                             cycle_l = substitute(cycle_l), 
                             cycle_starttime = substitute(cycle_starttime)))
      names(cost_item) <- paste(event_item, trt_item, 
                                sep = "_")
      if (is.null(data_list)) {
        data_list <- cost_item
      }
      else {
        data_list <- append(data_list, cost_item)
      }
    }
  }
  return(data_list)
}
