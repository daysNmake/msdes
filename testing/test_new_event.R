# Tests of new_event

test_new_event <- function (evt, env_ch = NULL) 
{
  # Names of the new events 
  new_evt_name <- names(evt)
  # Changes evt to a named vector new_evt
  new_evt <- setNames(unlist(evt), new_evt_name)
  # Gets input_list_trt from calling environment
  input_list_trt <- parent.frame()$input_list_trt
  # Creates a list with a single element equal to old event
  # list with appended new events
  evtlist_temp <- list(cur_evtlist = c(input_list_trt$cur_evtlist, 
                                       new_evt))
  # Sort the events in order of occurence (earliest to latest)
  evtlist_temp$cur_evtlist <- sort(evtlist_temp$cur_evtlist)
  # And update the cur_evtlist
  input_list_trt[["cur_evtlist"]] <- evtlist_temp$cur_evtlist
  # As in modify_event() it's unclear why the elements are being added to
  # calling environment.
  list2env(input_list_trt["cur_evtlist"], envir = parent.frame())
  # Assign input_list_trt in calling environment
  assign("input_list_trt", input_list_trt, envir = parent.frame())
}

# Creates a new cur_evtlist if not previously defined
# Allows multiple events with the same names
# And sorts in order of occurence
test_new_event(list(mi = 4 + runif(1, 0, 1),
  ich = 2 + runif(1, 0, 1))
)

# New events defined twice in calling environment
input_list_trt
cur_evtlist