# Test the modify_item function

# Potential error that adds unnecessary objects to calling environment

test_modify_item <- function (list_item, env_ch = NULL) 
{
  # Move the input_list_trt to scope/environment of test_modify_item()
  # Does this by creating temporary object rather than a list2env call
  input_list_trt <- parent.frame()$input_list_trt
  # Modify the items to match the elements of list_item
  input_list_trt[names(list_item)] <- lapply(list_item, unname)
  # I think the below is an error
  # It moves elements of list_item to the calling environment but then
  # doesn't use these variables.
  list2env(list_item, envir = parent.frame())
  # And assign/overwrite the new list in the environment that called modify_item
  assign("input_list_trt", input_list_trt, envir = parent.frame())
}

# Not clear what this function is doing
# Adds stroke time to the input_list_trt but also the original environment
input_list_trt <- list("edoxaban", "warfarin")
test_modify_item(list(stroke = 10))

