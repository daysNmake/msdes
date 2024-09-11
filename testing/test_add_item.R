# Tests of the add_item() function

test_add_item <- function (.data = NULL, ...) 
{
  data_list <- .data
  list_item <- as.list(substitute(...()))
  if (is.null(data_list)) {
    data_list <- list_item
  }
  else {
    data_list <- append(data_list, list_item)
  }
  return(data_list)
}

# Appears to be a list of expressions
x <- test_add_item(t_names = c("edoxaban", "warfarin"))
x

# Appends to previous list
y <- test_add_item(.data = x, pt_names = c("jim", "james"))
y
# 

# Can assign expressions to be random functions
z <- test_add_item(event_time = rnorm(1, 0, 1))
z$event_time
# What descem is likely doing is:
eval(z$event_time)

# Can assign expressions to be functions returning random vectors
w <- test_add_item(event_times = rnorm(10, 0, 1))
eval(w$event_times)