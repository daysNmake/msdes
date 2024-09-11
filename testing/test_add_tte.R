# Tests of add_tte() function

test_add_tte <- function (.data = NULL, trt, evts, other_inp = NULL, input) 
{
  data_list <- .data
  # trt can be any type of variable as no checks
  for (trt in trt) {
    if (!is.character(other_inp) & !is.null(other_inp)) {
      stop("other_inp argument is required to be a character vector or be set to NULL")
    }
    # But evts has to be a character vector of length > 1
    if (!is.character(evts) | length(evts) < 2) {
      stop("evts argument in add_tte for the intervention ", 
           trt, " is required to be a character vector with length >1")
    }
    # List with single list as element named as current treatment
    # Combined below with lists for other treatments
    # input is the expression with evts substituted
    evt_l <- list(list(expr = substitute(input), evts = evts, 
                       other_inp = other_inp))
    names(evt_l) <- paste(trt)
    # Use the list with first treatment if no previous data given and if on
    # first trt
    if (is.null(data_list)) {
      data_list <- evt_l
    }
    # For second trt and higher, or if data supplied by user, append the list
    # with the event expression
    else {
      data_list <- append(data_list, evt_l)
    }
  }
  # The names of data_list should be the treatments
  # trt needs to have no repeats
  # Otherwise would set multiple times to events for a single treatment
  if (length(names(data_list)) != length(unique(names(data_list)))) {
    stop("More than one initial set of time to events has been defined for an intervention.")
  }
  return(data_list)
}


x <- add_tte(trt = c("edoxaban", "warfarin"),
             evts = c("stroke", "bleed") ,
             input={ 
               stroke <- 0
               bleed <- draw_tte(
                 1, 
                 dist = "exp", 
                 coef1 = log(0.2),
                 hr = 1.1
               )
             })

# Each time we evaluate the expression it defines a new event time for bleed
# and sets stroke to be 0, and does so in current scope (e.g. global or local
# to a function)
eval(x$edoxaban$expr)
bleed

# Gives an error as expected
y <- add_tte(trt = c("edoxaban", "warfarin", "warfarin"),
                 evts = c("stroke") ,
                 input={ 
                   stroke <- 0
                 })

# No error even though multiple identical events are defined
y <- add_tte(trt = c("edoxaban", "warfarin"),
             evts = c("stroke", "stroke") ,
             input={ 
               stroke <- 0
             })
eval(y$edoxaban$expr)

# No error even though event times not defined for all events!
y <- add_tte(trt = c("edoxaban", "warfarin"),
             evts = c("stroke", "bleed") ,
             input={ 
               stroke <- 0
             })

# Throws an error as per definition
y <- add_tte(trt = c("edoxaban", "warfarin"),
             evts = c("s") ,
             input={ 
               s <- 0
             })

# But this does not throw an error for reasons unknown
y <- add_tte(trt = c("edoxaban", "warfarin"),
             evts = c("s", "b") ,
             input={ 
               s <- 0
               b < - runif(0, 0 , 1)
             })
