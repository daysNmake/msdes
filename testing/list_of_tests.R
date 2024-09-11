# Will random seeds work?
# A: No as the RunSim() function calls set.seed() as the simulation number
# analyses are therefore not reproducible

# Any general issues?
# A: Simulation doesn't work if only one event is defined

# List of descem functions to validate
# A: Are the answers resulting from individual function tests

# add_item()
# Q: Does it correctly store data (scalar, vector, matrix) and does it do any transformations?
# A: No transformations. It saves the expression as written
# What is happening when you set a value to be a random variable
# It stores the function itself as an expression which you can evaluate
# using eval()

# Is behaviour correct?
# common_all_inputs - common to all patients and treatments

# common_pt_inputs - common to all treatments but vary by patient
# unique_pt_inputs - vary by both treatment and patients

# add_tte()
# Is behaviour correct for init_event_list?
# If we set trt to all treatments does the event apply to all treatments?
# A: Yes it loops through the vector in trt
# What about evts?
# A: Only if a time is provided for each event. It doesn't throw an error if
# some event times are missed
# Are there restrictions on data type of trt and evts?
# A: trt needs to be non-repeated vector or throws an error.
# A: evts needs to be character vector of length >1 so gives error if
# a single event with single character name but not if >1 events with single
# character names. Inconsistent error handling but likely doesn't affect functionality.
# What should 'input' be? We're assuming it's a chunk of code that defines scalars with event times
# A: Any expression that defines variables (ideally event times) in current scope

# draw_tte()
# Any restrictions on coef?
# A: Coef have to be scalars but hr can be a vector or matrix and thus give
# anomalous behaviour
# Are distributions implemented correctly? 
# A: Scope for confusion if using rweibull and rweibullPH as shape and scale
# have different interpretations (latter is implemented using flexsurv)
# Are parameters on the correct scale?
# A: Not yet checked against documentation. Not critical to NICE MS MTA as
# only using the exponential distribution (which is correct)

# add_reactevt()
# Is it expecting a string for name_evt?
# A: Allows users to define them as numerics but then gives an error. Should impose restriction on 
# event names
# A: Also allows events to be named single characters despite add_tte() restricting to length >1
# Is input again a chunk of code?
# A: Yes an expression
# Is behaviour correct for init_event_list?

# modify_item()
# Takes a list of item names to change?
# A: yes
# Any conditions under which it doesn't change the item?
# A: no
# Potential error as defines events twice but need to better understand function

# new_event()
# A list of event names set to scalar times or functions? 
# A: Yes
# A: As in modify_item() it defines the new events twice in calling environment
# This is either an error or indicates some redundant coding in descem

# add_util()
# We're assuming it defines util conditional on all variables in model?
# A: Yes
# Is util a function?
# A: Yes util, cycle_l and cycle_starttime can all be functions
# Can evt and trt be all events and all treatments?
# A: Yes or just a subset
# Any restrictions on conditionality?
# A: No they are general expressions


# add_cost()
# Same questions as above
# Almost identical to add_util()
# Could probably have been implemented as a single shared function

# Is behaviour for util_ongoing correct? Applies an ongoing utility per unit time?
# Is behaviour for cost_ongoing correct? Ongoing cost per unit time?

# RunSim()

# Try extreme value tests on event times, utilities, costs
# Ensure you can get patients and parameter samples to be identical over each run
# What happens to utilities and costs if the cycle length and cycle start time are not defined?

# compute_outputs()
# Function hidden within descem