# function to extract vector of items from WARDEN environment (ex. lists saved in common inputs) 
# and indexing on a vector (ex. treatments, severity)
# Ayman Sadek 07-Mar-2026


pluck_vector_from_env <- function(anylist, anyindex, ...) {
  
  temp <- rep(NA, length(anyindex))
  names(temp) <- anyindex
  for (i in 1:length(anyindex)) {
    temp[i] <- purrr::pluck(anylist, anyindex[i])
    #temp[i] <- anylist[[anyindex[i]]]
  }
  return(temp)
}


