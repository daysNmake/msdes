#'
#' @description
#' function to extract vectors or condition on them when using environment in WARDEN pick_val() or input_block()
#' Makes use of the package purrr
#' Ayman Sadek 07-Mar-2026
#' 
#' @details
#' Vector of objects pulled from WARDEN environment (ex. lists saved in common inputs)
#' and indexing on a vector (ex. treatments, severity)
#' 
#' @param anylist The input parameters saved an environment object
#' @param anyindex conditioning on treatment or severity
#' @export
pluck_vector_from_env <- function(anylist, anyindex, ...) {
  
  temp <- rep(NA, length(anyindex))
  names(temp) <- anyindex
  for (i in 1:length(anyindex)) {
    temp[i] <- purrr::pluck(anylist, anyindex[i])
    #temp[i] <- anylist[[anyindex[i]]]
  }
  return(temp)
}


