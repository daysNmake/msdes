#' SetupModel as a function
#' 
#' `rrmsdessetup()`RRMS DES model inputs written as a function to be called from Shiny APP
#' 
#' For quick deployment, also used in ShinyApp.
#' 
#' @export
#model setup with inputs allowing for reactive changes to take place in shiny
rrmsdessetup <- function(drc=drc, drq=drq,
                         time_horizon = time_horizon, p_eid=p_eid,
                         jcvtest_cost=jcvtest_cost){
  ## Treatments at second line 
  v_treatment_names <<- c("Natalizumab-IV",
                          "Natalizumab-SC",
                          "Natalizumab biosimilar-IV",
                          "Natalizumab biosimilar-SC",
                          "Cladribine", 
                          "Ublituximab" ,
                          "Ofatumumab",
                          "Ocrelizumab")
  
  ## number of treatments
  n_treatments <<- length(v_treatment_names)
  
  ## specify codes for the treatments
  v_treatment_codes <<- c(seq(1, n_treatments))
  names(v_treatment_codes) <<- v_treatment_names
  
  ## Treatments that patients can move to at 3rd and higher line 
  v_higher_line_treatments <<- c("Natalizumab-IV",
                                 "Natalizumab-SC",
                                 "Natalizumab biosimilar-IV",
                                 "Natalizumab biosimilar-SC",
                                 "Cladribine",
                                 "Ublituximab",
                                 "Ofatumumab",
                                 "Ocrelizumab"
  )
  v_higher_line_treatment_codes <<- c(seq(1, length(v_higher_line_treatments)))
  names(v_higher_line_treatment_codes) <<- v_higher_line_treatments
  
  ## prior treatment index
  v_prior_treatment_indices <<- c(rep(0, length(v_higher_line_treatments)))
  names(v_prior_treatment_indices) <<- v_higher_line_treatments
  
  ## Model event states
  v_event_names <<- c("initialise","age" , "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "annual_treatment", "serious_adverse_event", "death"
  )
  
  ## Define the possible EDSS values
  v_edss_values <<-  c(0:9)
  names(v_edss_values) <<- paste("EDSS", v_edss_values)
  
  # Number of EDSS categories
  n_edss_values <<- length(v_edss_values)
  
  #Function to generate inputs, the same for both packages
  #source("R/generate_inputs.R")
  ## Model inputs generated using the generate inputs function
  inputs <<- gen_inputs(drc=drc, drq=drq,
                        time_horizon = time_horizon, p_eid=p_eid, jcvtest_cost=jcvtest_cost)
}