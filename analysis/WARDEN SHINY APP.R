#### RRMS DES using the WARDN package ####
## Authored: Howard Thom & Ayman Sadek ###
## Validated: Javier Sanchez Alvarez ####
## ACM2 Committee preferred assumptions##
# Public Prices  #############

rm(list = ls())

library(WARDEN)
library(MASS)
library(VGAM)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)
library(BCEA)
library(ggplot2)
library(shiny)



# call model function
source("analysis/WARDEN_HEMBUG DEMO PUBLIC PRICES MODEL AS A FUNCTION.R")
# Function to summarise the results
source("R/summarise_samples.R")
source("R/summarise_ce_results.R")

# Shiny App User Interface
ui <- 
  #Project Page
  fluidPage(      
  
    #Project title
    titlePanel( "MS-DES Model"),
    
    #Page layout
    sidebarLayout(
      
      #Side bar panel
      sidebarPanel(
        #slider for number of patients
        sliderInput(
          inputId = "si_n_patients" , 
          label = "Number of simulated patients",
          value = 2, min=1, max=1000),
        #slider for number of simulations
        sliderInput(
          inputId = "si_n_samples" , 
          label = "Number of simulations to run",
          value = 2, min=1, max=1000),
        #button to run model
        actionButton(
          inputId = "ab_run_model", label = "Run DES Model")
        ),
        
      #Main panel
      mainPanel(
        #Heading
        h3("Results Table"),
        #Output
        tableOutput(outputId = "to_results_table"),
        #Heading 2
        h3("Cost Effectiveness Plane"),
        #Output 2
        plotOutput(outputId = "po_ce_plane"),
        #Heading 3
        h3("Cost Effectiveness Acceptability Curve"),
        #Output 3
        plotOutput(outputId = "po_ceac")
      )  
    )
  )

# Shiny App Server
server <- 
  # server function with inputs and outputs
  function(input, output){
    
    #Action triggered when button is pressed
    observeEvent(input$ab_run_model,  {

      #Run model
      rrmsdesmodel(n_patients = input$si_n_patients , n_samples = input$si_n_samples)

      #create output
      output$to_results_table <- renderTable({

        # WARDEN::summary_results_det(results[[1]][[1]]) , # only use if 1 simulation
        n_patients<- input$si_n_patients
        n_samples <- input$si_n_samples
        
        
        total_costs_ipd <- total_qalys_ipd <- array(NA, dim = c(n_samples, n_treatments, n_patients),
                                                    dimnames = list(NULL, v_treatment_names, NULL))
        
        # Below is not very efficient but only needs to be run once at end of simulation
        for(treatment_name in v_treatment_names) {
          for(i_sample in 1:n_samples) {
            for(i_patient in 1:n_patients) {
              # With statement is neater but much slower
              temp_index <- results[[1]][[i_sample]]$merged_df$pat_id == i_patient &
                results[[1]][[i_sample]]$merged_df$arm == treatment_name & #
                results[[1]][[i_sample]]$merged_df$evtname == "death"
              
              total_costs_ipd[i_sample, treatment_name, i_patient] <-
                results[[1]][[i_sample]]$merged_df$total_costs[temp_index]
              
              total_qalys_ipd[i_sample, treatment_name, i_patient] <-
                results[[1]][[i_sample]]$merged_df$total_qalys[temp_index]
            } # End loop over patients
            # Summarise for just this sample
            
          }
        }
        
        options(scipen = 999)
        total_costs <<- apply(total_costs_ipd, c(1, 2), mean)
        total_qalys <<- apply(total_qalys_ipd, c(1, 2), mean)
        

        
        
        #drop natalizumab-biosimilar-SC and cladrebiene
        total_costs <<- total_costs[,-c(4,5)]
        total_qalys <<- total_qalys[,-c(4,5)]
        
        results_table <- summarise_ce_results(total_qalys = total_qalys, 
                                              total_costs = total_costs,
                                              reference = "Natalizumab-IV")
        
        results_table},
        rownames=T)
      
      output$po_ce_plane <- renderPlot({
        #plot results
        v_results_treatment_names<- v_treatment_names[-c(4,5)]
        
        # Create a BCEA object
        des_bcea <<- bcea(eff = total_qalys,
                         cost = total_costs,
                         ref = 1,
                         interventions = v_results_treatment_names)
        #plot ce plane
        ceplane.plot(des_bcea, wtp = 30000,
                     graph = "ggplot2")
      })
      
      output$po_ceac <-  renderPlot({
        
        #plot a CEAC
        des_multi_ce <- multi.ce(des_bcea)
        
        ceac.plot(des_multi_ce, graph = "ggplot2",
                  line = list(color = c("darkorange", "palegreen3", "blue", "grey",
                                        "cyan3", "black"),
                              size=2 ),
                  #ylim=c(0, 0.5),
                  pos = c(1, 1)
        )
        
      })

      #save results

    })
    
  }
  
# RUN APP
shinyApp(ui, server)


# run model function
#rrmsdesmodel(n_patients =1 , n_samples = 1)


# Summarise the results
#results_summary_determenistic <- summary_results_det(results[[1]][[1]]) # only use if 1 simulation

# results_summary_probabalistic <- summary_results_sim(results[[1]]) # use if >1 simulation
# View(results_summary_probabalistic)


write.csv(results_table, paste0(c("results/results warden determenistic", n_samples, "x", n_patients, ".csv"), collapse = " "))




