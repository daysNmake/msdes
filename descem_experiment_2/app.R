#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#

rm(list = ls())


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sick-Sick-Sicker DES"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("SI_n_patients",
                        "Number of patients:",
                        min = 1,
                        max = 10000,
                        value = 10),
            
            sliderInput("SI_n_sims",
                        "Number of simulations:",
                        min = 1,
                        max = 100,
                        value = 1)
        ),
        actionButton(inputId = "run_model",     # id of action button, used in server
                     label   = "Run model")     # action button label (on button)
),
        # Show a plot of the generated distribution
        mainPanel(
          tableOutput(outputId = "SO_icer_table"),   # tableOutput id = icer_table, from server
          plotOutput(outputId = "SO_CE_plane")       # plotOutput id = CE_plane, from server
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  source("scripts/descem_experiment_2_shiny.R")
  observeEvent(input$run_model,       # when action button pressed ...
               ignoreNULL = T, {
                 
                 # Run model wrapper function with the Shiny inputs and store as data-frame
                 results =  f_shiny_model(n_patients = input$SI_n_patients,
                                          n_samples = input$SI_n_sims)
                 # Get the total costs and QALYs for each patient
                 n_samples <- mean(results[["final_output"]][["merged_df"]][["simulation"]])
                 n_treatments <- length(results[["final_output"]][["trt_list"]])
                 n_patients<- max(results[["final_output"]][["merged_df"]][["pat_id"]])
                 treatment_names <- results[["output_psa"]][[1]][["trt_list"]]
                 total_costs_ipd <- total_qalys_ipd <- array(NA, dim = c(n_samples, n_treatments, n_patients),
                                                             dimnames = list(NULL, treatment_names, NULL))
                 
                 # Below is not very efficient but only needs to be run once at end of simulation
                 for(treatment_name in treatment_names) {
                   for(i_sample in 1:n_samples) {
                     for(i_patient in 1:n_patients) {
                       # With statement is neater but much slower
                       temp_index <- results$output_psa[[i_sample]]$merged_df$pat_id == i_patient &
                         results$output_psa[[i_sample]]$merged_df$trt == treatment_name &
                         results$output_psa[[i_sample]]$merged_df$evttime == 0
                       
                       total_costs_ipd[i_sample, treatment_name, i_patient] <-
                         results$output_psa[[i_sample]]$merged_df$total_costs[temp_index]
                       
                       total_qalys_ipd[i_sample, treatment_name, i_patient] <-
                         results$output_psa[[i_sample]]$merged_df$total_qalys[temp_index]
                     } # End loop over patients
                     # Summarise for just this sample
                     
                   }
                 }
                 
                 
                 
                 #--- CREATE COST EFFECTIVENESS TABLE ---#
                 output$SO_icer_table <- renderTable({
                   df_res_table <- data.frame(summary_results_psa(results$output_psa))
                   row.names(df_res_table) <- c("costs", "Life years", "QALYs", "ICER", "ICUR")
                   df_res_table
                 }) # table plot end.
                 
                 #---  CREATE COST EFFECTIVENESS PLANE ---#
                 output$SO_CE_plane <- renderPlot({ # render plot repeatedly updates.
                   

                   
                   total_costs_ipd <- as.matrix(total_costs_ipd[,,-1])
                   total_costs_ipd <- t(total_costs_ipd)
                   total_qalys_ipd <- as.matrix(total_qalys_ipd[,,-1])
                   total_qalys_ipd <- t(total_qalys_ipd)
                   # Create a BCEA object
                   des_bcea <- bcea(eff = total_qalys_ipd,
                                    cost = total_costs_ipd,
                                    ref = 1,
                                    interventions = treatment_names)
                   
                   
                   # Plot the CE plane
                   ceplane.plot(des_bcea, wtp = 20000, xlim = c(-0.05, 0.5), ylim = c(-500, 3000))
                   
                   
                  
                 }) # renderplot end
      
                 
    })  #observe event end
} #Server End

# Run the application 
shinyApp(ui = ui, server = server)
