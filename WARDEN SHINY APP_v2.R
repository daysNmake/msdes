#================= RRMS DES in WARDN Shiny APP ==================#
#======== Authored: by Ayman Sadek  ============================#
#======= ACM2 Committee preferred assumptions(Public Prices) =#


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
library(shinyjs)
library(shinyWidgets)
library(bslib)


# call model function
source("analysis/WARDEN_HEMBUG DEMO PUBLIC PRICES MODEL AS A FUNCTION.R")
# Function to summarise the results
source("R/summarise_samples.R")
source("R/summarise_ce_results.R")
rrmsdessetup() #call in the inputs for the app

# Shiny App User Interface
ui <- 
  
  #Project Page
  page_fillable(    
    
    title = NULL,
    
    
    #navigation bar  
    page_navbar(
      
      #Navigation bar title displays the logo on the top of all pages
      title = div(img(src="https://www.bristol.ac.uk/media-library/sites/populationhealth/images/TAG%20icon%20long%2016kb.png", 
                               id ="logo")),
    
      
      
      
      #Introduction nav panel displays model diagram and description
      nav_panel(  
        title =  "Model Info",
        
       layout_column_wrap( 
        card(
          card_title("Model Diagram"),
          card_image("shiny/model_diagram.png")
        ),
        
        card(uiOutput("uo_modedescription"))
      ) #Introduction nav panel
      ),
      #Model inputs tab panel
      nav_panel(  
        title =  "Model Inputs",
        
        
        #Main panel for model run
        navset_card_underline(
          nav_panel(  #Heading
            title = "Treatment costs",
            header= "text for header",
            card(#Output
              card_header("card header"),
            
            tableOutput(outputId = "to_treatment_costs"),
            
            radioButtons("trt", "select treatment for which cost is to be updated:",
                         v_treatment_names),
            
            numericInput( 
              "ntc", 
              "enter new cost for selected treatment:",
              value= 0 ),
            
            actionButton("update", "Update with new treatment cost"),
            
            uiOutput("uo_costs_readme"),
            
            card_footer("cardfooter")
            
            )),
          
          nav_panel(#Heading 2
            title = "administration costs",
            #Output
            tableOutput(outputId = "to_administration_costs")),
          
          nav_panel(#Heading 3
            title = "monitoring costs",
            #Output
            tableOutput(outputId = "to_monitoring_costs")),
          
          
        )#card
      ), #Introduction tab panel
      
      #MS-Model tab panel
      nav_panel(  
        
        title =  "MS-DES Model",
        #side bar panel for 
        layout_sidebar(
          title="",
          #Side bar panel for model inputs
          sidebar= sidebar(
            title="",
            #slider for number of patients
            sliderInput(
              inputId = "si_n_patients" , 
              label = "Number of simulated patients",
              value = 10, min=1, max=100),
            #slider for number of simulations
            sliderInput(
              inputId = "si_n_samples" , 
              label = "Number of simulations to run",
              value = 10, min=1, max=100),
            #progress bar
            column(4,progressBar(id = "pb4", value = 0, display_pct = T)),
            #button to run model
            actionButton(
              inputId = "ab_run_model", label = "Run DES Model"),
            useShinyjs(),
            textOutput("progress_text")
          ), #sidebar panel
          
          
          #Card panel for model run
          navset_card_underline(

            
              nav_panel(  #Heading
                title = "Results Table",
                #Output
                tableOutput(outputId = "to_results_table")),

              nav_panel(#Heading 2
                title = "Cost Effectiveness Plane",
                #Output 2
                plotOutput(outputId = "po_ce_plane")),

              nav_panel(#Heading 3
                title = "Cost Effectiveness Acceptability Curve",
                #Output 3
                plotOutput(outputId = "po_ceac"))
            )#card
        ) #sidebar layout
      )#Ms model results panel 
      
    ) #Page navbar
    
  )# fillable page

# Shiny App Server
server <- 
  # server function with inputs and outputs
  function(input, output, session){
    # #render model description file
    output$uo_modedescription <- renderUI({
      includeMarkdown("shiny/model description.Rmd")
    })
    

    
    #display data input

    output$to_treatment_costs <- renderTable({
      #loadinputs
      # rrmsdessetup()
      #display treatment costs  
    inputs[["df_c_tx"]]
      
    },rownames=TRUE)
    
    output$uo_costs_readme <- renderUI({
      includeMarkdown("README.md")})
    
    output$to_administration_costs <- renderTable({
      #loadinputs
      # rrmsdessetup()
      #display treatment costs
      inputs[["df_c_tx_admin"]]
      
    },rownames=TRUE)
    
    output$to_monitoring_costs <- renderTable({
      #loadinputs
      # rrmsdessetup()
      #display treatment costs
      inputs[["df_c_tx_monitor"]]
      
    },rownames=TRUE)
    
        # Modify inputs with update action button

            
            #update treatment costs reactive function this works but is slow! observeEvent is faster
    
            # tc_update <- eventReactive(input$update, {
            #   #inputs<<-inputs
            #   inputs$df_c_tx["cost_annual_yr1", "Natalizumab-IV"] <<- input$ntc
            #   inputs$df_c_tx["cost_annual_yr2", "Natalizumab-IV"] <<- input$ntc
            #   })
            # 
            # output$to_treatment_costs <- renderTable({
            #   
            #   tc_update()
            #   inputs[["df_c_tx"]]
            #   
            # },rownames=TRUE)
            
              
              observeEvent(input$update, {
                #update inputs
                 var_new = reactiveVal(input$ntc)
                inputs$df_c_tx["cost_annual_yr1", input$trt] <<- var_new()
                inputs$df_c_tx["cost_annual_yr2", input$trt] <<- var_new()


                #display updated treatment costs
                output$to_treatment_costs <- 
                  renderTable({

                  #tc_update()
                  inputs[["df_c_tx"]]

                },rownames=TRUE)
                  # renderPrint(input$trt)

              })
    
     #input$new_treatment_cost <- inputs$df_c_tx["cost_annual_yr1", "Natalizumab-IV"]
    
    #Action triggered when button is pressed
    observeEvent(input$ab_run_model,  {
      
      #Run model
      withCallingHandlers({
        
        html("progress_text", "")
        
        
        #Run model  
        rrmsdesmodel(n_patients = input$si_n_patients , n_samples = input$si_n_samples)
        
        
      },
      message = function(m) {
        html(id = "progress_text", html = paste0(m$message, '<br>'), add = TRUE)
        i<<- ifelse(exists("i"), (i+1) , 1 )
        updateProgressBar(session = session, id = "pb4", value = (i/((input$si_n_samples*2)+4))*100 )
      }
      
      )
      
      
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
      
      #write.csv(results_table, paste0(c("results/results warden determenistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
      
      
      
    })
    
  }

# RUN APP
shinyApp(ui, server)






