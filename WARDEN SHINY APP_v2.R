#================= RRMS DES in WARDN Shiny APP ==================#
#======== Authored: by Ayman Sadek  ============================#
#======= ACM2 Committee preferred assumptions(Public Prices) =#


library(WARDEN)
library(MASS)
library(VGAM)
library(dplyr)
library(flexsurv)
library(kableExtra)
library(purrr)
library(tidyr)
library(BCEA)
library(ggplot2)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

##TODO

# Not sure why the model readme file is rendering within the treatment costs, to separate?
# add warning to update scenarios




rm(list = ls())
devtools::load_all()


# call model as function run once to load inputs for ui first (best to automate?)
P_eid_default <- 0.0
rrmsdessetup(
  p_eid=0 # proportion of patients on external interval dosing 
)


# Shiny App User Interface
ui <- 
 
  #Project Page
  page_fillable(    
    
    title = NULL,
    
    # set model assumptions needed for the UI to run
  
    #navigation bar style, could opt for navset_card_tab() or navset_card_pill()
    page_navbar(
      
      #Navigation bar title displays the logo on the top of all pages
      title = div(img(src="https://www.bristol.ac.uk/media-library/sites/populationhealth/images/TAG%20icon%20long%2016kb.png", 
                               id ="logo")),
    
      
      # nav panel 1 Introduction : displays model diagram and model description
      nav_panel(  
        title =  "Model Info",
        
       layout_column_wrap( 
        card(
          card_header(class = "bg-light text-primary", "Diagram"),
          card_image("shiny/model_diagram.png")
        ),
        
        card(
          card_header(class = "bg-light text-primary","Description"),
          card_body(markdown(uiOutput("uo_modedescription")))
          )
        
      ) #Layout is column wrap
      ),# nav panel 1 Introduction
      

      #nav panel 2 Model inputs : tabs: treatment costs, admin & monitoring costs
      nav_panel(  
        title =  "Model Inputs",
        
        
        #Main panel for model inputs
        navset_card_underline(
          
          #model inputs Relative Treatment Effects
          nav_panel(
            title = "Treatment Effects",
            #CDP6
            card(
              card_header(class="bg-light text-primary",
                          "Relative treatment effects relative to Natalizumab, 
                          assuming equivilence of Natalizumab originator and biosimilar. 
                          Equivilence of Ublituximab, ofatumumab and ocrelizumab"),
              
              # card body for CDP6
              card_body(height=200 ,
              "Confirmed Disability progresssion at 6 months CDP6 Hazard ratios",
              tableOutput(outputId = "to_CDP6"),
              ), #card body
          
            # card body for ARR
            card_body(height=200 ,
              " Annualized Relapse Rates ARR" , 
                      tableOutput(outputId = "to_ARR"),
            ), #card body
            
            # card body for AELtD
            card_body(height=200 ,
                      " Adverse Events Leading to Discontinuation AELtD" , 
                      tableOutput(outputId = "to_AELtD"),
            ), #card body
            
            
          ), #card 
            ), # navpanel tretament effects
          
          
          #model inputs treatment costs
          nav_panel(
            title = "Treatment costs",
            
           # card with header, card body x2, and footer 
            card(
              card_header(class="bg-light text-primary",
                "Total Annual Treatment costs per DMT"),

            # card body for treatment costs
            card_body(height=300 ,
                  tableOutput(outputId = "to_treatment_costs"),
            ), #card body
           
            # card body for description of treatment costs
            card_body(height=100,
              markdown(uiOutput("uo_costs_readme")),
              ), # card body
            
            card_footer(class="bg-light", "Refer to publication for more info DOI:")
            
            ) #card layout with header and footer
            ), #nav panel model inputs treatment costs
          
          #model inputs admin costs
          nav_panel(
            title = "administration & monitoring costs",
            #Output
            card(
              
            card_body(
              card_title(class="bg-light text-primary","Total annual treatment administration costs"),
              tableOutput(outputId = "to_administration_costs")),
            
            card_body(
              card_title(class="bg-light text-primary","Total annual treatment monitoring costs"),
              tableOutput(outputId = "to_monitoring_costs"))
            )
            ) #model inputs admin & monitoring costs
          
          
        )#navset card underline
      ), ##nav panel 2 Model inputs
      
      
      #Model set-up navigation panel gives model default settings and options for scenarios
      nav_panel(  
        title =  "Model default settings and scenarios",
        
        #layout_column_wrap( 
          card(
            card_title("Default settings & Scenarios"),
            #card_image("shiny/model_diagram.png")
            
         #card body for EID assumption:
          card_body(
            "Assumption for the perventage of patitents reciving the 6-week interval dosing, this was assumed to be 60% in the appraisal
            It is set to to be 0% by default,",
          
            numericInput( 
            "n_p_eid", 
            "proportion of people on  extended interval dosing to be changed here:",
            value= P_eid_default, min = 0, max=1, step=0.01 ),
          
          actionButton("update_eid", "Update the EID assumption"),
          
          uiOutput("uo_eid")
          ), #card body
         # card body for custom treatment costs
         card_body(
                   # tableOutput(outputId = "to_treatment_costs"),
                   "Option to set custom treatment costs for DMTs of choice",
                   radioButtons("trt", "Select DMT for which cost is to be updated:",
                                v_treatment_names, inline = TRUE),
                   
                   numericInput( 
                     "ntc", 
                     "Enter new cost for selected DMT:",
                     value= 0, min=0, max = 50000),
                   
                   actionButton("update_trt_cost", "Update with custom treatment cost"),
         ), #card body
          ) #card
        #) #layout_column_wrap
      ), #model setup nav panel
      
      #MS-Model run and results navigation panel
      nav_panel(  
        
        title =  "MS-DES Model",
        #side bar panel has model setup options no. of patients, no. of simulation, and progress bar 
        layout_sidebar(
          title="",
          #Side bar panel for model inputs
          sidebar= sidebar(
            title="",
            #slider for number of patients
            numericInput(
              inputId = "si_n_patients" , 
              label = "Number of simulated patients",
              value = 10, min=1, max=100, step=10),
            #slider for number of simulations
            numericInput(
              inputId = "si_n_samples" , 
              label = "Number of simulations to run",
              value = 10, min=1, max=100, step=10),
            #progress bar
            column(4,progressBar(id = "pb4", value = 0, display_pct = T)),
            #button to run model
            actionButton(
              inputId = "ab_run_model", label = "Run DES Model"),
            useShinyjs(),
            textOutput("progress_text")
          ), #sidebar panel
          
          
          #Card panel for model results
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
    
    observeEvent(input$update_eid, {
      #update scenarios: p_eid
      var_new1 = reactiveVal(input$n_p_eid)
      rrmsdessetup(p_eid=var_new1()) #call in the inputs for the app
      
      #display updated treatment costs after assumption change
      output$to_treatment_costs <- 
        renderTable({
          inputs[["df_c_tx"]]
        },rownames=TRUE)
      
      showNotification("Proportion of patients on EID updated, look at Model inputs > Treatment Costs to view the changes", type = "message")
      showNotification("Please do not update EID assumption more than once, restart app if needed", type = "warning", duration = 10)
      
    }) #reactive update of eid assumption
    
    
    #display data input
    
    output$to_CDP6 <- renderTable({
        cdp6<- c(1, 1, 1, 1, 
               exp(inputs$m_hr_rrms_cdp6_fe["Cladribine O3.5","mean"]), 
               exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), 
               exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]), 
               exp(inputs$m_hr_rrms_cdp6_fe["Ocrelizumab IV600","mean"]) )
        names(cdp6) <- v_treatment_names
        t(cdp6)
    })
    output$to_ARR <-renderTable({
      arr<- c(1, 1, 1, 1, 
               exp(inputs$m_hr_rrms_arr_fe["Cladribine O3.5","mean"]), 
               exp(inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]), 
               exp(inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]), 
               exp(inputs$m_hr_rrms_arr_fe["Ocrelizumab IV600","mean"]) )
      names(arr) <- v_treatment_names
      t(arr)
    })
    
    output$to_AELtD <-renderTable({
      AELtD<- c(1, 1, 1, 1, 
              exp(inputs$m_hr_rrms_discontinuation_fe["Cladribine O3.5","mean"]), 
              exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), 
              exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]), 
              exp(inputs$m_hr_rrms_discontinuation_fe["Ocrelizumab IV600","mean"]) )
      names(AELtD) <- v_treatment_names
      t(AELtD)
    })

    output$to_treatment_costs <- renderTable({
    inputs[["df_c_tx"]]
    },rownames=TRUE)
    
    output$uo_costs_readme <- renderUI({
      includeMarkdown("README.md")})
    
    output$to_administration_costs <- renderTable({
      inputs[["df_c_tx_admin"]]
      
    },rownames=TRUE)
    
    output$to_monitoring_costs <- renderTable({

      inputs[["df_c_tx_monitor"]]
      
    },rownames=TRUE)
    

            # },rownames=TRUE)
            

              observeEvent(input$update_trt_cost, {
                #update inputs: treatment costs
                 var_new = reactiveVal(input$ntc)
                inputs$df_c_tx["cost_annual_yr1", input$trt] <<- var_new()
                inputs$df_c_tx["cost_annual_yr2", input$trt] <<- var_new()


                #display updated treatment costs after assumption change
                output$to_treatment_costs <- 
                  renderTable({

                  inputs[["df_c_tx"]]

                },rownames=TRUE)

                
                showNotification("Custom treatment costs specified, look at Model inputs > Treatment Costs to view the changes", type = "message")

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






