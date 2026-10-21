#================= RRMS DES in WARDN Shiny APP ==================#
#======== Authored: by Ayman Sadek  ============================#
#======= ACM2 Committee preferred assumptions(Public Prices) =#


library(WARDEN)
library(MASS)
library(VGAM)
library(flexsurv)
library(tidyverse)
library(BCEA)
library(voi)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(DT)

##TODO

# Not sure why the model readme file is rendering within the treatment costs, to separate?
# add warning to update scenarios




rm(list = ls())
devtools::load_all()


# call model as function run once to load inputs for ui first (best to automate?)

rrmsdessetup(
  drc= 0.035, #3.5% discount rate for costs
  drq= 0.035, #3.5% discount rate for qalys
  time_horizon = 70, # number of years model is running for starting at intial age
  p_eid=0, # proportion of patients on external interval dosing
  jcvtest_cost= 0 #assumed free due to home service, test cost could be £247 privately but not offered in the NHS
  
)


# Shiny App User Interface
ui <- 
 
  #Project Page
  page_fillable(    
    
    title = "Project title",
    
    # set model assumptions needed for the UI to run
  
    #navigation bar style, could opt for navset_card_tab() or navset_card_pill()
    page_navbar(
      
      #theme = bs_theme(5, "flatly"),
      navbar_options = navbar_options(bg= "#daddf0", class = "bg-primary", theme = "light"),
      
      #Navigation bar title displays the logo on the top of all pages
      title = div(img(src="https://www.bristol.ac.uk/media-library/sites/populationhealth/images/TAG%20icon%20long%2016kb.png", 
                               id ="logo")),
    
      # 01  nav panel Project Background:
      nav_panel(  
        title =  "Project Background",
          card(
            card_body(
              markdown(uiOutput("uo_projectbackground"))),
            
          ),

      ),# nav panel 1 Project Background
      
      # 02 nav panel Introduction : displays model diagram and model description
      nav_panel(  
        title =  "Model Info",
        
        # side by side column layout:
       layout_column_wrap( 
         
         #model description and treatment sequence diagram:
         card(
           card_header(class = "bg-light text-primary","Model Description"),
           card_body(markdown(uiOutput("uo_modedescription"))),
           card_image("shiny/trtsq_diagram.png")
         ),
         #model diagram:
        card(
          card_header(class = "bg-light text-primary", "Model Diagram"),
          "Figure-1",
          card_image("shiny/model_diagram.png")
        )

        
      ) #Layout is column wrap
      ),# nav panel 1 Introduction
      

      #nav panel 3 Model inputs : tabs: treatment effects, treatment costs, admin & monitoring costs
      nav_panel(  
        title =  "Model Inputs",
        
        
        #03 nav panel for model inputs
        navset_card_underline(
          
          #02A nav panel for model inputs Relative Treatment Effects
          nav_panel(
            title = "Treatment Effects",
          layout_column_wrap( 
            width=1/2,
            
              # card body for CDP6
              card(
                   card_header(class="bg-light text-primary",
                               "Model inputs: Confirmed Disability progresssion at 6 months CDP6 Hazard ratios in comparison to Natalizumab as the reference treatment.
                               Model inputs assuming equivilence of Natalizumab originator, and biosimilar.
                          "),
              "Model inputs assuming equivilence of Ublituximab, ofatumumab and ocrelizumab",
              DTOutput(outputId = "to_CDP6"),
              ), #card body
          
            # card body for ARR
            card(
              card_header(class="bg-light text-primary",
                          "Model inputs: Annualized Relapse Rates ARR rate ratios in comparison to Natalizumab as the reference treatment.
                          Model inputs assuming equivilence of Natalizumab originator, and biosimilar.
                          "),
              "Model inputs assuming equivilence of Ublituximab and ocrelizumab" , 
                      DTOutput(outputId = "to_ARR"),
            ), #card body
            
            # card body for AELtD
            card(
              card_header(class="bg-light text-primary",
                          "Model inputs: Adverse Events Leading to Discontinuation AELtD  rate ratios in comparison to Natalizumab as the reference treatment. 
                          Model inputs assuming equivilence of Natalizumab originator, and biosimilar.
                          "),
                      "Model inputs assuming equivilence of Ublituximab, ofatumumab and ocrelizumab" , 
                      DTOutput(outputId = "to_AELtD"),
            ), #card body
            
            ), #layout column wrap
          #), #card 
            ), # navpanel tretament effects
          
          
          #02B nav panel for model inputs treatment costs
          nav_panel(
            title = "Treatment Costs",
            
           # card with header, card body x2, and footer 
            card(
              

            # card body for treatment costs
            card_body(height=250 ,
                      card_title(class="bg-light text-primary",
                                  "Model inputs: Total Annual Treatment costs per DMT"),
                      "Model inputs: Total annual DMT costs can be updated in place by selecting the individual cells in the table and enetring new values:",     
                  DTOutput(outputId = "to_treatment_costs"),
            ), #card body
           
            # card body for description of treatment costs
            card_body(height=250,
                      card_title(class="bg-light text-primary",
                                  "Background information on DMT costs"),
              markdown(uiOutput("uo_costs_readme")),
              ), # card body
            
            ) #card layout with header and footer
            ), #nav panel model inputs treatment costs
          
          #02C nav panel for model inputs admin costs
          nav_panel(
            title = "Administration & Monitoring Costs",
            #Output
            card(
              
            card_body(
              card_title(class="bg-light text-primary","Model inputs: Total annual treatment administration costs"),
              "Model inputs: Total annual treatment administration costs can be updated in place by selecting the individual cells in the table and enetring new values:",
              DTOutput(outputId = "to_administration_costs")),
            
            card_body(
              card_title(class="bg-light text-primary","Model inputs: Total annual treatment monitoring costs"),
              "Model inputs: Total annual treatment monitoring costs can be updated in place by selecting the individual cells in the table and enetring new values:",
              DTOutput(outputId = "to_monitoring_costs"))
        
            )
            ) #model inputs admin & monitoring costs
          
          
        )#navset card underline
      ), ##nav panel 2 Model inputs
      
      
      #04 nav panel Model set-up navigation panel gives model default settings and options for scenarios
      nav_panel(  
        title =  "Model settings and scenarios",
        
        layout_column_wrap( 
          card(
            card_body(
            card_title(class="bg-light text-primary", "Model default Settings apply independent of all model inputs & scenarios"),
            "Change default setting here:",
            
            #input for time horizon
            numericInput(
              inputId = "ni_time_horizon" , 
              label = "Time Horizon, number of years for the model to run:",
              value = 70, min=10, max=100, step=10),
            #input for discount rate
            numericInput(
              inputId = "ni_drc" , 
              label = "Discount rate for costs:",
              value = 0.035, min=0, max=1, step=0.01),
            numericInput(
              inputId = "ni_drq" , 
              label = "Discount rate for QALYs:",
              value = 0.035, min=0, max=1, step=0.01),
            
            "Click button to change default settings:",
            
            #button to run model
            actionButton(
              inputId = "ab_update_default_settings", label = "Update default settings")
            
            
            
            ), # card body
          ), #card
        
        card(
          #card body for EID assumption:
          card_body(
            card_title(class="bg-light text-primary",
                       "Scenarios are applied to default model inputs, they will relpace custom model inputs by the user"),
            "Model inputs assumption: the percentage of patitents reciving the 6-week interval dosing, this was assumed to be 60% in the appraisal
            It is set to be 0% by default in the model,",
            
            numericInput( 
              "n_p_eid", 
              "proportion of people on  extended interval dosing to be changed here:",
              value= 0.0, min = 0, max=1, step=0.01 ),
            
            "Model inputs assumption: manufacturer offering free JVC test as the test is not offered on the NHS, the test is avaible privately",
            
            numericInput( 
              "n_jcv_test_cost", 
              "JCV test cost could be changed here:",
              value= 0.0, min = 0, max=1000, step=1 ),
            
            actionButton("ab_update_scenarios", "Update the scenarios assumptions"),
            
            uiOutput("uo_eid")
          ), #card body
        ) #card
        
        ) #layout_column_wrap
      ), #model setup nav panel
      
      #05 MS-Model run and results navigation panel
      nav_panel(  
        
        title =  "MS-DES Model",
        #side bar panel has model setup options no. of patients, no. of simulation, and progress bar 
        layout_sidebar(
          #Side bar panel for model inputs
          sidebar= sidebar(
          accordion(  
            open=c("Willingness to Pay Threshhold (WTP)", "Simulation settings", "Run Model"),
            
            accordion_panel(
              "Willingness to Pay Threshhold (WTP)",
              
              numericInput(
                inputId = "ni_wtp1" , 
                label = "WTP lower bound",
                value = 25000, min=1, max=1000000, step=1000),
              #slider for number of simulations
              numericInput(
                inputId = "ni_wtp2" , 
                label = "WTP Upper bound",
                value = 35000, min=1, max=1000000, step=1000),
            ),
            
            accordion_panel(
              "Simulation settings",
             #slider for number of patients 
            numericInput(
              inputId = "si_n_patients" , 
              label = "Number of simulated patients",
              value = 10, min=1, max=1000, step=10),
            #slider for number of simulations
            numericInput(
              inputId = "si_n_samples" , 
              label = "Number of simulations to run",
              value = 100, min=1, max=1000, step=10),
            ),
            accordion_panel(
              "Run Model",
            #progress bar
            column(4,progressBar(id = "pb4", value = 0, display_pct = T)),
            #button to run model
            actionButton(
              inputId = "ab_run_model", label = "Run DES Model"),
            useShinyjs(),
            textOutput("progress_text")
            ),
            ), #accordion
          ), #sidebar 
          
          # sidebar= sidebar(
          #   title="other input",
          #   #slider for number of patients
          #   numericInput(
          #     inputId = "si_wtp" , 
          #     label = "willingness to pay threshold",
          #     value = 25000, min=1, max=1000000, step=1000)
          # ),
          
          #Card panels for model results
          navset_card_underline(

            
              nav_panel(  #04A
                title = "Cost Effectiveness Results Table",
                tableOutput(outputId = "to_results_table")),

              nav_panel(#04B
                title = "Cost Effectiveness Plane",
                plotOutput(outputId = "po_ce_plane")),

              nav_panel(#04C
                title = "Cost Effectiveness Acceptability Curve",
                plotOutput(outputId = "po_ceac")),
              
              nav_panel(#04D
                title = "Key Model Events",
                DTOutput(outputId = "po_keyevents")),
              
              nav_panel(#04E
                title = "Mortality Overtime",
                plotOutput(outputId = "po_mortality"))
            )#card
        ) #sidebar layout
      ),#Ms model results panel 
      
      #06 Value of Information Analysis and results navigation panel
      nav_panel(  
        
        title =  "Value of Information",
        #side bar panel has model setup options no. of patients, no. of simulation, and progress bar 
        layout_sidebar(
          title="",
          #Side bar panel for model inputs
          sidebar= sidebar(
            title="VOI settings",
            #progress bar
            column(4,progressBar(id = "pbvoi", value = 0, display_pct = T)),
            #button to run VOI model fitting
            actionButton(
              inputId = "ab_run_VOI", label = "VOI EVPPI"),
            useShinyjs(),
            textOutput("progress_text_voi")
          ), #sidebar panel
          
          
          #Card panels for model results
          navset_card_underline(
            #wtp25k
            nav_panel(
              title = paste("EVPPI at WTP £25,000"),
              card(
                height = 300,
                style = "resize:vertical;",
              card_body(  
                min_height = 250,
              plotOutput(outputId = "po_voi_25k_a"),
              plotOutput(outputId = "po_voi_25k_b")
              )# card body
              ) # card
              ), #wtp 25k
            
            #wtp 35k
            nav_panel(
              title = "EVPPI at WTP £35,000",
              card(
                height = 300,
                style = "resize:vertical;",
              card_body(
                min_height = 250,
              plotOutput(outputId = "po_voi_35k_a"),
              plotOutput(outputId = "po_voi_35k_b")
              ) # card body
              ) #card
              ) #wtp 35k
          )#card
        ) #sidebar layout
      )#VOI panel
      
    ) #Page navbar
    
  )# fillable page

# Shiny App Server
server <- 
  # server function with inputs and outputs
  function(input, output, session){
    
    # #render project background file
    output$uo_projectbackground <- renderUI({
      includeMarkdown("shiny/project background.md")
    })
    
    # #render model description file
    output$uo_modedescription <- renderUI({
      includeMarkdown("shiny/model description.md")
    })
    
    #initialize reactive variable with temp model inputs: 
    temp_cdp6 <- read.csv("data/Releff_RRMS_CDP6_FE.csv",header = TRUE, row.names=1)
    temp_cdp6 <- round(exp(temp_cdp6[c(3,9),c(3,5,9)]), digits=3)
    temp_arr <- read.csv("data/Releff_RRMS_ARR_FE.csv",header = TRUE, row.names=1)
    temp_arr <- round(exp(temp_arr[c(3,12, 13),c(3,5,9)]), digits=3)
    temp_discontinuation <- read.csv("data/Releff_RRMS_Discontinuation_FE.csv", header = TRUE, row.names=1)
    temp_discontinuation <- round(exp(temp_discontinuation[c(3,11),c(3,5,9)]), digits=3)
    
    temp_inputs <- reactiveValues(
      tx_costs={inputs[["df_c_tx"]]},
      tx_admin_costs={inputs[["df_c_tx_admin"]]},
      tx_monitor_costs={inputs[["df_c_tx_monitor"]]},
      cdp6={matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                     temp_cdp6["Cladribine O3.5","mean"], temp_cdp6["Cladribine O3.5","X2.5."], temp_cdp6["Cladribine O3.5","X97.5."],
                     temp_cdp6["Ocrelizumab IV600","mean"], temp_cdp6["Ocrelizumab IV600","X2.5."], temp_cdp6["Ocrelizumab IV600","X97.5."],
                     temp_cdp6["Ocrelizumab IV600","mean"], temp_cdp6["Ocrelizumab IV600","X2.5."], temp_cdp6["Ocrelizumab IV600","X97.5."],
                     temp_cdp6["Ocrelizumab IV600","mean"], temp_cdp6["Ocrelizumab IV600","X2.5."], temp_cdp6["Ocrelizumab IV600","X97.5."])
                   , byrow = TRUE, nrow=length(v_treatment_names),
                   dimnames=list(c(v_treatment_names),c("CDP6 Hazard Ratio", " (95%", "CrI) "))
                   )},
      arr={matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    temp_arr["Cladribine O3.5","mean"], temp_arr["Cladribine O3.5","X2.5."], temp_arr["Cladribine O3.5","X97.5."],
                    temp_arr["Ocrelizumab IV600","mean"], temp_arr["Ocrelizumab IV600","X2.5."], temp_arr["Ocrelizumab IV600","X97.5."],
                    temp_arr["Ofatumumab SC20","mean"], temp_arr["Ofatumumab SC20","X2.5."], temp_arr["Ofatumumab SC20","X97.5."],
                    temp_arr["Ocrelizumab IV600","mean"], temp_arr["Ocrelizumab IV600","X2.5."], temp_arr["Ocrelizumab IV600","X97.5."])
                  , byrow = TRUE, nrow=length(v_treatment_names),
                  dimnames=list(c(v_treatment_names),c("ARR Rate Ratio", " (95%", "CrI) "))
                  )},
      AELtD={matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      temp_discontinuation["Cladribine O3.5","mean"], temp_discontinuation["Cladribine O3.5","X2.5."], temp_discontinuation["Cladribine O3.5","X97.5."],
                      temp_discontinuation["Ocrelizumab IV600","mean"], temp_discontinuation["Ocrelizumab IV600","X2.5."], temp_discontinuation["Ocrelizumab IV600","X97.5."],
                      temp_discontinuation["Ocrelizumab IV600","mean"], temp_discontinuation["Ocrelizumab IV600","X2.5."], temp_discontinuation["Ocrelizumab IV600","X97.5."],
                      temp_discontinuation["Ocrelizumab IV600","mean"], temp_discontinuation["Ocrelizumab IV600","X2.5."], temp_discontinuation["Ocrelizumab IV600","X97.5."])
                    , byrow = TRUE, nrow=length(v_treatment_names),
                    dimnames=list(c(v_treatment_names),c("CDP6 Hazard Ratio", " (95%", "CrI) "))
                  )}
      
    )

    #display model input in tables with editable fields 
    output$to_treatment_costs <- renderDT({datatable(temp_inputs$tx_costs, editable = TRUE, options = list(dom = 't'))})
    output$uo_costs_readme <- renderUI({includeMarkdown("shiny/treatmentcostdescription.md")})
    
    output$to_administration_costs <- renderDT({datatable(temp_inputs$tx_admin_costs, editable = TRUE, options = list(dom = 't'))})
    output$to_monitoring_costs <- renderDT({datatable(temp_inputs$tx_monitor_costs, editable = TRUE, options = list(dom = 't'))})
    
    output$to_CDP6 <- renderDT({datatable(temp_inputs$cdp6, editable = FALSE, options = list(dom = 't'))})
    output$to_ARR <-renderDT({datatable(temp_inputs$arr, editable = FALSE, options = list(dom = 't'))})
    output$to_AELtD <-renderDT({datatable(temp_inputs$AELtD, editable = FALSE, options = list(dom = 't'))})
    

            # reactive table allowing for the direct edit of model inputs
              observeEvent(input$to_treatment_costs_cell_edit, {
                #assign table rows columns and values
                info = input$to_treatment_costs_cell_edit
                i = as.numeric(info$row)
                j = as.numeric(info$col)
                k = as.numeric(info$value)
                #write user values to reactive
                temp_inputs$tx_costs[i,j] <- k
                #write back the data to model inputs
                inputs[["df_c_tx"]] <<- temp_inputs$tx_costs
                # custom message confirming action
                showNotification("Custom treatment costs specified", type = "message")
              })
              
              observeEvent(input$to_administration_costs_cell_edit, {
                #assign table rows columns and values
                info = input$to_administration_costs_cell_edit
                i = as.numeric(info$row)
                j = as.numeric(info$col)
                k = as.numeric(info$value)
                #write user values to reactive
                temp_inputs$tx_admin_costs[i,j] <- k
                #write back the data to model inputs
                inputs[["df_c_tx_admin"]] <<- temp_inputs$tx_admin_costs
                # custom message confirming action
                showNotification("Custom adminstration costs specified", type = "message")
              })
              
              observeEvent(input$to_monitoring_costs_cell_edit, {
                #assign table rows columns and values
                info = input$to_monitoring_costs_cell_edit
                i = as.numeric(info$row)
                j = as.numeric(info$col)
                k = as.numeric(info$value)
                #write user values to reactive
                temp_inputs$tx_monitor_costs[i,j] <- k
                #write back the data to model inputs
                inputs[["df_c_tx_monitor"]] <<- temp_inputs$tx_monitor_costs
                # custom message confirming action
                showNotification("Custom monitoring costs specified", type = "message")
              })
              
              # reactive update default settings
              observeEvent(input$ab_update_default_settings, {
                
                # new default settings
                new_time_horizon = reactiveVal(input$ni_time_horizon)
                new_drc = reactiveVal(input$ni_drc)
                new_drq = reactiveVal(input$ni_drq)
                
                # save new default setting to model inputs
                inputs[["time_horizon"]] <<- new_time_horizon()
                inputs[["drc"]] <<- new_drc()
                inputs[["drq"]] <<- new_drq()

                showNotification("default settings updated", type = "message")
                

              }) #reactive update default settings
    
            # reactive update of scenarios
              observeEvent(input$ab_update_scenarios, {
                #update scenarios: p_eid
                var_new1 = reactiveVal(input$n_p_eid)
                #update JCV test cost
                var_new2 = reactiveVal(input$n_jcv_test_cost)
                
                #reload model inputs with scenarios
                rrmsdessetup(  drc=inputs[["drc"]], #default or updated default setting
                               drq=inputs[["drq"]], #default or updated default setting
                               time_horizon=inputs[["time_horizon"]], #default or updated default setting
                               p_eid=var_new1(), #new eid scenario
                               jcvtest_cost= var_new2() #new jcv scenario
                               ) #call in the inputs for the app
                
                #display updated  costs after assumption change
                output$to_treatment_costs <- 
                  renderDT({datatable(inputs[["df_c_tx"]], editable = TRUE, options = list(dom = 't'))})
                output$to_administration_costs <- 
                  renderDT({datatable(inputs[["df_c_tx_admin"]], editable = TRUE, options = list(dom = 't'))})
                output$to_monitoring_costs <- 
                  renderDT({datatable(inputs[["df_c_tx_monitor"]], editable = TRUE, options = list(dom = 't'))})

                
                showNotification("Scenarios  updated, look at Model inputs to view the changes", type = "message")
                
              }) #reactive update of scenarios
                
    
    #Action triggered by run model button pressed
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
        
        n_patients<- input$si_n_patients
        n_samples <- input$si_n_samples
        # Summarise the results
        
        dim_results <- c(n_samples, n_treatments, n_patients)
        trt_idx <- setNames(seq_along(v_treatment_names), v_treatment_names)
        total_costs_ipd <- total_qalys_ipd <-  array(
          NA_real_,
          dim = dim_results,
          dimnames = list(NULL, v_treatment_names, NULL)
        )
        
        for (i_sample in seq_len(n_samples)) {
          
          df <- results[[1]][[i_sample]]$merged_df
          
          # Keep only final ("death") rows
          df <- df[df$evtname == "death", ]
          
          idx <- cbind(
            i_sample,
            trt_idx[df$arm],
            df$pat_id
          )
          
          total_costs_ipd[idx]         <- df$total_costs
          total_qalys_ipd[idx]         <- df$total_qalys
        }
        
        options(scipen = 999)
        total_costs <<- apply(total_costs_ipd, c(1, 2), mean)
        total_qalys <<- apply(total_qalys_ipd, c(1, 2), mean)

        
        #drop natalizumab-biosimilar-SC and cladrebiene
        total_costs <<- total_costs[,-c(4,5)]
        total_qalys <<- total_qalys[,-c(4,5)]
        
        results_table <- summarise_ce_results(wtp1 = input$ni_wtp1, wtp2 = input$ni_wtp2,
                                              total_qalys = total_qalys, 
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
        ceplane.plot(des_bcea, wtp = input$ni_wtp2,
                     graph = "ggplot2")
      })
      
      output$po_ceac <-  renderPlot({
        
        #plot a CEAC
        des_multi_ce <- multi.ce(des_bcea)
        
        ceac.plot(des_multi_ce, graph = "ggplot2",
                  line = list(color = c("darkorange", "palegreen3", "blue", "grey",
                                        "cyan3", "black"),
                              size=2 ),
                  xlim=c(-2, 2),
                  pos = c(1, 1)
        )
        
      })
      
      output$po_keyevents<- renderDT({
        
        #compare key events by treatment groups
        modelresults_summarised <<- summary_results_sim(results[[1]])
        proportion_spms  <<- modelresults_summarised["spms_flag",]
        average_numberof_relapses <<- modelresults_summarised["n_relapse",] 
        average_numberof_edssincreases <<- modelresults_summarised["n_edssinc_evts",] 
        average_numberof_edssdecreases<<- modelresults_summarised["n_edssdec_evts",] 
        average_numberof_saes<<- modelresults_summarised["n_sae_evts",] 
        final_edss_value <<- modelresults_summarised["edss_value",]
        average_numberof_treatmentswitches <<- modelresults_summarised["n_switch",] 
        average_numberof_yearsontreatment <<- modelresults_summarised["years_on_treatment",] 
        average_numberof_yearson2ndline<<- modelresults_summarised["t_2ndline",] 
        average_numberof_yearson3rdline<<- modelresults_summarised["t_3rdline",] 
        average_numberof_yearson4thline<<- modelresults_summarised["t_4thline",]
        
        key_events_trt_grps <<- as.data.frame(rbind(
          proportion_spms,   average_numberof_relapses, 
          average_numberof_edssincreases,   average_numberof_edssdecreases, 
          final_edss_value,   average_numberof_saes,
          average_numberof_treatmentswitches,  average_numberof_yearsontreatment,
          average_numberof_yearson2ndline,  average_numberof_yearson3rdline,
          average_numberof_yearson4thline),
          row.names=c("proportion with spms", "average no. of relapses", 
                      "average no. of edss increases", "average no. of edss decreases", 
                      "final edss value", "average no. of SAEs",
                      "average no. of treatment switches", "average no. of years on treatment",
                      "average no. of years on 2ndline", "average no. of years on 3rdline",
                      "average no. of years on 4thline")
        )
        
        datatable(key_events_trt_grps, editable = FALSE, options = list(dom = 't'))
        
        
      })
      
      output$po_mortality <- renderPlot({
        ## mortality by treatment group
        death_ipd<- array(NA, dim = c(n_samples, n_treatments, n_patients),
                          dimnames = list(NULL, v_treatment_names, NULL))
        
        # Below is not very efficient but only needs to be run once at end of simulation
        #  results[[1]][[i_sample]] and $arm works for WARDEN to run for DESCEM replace  with results$output_psa[[i_sample]] and $trt
        for(treatment_name in v_treatment_names) {
          for(i_sample in 1:n_samples) {
            for(i_patient in 1:n_patients) {
              # With statement is neater but much slower
              temp_index <- results[[1]][[i_sample]]$merged_df$pat_id == i_patient &
                results[[1]][[i_sample]]$merged_df$arm == treatment_name &
                results[[1]][[i_sample]]$merged_df$evtname == "death" # condition on death to count final n relapses and edss value
              
              
              death_ipd[i_sample, treatment_name, i_patient] <-
                results[[1]][[i_sample]]$merged_df$evttime[temp_index]
              
            } # End loop over patients
            # Summarise for just this sample
            
          }
        }
        # Sort death times ascending
        for (i_sample in 1:n_samples){
          for(treatment_name in v_treatment_names){
            death_ipd[i_sample, treatment_name,] <- sort(death_ipd[i_sample, treatment_name,])
          }}
        
        
        # save event times in a list
        for (i in 1:n_treatments){
          assign(paste("V", i, sep=""), matrix(rep(0, n_patients* n_samples) , nrow = n_patients, ncol =  n_samples))
          assign(paste("V", i, sep=""), tibble::as_tibble(death_ipd[,i,]))
          
        }
        
        xyz<-list(V1, V2, V3, V4, V5, V6, V7, V8) #xyz <- list(c(noquote(paste("V", 1:n_treatments, "," , sep="")),n_treatments)) 
        remove(V1, V2, V3, V4, V5, V6, V7, V8)
        
        library(tidyverse)
        #model the overall survial 
        for (i in 1:length(xyz)){
          xyz[[i]] <- data.frame(xyz[[i]]) %>%
            mutate(event = 1) %>%
            pivot_longer(cols=starts_with("V"), values_to = "time")
          xyz[[i]] <- data.frame(xyz[[i]][-2])
          xyz[[i]] <- sort_by.data.frame(xyz[[i]], xyz[[i]][["time"]])
          xyz[["model"]][[i]] <- survival::survfit(Surv(time, event)~1, data =xyz[[i]] , type="kaplan-meier")
          xyz[["plot"]][[i]] <- data.frame(xyz[["model"]][[i]][["time"]], xyz[["model"]][[i]][["surv"]])
          #xyz[[i]] <- xyz[[i]]
        }
        
        
        # plot overall survival by treatment
        plot_mortality<- 
          ggplot()+
          geom_step(size=1)+
          geom_step(data=xyz[["plot"]][[1]], aes(xyz[["plot"]][[1]][[1]], xyz[["plot"]][[1]][[2]], col=v_treatment_names[1]), size=1) +
          geom_step(data=xyz[["plot"]][[2]], aes(xyz[["plot"]][[2]][[1]], xyz[["plot"]][[2]][[2]], col=v_treatment_names[2]), size=1)+
          geom_step(data=xyz[["plot"]][[3]], aes(xyz[["plot"]][[3]][[1]], xyz[["plot"]][[3]][[2]], col=v_treatment_names[3]), size=1)+
          geom_step(data=xyz[["plot"]][[4]], aes(xyz[["plot"]][[4]][[1]], xyz[["plot"]][[4]][[2]], col=v_treatment_names[4]), size=1)+
          geom_step(data=xyz[["plot"]][[5]], aes(xyz[["plot"]][[5]][[1]], xyz[["plot"]][[5]][[2]], col=v_treatment_names[5]), linetype ="longdash")+
          geom_step(data=xyz[["plot"]][[6]], aes(xyz[["plot"]][[6]][[1]], xyz[["plot"]][[6]][[2]], col=v_treatment_names[6]))+
          geom_step(data=xyz[["plot"]][[7]], aes(xyz[["plot"]][[7]][[1]], xyz[["plot"]][[7]][[2]], col=v_treatment_names[7]), linetype ="dotted")+
          geom_step(data=xyz[["plot"]][[8]], aes(xyz[["plot"]][[8]][[1]], xyz[["plot"]][[8]][[2]], col=v_treatment_names[8]), linetype ="dotdash") +
          labs(title ="Predicted mortality over time - HARRMS Model ", y = "Survival (probability)", x = "Time (years)") +
          xlim(0,65) +
          ylim(0,1) +
          theme_bw() +
          theme(legend.position = "bottom")
        
        plot_mortality
      })
      
      
      
    })
    
    #Action triggered by VOI button pressed
    observeEvent(input$ab_run_VOI,  {
      withCallingHandlers({
        
        html("progress_text_voi", "")
        
      # Total EVPPI
      # All  parameters excluding hazard ratios that are constant as it causes errors
      df_inputs <<- as.data.frame(results[["input_matrix"]][,-c(73:76,81:84,89:92,79:101)])
      parameters <<- as.list(colnames(df_inputs))
      
      total_evppi <<- voi::evppi(outputs = list(c = total_costs,
                                               e = total_qalys,
                                               k = c(input$ni_wtp1, input$ni_wtp2)),
                                inputs = df_inputs,
                                pars = parameters,
                                method = "bart",
                                verbose = TRUE)
      
      },
      message = function(m) {
        html(id = "progress_text_voi", html = paste0(m$message, '<br>'), add = TRUE)
        i<<- ifelse(exists("i"), (i+1) , 1 )
        updateProgressBar(session = session, id = "pbvoi", value = (
          ((i/input$si_n_samples))*10 # this is not correct
          #(round(i/sum(dim(df_inputs)))-1)*10
          ) )
      }
      )
      
      
      
      #VOI plots
      
      output$po_voi_25k_a <- renderPlot({
        #plot results
        total_evppi_25k_a <<- total_evppi$evppi[total_evppi$k ==input$ni_wtp1 & total_evppi$evppi > mean(total_evppi$evppi)]
        names(total_evppi_25k_a) <<- total_evppi$pars[total_evppi$k ==input$ni_wtp1 & total_evppi$evppi > mean(total_evppi$evppi)]
        par(las=1, bg="white", mai=c(0.85, 4, 0, 0.5))
        barplot(
          sort(total_evppi_25k_a, decreasing=FALSE), horiz = TRUE, cex.names=0.9, xlab= "£"
        )
      })
      
      output$po_voi_25k_b <- renderPlot({
        #plot results
        total_evppi_25k_b <<- total_evppi$evppi[total_evppi$k ==input$ni_wtp1 & total_evppi$evppi < mean(total_evppi$evppi)]
        names(total_evppi_25k_b) <<- total_evppi$pars[total_evppi$k ==input$ni_wtp1 & total_evppi$evppi < mean(total_evppi$evppi)]
        par(las=1, bg="white", mai=c(0.85, 4, 0, 0.5))
        barplot(
          sort(total_evppi_25k_b, decreasing=FALSE), horiz = TRUE, cex.names=0.6, xlab= "£"
        )
      })
      
      output$po_voi_35k_a <- renderPlot({
        #plot results
        total_evppi_35k_a <<- total_evppi$evppi[total_evppi$k ==input$ni_wtp2 & total_evppi$evppi > mean(total_evppi$evppi)]
        names(total_evppi_35k_a) <<- total_evppi$pars[total_evppi$k ==input$ni_wtp2 & total_evppi$evppi > mean(total_evppi$evppi)]
        par(las=1, bg="white", mai=c(0.85, 4, 0, 0.5))
        barplot(
          sort(total_evppi_35k_a, decreasing=FALSE), horiz = TRUE, cex.names=0.6, xlab= "£"
        )
      })
      
      output$po_voi_35k_b <- renderPlot({
        #plot results
        total_evppi_35k_b <<- total_evppi$evppi[total_evppi$k ==input$ni_wtp2 & total_evppi$evppi < mean(total_evppi$evppi)]
        names(total_evppi_35k_b) <<- total_evppi$pars[total_evppi$k ==input$ni_wtp2 & total_evppi$evppi < mean(total_evppi$evppi)]
        par(las=1, bg="white", mai=c(0.85, 4, 0, 0.5))
        barplot(
          sort(total_evppi_35k_b, decreasing=FALSE), horiz = TRUE, cex.names=0.6, xlab= "£"
        )
      })
      
    })
    
  }

# RUN APP
shinyApp(ui, server)






