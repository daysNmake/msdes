# simple

## Introduction

This is a simple overview of the internal workings of the MS discrete
event simulation model. Refer to articles for mode detailed
explanations.

### Main options

Load the required packages

``` r

library(msdes)
library(WARDEN)
library(MASS)
library(VGAM)
#> Loading required package: stats4
#> Loading required package: splines
library(utils)
library(flexsurv)
#> Loading required package: survival
#> 
#> Attaching package: 'flexsurv'
#> The following objects are masked from 'package:VGAM':
#> 
#>     AICc, dgompertz, pgompertz, qgompertz, rgompertz
library(hesim)
library(purrr)
library(rlang)
#> 
#> Attaching package: 'rlang'
#> The following objects are masked from 'package:purrr':
#> 
#>     flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl,
#>     flatten_raw, invoke, splice
library(progressr)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:rlang':
#> 
#>     :=
#> The following object is masked from 'package:purrr':
#> 
#>     transpose
#> The following object is masked from 'package:base':
#> 
#>     %notin%
library(BCEA)
#> Registered S3 method overwritten by 'BCEA':
#>   method     from
#>   plot.evppi voi
#> The BCEA version loaded is: 2.4.83 (stable version)
#> A development version is available at https://giabaio.r-universe.dev/BCEA
#> 
#> Attaching package: 'BCEA'
#> The following object is masked from 'package:graphics':
#> 
#>     contour
library(voi)
#> 
#> Attaching package: 'voi'
#> The following object is masked from 'package:BCEA':
#> 
#>     evppi
```

### model setup

Treatment names, treatment lines, MS severity states and DES key events
are specified.

``` r


## Treatments at second line 
v_treatment_names <- c("Natalizumab-IV",
                       "Natalizumab-SC",
                       "Natalizumab biosimilar-IV",
                       "Natalizumab biosimilar-SC",
                       "Cladribine", 
                       "Ublituximab" ,
                       "Ofatumumab",
                       "Ocrelizumab")

## number of treatments
n_treatments <- length(v_treatment_names)

## specify codes for the treatments
v_treatment_codes <- c(seq(1, n_treatments))
names(v_treatment_codes) <- v_treatment_names

## Treatments that patients can move to at 3rd and higher line 
v_higher_line_treatments <- c("Natalizumab-IV",
                              "Natalizumab-SC",
                              "Natalizumab biosimilar-IV",
                              "Natalizumab biosimilar-SC",
                              "Cladribine",
                              "Ublituximab",
                              "Ofatumumab",
                              "Ocrelizumab"
                              )
v_higher_line_treatment_codes <- c(seq(1, length(v_higher_line_treatments)))
names(v_higher_line_treatment_codes) <- v_higher_line_treatments

## prior treatment index
v_prior_treatment_indices <- c(rep(0, length(v_higher_line_treatments)))
names(v_prior_treatment_indices) <- v_higher_line_treatments

## Model event states
v_event_names <- c("initialise","age" , "relapse", "edss_increase", "edss_decrease", "progression", "treatment_switch", "annual_treatment", "serious_adverse_event", "death"
                   )

## Define the possible EDSS values
v_edss_values <- c(0:9)
names(v_edss_values) <- paste("EDSS", v_edss_values)

# Number of EDSS categories
n_edss_values <- length(v_edss_values)
```

### Load Data

Data is loaded in the function
\[[`gen_inputs()`](https://daysnmake.github.io/msdes/reference/gen_inputs.md)\]
below is the default data are contained within this function

``` r


# saves the inputs in a list
inputs <- gen_inputs(
  # change model settings or assumptions used in scenario analyses
  drc= 0.035, #3.5% discount rate for costs
  drq= 0.035, #3.5% discount rate for qalys
  time_horizon = 70, # number of years model is running for starting at intial age
  p_eid=0.6, # proportion of patients on external interval dosing
  jcvtest_cost= 0 #assumed free due to home service, test cost could be £247 privately but not offered in the NHS
                     )

# Global parameters of the model as they are common to all patients and treatments
indicators <-1
common_all_inputs <- gen_common_all_inputs()

# patient parameters reflecting heterogeneous characteristics that depend on treatment
unique_pt_inputs <- gen_unique_pt_inputs()

# Generate initial events and reaction events
init_event_list <- gen_init_events(RRMS_events=v_event_names)
evt_react_list <- gen_react_events(RRMS_events=v_event_names)

#utilities and costs
util_ongoing <- "utilities_disease"
cost_ongoing <- "costs_disease"
util_instant <- c("utilities_relapse", "utilities_sae")
cost_instant <- c("costs_treatment", "costs_relapse", "costs_sae")
```

### Run Model

Model is run

``` r


## set a seed 
n_seed<-243523452
set.seed(n_seed)

## number of patients and samples
n_patients <- 10
n_samples <- 10


# Run the simulation
environment(run_sim_new) <- asNamespace("WARDEN") 

total_runtime <- system.time({
  results <- run_sim_new(  
    npats = n_patients, 
    n_sim = n_samples,        # number of simulations to run
    psa_bool = TRUE,            # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
    debug = FALSE,
    sensitivity_bool= FALSE,                  # sensitivity analysis not implemented,
    arm_list = v_treatment_names,             # intervention list
    common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
    #common_pt_inputs = common_pt_inputs,     # inputs that change within a simulation but are not affected by the intervention
    unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
    init_event_list = init_event_list,        # initial event list
    evt_react_list = evt_react_list,          # reaction of events
    util_ongoing_list = util_ongoing,         # ongoing rrms or spms disease utilities
    cost_ongoing_list = cost_ongoing,         # ongoing rrms or spms disease costs
    util_instant_list = util_instant,
    cost_instant_list = cost_instant,         # instantaneous treatment costs
    input_out=c("age", "sex", "spms_flag", "edss_value", "n_relapse", "n_sae_evts", "n_edssinc_evts", "n_edssdec_evts", "current_treatment_code", "treatment_line", "n_switch", "years_on_treatment", "treatment_stop", 
                "t_2ndline", "t_3rdline", "t_4thline", "waning"), # patient history to monitor
     save_input_matrix = TRUE # Export the input matrix for EVPPI
  )
})
#> Analysis number: 1
#> Simulation number: 1
#> Time to run simulation 1: 0.72s
#> Simulation number: 2
#> Time to run simulation 2: 0.82s
#> Simulation number: 3
#> Time to run simulation 3: 0.72s
#> Simulation number: 4
#> Time to run simulation 4: 0.72s
#> Simulation number: 5
#> Time to run simulation 5: 0.79s
#> Simulation number: 6
#> Time to run simulation 6: 0.98s
#> Simulation number: 7
#> Time to run simulation 7: 0.75s
#> Simulation number: 8
#> Time to run simulation 8: 0.68s
#> Simulation number: 9
#> Time to run simulation 9: 0.73s
#> Simulation number: 10
#> Time to run simulation 10: 0.76s
#> Time to run analysis 1: 7.68s
#> Total time to run: 7.68s
#> Simulation finalized;
```

### Plot Results

``` r

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
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)


#drop natalizumab-biosimilar-SC and cladrebiene
total_costs <- total_costs[,-c(4,5)]
total_qalys <- total_qalys[,-c(4,5)]

results_table <- summarise_ce_results(wtp1 = 25000, wtp2 = 35000,
                                      total_qalys = total_qalys, 
                                      total_costs = total_costs,
                                      reference = "Natalizumab-IV")

knitr::kable(results_table) 
```

|  | Total costs (95% CrI) | Total QALYs (95% CrI) | Net benefit at £25000/QALY (95% CrI) | Net benefit at £35000/QALY (95% CrI) | Incremental costs (95% CrI) | Incremental QALYs (95% CrI) | INB at £25000/QALY (95% CrI) | INB at £35000/QALY (95% CrI) | CEAC at £25000/QALY (95% CrI) | CEAC at £35000/QALY (95% CrI) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Natalizumab-IV | 351,983.65 (307,662.42, 406,091.90) | 9.53 (7.81, 11.37) | -113,842.77 (-174,017.68, -26,843.39) | -18,586.42 (-92,828.03, 85,484.22) | 0.00 (0.00, 0.00) | 0.00 (0.00, 0.00) | 0.00 (0.00, 0.00) | 0.00 (0.00, 0.00) | 0.2 | 0.3 |
| Natalizumab-SC | 307,119.99 (254,825.07, 349,972.93) | 8.79 (6.42, 10.81) | -87,475.73 (-130,088.51, -16,054.09) | 381.97 (-57,166.74, 88,724.47) | -44,863.65 (-117,556.93, 23,377.02) | -0.74 (-3.16, 1.56) | 26,367.04 (-19,743.08, 65,111.96) | 18,968.39 (-49,966.19, 74,651.46) | 0.3 | 0.2 |
| Natalizumab biosimilar-IV | 335,639.43 (300,172.95, 366,383.61) | 9.37 (5.17, 13.02) | -101,346.79 (-203,893.87, -24,260.76) | -7,629.74 (-152,180.34, 104,096.81) | -16,344.21 (-63,438.11, 30,093.22) | -0.15 (-3.37, 3.64) | 12,495.98 (-38,068.58, 85,145.35) | 10,956.69 (-68,749.69, 121,517.63) | 0.2 | 0.2 |
| Ublituximab | 358,531.30 (320,909.58, 382,255.80) | 9.52 (6.39, 12.86) | -120,565.32 (-200,000.73, -56,702.17) | -25,378.93 (-136,137.79, 68,986.73) | 6,547.66 (-39,880.26, 56,276.70) | -0.007 (-2.99, 3.24) | -6,722.55 (-77,678.40, 94,597.22) | -6,792.51 (-104,922.20, 124,893.82) | 0.3 | 0.3 |
| Ofatumumab | 376,459.79 (333,842.01, 424,864.00) | 8.88 (7.22, 10.29) | -154,457.35 (-231,609.74, -80,398.90) | -65,656.38 (-156,666.10, 21,522.09) | 24,476.14 (-37,033.99, 65,102.60) | -0.65 (-1.87, 0.46) | -40,614.58 (-88,075.19, 21,261.62) | -47,069.95 (-104,895.79, 16,015.75) | 0 | 0 |
| Ocrelizumab | 398,560.74 (342,704.38, 444,491.77) | 9.44 (6.78, 12.89) | -162,498.18 (-219,503.94, -102,139.76) | -68,073.15 (-147,570.83, 21,565.49) | 46,577.10 (-2,960.08, 108,924.42) | -0.083 (-1.78, 3.16) | -48,655.40 (-116,194.26, 9,154.72) | -49,486.73 (-123,248.85, 40,781.92) | 0 | 0 |

``` r

#write.csv(results_table, paste0(c("results/results warden determenistic", n_samples, "x", n_patients, ".csv"), collapse = " "))

#plot results
v_results_treatment_names<- v_treatment_names[-c(4,5)]

# Create a BCEA object
des_bcea <- bcea(eff = total_qalys,
                 cost = total_costs,
                 ref = 1,
                 interventions = v_results_treatment_names)
ceplane.plot(des_bcea, wtp = 35000,
             graph = "ggplot2")
```

![](simple_files/figure-html/plot_results-1.png)

``` r


#plot a CEAC
des_multi_ce <- multi.ce(des_bcea)
ceac.plot(des_multi_ce, graph = "ggplot2",
          line = list(color = c("darkorange", "palegreen3", "blue", "grey",
                                "cyan3", "black"),
                      size=2 ),
          #ylim=c(0, 0.5),
          pos = c(1, 1)
)
```

![](simple_files/figure-html/plot_results-2.png)

``` r




#compare key events by treatment groups
modelresults_summarised <- summary_results_sim(results[[1]])
proportion_spms  <- modelresults_summarised["spms_flag",]
average_numberof_relapses <- modelresults_summarised["n_relapse",] 
average_numberof_edssincreases <- modelresults_summarised["n_edssinc_evts",] 
average_numberof_edssdecreases<- modelresults_summarised["n_edssdec_evts",] 
average_numberof_saes<- modelresults_summarised["n_sae_evts",] 
final_edss_value <- modelresults_summarised["edss_value",]
average_numberof_treatmentswitches <- modelresults_summarised["n_switch",] 
average_numberof_yearsontreatment <- modelresults_summarised["years_on_treatment",] 
average_numberof_yearson2ndline<- modelresults_summarised["t_2ndline",] 
average_numberof_yearson3rdline<- modelresults_summarised["t_3rdline",] 
average_numberof_yearson4thline<- modelresults_summarised["t_4thline",]

key_events_trt_grps <- as.data.frame(rbind(
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
knitr::kable(key_events_trt_grps)
```

|  | Natalizumab-IV | Natalizumab-SC | Natalizumab biosimilar-IV | Natalizumab biosimilar-SC | Cladribine | Ublituximab | Ofatumumab | Ocrelizumab |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| proportion with spms | 0.29 (0.1; 0.5) | 0.32 (0.1; 0.6) | 0.35 (0.1; 0.8) | 0.26 (0; 0.6) | 0.35 (0.2; 0.7) | 0.34 (0.1; 0.5) | 0.35 (0.2; 0.5) | 0.27 (0; 0.5) |
| average no. of relapses | 2.48 (1; 4.3) | 2.13 (1.2; 3.7) | 2.51 (0.8; 4.1) | 2.23 (0.9; 3.2) | 2.51 (1.3; 3.7) | 2.25 (0.7; 3.4) | 2.5 (1; 4.3) | 2.57 (1.3; 5.2) |
| average no. of edss increases | 2.66 (1.5; 4) | 2.31 (1; 4.1) | 2.27 (1.1; 3.8) | 2.58 (1.2; 4.6) | 3 (1.2; 6.3) | 2.71 (1.1; 3.8) | 2.39 (0.9; 4.2) | 2.34 (1.4; 3.8) |
| average no. of edss decreases | 1.74 (1.4; 2.3) | 1.59 (1.2; 2) | 1.5 (1; 1.9) | 1.66 (1.4; 2) | 1.63 (1; 2.8) | 1.6 (1.1; 2.2) | 1.55 (1; 2.2) | 1.65 (1; 2.3) |
| final edss value | 4.65 (3; 5.6) | 4.61 (3.2; 6.2) | 4.7 (3.7; 6.4) | 4.69 (2.7; 6) | 5.18 (4; 6.7) | 4.77 (2.9; 5.9) | 4.76 (3.9; 5.9) | 4.6 (3.1; 6) |
| average no. of SAEs | 4.09 (2.3; 7.2) | 3.58 (2.8; 7) | 4.65 (2.6; 6.6) | 4.1 (3.2; 5.9) | 4.12 (2.7; 6.8) | 4.02 (2.5; 7.4) | 7.42 (3.4; 20.8) | 4.39 (2.4; 6.4) |
| average no. of treatment switches | 1.11 (0.4; 1.9) | 1.19 (0.6; 1.8) | 1.12 (0.6; 1.9) | 1.17 (0.6; 1.8) | 1.14 (0.6; 1.7) | 1.12 (0.4; 1.7) | 1.25 (0.5; 2.2) | 1.11 (0.6; 1.9) |
| average no. of years on treatment | 20.75 (15.1; 30.7) | 18.36 (8.7; 24.5) | 20.85 (11.9; 35.7) | 19.54 (12.6; 31.1) | 15.02 (3.3; 23.8) | 18.33 (9.9; 25.1) | 19.33 (10.3; 23.1) | 18.19 (11.4; 24.7) |
| average no. of years on 2ndline | 19.43 (11.3; 32.4) | 16.49 (10; 24.7) | 17.65 (6.4; 32.8) | 18.79 (13.1; 28.5) | 14.58 (7.7; 19.7) | 17.37 (7.5; 26.9) | 15.58 (10.1; 21.2) | 18.06 (9; 28.1) |
| average no. of years on 3rdline | 6.97 (1.4; 12.2) | 5.53 (2.2; 9.9) | 5.85 (2; 14.4) | 6.26 (0.6; 14.3) | 5.36 (1.6; 13.9) | 6.5 (2.8; 14.8) | 6.17 (2.5; 15.8) | 6.75 (2.9; 11.4) |
| average no. of years on 4thline | 4.78 (0.9; 12.2) | 5.27 (0; 15.9) | 4 (0; 9.8) | 4.83 (1.9; 8) | 3.6 (0.1; 7.4) | 4.26 (0; 7) | 5.21 (0; 12.2) | 4.34 (0.2; 7) |

``` r



#  VOI analysis
#total EVPI

total_evpi <- voi::evpi(outputs = list(c = total_costs,
                                       e = total_qalys,
                                       k = c(0, 25000, 50000, 75000, 100000)))

knitr::kable(total_evpi)
```

|      k |       evpi |
|-------:|-----------:|
|      0 |   6620.079 |
|  25000 |  16847.855 |
|  50000 |  74268.245 |
|  75000 | 118801.232 |
| 100000 | 163334.219 |

``` r

plot(x=total_evpi$k, y=total_evpi$evpi, type="l")
```

![](simple_files/figure-html/plot_results-3.png)

``` r


# Total EVPPI
# All  parameters excluding hazard ratios that are constant as it causes errors
# df_inputs <- as.data.frame(results[["input_matrix"]][,-c(73:76,81:84,89:92,79:101)])
# parameters <- as.list(colnames(df_inputs))
# 
# total_evppi <- voi::evppi(outputs = list(c = total_costs,
#                                          e = total_qalys,
#                                          k = c(25000, 35000)),
#                           inputs = df_inputs,
#                           pars = parameters,
#                           method = "bart", 
#                           verbose = TRUE)
# 
# knitr::kable(total_evppi)
# 
# #plot in base R split ober A and B panels as there are too many parameters to plot
# total_evppi_25k_a <- total_evppi$evppi[total_evppi$k ==25000 & total_evppi$evppi > mean(total_evppi$evppi)]
# names(total_evppi_25k_a) <- total_evppi$pars[total_evppi$k ==25000 & total_evppi$evppi > mean(total_evppi$evppi)]
# par(las=1, bg="white", mai=c(0.85, 4, 0.5, 0.5))
# barplot(
#   sort(total_evppi_25k_a, decreasing=FALSE), horiz = TRUE, cex.names=0.9, main="EVPPI WTP 25,000/QALY (A)", xlab= "£"
# )
# 
# total_evppi_25k_b <- total_evppi$evppi[total_evppi$k ==25000 & total_evppi$evppi < mean(total_evppi$evppi)]
# names(total_evppi_25k_b) <- total_evppi$pars[total_evppi$k ==25000 & total_evppi$evppi < mean(total_evppi$evppi)]
# barplot(
#   sort(total_evppi_25k_b, decreasing=FALSE), horiz = TRUE, cex.names=0.6, main="EVPPI WTP 25,000/QALY (B)", xlab= "£"
# )
# 
# total_evppi_35k_a <- total_evppi$evppi[total_evppi$k ==35000 & total_evppi$evppi > mean(total_evppi$evppi)]
# names(total_evppi_35k_a) <- total_evppi$pars[total_evppi$k ==35000 & total_evppi$evppi > mean(total_evppi$evppi)]
# par(las=1, bg="white", mai=c(0.85, 4, 1, 0.5))
# barplot(
#   sort(total_evppi_35k_a, decreasing=FALSE), horiz = TRUE, cex.names=0.5, main="EVPPI WTP 35,000/QALY (A)", xlab= "£"
# )
# 
# total_evppi_35k_b <- total_evppi$evppi[total_evppi$k ==35000 & total_evppi$evppi < mean(total_evppi$evppi)]
# names(total_evppi_35k_b) <- total_evppi$pars[total_evppi$k ==35000 & total_evppi$evppi < mean(total_evppi$evppi)]
# par(las=1, bg="white", mai=c(0.85, 4, 1, 0.5))
# barplot(
#   sort(total_evppi_35k_b, decreasing=FALSE), horiz = TRUE, cex.names=0.6, main="EVPPI WTP 35,000/QALY (B)",xlab= "£"
# )
```
