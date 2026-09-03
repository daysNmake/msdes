
## *Current status of update to WARDEN*

**Model is working  using WARDEN** some items pending described below.
The models appear to be giving similar results in the deterministic both WARDEN and DESCEM.
The validation of probabilistic model are discussed below. 
Things to check or correct:

 + sense check: age is going above max age, check to see if this has been resolved.
 
 + mistake: annual treatment stops when edss >6, but then will not restart if edss drops =<6
 
 + fatal error: probabilistic warden version of the model does not run the full 100 simulations (throws up an error by sim no.66, traceback() indicates there might be an issue with sampling next treatment when switching)
 
 + speed: could explore dropping annul cycles but will deprive us from the validation endpoints compared bellow.
 
 + debug mode: Why does it slow down the probabilistic model and fails to complete?

Model validation can be run with the "Validation_main.R" script for either DESCEM or Warden scripts 

## *validation of update to WARDEN*

#### Key outcomes by treatment groups
Both models are clinically plausible in the sense that is very little differences between treatments and the final edss value is consistently below 7. The WARDEN version is producing a higher number of events in comparison to DECEM version with a slightly higher final edss score. Number of events cannot be validated from a data source, is it worth investigating why these are occurring?

```{r echo=FALSE, results='asis'}
library(knitr)
kable(read.csv("results/keyeventstrtgrps warden probabilistic 60 x 100 .csv "), caption = "WARDEN 100 patients x 60 Samples Probabilistic")

kable(read.csv("results/keyeventstrtgrps descem probabilistic 100 x 100 .csv "), caption = "DESCEM 100 patients x 100 Samples Probabilistic")
```

#### Mortality by treatment groups
Clinically plausible as in there is no difference between treatments, but patients in warden version appear to be living to beyond the age of 100?


#### Average severity over time
The models are consistent.


#### Average number of events over time
Theses outcomes were averaged over all patients and simulations because the number of event-times (ex eventime 5) donot correspond exactly to the number of patients and simulation, so could not pre-specify an array to be looped over.

The models appear to be consistent with slight variation for these outcomes:
+ Average number of edss increases / decreases over time
+ Average number of saes over time
+ Average proportion with SPMS over time

We give an example below, the remainder other tables are stored in the results folder.

##### Average number of relapses over time
The models appear to be consistent with slight variation.





## *work done when updating to WARDEN*
**Generate inputs functions** were updated to WARDEN, from: `add_item( if(psa_bool) {} else{})`  to : `add_item(pick_val_v())`. All vectors must be lists (ex: `l_hr_arr[["Natalizumab-SC"]]`).

**Lists only** are used as its no longer possible to use vectors or matrices within events, examples of variables that have changed:

| **Old variable** | **New variables** | 
| :---|:---|
| v_c_trt_admin       | l_c_trt_admin_yr1   & l_c_trt_admin_yr2 |
| v_c_trt_monitor     | l_c_trt_monitor_yr1 & l_c_trt_monitor_yr2 |
|  m_prop_2nd_3rd_trt  | l_prop_2ndline_trt  & l_prop_3rdline_trt |
|v_u_rrms_edss, v_u_spms_edss, v_u_relapse, v_u_sae_events|l_u_rrms_edss, l_u_spms_edss, l_u_relapse, l_u_sae_events|
|v_c_rrms_edss, v_c_spms_edss, df_c_tx|l_c_rrms_edss, l_c_spms_edss, l_c_trt_admin_yr1 , l_c_trt_admin_yr2, l_c_trt_monitor_yr1, l_c_trt_monitor_yr2, l_c_relapse, l_c_sae_events|

**Now indexing on lists** in the initial and reaction events, this was accomplished using `purrr::pluck()` 
  
| **Old **|**New **| 
| :---|:---|
|**Examples of changes to formulas:** |
| v_hr_smr_edss[edss_value == v_edss_values] | purrr::pluck(l_hr_smr_edss, names(v_edss_values[edss_value == v_edss_values])) |
| v_hr_edss_increase[current_treatment_code] | purrr::pluck(l_hr_edss_increase, names(v_treatment_codes[current_treatment_code == v_treatment_codes])) |
| v_baseline_harrms_edss_increase["DMTNatalizumab"] | purrr::pluck(l_baseline_harrms_edss_increase, "DMTNatalizumab") |
| v_spms_edss_increase["rate"] | purrr::pluck(l_spms_edss_increase, "rate") |
| v_rrms_edss_decrease["BaselineWebEDSS"] | purrr::pluck(l_rrms_edss_decrease, "BaselineWebEDSS")|
| v_hr_saes - should be changed to depend on treatment?| purrr::pluck(l_hr_saes, names(v_treatment_codes[ v_treatment_codes])) |
| m_prop_2nd_3rd_trt["3",current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]] | purrr::pluck(l_prop_3rdline_trt, names(v_higher_line_treatment_codes[current_treatment_code != v_higher_line_treatment_codes[!v_prior_treatment_indices]])) **this one might not be working correctly**|

| **some variables not changed:** |
| :--- |
| spms_edss_decrease - not used in model? |
| prop_2ndline_trt - not used in model? |


**costs and utilities** moved into unique patient inputs and reaction events because DESCEM cost and utility functions have been deprecated.
  + WARDEN vignette stipulated to add ongoing costs and utilities to individual patient level inputs and at every reaction event.
  + Could we implement new functionality which allows us to remove annual cycles and obtain similar results?

**Mortality** using modified function calling hesim, could ask JSA to write a native function

**Validation variables** used help understand natural history.

| **name ** | **description ** | **active in model update? ** |
| :--- |:--- | :--- |
|n_relapse | Number of relapses |Yes |
|n_sae_evts | Number of serious adverse events | Yes |
|n_edssinc_evts | Number of severity increases | Yes |
|n_edssdec_evts | Number of severity decreases | Yes |
|t_2ndline, t_3rdline, t_4thline| Time on treatment on each line | Yes |
|n_switch | treatment switch counter | Yes |
|years_on_treatment | annual treatment counter | Yes |
|t_progression | time to progression | No |
|tto_dp1, tto_dp2, tto_dp3, tto_dp4, tto_dp5, tto_dp6, tto_dp7 | time to progression for each event upto 7 events | No |
|tto_rel1, tto_rel2, tto_rel3, tto_rel4, tto_rel5, tto_rel6, tto_rel7 | time to progression for each event upto 7 events| No |
| yrs_edss_0, yrs_edss_1, yrs_edss_2, yrs_edss_3, yrs_edss_4, yrs_edss_5, yrs_edss_6, yrs_edss_7, yrs_edss_8, yrs_edss_9 |  annual severity state counter | No |


**Modularisation**

  + scripts are now called in from dedicated sheets
  + limited use of custom functions to errors when arguments are passed through
  + identify chunks of repetitive code to modularise
  





