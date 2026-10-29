# Generate RRMS DES Model Inputs

## Usage

``` r
gen_inputs(
  drc = drc,
  drq = drq,
  time_horizon = time_horizon,
  p_eid = p_eid,
  jcvtest_cost = jcvtest_cost
)
```

## Arguments

- drc:

  Numeric. Annual discount rate for costs, expressed as a proportion
  (e.g. `0.035` for 3.5\\

  drqNumeric. Annual discount rate for QALYs, expressed as a proportion
  (e.g. `0.035` for 3.5\time_horizonInteger. Model time horizon in
  years.p_eidNumeric. Proportion of patients receiving extended interval
  dosing (EID) of Natalizumab, expressed as a proportion (e.g. `0.6` for
  60\\ for all patients.jcvtest_costNumeric. Unit cost (£) of a JC virus
  (JCV) antibody test. Set to `0` if the test is assumed to be provided
  free of charge (e.g. via manufacturer home service). The private cost
  is £247. A named list of model inputs. Key elements include:

  `initial_age`

  :   Numeric. Baseline age in years.

  `time_horizon`

  :   Integer. Model time horizon in years.

  `prop_female`

  :   Numeric. Proportion of patients who are female (0.70).

  `sd_psa`

  :   Numeric. Default PSA standard deviation (10).

  `p_eid`

  :   Numeric. Proportion on extended interval dosing.

  `drc`, `drq`

  :   Numeric. Discount rates for costs and QALYs.

  `jcvtest_cost`

  :   Numeric. JCV test unit cost (£).

  `m_n_patients_edss`

  :   Matrix. Baseline EDSS distribution from the MS Registry.

  `df_c_tx`, `df_c_tx_admin`, `df_c_tx_monitor`

  :   Data frames of annual treatment, administration, and monitoring
      costs per DMT, adjusted for EID and JCV test assumptions.

  `df_c_tx_load`

  :   Data frame of annual treatment scheduled load factors for years
      1–5.

  `v_smr_severity_harding`

  :   Matrix. Excess all-cause mortality SMRs by EDSS severity (Harding
      et al.).

  `m_proportion_ontrtline`

  :   Matrix. Proportions of patients on 2nd and 3rd line treatment.

  `v_tto_edss_increase_harrms_mu`, `m_tto_edss_increase_harrms_sigma`

  :   Time-to-event parameters for EDSS increase in HARRMS patients (MS
      Registry).

  `v_tto_edss_increase_rrms_mu`, `m_tto_edss_increase_rrms_sigma`

  :   Time-to-event parameters for EDSS increase in RRMS patients.

  `v_tto_edss_increase_spms_mu`, `m_tto_edss_increase_spms_sigma`

  :   Time-to-event parameters for EDSS increase in SPMS patients.

  `v_tto_edss_decrease_rrms_mu`, `m_tto_edss_decrease_rrms_sigma`

  :   Time-to-event parameters for EDSS decrease in RRMS patients.

  `v_tto_edss_decrease_spms_mu`, `m_tto_edss_decrease_spms_sigma`

  :   Time-to-event parameters for EDSS decrease in SPMS patients.

  `v_tto_relapse_harrms_mu`, `tto_relapse_harrms_sigma`

  :   Time-to-event parameters for relapse in HARRMS patients.

  `v_tto_relapse_spms_mu`, `m_tto_relapse_spms_sigma`

  :   Time-to-event parameters for relapse in SPMS patients.

  `v_tto_sp_conversion_harrms_mu`, `m_tto_sp_conversion_harrms_sigma`

  :   Time-to-event parameters for secondary progressive conversion in
      HARRMS patients.

  `initial_spms_prop`

  :   Numeric. Baseline proportion of patients with SPMS (0).

  `p_relapse_hospital`

  :   Numeric. Proportion of relapses leading to hospital admission
      (Hawton et al. 2026).

  `sae_names`

  :   Character vector of serious adverse event names from AFFIRM.

  `n_sae`

  :   Integer. Number of serious adverse events in the model.

  `v_weight_sae`

  :   Numeric vector. Weights for SAE cost and utility calculations.

  `m_saes`

  :   Matrix. SAE incidence on Natalizumab from AFFIRM (Polman 2006).

  `v_smr`

  :   Numeric vector. Excess all-cause mortality SMRs (Jick 2014).

  `m_r_mortality`

  :   Matrix. ONS all-cause mortality rates by age, male, and female.

  `m_c_edss_rrms`, `m_c_edss_spms`

  :   Matrices of direct medical costs by EDSS state for RRMS and SPMS
      (Tyas et al. 2005, inflated to 2022/23).

  `df_c_spms`

  :   Matrix of SPMS treatment costs.

  `p_siponimod`

  :   Numeric. Proportion treated with Siponimod based on MS Registry
      data (0.557).

  `m_c_relapse`

  :   Matrix of annualised relapse costs (Hawton 2016, inflated to
      2023).

  `df_c_sae`

  :   Matrix of serious adverse event costs (MS-MTA report).

  `m_u_edss_rrms`, `m_u_edss_spms`

  :   Matrices of EDSS utilities for RRMS and SPMS (Orme).

  `df_u_relapse`

  :   Data frame of relapse utilities.

  `m_u_carer`

  :   Matrix of carer disutilities.

  `df_u_sae`

  :   Matrix of serious adverse event disutilities (MS-MTA report).

  `m_discontinuation`

  :   Matrix of baseline discontinuation rates (AFFIRM and Antilope).

  `m_hr_rrms_cdp3_fe`

  :   Matrix. CDP3 NMA hazard ratios (mean, SD).

  `m_hr_rrms_cdp6_fe`

  :   Matrix. CDP6 NMA hazard ratios (mean, SD).

  `m_hr_rrms_arr_fe`

  :   Matrix. ARR NMA rate ratios (mean, SD).

  `m_hr_rrms_saes_fe`

  :   Matrix. SAE NMA rate ratios (mean, SD).

  `m_hr_rrms_discontinuation_fe`

  :   Matrix. Discontinuation NMA hazard ratios (mean, SD).

  Generates all model inputs from package data and user-specified
  assumptions, returning a named list consumed by
  [`rrmsdessetup`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md)
  and subsequently passed to
  [`rrmsdesmodel`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md)
  via the global `inputs` object. All default parameter values are
  stored within this function. The following processing steps are
  performed before inputs are returned:**Population**

  - Baseline age is drawn from `Normal(36, 8/36)` reflecting both arms
    of the AFFIRM trial (N = 942).

  - Proportion female set to 0.70 (n = 660, AFFIRM).

  - PSA standard deviation set to 1/10 of the mean (`sd_psa = 10`) where
    uncertainty is not reported.

  **Treatment costs (`df_c_tx`, `df_c_tx_admin`, `df_c_tx_monitor`)**

  - Row 3 is dropped from each cost matrix and columns are restricted to
    active treatment names in `v_treatment_names`.

  - Natalizumab IV and SC (columns 1–4) are adjusted for the proportion
    on EID using a 5.55-week interval dosing schedule
    (`round(52/5.55, 2)/13`).

  - Administration costs for Natalizumab SC in year 1 and year 2 are
    scaled by `s_c_tx_adminyr1_natsc` and `s_c_tx_adminyr2_natsc`
    respectively (both defaulting to 0.5).

  - Zero administration costs are set to 0.1 to prevent `NaN` values in
    downstream calculations.

  - Monitoring costs for all four Natalizumab formulations are reduced
    by £247 and replaced with `jcvtest_cost`.

  **Mortality**

  - standardised mortality ratios (SMR) EDSS 8 and 9 are assumed equal
    to EDSS 7 (Harding et al.).

  - ONS all-cause mortality rates by age, male, ands ex.

  **Relapse costs**

  - Annualisedrelapse costs (Hawton 2016, inflated to 2023)

  **NMA data**

  - Mean and standard deviation columns are extracted from each NMA
    result object (CDP3, CDP6, ARR, SAEs, discontinuation)
