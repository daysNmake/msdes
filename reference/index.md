# Package index

## Functions

Functions exported by the package

- [`gen_inputs()`](https://daysnmake.github.io/msdes/reference/gen_inputs.md)
  : Generate RRMS DES Model Inputs
- [`gen_common_all_inputs()`](https://daysnmake.github.io/msdes/reference/gen_common_all_inputs.md)
  : Generate Common Global Model Inputs
- [`gen_unique_pt_inputs()`](https://daysnmake.github.io/msdes/reference/gen_unique_pt_inputs.md)
  : Generate Unique Individual Patient Inputs
- [`gen_init_events()`](https://daysnmake.github.io/msdes/reference/gen_init_events.md)
  : Generate Initial Events for the RRMS DES Model
- [`gen_react_events()`](https://daysnmake.github.io/msdes/reference/gen_react_events.md)
  : Generate Reaction Events for the RRMS DES Model
- [`draw_tte_new_warden()`](https://daysnmake.github.io/msdes/reference/draw_tte_new_warden.md)
  : Draw time to Event custom function
- [`plot_mortality_curves()`](https://daysnmake.github.io/msdes/reference/plot_mortality_curves.md)
  : Plot Predicted Mortality Over Time by Treatment Arm
- [`rrmsdessetup()`](https://daysnmake.github.io/msdes/reference/rrmsdessetup.md)
  : Set Up RRMS DES Model Inputs
- [`rrmsdesmodel()`](https://daysnmake.github.io/msdes/reference/rrmsdesmodel.md)
  : Run the RRMS Discrete Event Simulation Model
- [`run_sim_new()`](https://daysnmake.github.io/msdes/reference/run_sim_new.md)
  : Run Sim new
- [`summarise_ce_results()`](https://daysnmake.github.io/msdes/reference/summarise_ce_results.md)
  : Summarise Cost-effectiveness results
- [`summarise_ipd()`](https://daysnmake.github.io/msdes/reference/summarise_ipd.md)
  : Summarise Individual Patient Data from DES Results
- [`summarise_samples()`](https://daysnmake.github.io/msdes/reference/summarise_samples.md)
  : Summarise Samples

## Data

Datasets included in the package

- [`df_c_tx`](https://daysnmake.github.io/msdes/reference/df_c_tx.md) :
  DMT costs by year
- [`df_c_tx_admin`](https://daysnmake.github.io/msdes/reference/df_c_tx_admin.md)
  : Treatment administration costs by year
- [`df_c_tx_load`](https://daysnmake.github.io/msdes/reference/df_c_tx_load.md)
  : Annual treatment scheduled load factors years 1-5
- [`df_c_tx_monitor`](https://daysnmake.github.io/msdes/reference/df_c_tx_monitor.md)
  : Treatment monitoring costs by year
- [`m_n_patients_edss`](https://daysnmake.github.io/msdes/reference/m_n_patients_edss.md)
  : Patient counts by EDSS score
- [`m_proportion_ontrtline`](https://daysnmake.github.io/msdes/reference/m_proportion_ontrtline.md)
  : A matrix of patient counts stratified by treatment
- [`m_tto_edss_increase_harrms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_harrms_sigma.md)
  [`m_tto_edss_increase_rrms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_harrms_sigma.md)
  [`tto_relapse_harrms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_harrms_sigma.md)
  [`m_tto_sp_conversion_harrms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_harrms_sigma.md)
  : Natural history treatment dependent variance covariance.
- [`m_tto_edss_increase_spms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_spms_sigma.md)
  [`m_tto_edss_decrease_rrms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_spms_sigma.md)
  [`m_tto_edss_decrease_spms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_spms_sigma.md)
  [`m_tto_relapse_spms_sigma`](https://daysnmake.github.io/msdes/reference/m_tto_edss_increase_spms_sigma.md)
  : Natural history variance covariance.
- [`v_smr_severity_harding`](https://daysnmake.github.io/msdes/reference/v_smr_severity_harding.md)
  : A matrix of excess all cause mortality for MS patients by severity
- [`v_tto_edss_increase_harrms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_harrms_mu.md)
  [`v_tto_edss_increase_rrms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_harrms_mu.md)
  [`v_tto_relapse_harrms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_harrms_mu.md)
  [`v_tto_sp_conversion_harrms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_harrms_mu.md)
  : Natural history treatment dependent coefficients.
- [`v_tto_edss_increase_spms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_spms_mu.md)
  [`v_tto_edss_decrease_rrms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_spms_mu.md)
  [`v_tto_edss_decrease_spms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_spms_mu.md)
  [`v_tto_relapse_spms_mu`](https://daysnmake.github.io/msdes/reference/v_tto_edss_increase_spms_mu.md)
  : Natural history coefficients.
