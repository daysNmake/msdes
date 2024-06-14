## Run MS Model
source("R/01_load_data.R")
source("R/02_common_inputs.R")
source("R/03_individual_inputs.R")
source("R/04_a1_initial events.R")
source("R/05_a1_reaction_events.R")
source("R/06_simulation.R")

## View results

# results
summary_results_det(results$final_output)

psa_ipd <- bind_rows(map(results$output_psa, "merged_df")) 

psa_ipd[1:10,] %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

