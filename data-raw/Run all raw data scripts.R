## Run all raw data extraction scripts

source("data-raw/MS registry.R") # extract registry data
source("data-raw/serious_adverse_events.R") # extract SAEs cost utilities and weights
source("data-raw/all_cause_mortality.R") # extract all cause_mortality life tables
source("data-raw/costs.R") # extract heath state and relapse costs
# treatment acquisition (df_c_drugs.csv) administration (df_c_drugs_admin.csv) and monitoring (df_c_monitoring.csv) costs are in the data folder
source("data-raw/utilities.R") # extract heath-state, relapse and carer utilities
source("data-raw/relapse_severity.R") # extract proportions and annual rates of relapse by severity
