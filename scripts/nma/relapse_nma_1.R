# NMA of relapse rates on highly active RRMS NICE MTA
# Howard Thom 4-Oct-2024
# Largely follows multinma vignette of David Phillippo
# https://dmphillippo.github.io/multinma/articles/example_parkinsons.html

library(xlsx)
library(multinma)

# Notes to clinical reviewers
# Check study 943 as appears to be missing an intervention label in second arm.
# Check LCI for rate in 688 as shouldn't be zero
# In study 699, for example, what does an SD of 0.6 represent in relation to a mean rate of 0.4? In the NMA we only talk about SEs on the log scale as the rates themselves are skewed.
# Study 702 also appears to be missing an intervention label
# Ocrelizumab and interferon arm of 1360 is extracted twice with two (slightly) different hazard ratios
# Seems to be an empty fingolimod arm from study 888
# The "To be added" study arm for "Interferon beta 1a IM30" is duplicated with one having sample size 83 and the other 84

# Load the raw relapse data from the SLR
# This is a mixture of rate and rate ratio data
raw_relapse_data <- read.xlsx(file = "data/nma/Relapse rates.xlsx",
                              sheetIndex = 1)


# Simplify the column names
colnames(raw_relapse_data) <- gsub("Relapse.Rate..", "", colnames(raw_relapse_data))

# Change from arm to treatment name for simplicity
colnames(raw_relapse_data)[colnames(raw_relapse_data) == "Arm.label"] <- "Treatment"

#####################################################################################
##### Manual editing ###############################################################

# Remove extra ocrelizumab and interferon data from 1360 but need to check with clinical review team
ocr_row_to_remove <- which(raw_relapse_data$EndNote.ID == 1360 & raw_relapse_data$Treatment == "Ocrelizumab IV600" & raw_relapse_data$Effect.estimate == 0.53)
raw_relapse_data <- raw_relapse_data[-ocr_row_to_remove, ]
int_row_to_remove <- which(raw_relapse_data$EndNote.ID == 1360 & raw_relapse_data$Treatment == "Interferon beta 1a SC44" & raw_relapse_data$LCI == 0.23)
raw_relapse_data <- raw_relapse_data[-int_row_to_remove, ]

# Remove empty fingolimod data from study 888
fin_row_to_remove <- which(raw_relapse_data$EndNote.ID == 888 & raw_relapse_data$Treatment == "Fingolimod O0.5" & raw_relapse_data$Effect.estimate == "")
raw_relapse_data <- raw_relapse_data[-fin_row_to_remove, ]

# Assume treatment in control for study 943 is placebo
raw_relapse_data$Treatment[31] <- "Placebo"

# Remove extra arm from "To be added" study
int_row_to_remove <- which(raw_relapse_data$EndNote.ID == "To be added" & raw_relapse_data$Treatment == "Interferon beta 1a IM30" & raw_relapse_data$N == 83)
raw_relapse_data <- raw_relapse_data[-int_row_to_remove, ]

#####################################################################################


# Extract the unique study names
study_ids <- unlist(lapply(unique(raw_relapse_data$EndNote.ID), toString))

# Treatment names in alphabetical order, but with Placebo at the start,
# thus matching what mutlinma would use
treatment_names <- sort(unique(c(rrms_arm$Treatment, rrms_contrast$Treatment)))
treatment_names <- c("Placebo", treatment_names[-which(treatment_names == "Placebo")])

# Rate data
rrms_arm <- as.data.frame(raw_relapse_data[, c("EndNote.ID", "Treatment", "Mean", "LCI", "UCI", "N")])
# Remove rows with no rate data

# Remove studies that don't report LCI and UCI
# This includes some studies that report an SD but it's unclear what the SE represents
rrms_arm <- rrms_arm[rrms_arm$Mean != "" &
                       rrms_arm$LCI != "NR" & rrms_arm$LCI != "" &
                       rrms_arm$UCI != "NR" & rrms_arm$UCI != "", ]

# Make a continuity correction if the LCI is 0
rrms_arm$LCI[as.numeric(rrms_arm$LCI) == 0] <- 0.001

rrms_arm[rrms_arm == "NA"] <- NA
rrms_arm[rrms_arm == "NR"] <- NA
rrms_arm[rrms_arm == ""] <- NA

# Calculate the log mean and log SE for rate data
rrms_arm$y <- log(as.numeric(rrms_arm$Mean))
rrms_arm$se <- (log(as.numeric(rrms_arm$UCI)) - 
                  log(as.numeric(rrms_arm$LCI))) / (2 * 1.96)

# Ensure sample size is numeric
rrms_arm$N <- as.numeric(rrms_arm$N)

# Rate ratio data
rrms_contrast <- as.data.frame(raw_relapse_data[, c("EndNote.ID", "Treatment", "Effect.estimate", "Effect.Estimate.Type", "LCI.1", "UCI.1", "N")])
# Remove studies with no effect estimate and no reporting rate ratio
# Don't need code to remove those without 95% CI as there are none
# Multinma expects an NA for the reference treatment are for differences
# so need to include the blank row from each study
contrast_studies <- rrms_contrast$EndNote.ID[rrms_contrast$Effect.estimate != "" &
                                 rrms_contrast$Effect.estimate != "NR" &
                                 rrms_contrast$Effect.Estimate.Type == "Rate ratio"] 

rrms_contrast <- rrms_contrast[is.element(rrms_contrast$EndNote.ID, contrast_studies), ]

# Remove rows without intervention names
rrms_contrast <- rrms_contrast[rrms_contrast$Treatment != "", ]

# Remove studies with only a single row (After the above removal)
for(study_id in study_ids) {
  if(sum(rrms_contrast$EndNote.ID == study_id) == 1)
    rrms_contrast <- rrms_contrast[-which(rrms_contrast$EndNote.ID == study_id), ]
}

# Calculate the log mean and log SE for rate ratio data
rrms_contrast$diff <- log(as.numeric(rrms_contrast$Effect.estimate))
rrms_contrast$se_diff <- (log(as.numeric(rrms_contrast$UCI.1)) - 
                       log(as.numeric(rrms_contrast$LCI.1))) / (2 * 1.96)

# Ensure sample size is numeric
rrms_contrast$N <- as.numeric(rrms_contrast$N)

# Make missing data consistent
rrms_contrast[rrms_contrast == "NA"] <- NA
rrms_contrast[rrms_contrast == "NR"] <- NA
rrms_contrast[rrms_contrast == ""] <- NA

# Remove duplicate empty rows from the contrast data (these are included as dichotomous data are 
# added in separate rows)
rrms_contrast <- rrms_contrast[!duplicated(rrms_contrast), ]


# Remove studies from the arm dataset that are in the contrast dataset
rrms_arm <- rrms_arm[-which(is.element(rrms_arm$EndNote.ID, rrms_contrast$EndNote.ID)), ]


# Create network for rate data
mix_arm_net <- set_agd_arm(rrms_arm, 
                           study = EndNote.ID,
                           trt = Treatment,
                           y = y, 
                           se = se,
                           sample_size = N)

# Create network for rate ratio data
mix_contrast_net <- set_agd_contrast(rrms_contrast, 
                                  study = EndNote.ID,
                                  trt = Treatment,
                                  y = diff, 
                                  se = se_diff,
                                  sample_size = N)

# Combine the networks together
mix_net <- combine_network(mix_arm_net, mix_contrast_net)

# Export a summary of the network
mix_net

# Generate a network plot with nodes weighted by sample size
# and edges by number of studies
plot(mix_net, weight_edges = TRUE, weight_nodes = TRUE)

# Fixed effects NMA
mix_fit_FE <- nma(mix_net, 
                  trt_effects = "fixed",
                  prior_intercept = normal(scale = 100),
                  prior_trt = normal(scale = 100))

# Can look at estimates of basic log hazard ratios relative to placebo
# Lower values indicate lower risk of relapse
# As expected alemtuzumab has lowest risk although natalizumab is second (joint with
# ocrelizumab)
mix_fit_FE

# Random effects NMA
# Note that we're using a non-informative prior. This leads to poor convergence
# May want to use informative prior
mix_fit_RE <- nma(mix_net, 
                  seed = 437219664,
                  trt_effects = "random",
                  prior_intercept = normal(scale = 100),
                  prior_trt = normal(scale = 100),
                  prior_het = half_normal(scale = 5),
                  adapt_delta = 0.99)

# Model comparison via DIC
# Although noting random effects haven't converged there isn't much 
# difference between the two models
(mix_dic_FE <- dic(mix_fit_FE))
(mix_dic_RE <- dic(mix_fit_RE))

# Treatment effects relative to natalizumab IV300
# Values >= 0 favour natalizumab
(mix_releff_FE <- relative_effects(mix_fit_FE, trt_ref = "Natalizumab IV300"))

# Forest plot but on log rate ratio scale
# Ideally want some way to plot on natural scale
plot(mix_releff_FE, ref_line = 1)
