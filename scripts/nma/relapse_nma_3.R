# NMA of relapse rates on highly active RRMS NICE MTA
# Howard Thom 4-Oct-2024
# Modified by catalina Lopez 10-Oct-2024
# Largely follows multinma vignette of David Phillippo
# https://dmphillippo.github.io/multinma/articles/example_parkinsons.html

library(xlsx)
library(multinma)
library(dplyr)

# Load the raw relapse data from the SLR
# This is a mixture of rate and rate ratio data
raw_relapse_data <- read.xlsx(file = "data/nma/Relapse rates (4).xlsx",
                              sheetIndex = 1)


# Simplify the column names
colnames(raw_relapse_data) <- gsub("Relapse.Rate..", "", colnames(raw_relapse_data))

# Change from arm to treatment name for simplicity
colnames(raw_relapse_data)[colnames(raw_relapse_data) == "Arm.label"] <- "Treatment"


#####################################################################################


# Extract the unique study names
study_ids <- unlist(lapply(unique(raw_relapse_data$EndNote.ID), toString))
study_names <- unlist(lapply(unique(raw_relapse_data$Study.Name), toString))

# Rate data with 95%CI
  rrms_arm <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment", "Mean", "LCI", "UCI", "N")])

  # Remove studies that don't report LCI and UCI
  rrms_arm <- rrms_arm[rrms_arm$Mean != "" &
                         rrms_arm$LCI != "NR" & rrms_arm$LCI != "" &
                         rrms_arm$UCI != "NR" & rrms_arm$UCI != "", ]

  
  # Use timepoint with longer follow-up time available
  rrms_arm <- rrms_arm %>%
    group_by(EndNote.ID, Treatment) %>%
    filter(Time.point.reported..Months. == max(Time.point.reported..Months.)) %>%
    ungroup()
  
  rrms_arm$N <- as.numeric(rrms_arm$N)
  rrms_arm$Mean <- as.numeric(rrms_arm$Mean)
  rrms_arm$LCI <- as.numeric(rrms_arm$LCI)
  rrms_arm$UCI <- as.numeric(rrms_arm$UCI)
  
  # Calculate the log mean and log SE for rate data
  rrms_arm$y <- log(as.numeric(rrms_arm$Mean))
  rrms_arm$se <- (log(as.numeric(rrms_arm$UCI)) - 
                    log(as.numeric(rrms_arm$LCI))) / (2 * 1.96)
  
# Rate ratio data
  rrms_contrast <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID","Time.point.reported..Months.", "Treatment", "Effect.estimate", "Effect.Estimate.Type", "LCI.1", "UCI.1", "N")])
  # Remove studies with no effect estimate and no reporting rate ratio
  rrms_contrast <- rrms_contrast[rrms_contrast$Effect.estimate != "" &
                         rrms_contrast$LCI.1 != "NR" & rrms_contrast$LCI.1 != "" &
                           rrms_contrast$UCI.1 != "NR" & rrms_contrast$UCI.1 != "", ]
  
  rrms_contrast$N <- as.numeric(rrms_contrast$N)
  rrms_contrast$Effect.estimate <- as.numeric(rrms_contrast$Effect.estimate)
  rrms_contrast$LCI.1 <- as.numeric(rrms_contrast$LCI.1)
  rrms_contrast$UCI.1 <- as.numeric(rrms_contrast$UCI.1)
  
  # Don't need code to remove those without 95% CI as there are none
  # All studies have a contrast row so don't need to include the blank row from each study
  # All rows have intervention names so don't need to remove rows without intervention names
  # No studies with single rows, so don't need to remove studies with only a single row (After the above removal)

  # Calculate the log mean and log SE for rate ratio data
  rrms_contrast$diff <- log(as.numeric(rrms_contrast$Effect.estimate))
  rrms_contrast$se_diff <- (log(as.numeric(rrms_contrast$UCI.1)) - 
                              log(as.numeric(rrms_contrast$LCI.1))) / (2 * 1.96)
  

# Rate data without 95%CI
  rrms_contrast_2 <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment","Reference", "Mean", "N")])

  # Remove rows with no rate data
  rrms_contrast_2 <- rrms_contrast_2[rrms_contrast_2$Mean != "", ]
 
  # Use time point with longer follow-up time available
  rrms_contrast_2 <- rrms_contrast_2 %>%
    group_by(EndNote.ID, Treatment) %>%
    filter(Time.point.reported..Months. == max(Time.point.reported..Months.)) %>%
    ungroup()
  
  # Make sure "Mean" and "N" are numeric
  rrms_contrast_2$Mean <- as.numeric(rrms_contrast_2$Mean)
  rrms_contrast_2$N <- as.numeric(rrms_contrast_2$N)
  
  #Convert "NA" values in Reference to NA
  rrms_contrast_2$Reference[rrms_contrast_2$Reference == "NA"] <- NA
  
  #Calculate Rate Ratios from Rates
  rrms_contrast_2 <- rrms_contrast_2 %>%
    group_by(EndNote.ID) %>%
    mutate(
      Effect.Estimate = ifelse(
        is.na(Reference),
        NA,
        Mean / Mean[which(is.na(Reference))]
      )
    ) %>%
    ungroup()
  
  # Calculate the log mean for rates data
  rrms_contrast_2$diff <- log(as.numeric(rrms_contrast_2$Effect.Estimate))
  
  # Calculate the log SE for rates data
  rrms_contrast_2 <- rrms_contrast_2 %>%
    group_by(EndNote.ID) %>%
    mutate(
      ref_mean = Mean[which(is.na(Reference))[1]],  # Get Mean where Reference is NA within each EndNote.ID
      ref_n = N[which(is.na(Reference))[1]],  # Get N where Reference is NA within each EndNote.ID
      se_diff = ifelse(
        is.na(Effect.Estimate),
        NA,
        sqrt((1 / (Mean * N)) + (1 / (ref_mean * ref_n)))
      )
    ) %>%
    ungroup() %>%
    select(-ref_mean, -ref_n)
  
 # Calculate a small positive standard error based on non-baseline arms
  small_positive_se <- rrms_contrast_2 %>%
    filter(!is.na(se_diff)) %>%
    summarise(min_se = min(se_diff[se_diff > 0], na.rm = TRUE)) %>%
    pull(min_se) * 0.1  # Use 10% of the smallest positive SE
  
  # Assign this small positive SE to baseline arms where se_diff is NA
  rrms_contrast_2 <- rrms_contrast_2 %>%
    mutate(
      se_diff = ifelse(is.na(se_diff) & is.na(Reference), small_positive_se, se_diff)
    )
  
# Treatment names in alphabetical order, but with Placebo at the start,
# thus matching what mutlinma would use
 treatment_names <- sort(unique(c(rrms_arm$Treatment, rrms_contrast_2$Treatment, rrms_contrast$Treatment)))
  treatment_names <- c("Placebo", treatment_names[-which(treatment_names == "Placebo")])

  
# Remove studies from the arm dataset that are in the contrast dataset
rrms_arm <- rrms_arm[-which(is.element(rrms_arm$EndNote.ID, rrms_contrast$EndNote.ID)), ]

# Remove studies from the contrast_2 dataset that are in the contrast dataset
rrms_contrast_2 <- rrms_contrast_2[-which(is.element(rrms_contrast_2$EndNote.ID, rrms_contrast$EndNote.ID)), ]

# Remove studies from the contrast_2 dataset that are in the arm dataset
rrms_contrast_2 <- rrms_contrast_2[-which(is.element(rrms_contrast_2$EndNote.ID, rrms_arm$EndNote.ID)), ]


# Create network for rate data
mix_arm_net <- set_agd_arm(rrms_arm, 
                           study = Study.Name,
                           trt = Treatment,
                           y = y, 
                           se = se,
                           sample_size = N)

# Create network for rate ratio data
mix_contrast_net <- set_agd_contrast(rrms_contrast, 
                                     study = Study.Name,
                                     trt = Treatment,
                                     y = diff, 
                                     se = se_diff,
                                     sample_size = N)

# Create network for rates without 95%CI data
mix_contrast_2_net <- set_agd_contrast(rrms_contrast_2, 
                                     study = Study.Name,
                                     trt = Treatment,
                                     y = diff, 
                                     se = se_diff,
                                     sample_size = N)

# Combine the networks together
mix_net <- combine_network(mix_arm_net, mix_contrast_net, mix_contrast_2_net)

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
plot(mix_dic_FE, ref_line = 0)


# Treatment effects relative to natalizumab IV300
# Values >= 0 favour natalizumab
(mix_releff_FE <- relative_effects(mix_fit_FE, trt_ref = "Natalizumab IV300"))

# Forest plot but on log rate ratio scale
# Ideally want some way to plot on natural scale
plot(mix_releff_FE, ref_line = 0)
