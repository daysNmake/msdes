# NMA of CDP3 and CDP6 on RRMS NICE MTA
# Catalina Lopez 14-Oct-2024
# Largely follows multinma vignette of David Phillippo
# https://dmphillippo.github.io/multinma/articles/example_parkinsons.html

library(xlsx)
library(multinma)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)


#CDP3 Network

# Load the raw CDP3 from the SLR
# This is a mixture of hazard ratio and dichotomous raw data
CDP3_data <- read.xlsx(file = "data/nma/CDP3 (2).xlsx",
                              sheetIndex = 1)


# Simplify the column names
colnames(CDP3_data) <- gsub("CDP3..", "", colnames(CDP3_data))

# Change from arm to treatment name for simplicity
colnames(CDP3_data)[colnames(CDP3_data) == "Arm.label"] <- "Treatment"

# Delete blank rows
CDP3_data <- CDP3_data %>%
  filter(Treatment != "")

# Remove disconnected studies
CDP3_data <- CDP3_data %>%
  filter(!(Study.Name %in% c("ASCLEPIOS I", "ASCLEPIOS II", "OPTIMUM")))


#####################################################################################


# Extract the unique study names
CDP3_study_ids <- unlist(lapply(unique(CDP3_data$EndNote.ID), toString))
CDP3_study_names <- unlist(lapply(unique(CDP3_data$Study.Name), toString))

# Hazard ratio data
CDP3_contrast <- as.data.frame(CDP3_data[, c("Study.Name", "EndNote.ID","Time.point.reported..Months.", "Treatment", "Reference", "Estimate", "Effect.Estimate.Type", "LCI", "UCI", "N")])

# Remove studies with no effect estimate and no reporting rate ratio
CDP3_contrast <- CDP3_contrast %>%
  group_by(Study.Name) %>%
  filter(any(Estimate != "" & Estimate != "NR")) %>%
  ungroup()

#Ensure all relevant data data is numeric
CDP3_contrast$N <- as.numeric(CDP3_contrast$N)
CDP3_contrast$Estimate <- as.numeric(CDP3_contrast$Estimate)
CDP3_contrast$LCI <- as.numeric(CDP3_contrast$LCI)
CDP3_contrast$UCI <- as.numeric(CDP3_contrast$UCI)

# Don't need code to remove those without 95% CI as there are none
# All studies have a contrast row so don't need to include the blank row from each study
# All rows have intervention names so don't need to remove rows without intervention names
# No studies with single rows, so don't need to remove studies with only a single row (After the above removal)

# Calculate the log mean and log SE for hazard ratio data
CDP3_contrast$diff <- log(as.numeric(CDP3_contrast$Estimate))
CDP3_contrast$se_diff <- (log(as.numeric(CDP3_contrast$UCI)) - 
                            log(as.numeric(CDP3_contrast$LCI))) / (2 * 1.96)

# Calculate a small positive standard error based on non-baseline arms
small_positive_se <- CDP3_contrast %>%
  filter(!is.na(se_diff)) %>%
  summarise(min_se = min(se_diff[se_diff > 0], na.rm = TRUE)) %>%
  pull(min_se) * 0.1  # Use 1% of the smallest positive SE

# Assign this small positive SE to baseline arms where se_diff is NA
CDP3_contrast <- CDP3_contrast %>%
  mutate(
    se_diff = ifelse(is.na(se_diff), small_positive_se, se_diff)
  )

#Convert "NA" values in Reference to NA
CDP3_contrast$Reference[CDP3_contrast$Reference == "NA"] <- NA

# Raw dichotomous data
CDP3_event <- as.data.frame(CDP3_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment","Reference", "N", "No..of.pt.s.with.events")])

# Simplify variable names
CDP3_event <- CDP3_event %>%
  rename(Follow.Up.Time = Time.point.reported..Months.,
         Events = No..of.pt.s.with.events)

# Remove studies that don't report number of events
CDP3_event <- CDP3_event[CDP3_event$Events != "NR", ]

#Ensure all relevant data data is numeric
CDP3_event$N <- as.numeric(CDP3_event$N)
CDP3_event$Events <- as.numeric(CDP3_event$Events)
CDP3_event$Follow.Up.Time <- as.numeric(CDP3_event$Follow.Up.Time)

#Calculate person-years at risk
CDP3_event <- CDP3_event %>%
  mutate(T = (Follow.Up.Time * N) / 12)

#Calculate hazard rates
CDP3_event <- CDP3_event %>%
  mutate(HR = Events/T)

#Convert "NA" values in Reference to NA
CDP3_event$Reference[CDP3_event$Reference == "NA"] <- NA

#Calculate hazard Ratios from Rates
CDP3_event <- CDP3_event %>%
  group_by(EndNote.ID) %>%
  mutate(
    Hazard.Ratio = ifelse(
      is.na(Reference),
      NA,
      HR / HR[which(is.na(Reference))]
    )
  ) %>%
  ungroup()

# Calculate the log mean for events data
CDP3_event$diff <- log(as.numeric(CDP3_event$Hazard.Ratio))

  
# Calculate the log SE for events data
CDP3_event <- CDP3_event %>%
  group_by(EndNote.ID) %>%
  mutate(
    ref_Events = Events[which(is.na(Reference))[1]],  # Get Events where Reference is NA within each EndNote.ID
    se_diff = ifelse(
      is.na(Hazard.Ratio),
      NA,
      sqrt((1 / Events) + (1 / ref_Events))
    )
  ) %>%
  ungroup() %>%
  select(-ref_Events)
  
# Treatment names in alphabetical order, but with Placebo at the start,
# thus matching what mutlinma would use
CDP3_treatment_names <- sort(unique(c(CDP3_event$Treatment, CDP3_contrast$Treatment)))
CDP3_treatment_names <- c("Placebo", CDP3_treatment_names[-which(CDP3_treatment_names == "Placebo")])

  
# Remove studies from the events dataset that are in the contrast dataset
CDP3_event <- CDP3_event[-which(is.element(CDP3_event$EndNote.ID, CDP3_contrast$EndNote.ID)), ]


# Create network for events data
CDP3_mix_event_net <- set_agd_contrast(CDP3_event, 
                           study = Study.Name,
                           trt = Treatment,
                           y = diff, 
                           se = se_diff,
                           trt_ref = "Placebo",
                           sample_size = N)

CDP3_mix_event_net

# Create network for hazard ratio data
CDP3_mix_contrast_net <- set_agd_contrast(CDP3_contrast, 
                                     study = Study.Name,
                                     trt = Treatment,
                                     y = diff, 
                                     se = se_diff,
                                     sample_size = N)
CDP3_mix_contrast_net

# Combine the networks together
CDP3_mix_net <- combine_network(CDP3_mix_event_net, CDP3_mix_contrast_net)

# Export a summary of the network
CDP3_mix_net

# Generate a network plot with nodes weighted by sample size
# and edges by number of studies
plot(CDP3_mix_net, weight_edges = TRUE, weight_nodes = TRUE, nudge = 0.1) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing = ggplot2::unit(0.5, "lines")
  )

mix_net <- CDP3_mix_net

# Can look at estimates of basic log hazard ratios relative to placebo
CDP3_mix_fit_FE <- nma(CDP3_mix_net, 
                       seed = 437219664,
                       trt_effects = "fixed",
                       prior_intercept = normal(0, scale = 10),
                       prior_trt = normal(0, scale = 10))
CDP3_mix_fit_FE

# Create dataframe with results summary
CDP3_mix_fit_FE_summary <- summary(CDP3_mix_fit_FE)
CDP3_mix_fit_FE_summary_df <- as.data.frame(CDP3_mix_fit_FE_summary)
CDP3_intervention_FE_df <- CDP3_mix_fit_FE_summary_df[grep("^d", CDP3_mix_fit_FE_summary_df$parameter), ]
CDP3_intervention_FE_df$parameter <- gsub("^d\\[|\\]$", "", CDP3_intervention_FE_df$parameter)

# Calculate RR and 95% CI in the exponential scale
CDP3_intervention_FE_df <- CDP3_intervention_FE_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )


# Create a new column for the formatted results
CDP3_intervention_FE_df$label <- with(CDP3_intervention_FE_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Create label with intervention name and results
CDP3_intervention_FE_df$intervention_label <- paste(CDP3_intervention_FE_df$parameter, CDP3_intervention_FE_df$label, sep = " - ")

# Reorder the 'parameter' factor levels in alphabetical order
CDP3_intervention_FE_df$intervention_label <- factor(CDP3_intervention_FE_df$intervention_label, 
                                             levels = rev(unique(CDP3_intervention_FE_df$intervention_label)))

# Create the ggplot with interventions in alphabetical order and results labels
ggplot(CDP3_intervention_FE_df, aes(x = intervention_label, y = HR)) +
  geom_point(color = "blue") +  # Plot the HR values as blue points
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add reference line at y = 1
  theme_minimal() +
  labs(x = "Intervention - HR (95% CI)", y = "Hazard Ratio (HR)", title = NULL) +  # Set axis labels and remove title
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  # Add annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(CDP3_intervention_FE_df$intervention_label)) + 1.5, y = 0.5, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(CDP3_intervention_FE_df$intervention_label)) + 1.5, y = 1.5, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  # Expand the x-axis limits to ensure the annotations are fully visible
  expand_limits(x = length(unique(CDP3_intervention_FE_df$intervention_label)) + 2) +
  # Adjust the plot margins to ensure the annotations are fully visible
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))


# Random effects NMA
# Note that we're using a non-informative prior. This leads to poor convergence
# May want to use informative prior
CDP3_mix_fit_RE <- nma(CDP3_mix_net, 
                       seed = 437219664,
                       trt_effects = "random",
                       prior_intercept = normal(0, scale = 5),
                       prior_trt = normal(0, scale = 10),
                       prior_het = half_normal(scale = 7),
                       adapt_delta = 0.999)


CDP3_mix_fit_RE <- nma(mix_net,
                  seed = 437219664,
                  trt_effects = "random",
                  prior_intercept = normal(0, scale = 5),
                  prior_trt = normal(0, scale = 5),
                  prior_het = half_normal(scale = 7),
                  adapt_delta = 0.999,
                  control = list(max_treedepth = 12),
                  iter = 4000)



CDP3_mix_fit_RE

# Create dataframe with results summary
CDP3_mix_fit_RE_summary <- summary(CDP3_mix_fit_RE)
CDP3_mix_fit_RE_summary_df <- as.data.frame(CDP3_mix_fit_RE_summary)
CDP3_intervention_RE_df <- CDP3_mix_fit_RE_summary_df[grepl("^d", CDP3_mix_fit_RE_summary_df$parameter) & !grepl("^delta", CDP3_mix_fit_RE_summary_df$parameter), ]
CDP3_intervention_RE_df$parameter <- gsub("^d\\[|\\]$", "", CDP3_intervention_RE_df$parameter)

# Calculate RR and 95% CI in the exponential scale
CDP3_intervention_RE_df <- CDP3_intervention_RE_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )


# Create a new column for the formatted results
CDP3_intervention_RE_df$label <- with(CDP3_intervention_RE_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Create label with intervention name and results
CDP3_intervention_RE_df$intervention_label <- paste(CDP3_intervention_RE_df$parameter, CDP3_intervention_RE_df$label, sep = " - ")

# Reorder the 'parameter' factor levels in alphabetical order
CDP3_intervention_RE_df$intervention_label <- factor(CDP3_intervention_RE_df$intervention_label, 
                                                     levels = rev(unique(CDP3_intervention_RE_df$intervention_label)))

# Create the ggplot with interventions in alphabetical order and results labels
ggplot(CDP3_intervention_RE_df, aes(x = intervention_label, y = HR)) +
  geom_point(color = "blue") +  # Plot the HR values as blue points
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add reference line at y = 1
  theme_minimal() +
  labs(x = "Intervention - HR (95% CI)", y = "Hazard Ratio (HR)", title = NULL) +  # Set axis labels and remove title
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  # Add annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(CDP3_intervention_RE_df$intervention_label)) + 1.5, y = 0.5, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(CDP3_intervention_RE_df$intervention_label)) + 1.5, y = 1.5, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  # Expand the x-axis limits to ensure the annotations are fully visible
  expand_limits(x = length(unique(CDP3_intervention_RE_df$intervention_label)) + 2) +
  # Adjust the plot margins to ensure the annotations are fully visible
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

# Model comparison via DIC
(CDP3_mix_dic_FE <- dic(CDP3_mix_fit_FE))
(CDP3_mix_dic_RE <- dic(CDP3_mix_fit_RE))

plot(CDP3_mix_dic_FE, ref_line = 0)
plot(CDP3_mix_dic_RE, ref_line = 0)


# Treatment effects relative to natalizumab IV300

#Fixed effects model
# Values >= 0 favour natalizumab
(CDP3_mix_releff_FE <- relative_effects(CDP3_mix_fit_FE, trt_ref = "Natalizumab IV300"))

# Extract the summary of the relative effects analysis
CDP3_mix_releff__FE_summary <- CDP3_mix_releff_FE$summary

#Convert to a dataframe
CDP3_mix_releff_df_FE <- as.data.frame(CDP3_mix_releff__FE_summary)

# Saving the dataframe as a CSV file
write.csv(CDP3_mix_releff_df_FE, "Releff_RRMS_CDP3_FE.csv", row.names = FALSE)

#Random effects model
# Values >= 0 favour natalizumab
(CDP3_mix_releff_RE <- relative_effects(CDP3_mix_fit_RE, trt_ref = "Natalizumab IV300"))

# Extract the summary of the relative effects analysis
CDP3_mix_releff__RE_summary <- CDP3_mix_releff_RE$summary

#Convert to a dataframe
CDP3_mix_releff_df_RE <- as.data.frame(CDP3_mix_releff__RE_summary)

# Saving the dataframe as a CSV file
write.csv(CDP3_mix_releff_df_RE, "Releff_RRMS_CDP3_RE.csv", row.names = FALSE)

# Fixed effect model rankings and comparisons

# Calculate treatment rankings
CDP3_rankings_FE <- posterior_ranks(CDP3_mix_fit_FE, lower_better = TRUE)

# Extract the summary of the treatment rankings
CDP3_ranking_summary_FE <- CDP3_rankings_FE$summary

# Sort the ranking summary in alphabetical order by treatment
CDP3_ranking_summary_alphabetical_FE <- CDP3_ranking_summary_FE %>%
  arrange(parameter)

# Save the sorted ranking summary as a dataframe
CDP3_ranking_summary_FE_df <- as.data.frame (CDP3_ranking_summary_alphabetical_FE)

# Generate a league table for the fixed effects model
CDP3_league_table_FE <- relative_effects(CDP3_mix_fit_FE,
                                    all_contrasts = TRUE,
)

# Convert the league table to a data frame
CDP3_league_df_FE <- as.data.frame(CDP3_league_table_FE)

CDP3_interventions_FE <- unique(c(CDP3_league_df_FE$.trtb, CDP3_league_df_FE$.trta))

# Initialize an empty matrix to store the formatted mean (SD) values
CDP3_relative_effects_FE_matrix <- matrix(NA, nrow = length(CDP3_interventions_FE), ncol = length(CDP3_interventions_FE), 
                                       dimnames = list(CDP3_interventions_FE, CDP3_interventions_FE))

# Round the relative effects to 2 decimal places
CDP3_relative_effects_FE_df <- as.data.frame(round(CDP3_relative_effects_FE_matrix, 2))

# Fill the matrix with the "mean (SD)" formatted values
for (i in 1:nrow(CDP3_league_df_FE)) {
  # Get the interventions, mean, and SD
  int1 <- CDP3_league_df_FE$.trtb[i]
  int2 <- CDP3_league_df_FE$.trta[i]
  mean_val <- round(CDP3_league_df_FE$mean[i], 2)
  sd_val <- round(CDP3_league_df_FE$sd[i], 2)
  
  # Create the "mean (SD)" formatted string
  CDP3_effect_str_FE <- paste0(mean_val, " (", sd_val, ")")
  
  # Populate the matrix for both directions of comparison
  CDP3_relative_effects_FE_matrix[int1, int2] <- CDP3_effect_str_FE
  CDP3_relative_effects_FE_matrix[int2, int1] <- CDP3_effect_str_FE  # Use the same value for symmetry
}

# Convert the matrix to a dataframe
CDP3_relative_effects_FE_df <- as.data.frame(CDP3_relative_effects_FE_matrix)


# Random effect model rankings and comparisons

# Calculate treatment rankings
CDP3_rankings_RE <- posterior_ranks(CDP3_mix_fit_RE, lower_better = TRUE)

# Extract the summary of the treatment rankings
CDP3_ranking_summary_RE <- CDP3_rankings_RE$summary

# Sort the ranking summary in alphabetical order by treatment
CDP3_ranking_summary_alphabetical_RE <- CDP3_ranking_summary_RE %>%
  arrange(parameter)

# Save the sorted ranking summary as a dataframe
CDP3_ranking_summary_RE_df <- as.data.frame (CDP3_ranking_summary_alphabetical_RE)

# Generate a league table for the fixed effects model
CDP3_league_table_RE <- relative_effects(CDP3_mix_fit_RE,
                                         all_contrasts = TRUE,
)

# Convert the league table to a data frame
CDP3_league_df_RE <- as.data.frame(CDP3_league_table_RE)

CDP3_interventions_RE <- unique(c(CDP3_league_df_RE$.trtb, CDP3_league_df_RE$.trta))

# Initialize an empty matrix to store the formatted mean (SD) values
CDP3_relative_effects_RE_matrix <- matrix(NA, nrow = length(CDP3_interventions_RE), ncol = length(CDP3_interventions_RE), 
                                          dimnames = list(CDP3_interventions_RE, CDP3_interventions_RE))

# Round the relative effects to 2 decimal places
CDP3_relative_effects_RE_df <- as.data.frame(round(CDP3_relative_effects_RE_matrix, 2))

# Fill the matrix with the "mean (SD)" formatted values
for (i in 1:nrow(CDP3_league_df_RE)) {
  # Get the interventions, mean, and SD
  int1 <- CDP3_league_df_RE$.trtb[i]
  int2 <- CDP3_league_df_RE$.trta[i]
  mean_val <- round(CDP3_league_df_RE$mean[i], 2)
  sd_val <- round(CDP3_league_df_RE$sd[i], 2)
  
  # Create the "mean (SD)" formatted string
  CDP3_effect_str_RE <- paste0(mean_val, " (", sd_val, ")")
  
  # Populate the matrix for both directions of comparison
  CDP3_relative_effects_RE_matrix[int1, int2] <- CDP3_effect_str_RE
  CDP3_relative_effects_RE_matrix[int2, int1] <- CDP3_effect_str_RE  # Use the same value for symmetry
}

# Convert the matrix to a dataframe
CDP3_relative_effects_RE_df <- as.data.frame(CDP3_relative_effects_RE_matrix)



##################################################################################################
##################################################################################################



#CDP6 Network

# Load the raw CDP6 from the SLR
# This is a mixture of hazard ratio and dichotomous raw data
CDP6_data <- read.xlsx(file = "data/nma/CDP6 (3).xlsx",
                       sheetIndex = 1)


# Simplify the column names
colnames(CDP6_data) <- gsub("CDP6..", "", colnames(CDP6_data))

# Change from arm to treatment name for simplicity
colnames(CDP6_data)[colnames(CDP6_data) == "Arm.label"] <- "Treatment"

# Delete blank rows
CDP6_data <- CDP6_data %>%
  filter(Treatment != "")

# Remove disconnected studies
CDP6_data <- CDP6_data %>%
  filter(!(Study.Name %in% c("ASCLEPIOS I", "ASCLEPIOS II", "OPTIMUM")))


#####################################################################################

# CDP6 network

# Extract the unique study names
CDP6_study_ids <- unlist(lapply(unique(CDP6_data$EndNote.ID), toString))
CDP6_study_names <- unlist(lapply(unique(CDP6_data$Study.Name), toString))

# Hazard ratio data
CDP6_contrast <- as.data.frame(CDP6_data[, c("Study.Name", "EndNote.ID","Time.point.reported..Months.", "Treatment", "Reference", "Estimate", "Effect.Estimate.Type", "LCI", "UCI", "N")])

# Remove studies with no effect estimate and no reporting rate ratio
CDP6_contrast <- CDP6_contrast %>%
  group_by(Study.Name) %>%
  filter(any(Estimate != "" & Estimate != "NR")) %>%
  ungroup()

#Ensure all relevant data data is numeric
CDP6_contrast$N <- as.numeric(CDP6_contrast$N)
CDP6_contrast$Estimate <- as.numeric(CDP6_contrast$Estimate)
CDP6_contrast$LCI <- as.numeric(CDP6_contrast$LCI)
CDP6_contrast$UCI <- as.numeric(CDP6_contrast$UCI)

# Don't need code to remove those without 95% CI as there are none
# All studies have a contrast row so don't need to include the blank row from each study
# All rows have intervention names so don't need to remove rows without intervention names
# No studies with single rows, so don't need to remove studies with only a single row (After the above removal)

# Calculate the log mean and log SE for hazard ratio data
CDP6_contrast$diff <- log(as.numeric(CDP6_contrast$Estimate))
CDP6_contrast$se_diff <- (log(as.numeric(CDP6_contrast$UCI)) - 
                            log(as.numeric(CDP6_contrast$LCI))) / (2 * 1.96)

# Calculate a small positive standard error based on non-baseline arms
small_positive_se <- CDP6_contrast %>%
  filter(!is.na(se_diff)) %>%
  summarise(min_se = min(se_diff[se_diff > 0], na.rm = TRUE)) %>%
  pull(min_se) * 0.1  # Use 1% of the smallest positive SE

# Assign this small positive SE to baseline arms where se_diff is NA
CDP6_contrast <- CDP6_contrast %>%
  mutate(
    se_diff = ifelse(is.na(se_diff), small_positive_se, se_diff)
  )

#Convert "NA" values in Reference to NA
CDP6_contrast$Reference[CDP6_contrast$Reference == "NA"] <- NA

# Raw dichotomous data
CDP6_event <- as.data.frame(CDP6_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment","Reference", "N", "No..of.pt.s.with.events")])

# Simplify variable names
CDP6_event <- CDP6_event %>%
  rename(Follow.Up.Time = Time.point.reported..Months.,
         Events = No..of.pt.s.with.events)


# Remove studies that don't report number of events
CDP6_event <- CDP6_event[!is.na(CDP6_event$Events), ]

#Ensure all relevant data data is numeric
CDP6_event$N <- as.numeric(CDP6_event$N)
CDP6_event$Events <- as.numeric(CDP6_event$Events)
CDP6_event$Follow.Up.Time <- as.numeric(CDP6_event$Follow.Up.Time)

#Calculate person-years at risk
CDP6_event <- CDP6_event %>%
  mutate(T = (Follow.Up.Time * N) / 12)

#Calculate hazard rates
CDP6_event <- CDP6_event %>%
  mutate(HR = Events/T)

#Convert "NA" values in Reference to NA
CDP6_event$Reference[CDP6_event$Reference == "NA"] <- NA

#Calculate hazard Ratios from Rates
CDP6_event <- CDP6_event %>%
  group_by(EndNote.ID) %>%
  mutate(
    Hazard.Ratio = ifelse(
      is.na(Reference),
      NA,
      HR / HR[which(is.na(Reference))]
    )
  ) %>%
  ungroup()

# Calculate the log mean for events data
CDP6_event$diff <- log(as.numeric(CDP6_event$Hazard.Ratio))


# Calculate the log SE for events data
CDP6_event <- CDP6_event %>%
  group_by(EndNote.ID) %>%
  mutate(
    ref_Events = Events[which(is.na(Reference))[1]],  # Get Events where Reference is NA within each EndNote.ID
    se_diff = ifelse(
      is.na(Hazard.Ratio),
      NA,
      sqrt((1 / Events) + (1 / ref_Events))
    )
  ) %>%
  ungroup() %>%
  select(-ref_Events)

# Treatment names in alphabetical order, but with Placebo at the start,
# thus matching what mutlinma would use
CDP6_treatment_names <- sort(unique(c(CDP6_event$Treatment, CDP6_contrast$Treatment)))
CDP6_treatment_names <- c("Placebo", CDP6_treatment_names[-which(CDP6_treatment_names == "Placebo")])


# Remove studies from the events dataset that are in the contrast dataset
CDP6_event <- CDP6_event[-which(is.element(CDP6_event$EndNote.ID, CDP6_contrast$EndNote.ID)), ]


# Create network for events data
CDP6_mix_event_net <- set_agd_contrast(CDP6_event, 
                                       study = Study.Name,
                                       trt = Treatment,
                                       y = diff, 
                                       se = se_diff,
                                       trt_ref = "Placebo",
                                       sample_size = N)

CDP6_mix_event_net


# Create network for hazard ratio data
CDP6_mix_contrast_net <- set_agd_contrast(CDP6_contrast, 
                                          study = Study.Name,
                                          trt = Treatment,
                                          y = diff, 
                                          se = se_diff,
                                          trt_ref = "Placebo",
                                          sample_size = N)
CDP6_mix_contrast_net

plot(CDP6_mix_contrast_net, weight_edges = TRUE, weight_nodes = TRUE, nudge = 0.1) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing = ggplot2::unit(0.5, "lines")
  )


# Combine the networks together
CDP6_mix_net <- combine_network(CDP6_mix_event_net, CDP6_mix_contrast_net)

# Export a summary of the network
CDP6_mix_net

# Generate a network plot with nodes weighted by sample size
# and edges by number of studies
plot(CDP6_mix_net, weight_edges = TRUE, weight_nodes = TRUE, nudge = 0.1) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing = ggplot2::unit(0.5, "lines")
  )



# Can look at estimates of basic log hazard ratios relative to placebo
CDP6_mix_fit_FE <- nma(CDP6_mix_net, 
                       seed = 437219664,
                       trt_effects = "fixed",
                       prior_intercept = normal(0, scale = 10),
                       prior_trt = normal(0, scale = 10))

mix_fit_FE <- CDP6_mix_fit_FE

# Create dataframe with results summary
CDP6_mix_fit_FE_summary <- summary(CDP6_mix_fit_FE)
CDP6_mix_fit_FE_summary_df <- as.data.frame(CDP6_mix_fit_FE_summary)
CDP6_intervention_FE_df <- CDP6_mix_fit_FE_summary_df[grep("^d", CDP6_mix_fit_FE_summary_df$parameter), ]
CDP6_intervention_FE_df$parameter <- gsub("^d\\[|\\]$", "", CDP6_intervention_FE_df$parameter)

# Calculate RR and 95% CI in the exponential scale
CDP6_intervention_FE_df <- CDP6_intervention_FE_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )


# Create a new column for the formatted results
CDP6_intervention_FE_df$label <- with(CDP6_intervention_FE_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Create label with intervention name and results
CDP6_intervention_FE_df$intervention_label <- paste(CDP6_intervention_FE_df$parameter, CDP6_intervention_FE_df$label, sep = " - ")

# Reorder the 'parameter' factor levels in alphabetical order
CDP6_intervention_FE_df$intervention_label <- factor(CDP6_intervention_FE_df$intervention_label, 
                                                     levels = rev(unique(CDP6_intervention_FE_df$intervention_label)))

intervention_df<-CDP6_intervention_FE_df

# List of specific parameter values
direct_parameters <- c(
  "Cladribine O3.5", 
  "Fingolimod O0.5", 
  "Natalizumab IV300",
  "Peginterferon beta 1a SC125",
  "Interferon beta 1a IM30"
)


# Create the new 'evidence' column
intervention_df$evidence <- ifelse(
  intervention_df$parameter %in% direct_parameters, 
  "direct", 
  "indirect"
)


# Random effects NMA
# Note that we're using a non-informative prior. This leads to poor convergence
# May want to use informative prior
mix_fit_RE <- nma(CDP6_mix_net, 
                  seed = 437219664,
                  trt_effects = "random",
                  prior_intercept = normal(0, scale = 10),
                  prior_trt = normal(0, scale = 10),
                  prior_het = half_normal(scale = 2),
                  control = list(max_treedepth = 12),
                  adapt_delta = 0.99)




# Create dataframe with results summary
mix_fit_RE_summary <- summary(mix_fit_RE)
mix_fit_RE_summary_df <- as.data.frame(mix_fit_RE_summary)
intervention_RE_df <- mix_fit_RE_summary_df[grepl("^d", mix_fit_RE_summary_df$parameter) & !grepl("^delta", mix_fit_RE_summary_df$parameter), ]
intervention_RE_df$parameter <- gsub("^d\\[|\\]$", "", intervention_RE_df$parameter)

# Calculate RR and 95% CI in the exponential scale
intervention_RE_df <- intervention_RE_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )


# Create a new column for the formatted results
intervention_RE_df$label <- with(intervention_RE_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))


# Reorder the 'parameter' factor levels in alphabetical order
intervention_RE_df$label <- factor(intervention_RE_df$intervention_label, 
                                                     levels = rev(unique(CDP6_intervention_RE_df$intervention_label)))

# Create the new 'evidence' column
intervention_RE_df$evidence <- ifelse(
  intervention_RE_df$parameter %in% direct_parameters, 
  "direct", 
  "indirect"
)

ggplot(intervention_RE_df, aes(x = parameter, y = HR, color = evidence)) +
  geom_point() +  # Plot the HR values as points, color based on evidence type
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "#D55E00") +  # Add reference line at y = 1
  theme_minimal() +
  labs(
    x = "Intervention", 
    y = "Hazard Ratio (HR)", 
    title = NULL,
    color = "Evidence Type"  # Label the legend
  ) +  
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  scale_color_manual(values = c("direct" = "#009E73", "indirect" = "#CC79A7")) +  # Map evidence type to colors
  annotate("text", x = length(unique(intervention_RE_df$parameter)) + 1.5, y = 0.5, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_RE_df$parameter)) + 1.5, y = 1.5, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  expand_limits(x = length(unique(intervention_RE_df$parameter)) + 3) +
  expand_limits(y = max(c(intervention_RE_df$UCI)) * 1.2) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
  
  annotate("text", x = length(unique(intervention_RE_df$label)) + 1.5, y = 3, 
           label = "HR", color = "black", size = 3, fontface = "bold", hjust = 0) + 
geom_text(data = intervention_RE_df, aes(x = parameter, y = 3, label = label), 
          hjust = 0, color = "black", size = 3)


ggplot(intervention_df, aes(x = parameter, y = HR, color = evidence)) +
  geom_point() +  # Plot the HR values as points, color based on evidence type
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "#D55E00") +  # Add reference line at y = 1
  theme_minimal() +
  labs(
    x = "Intervention", 
    y = "Hazard Ratio (HR)", 
    title = NULL,
    color = "Evidence Type"  # Label the legend
  ) +  
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  scale_color_manual(values = c("direct" = "#009E73", "indirect" = "#CC79A7")) +  # Map evidence type to colors
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 0.6, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 1.1, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  expand_limits(x = length(unique(intervention_df$parameter)) + 3) +
  expand_limits(y = max(c(intervention_df$UCI)) * 1.5) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
  
  annotate("text", x = length(unique(intervention_df$label)) + 1.5, y = 1.75, 
           label = "HR (95% CrI)", color = "black", size = 3, fontface = "bold", hjust = 0) + 
  geom_text(data = intervention_df, aes(x = parameter, y = 1.75, label = label), 
            hjust = 0, color = "black", size = 3)


# Model comparison via DIC
(CDP6_mix_dic_FE <- dic(CDP6_mix_fit_FE))
(CDP6_mix_dic_RE <- dic(mix_fit_RE))

plot(CDP6_mix_dic_FE, ref_line = 0)
plot(CDP6_mix_dic_RE, ref_line = 0)


# Treatment effects relative to natalizumab IV300

#Fixed effects model
# Values >= 0 favour natalizumab
(CDP6_mix_releff_FE <- relative_effects(CDP6_mix_fit_FE, trt_ref = "Natalizumab IV300"))

# Extract the summary of the relative effects analysis
CDP6_mix_releff__FE_summary <- CDP6_mix_releff_FE$summary

#Convert to a dataframe
CDP6_mix_releff_df_FE <- as.data.frame(CDP6_mix_releff__FE_summary)

# Saving the dataframe as a CSV file
write.csv(CDP6_mix_releff_df_FE, "Releff_RRMS_CDP6_FE.csv", row.names = FALSE)

#Random effects model
# Values >= 0 favour natalizumab
(CDP6_mix_releff_RE <- relative_effects(mix_fit_RE, trt_ref = "Natalizumab IV300"))

# Extract the summary of the relative effects analysis
CDP6_mix_releff__RE_summary <- CDP6_mix_releff_RE$summary

#Convert to a dataframe
CDP6_mix_releff_df_RE <- as.data.frame(CDP6_mix_releff__RE_summary)

# Saving the dataframe as a CSV file
write.csv(CDP6_mix_releff_df_RE, "Releff_RRMS_CDP6_RE.csv", row.names = FALSE)

# Fixed effect model rankings and comparisons

# Calculate treatment rankings
CDP6_rankings_FE <- posterior_ranks(CDP6_mix_fit_FE, lower_better = TRUE)

# Extract the summary of the treatment rankings
CDP6_ranking_summary_FE <- CDP6_rankings_FE$summary

# Sort the ranking summary in alphabetical order by treatment
CDP6_ranking_summary_alphabetical_FE <- CDP6_ranking_summary_FE %>%
  arrange(parameter)

# Save the sorted ranking summary as a dataframe
CDP6_ranking_summary_FE_df <- as.data.frame (CDP6_ranking_summary_alphabetical_FE)



# Generate a league table for the fixed effects model
league_table_FE <- relative_effects(mix_fit_FE,
                                    all_contrasts = TRUE,
)

# Convert the league table to a data frame
league_df_FE <- as.data.frame(league_table_FE)

# Calculate RR and 95% CI in the exponential scale
league_df_FE <- league_df_FE %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

# Create a new column for the formatted results
league_df_FE$label <- with(league_df_FE, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))


# Ensure unique interventions and Placebo at the end
interventions_FE <- unique(c(league_df_FE$.trtb, league_df_FE$.trta))
interventions_FE <- interventions_FE[order(interventions_FE != "Placebo")]

# Initialize an empty matrix with correct dimensions
relative_effects_FE_matrix <- matrix(NA, nrow = length(interventions_FE), ncol = length(interventions_FE),
                                     dimnames = list(interventions_FE, interventions_FE))

# Loop through the rows of league_df_FE to populate the matrix
for (i in 1:nrow(league_df_FE)) {
  int1 <- league_df_FE$.trtb[i]  # Row intervention
  int2 <- league_df_FE$.trta[i]  # Column intervention
  label_val <- as.character(league_df_FE$label[i])  # Ensure label_val is a single value and character type
  
  # Check if the label_val is valid and place it in the matrix
  if (!is.na(int1) && !is.na(int2) && length(label_val) == 1) {
    relative_effects_FE_matrix[int1, int2] <- label_val
  }
}

# Convert the matrix to a data frame for easier display
relative_effects_FE_df <- as.data.frame(relative_effects_FE_matrix)

# View the resulting data frame
relative_effects_FE_df


# Calculate treatment rankings
rankings_FE <- posterior_ranks(mix_fit_FE, lower_better = TRUE)

# Extract the summary of the treatment rankings
ranking_summary_FE <- rankings_FE$summary

# Sort the ranking summary by mean rank in ascending order
ranking_summary_FE_sorted <- ranking_summary_FE[order(ranking_summary_FE$mean), ]

rankings_cum_FE <- posterior_rank_probs(mix_fit_FE, lower_better = TRUE,
                                        cumulative = TRUE)

ranking_cum_summary_FE <- rankings_cum_FE$summary

# Random effect model rankings and comparisons

# Calculate treatment rankings
CDP6_rankings_RE <- posterior_ranks(CDP6_mix_fit_RE, lower_better = TRUE)

# Extract the summary of the treatment rankings
CDP6_ranking_summary_RE <- CDP6_rankings_RE$summary

# Sort the ranking summary in alphabetical order by treatment
CDP6_ranking_summary_alphabetical_RE <- CDP6_ranking_summary_RE %>%
  arrange(parameter)

# Save the sorted ranking summary as a dataframe
CDP6_ranking_summary_RE_df <- as.data.frame (CDP6_ranking_summary_alphabetical_RE)

# Generate a league table for the fixed effects model
CDP6_league_table_RE <- relative_effects(CDP6_mix_fit_RE,
                                         all_contrasts = TRUE,
)

# Convert the league table to a data frame
CDP6_league_df_RE <- as.data.frame(CDP6_league_table_RE)

CDP6_interventions_RE <- unique(c(CDP6_league_df_RE$.trtb, CDP6_league_df_RE$.trta))

# Initialize an empty matrix to store the formatted mean (SD) values
CDP6_relative_effects_RE_matrix <- matrix(NA, nrow = length(CDP6_interventions_RE), ncol = length(CDP6_interventions_RE), 
                                          dimnames = list(CDP6_interventions_RE, CDP6_interventions_RE))

# Round the relative effects to 2 decimal places
CDP6_relative_effects_RE_df <- as.data.frame(round(CDP6_relative_effects_RE_matrix, 2))

# Fill the matrix with the "mean (SD)" formatted values
for (i in 1:nrow(CDP6_league_df_RE)) {
  # Get the interventions, mean, and SD
  int1 <- CDP6_league_df_RE$.trtb[i]
  int2 <- CDP6_league_df_RE$.trta[i]
  mean_val <- round(CDP6_league_df_RE$mean[i], 2)
  sd_val <- round(CDP6_league_df_RE$sd[i], 2)
  
  # Create the "mean (SD)" formatted string
  CDP6_effect_str_RE <- paste0(mean_val, " (", sd_val, ")")
  
  # Populate the matrix for both directions of comparison
  CDP6_relative_effects_RE_matrix[int1, int2] <- CDP6_effect_str_RE
  CDP6_relative_effects_RE_matrix[int2, int1] <- CDP6_effect_str_RE  # Use the same value for symmetry
}

# Convert the matrix to a dataframe
CDP6_relative_effects_RE_df <- as.data.frame(CDP6_relative_effects_RE_matrix)
