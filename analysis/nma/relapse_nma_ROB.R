# NMA of relapse rates on highly active RRMS NICE MTA
# Howard Thom 4-Oct-2024
# Modified by catalina Lopez 10-Oct-2024
# Largely follows multinma vignette of David Phillippo
# https://dmphillippo.github.io/multinma/articles/example_parkinsons.html

library(xlsx)
library(multinma)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)

# Load the raw relapse data from the SLR
# This is a mixture of rate and rate ratio data
raw_relapse_data <- read.xlsx(file = "data/nma/Relapse rates (4).xlsx",
                              sheetIndex = 1)


# Simplify the column names
colnames(raw_relapse_data) <- gsub("Relapse.Rate..", "", colnames(raw_relapse_data))

# Change from arm to treatment name for simplicity
colnames(raw_relapse_data)[colnames(raw_relapse_data) == "Arm.label"] <- "Treatment"

#####################################################################################

#Create 3 independent and mutually exclusive dataframes and tidy up 

# Extract the unique study names
study_ids <- unlist(lapply(unique(raw_relapse_data$EndNote.ID), toString))
study_names <- unlist(lapply(unique(raw_relapse_data$Study.Name), toString))

#Rate ratio data with 95% CI - rrms_1
rrms_1 <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID", "ROB", "Time.point.reported..Months.", "Treatment", "Reference", "Effect.estimate", "Effect.Estimate.Type", "LCI.1", "UCI.1", "N")])
# Remove studies with no effect estimate and no reporting rate ratio
rrms_1<- rrms_1[rrms_1$Effect.estimate != "" &
                                 rrms_1$LCI.1 != "NR" & rrms_1$LCI.1 != "" &
                                 rrms_1$UCI.1 != "NR" & rrms_1$UCI.1 != "", ]

rrms_1$N <- as.numeric(rrms_1$N)
rrms_1$Effect.estimate <- as.numeric(rrms_1$Effect.estimate)
rrms_1$LCI.1 <- as.numeric(rrms_1$LCI.1)
rrms_1$UCI.1 <- as.numeric(rrms_1$UCI.1)

#Convert "NA" values in Reference to NA
rrms_1$Reference[rrms_1$Reference == "NA"] <- NA

#Remove studies with ROB "Some concerns" or "High"
rrms_1 <- rrms_1[rrms_1$ROB == "Low", ]

# Rate data with 95%CI - rrms_2
rrms_2 <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID", "ROB", "Time.point.reported..Months.", "Treatment", "Mean", "LCI", "UCI", "N")])

# Remove studies that don't report LCI and UCI
rrms_2 <- rrms_2[rrms_2$Mean != "" &
                       rrms_2$LCI != "NR" & rrms_2$LCI != "" &
                       rrms_2$UCI != "NR" & rrms_2$UCI != "", ]

# Use timepoint with longer follow-up time available
rrms_2 <- rrms_2 %>%
  group_by(EndNote.ID, Treatment) %>%
  filter(Time.point.reported..Months. == max(Time.point.reported..Months.)) %>%
  ungroup()

# Ensure all relevant data is numeric
rrms_2$N <- as.numeric(rrms_2$N)
rrms_2$Mean <- as.numeric(rrms_2$Mean)
rrms_2$LCI <- as.numeric(rrms_2$LCI)
rrms_2$UCI <- as.numeric(rrms_2$UCI)

# Remove studies reported with rate ratio data and 95%CI
rrms_2 <- rrms_2[-which(is.element(rrms_2$EndNote.ID, rrms_1$EndNote.ID)), ]

#Remove studies with ROB "Some concerns" or "High"
rrms_2 <- rrms_2[rrms_2$ROB == "Low", ]

# Rate data without 95%CI rrms_3
rrms_3 <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID", "ROB", "Time.point.reported..Months.", "Treatment","Reference", "Mean", "N")])

# Remove rows with no rate data
rrms_3 <- rrms_3[rrms_3$Mean != "", ]

# Use time point with longer follow-up time available
rrms_3 <- rrms_3 %>%
  group_by(EndNote.ID, Treatment) %>%
  filter(Time.point.reported..Months. == max(Time.point.reported..Months.)) %>%
  ungroup()

# Make sure "Mean" and "N" are numeric
rrms_3$Mean <- as.numeric(rrms_3$Mean)
rrms_3$N <- as.numeric(rrms_3$N)

#Convert "NA" values in Reference to NA
rrms_3$Reference[rrms_3$Reference == "NA"] <- NA

# Remove studies reported with rate ratio data and 95%CI
rrms_3 <- rrms_3[-which(is.element(rrms_3$EndNote.ID, rrms_1$EndNote.ID)), ]

# Remove studies reported with rate data and 95%CI
rrms_3 <- rrms_3[-which(is.element(rrms_3$EndNote.ID, rrms_2$EndNote.ID)), ]

#Remove studies with ROB "Some concerns" or "High"
rrms_3 <- rrms_3[rrms_3$ROB == "Low", ]

#######################################################################################################

# Get dataframes ready for NMA

# Calculate the log mean and log SE for for RRMS_1 (rate ratio + 95% CI)

rrms_1$diff <- log(as.numeric(rrms_1$Effect.estimate))
rrms_1$se_diff <- (log(as.numeric(rrms_1$UCI.1)) - 
                            log(as.numeric(rrms_1$LCI.1))) / (2 * 1.96)

# Calculate the ratios, log mean and log SE for for RRMS_3 (rate ratio without 95% CI)
rrms_3 <- rrms_3 %>%
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
rrms_3$diff <- log(as.numeric(rrms_3$Effect.Estimate))

# Calculate the log SE for rates data
rrms_3 <- rrms_3 %>%
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

# Combine rrms_1 and rrms_3 to create rrms_contrast
rrms_contrast <- bind_rows(
  rrms_1 %>% select(Study.Name, EndNote.ID, Time.point.reported..Months., Treatment, Reference, N, diff, se_diff),
  rrms_3 %>% select(Study.Name, EndNote.ID, Time.point.reported..Months., Treatment, Reference, N, diff, se_diff)
)

# Calculate a small positive standard error based on non-baseline arms
small_positive_se <- rrms_contrast %>%
  filter(!is.na(se_diff)) %>%
  summarise(min_se = min(se_diff[se_diff > 0], na.rm = TRUE)) %>%
  pull(min_se) * 0.1  # Use 10% of the smallest positive SE

# Assign this small positive SE to baseline arms where se_diff is NA
rrms_contrast <- rrms_contrast %>%
  mutate(
    se_diff = ifelse(is.na(se_diff) & is.na(Reference), small_positive_se, se_diff)
  )

#Calculate y and se for rate and 95%CI data (rrms_2)
rrms_2$y <- log(as.numeric(rrms_2$Mean))
rrms_2$se <- (log(as.numeric(rrms_2$UCI)) - 
                  log(as.numeric(rrms_2$LCI))) / (2 * 1.96)

#################################################################################

# Create networks and run NMA

# Create network for rrms_contrast
mix_contrast_net <- set_agd_contrast(rrms_contrast, 
                                     study = Study.Name,
                                     trt = Treatment,
                                     y = diff, 
                                     se = se_diff,
                                     sample_size = N)

# Create network for rrms_2 (rate and 95%CI data)
mix_arm_net <- set_agd_arm(rrms_2, 
                           study = Study.Name,
                           trt = Treatment,
                           y = y, 
                           se = se,
                           sample_size = N)

# Combine the networks together
mix_net <- combine_network(mix_arm_net, mix_contrast_net)

# Export a summary of the network
mix_net

# Generate a network plot with nodes weighted by sample size
# and edges by number of studies
plot(mix_net, weight_edges = TRUE, weight_nodes = TRUE, nudge = 0.1) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing = ggplot2::unit(0.5, "lines")
  )

# Fixed effects NMA
mix_fit_FE <- nma(mix_net, 
                  seed = 437219664,
                  trt_effects = "fixed",
                  prior_intercept = normal(0, scale = 10),
                  prior_trt = normal(0, scale = 10))



# Can look at estimates of basic log hazard ratios relative to placebo
# Lower values indicate lower risk of relapse
# As expected alemtuzumab has lowest risk although natalizumab is second (joint with
# ocrelizumab)
mix_fit_FE

# Create dataframe with results summary
mix_fit_FE_summary <- summary(mix_fit_FE)
mix_fit_FE_summary_df <- as.data.frame(mix_fit_FE_summary)
intervention_df <- mix_fit_FE_summary_df[grep("^d", mix_fit_FE_summary_df$parameter), ]
intervention_df$parameter <- gsub("^d\\[|\\]$", "", intervention_df$parameter)

# Calculate RR and 95% CI in the exponential scale
intervention_df <- intervention_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )


# Create a new column for the formatted results
intervention_df$label <- with(intervention_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))


# Reorder the 'parameter' factor levels in alphabetical order
intervention_df$parameter <- factor(intervention_df$parameter, 
                                             levels = rev(unique(intervention_df$parameter)))
# Create the ggplot with interventions in alphabetical order and results labels
ggplot(intervention_df, aes(x = parameter, y = HR)) +
  geom_point(color = "blue") +  # Plot the HR values as blue points
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add reference line at y = 1
  theme_minimal() +
  labs(x = NULL, y = "Rate Ratio (RR)", title = NULL) +  # Set axis labels and remove title
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  
  # Add annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 0.5, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 1.2, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  
  # Add a heading for the new column "RR (95% CrI)"
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 2, 
           label = "RR (95% CrI)", color = "black", size = 4, fontface = "bold", hjust = 0) +
  
  # Adjust the y-axis limits to ensure the labels are fully visible
  expand_limits(y = max(intervention_df$UCI) * 1.5) +  # Increase the upper limit of y-axis
  expand_limits(x = length(unique(intervention_df$parameter)) + 2) +
  
  # Adjust the plot margins to ensure the annotations are fully visible
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
  
  # Left-align intervention labels
  theme(axis.text.y = element_text(hjust = 0)) +
  
  # Add the 'label' column to the right of intervention labels with smaller font
  geom_text(aes(x = parameter, y = 2, label = label), hjust = 0, color = "black", size = 3)


# Random effects NMA
# We have good convergence, so no need to run informative priors
mix_fit_RE <- nma(mix_net, 
                  seed = 437219664,
                  trt_effects = "random",
                  prior_intercept = normal(0, scale = 5),
                  prior_trt = normal(0, scale = 10),
                  prior_het = half_normal(scale = 2),
                  adapt_delta = 0.99)

mix_fit_RE

# Create dataframe with results summary
mix_fit_RE_summary <- summary(mix_fit_RE)
mix_fit_RE_summary_df <- as.data.frame(mix_fit_RE_summary)
intervention_df_RE <- mix_fit_RE_summary_df[grepl("^d", mix_fit_RE_summary_df$parameter) & !grepl("^delta", mix_fit_RE_summary_df$parameter), ]
intervention_df_RE$parameter <- gsub("^d\\[|\\]$", "", intervention_df_RE$parameter)

# Calculate RR and 95% CI in the exponential scale
intervention_df_RE <- intervention_df_RE %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

# Create a new column for the formatted results
intervention_df_RE$label <- with(intervention_df_RE, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Reorder the 'parameter' factor levels in alphabetical order
intervention_df_RE$parameter <- factor(intervention_df_RE$parameter, 
                                             levels = rev(unique(intervention_df_RE$parameter)))

# Create the ggplot with interventions in alphabetical order and results labels
ggplot(intervention_df_RE, aes(x = parameter, y = HR)) +
  geom_point(color = "blue") +  # Plot the HR values as blue points
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add reference line at y = 1
  theme_minimal() +
  labs(x = NULL, y = "Rate Ratio (RR)", title = NULL) +  # Set axis labels and remove title
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  
  # Add annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(intervention_df_RE$parameter)) + 1.5, y = 0.5, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_df_RE$parameter)) + 1.5, y = 1.2, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  
  # Add a heading for the new column "RR (95% CrI)"
  annotate("text", x = length(unique(intervention_df_RE$parameter)) + 1.5, y = 2, 
           label = "RR (95% CrI)", color = "black", size = 4, fontface = "bold", hjust = 0) +
  
  # Adjust the y-axis limits to ensure the labels are fully visible
  expand_limits(y = max(intervention_df_RE$UCI) * 1.5) +  # Increase the upper limit of y-axis
  expand_limits(x = length(unique(intervention_df_RE$parameter)) + 2) +
  
  # Adjust the plot margins to ensure the annotations are fully visible
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
  
  # Left-align intervention labels
  theme(axis.text.y = element_text(hjust = 0)) +
  
  # Add the 'label' column to the right of intervention labels with smaller font
  geom_text(aes(x = parameter, y = 2, label = label), hjust = 0, color = "black", size = 3)




# Model comparison via DIC
(mix_dic_FE <- dic(mix_fit_FE))
(mix_dic_RE <- dic(mix_fit_RE))

plot(mix_dic_FE, ref_line = 0)
plot(mix_dic_RE, ref_line = 0)

# Treatment effects relative to natalizumab IV300
# Values >= 0 favour natalizumab

# Mixed effects model
(mix_releff_FE <- relative_effects(mix_fit_FE, trt_ref = "Natalizumab IV300"))
mix_releff_df <- as.data.frame(mix_releff_FE)

# Saving the dataframe as a CSV file
write.csv(mix_releff_df, "Releff_RRMS_ARR_FE.csv", row.names = FALSE)

# Random effects model
(mix_releff_RE <- relative_effects(mix_fit_RE, trt_ref = "Natalizumab IV300"))
mix_releff_df_RE <- as.data.frame(mix_releff_RE)

# Saving the dataframe as a CSV file
write.csv(mix_releff_df_RE, "Releff_RRMS_ARR_RE.csv", row.names = FALSE)

# Fixed effect model rankings and comparisons

# Calculate treatment rankings
rankings_FE <- posterior_ranks(mix_fit_FE, lower_better = TRUE)

# Extract the summary of the treatment rankings
ranking_summary_FE <- rankings_FE$summary

# Sort the ranking summary by mean rank in ascending order
ranking_summary_FE_sorted <- ranking_summary_FE[order(ranking_summary_FE$mean), ]

rankings_cum_FE <- posterior_rank_probs(mix_fit_FE, lower_better = TRUE,
                                   cumulative = TRUE)

ranking_cum_summary_FE <- rankings_cum_FE$summary

# Generate a league table for the fixed effects model
league_table_FE <- relative_effects(mix_fit_FE,
                                 all_contrasts = TRUE,
                                 )

# Convert the league table to a data frame
league_df_FE <- as.data.frame(league_table_FE)

# Get a list of unique interventions
interventions_FE <- unique(c(league_df_FE$.trtb, league_df_FE$.trta))

# Initialize an empty matrix to store the formatted mean (SD) values
relative_effects_FE_matrix <- matrix(NA, nrow = length(interventions_FE), ncol = length(interventions_FE), 
                                  dimnames = list(interventions_FE, interventions_FE))

# Round the relative effects to 2 decimal places
relative_effects_FE_df <- as.data.frame(round(relative_effects_FE_matrix, 2))

# Fill the matrix with the "mean (SD)" formatted values
for (i in 1:nrow(league_df_FE)) {
  # Get the interventions, mean, and SD
  int1 <- league_df_FE$.trtb[i]
  int2 <- league_df_FE$.trta[i]
  mean_val <- round(league_df_FE$mean[i], 2)
  sd_val <- round(league_df_FE$sd[i], 2)
  
  # Create the "mean (SD)" formatted string
  effect_str <- paste0(mean_val, " (", sd_val, ")")
  
  # Populate the matrix for both directions of comparison
  relative_effects_FE_matrix[int1, int2] <- effect_str
  relative_effects_FE_matrix[int2, int1] <- effect_str  # Use the same value for symmetry
}

# Convert the matrix to a dataframe
relative_effects_FE_df <- as.data.frame(relative_effects_FE_matrix)

# View the resulting dataframe
relative_effects_FE_df

# Random effect model rankings and comparisons

# Calculate treatment rankings
rankings_RE <- posterior_ranks(mix_fit_RE, lower_better = TRUE)

# Extract the summary of the treatment rankings
ranking_summary_RE <- rankings_RE$summary

# Sort the ranking summary in alphabetical order by treatment
ranking_summary_alphabetical_RE <- ranking_summary_RE %>%
  arrange(parameter)

# Save the sorted ranking summary as a dataframe
ranking_summary_RE_df <- as.data.frame (ranking_summary_alphabetical_RE)

# Generate a league table for the fixed effects model
league_table_RE <- relative_effects(mix_fit_RE,
                                    all_contrasts = TRUE,
                                    )

# Convert the league table to a data frame
league_df_RE <- as.data.frame(league_table_RE)

# Get a list of unique interventions
interventions_RE <- unique(c(league_df_RE$.trtb, league_df_RE$.trta))

# Initialize an empty matrix to store the formatted mean (SD) values
relative_effects_RE_matrix <- matrix(NA, nrow = length(interventions_RE), ncol = length(interventions_RE), 
                                     dimnames = list(interventions_RE, interventions_RE))

# Round the relative effects to 2 decimal places
relative_effects_RE_df <- as.data.frame(round(relative_effects_RE_matrix, 2))

# Fill the matrix with the "mean (SD)" formatted values
for (i in 1:nrow(league_df_RE)) {
  # Get the interventions, mean, and SD
  int1 <- league_df_RE$.trtb[i]
  int2 <- league_df_RE$.trta[i]
  mean_val <- round(league_df_RE$mean[i], 2)
  sd_val <- round(league_df_RE$sd[i], 2)
  
  # Create the "mean (SD)" formatted string
  effect_str_RE <- paste0(mean_val, " (", sd_val, ")")
  
  # Populate the matrix for both directions of comparison
  relative_effects_RE_matrix[int1, int2] <- effect_str_RE
  relative_effects_RE_matrix[int2, int1] <- effect_str_RE  # Use the same value for symmetry
}

# Convert the matrix to a dataframe
relative_effects_RE_df <- as.data.frame(relative_effects_RE_matrix)

# View the resulting dataframe
relative_effects_RE_df

