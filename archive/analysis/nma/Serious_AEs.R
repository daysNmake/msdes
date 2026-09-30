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
AE_data <- read.xlsx(file = "data/nma/Serious AEs (2).xlsx",
                              sheetIndex = 1)


# Simplify the column names
colnames(AE_data) <- gsub("Serious.AE..", "", colnames(AE_data))

# Change from arm to treatment name for simplicity
colnames(AE_data)[colnames(AE_data) == "Arm.label"] <- "Treatment"

# Delete blank rows
AE_data <- AE_data %>%
  filter(Treatment != "")


# Use timepoint with longer follow-up time available
AE_data <- AE_data %>%
  group_by(EndNote.ID, Treatment) %>%
  slice(which.max(Time.point.reported..Months.)) %>%  # Get the row with the maximum time point
  ungroup()

#Convert "NA" values in Reference to NA
AE_data$Reference[AE_data$Reference == "NA"] <- NA

# Raw dichotomous data
AE <- as.data.frame(AE_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment","Reference", "N", "No..of.pt.s.with.events")])

# Simplify variable names
AE <- AE %>%
  rename(Follow.Up.Time = Time.point.reported..Months.,
         Events = No..of.pt.s.with.events)

# Remove studies that don't report number of events
AE <- AE[AE$Events != "NR", ]

#Ensure all relevant data data is numeric
AE$N <- as.numeric(AE$N)
AE$Events <- as.numeric(AE$Events)
AE$Follow.Up.Time <- as.numeric(AE$Follow.Up.Time)

#Add 0.5 to "Events" and 1 to "N" in studies with zero cells
AE <- AE %>%
  group_by(EndNote.ID) %>%
  mutate(
    adjust = any(Events == 0),
    Events = if_else(adjust, Events + 0.5, Events),
    N = if_else(adjust, N + 1, N)
  ) %>%
  select(-adjust) %>%
  ungroup()


#Calculate person-years at risk
AE <- AE %>%
  mutate(T = (Follow.Up.Time * N) / 12)

#Calculate hazard rates
AE <- AE %>%
  mutate(HR = Events/T)


#Calculate hazard Ratios from Rates
AE <- AE %>%
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
AE$diff <- log(as.numeric(AE$Hazard.Ratio))


# Calculate the log SE for events data
AE <- AE %>%
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
AE_treatment_names <- sort(unique(c(AE$Treatment)))
AE_treatment_names <- c("Placebo", AE_treatment_names[-which(AE_treatment_names == "Placebo")])

# Calculate a small positive standard error based on non-baseline arms
small_positive_se <- AE %>%
  filter(!is.na(se_diff)) %>%
  summarise(min_se = min(se_diff[se_diff > 0], na.rm = TRUE)) %>%
  pull(min_se) * 0.1  # Use 10% of the smallest positive SE

# Assign this small positive SE to baseline arms where se_diff is NA
AE <- AE %>%
  mutate(
    se_diff = ifelse(is.na(se_diff) & is.na(Reference), small_positive_se, se_diff)
  )

# Extract the unique study names

study_ids <- unlist(lapply(unique(AE$EndNote.ID), toString))
study_names <- unlist(lapply(unique(AE$Study.Name), toString))

#################################################################################

# Create network and run NMA

mix_net <- set_agd_contrast(AE, 
                            study = Study.Name,
                            trt = Treatment,
                            y = diff, 
                            se = se_diff,
                            sample_size = N)

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

# Create label with intervention name and results
intervention_df$intervention_label <- paste(intervention_df$parameter, intervention_df$label, sep = " - ")

# Reorder the 'parameter' factor levels in alphabetical order
intervention_df$parameter <- factor(intervention_df$parameter, 
                                             levels = rev(unique(intervention_df$parameter)))

# Create the ggplot with interventions in alphabetical order and results labels
ggplot(intervention_df, aes(x = intervention_label, y = HR)) +
  geom_point(color = "blue") +  # Plot the HR values as blue points
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add reference line at y = 1
  theme_minimal() +
  labs(x = "Intervention - HR (95% CI)", y = "Hazard Ratio (HR)", title = NULL) +  # Set axis labels and remove title
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  # Add annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(intervention_df$intervention_label)) + 1.5, y = 0.5, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0) +
  annotate("text", x = length(unique(intervention_df$intervention_label)) + 1.5, y = 1.2, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  # Expand the x-axis limits to ensure the annotations are fully visible
  expand_limits(x = length(unique(intervention_df$intervention_label)) + 2) +
  expand_limits(y = 1.5) +
  # Adjust the plot margins to ensure the annotations are fully visible
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

# Random effects NMA
# We have good convergence, so no need to run informative priors
mix_fit_RE <- nma(mix_net, 
                  seed = 437219664,
                  trt_effects = "random",
                  prior_intercept = normal(0, scale = 10),
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

# Create label with intervention name and results
intervention_df_RE$intervention_label <- paste(intervention_df_RE$parameter, intervention_df_RE$label, sep = " - ")

# Reorder the 'parameter' factor levels in alphabetical order
intervention_df_RE$parameter <- factor(intervention_df_RE$parameterl, 
                                             levels = rev(unique(intervention_df_RE$parameter)))

# Create the ggplot with interventions in alphabetical order and results labels
ggplot(intervention_df_RE, aes(x = intervention_label, y = HR)) +
  geom_point(color = "blue") +  # Plot the HR values as blue points
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add reference line at y = 1
  theme_minimal() +
  labs(x = "Intervention - HR (95% CI)", y = "Hazard Ratio (HR)", title = NULL) +  # Set axis labels and remove title
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  # Add annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(intervention_df_RE$intervention_label)) + 1.5, y = 0.7, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_df_RE$intervention_label)) + 1.5, y = 1.2, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  # Expand the x-axis limits to ensure the annotations are fully visible
  expand_limits(x = length(unique(intervention_df_RE$intervention_label)) + 2) +
  # Adjust the plot margins to ensure the annotations are fully visible
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))


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
write.csv(mix_releff_df, "Releff_RRMS_AES_FE.csv", row.names = FALSE)

# Random effects model
(mix_releff_RE <- relative_effects(mix_fit_RE, trt_ref = "Natalizumab IV300"))
mix_releff_df_RE <- as.data.frame(mix_releff_RE)

# Saving the dataframe as a CSV file
write.csv(mix_releff_df_RE, "Releff_RRMS_AES_RE.csv", row.names = FALSE)

# Fixed effect model rankings and comparisons

# Calculate treatment rankings
rankings_FE <- posterior_ranks(mix_fit_FE, lower_better = TRUE)

# Extract the summary of the treatment rankings
ranking_summary_FE <- rankings_FE$summary

# Sort the ranking summary in alphabetical order by treatment
ranking_summary_alphabetical_FE <- ranking_summary_FE %>%
  arrange(parameter)

# Save the sorted ranking summary as a dataframe
ranking_summary_FE_df <- as.data.frame (ranking_summary_alphabetical_FE)

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

