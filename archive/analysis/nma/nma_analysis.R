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

# Reorder the 'parameter' factor levels in alphabetical order
intervention_df_RE$parameter <- factor(intervention_df_RE$parameter, 
                                       levels = rev(unique(intervention_df_RE$parameter)))

# Create the ggplot with interventions in alphabetical order and results labels
ggplot(intervention_df_RE, aes(x = parameter, y = HR)) +
  geom_point(color = "blue") +  # Plot the HR values as blue points
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +  # Add error bars for LCI and UCI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add reference line at y = 1
  theme_minimal() +
  labs(x = NULL, y = "Hazard Ratio (HR)", title = NULL) +  # Set axis labels and remove title
  coord_flip() +  # Flip coordinates to make the intervention names more readable
  
  # Add annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(intervention_df_RE$parameter)) + 1.5, y = 0.5, 
           label = "Favours Intervention", color = "black", size = 4, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_df_RE$parameter)) + 1.5, y = 1.2, 
           label = "Favours Placebo", color = "black", size = 4, hjust = 0) +
  
  # Add a heading for the new column "RR (95% CrI)"
  annotate("text", x = length(unique(intervention_df_RE$parameter)) + 1.5, y = 3.5, 
           label = "HR (95% CrI)", color = "black", size = 4, fontface = "bold", hjust = 0) +
  
  # Adjust the y-axis limits to ensure the labels are fully visible
  expand_limits(y = max(intervention_df_RE$UCI) * 1.5) +  # Increase the upper limit of y-axis
  expand_limits(x = length(unique(intervention_df_RE$parameter)) + 2) +
  
  # Adjust the plot margins to ensure the annotations are fully visible
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
  
  # Left-align intervention labels
  theme(axis.text.y = element_text(hjust = 0)) +
  
  # Add the 'label' column to the right of intervention labels with smaller font
  geom_text(aes(x = parameter, y = 3.5, label = label), hjust = 0, color = "black", size = 3)




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
write.csv(mix_releff_df, "results/nma/Releff_RRMS_CDP6_3_FE.csv", row.names = FALSE)

# Random effects model
(mix_releff_RE <- relative_effects(mix_fit_RE, trt_ref = "Natalizumab IV300"))
mix_releff_df_RE <- as.data.frame(mix_releff_RE)

# Saving the dataframe as a CSV file
write.csv(mix_releff_df_RE, "results/nma/Releff_RRMS_CDP6_3_RE.csv", row.names = FALSE)

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


# Random effect model rankings and comparisons

# Calculate treatment rankings
rankings_RE <- posterior_ranks(mix_fit_RE, lower_better = TRUE)

# Extract the summary of the treatment rankings
ranking_summary_RE <- rankings_RE$summary

# Sort the ranking summary by mean rank in ascending order
ranking_summary_RE_sorted <- ranking_summary_RE[order(ranking_summary_RE$mean), ]

rankings_cum_RE <- posterior_rank_probs(mix_fit_RE, lower_better = TRUE,
                                        cumulative = TRUE)

ranking_cum_summary_RE <- rankings_cum_RE$summary

# Generate a league table for the fixed effects model
league_table_RE <- relative_effects(mix_fit_RE,
                                    all_contrasts = TRUE,
)

# Convert the league table to a data frame
league_df_RE <- as.data.frame(league_table_RE)

# Calculate RR and 95% CI in the exponential scale
league_df_RE <- league_df_RE %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

# Create a new column for the formatted results
league_df_RE$label <- with(league_df_RE, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))


# Ensure unique interventions and Placebo at the end
interventions_RE <- unique(c(league_df_RE$.trtb, league_df_RE$.trta))
interventions_RE <- interventions_RE[order(interventions_RE != "Placebo")]

# Initialize an empty matrix with correct dimensions
relative_effects_RE_matrix <- matrix(NA, nrow = length(interventions_RE), ncol = length(interventions_RE),
                                     dimnames = list(interventions_RE, interventions_RE))

# Loop through the rows of league_df_RE to populate the matrix
for (i in 1:nrow(league_df_RE)) {
  int1 <- league_df_RE$.trtb[i]  # Row intervention
  int2 <- league_df_RE$.trta[i]  # Column intervention
  label_val <- as.character(league_df_RE$label[i])  # Ensure label_val is a single value and character type
  
  # Check if the label_val is valid and place it in the matrix
  if (!is.na(int1) && !is.na(int2) && length(label_val) == 1) {
    relative_effects_RE_matrix[int1, int2] <- label_val
  }
}

# Convert the matrix to a data frame for easier display
relative_effects_RE_df <- as.data.frame(relative_effects_RE_matrix)

# View the resulting data frame
relative_effects_RE_df



