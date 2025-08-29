# Get a list of all treatments excluding placebo
treatments <- unique(mix_net$data$Treatment)
treatments <- treatments[treatments != "Placebo"]

mix_fit_FE_nodesplit <- nma(mix_net,
                            seed = 437219664,
                            consistency = "nodesplit",
                            trt_effects = "fixed",
                            prior_intercept = normal(scale = 10),
                            prior_trt = normal(scale = 10))

# Extract the summary of node-splitting results
nodesplit_summary <- summary(mix_fit_FE_nodesplit)

# Extract rows where comparisons involve placebo
placebo_comparisons <- nodesplit_summary[nodesplit_summary$trt1 == "Placebo" | nodesplit_summary$trt2 == "Placebo", ]

# Initialize empty lists to store the extracted data
net_data <- list()
dir_data <- list()
ind_data <- list()

# Loop over each comparison in placebo_comparisons
for (i in seq_along(placebo_comparisons$summary)) {
  # Access the summary table for each comparison
  summary_df <- placebo_comparisons$summary[[i]]$summary
  
  # Extract the relevant rows and columns for each parameter type
  net_row <- summary_df[grep("^d_net", summary_df$parameter), c("parameter", "mean", "2.5%", "97.5%")]
  dir_row <- summary_df[grep("^d_dir", summary_df$parameter), c("parameter", "mean", "2.5%", "97.5%")]
  ind_row <- summary_df[grep("^d_ind", summary_df$parameter), c("parameter", "mean", "2.5%", "97.5%")]
  
  # Append to the lists
  net_data[[i]] <- net_row
  dir_data[[i]] <- dir_row
  ind_data[[i]] <- ind_row
}

# Combine lists into dataframes
net_df <- do.call(rbind, net_data)
dir_df <- do.call(rbind, dir_data)
ind_df <- do.call(rbind, ind_data)

# Print the resulting dataframes
net_df
dir_df
ind_df

# Calculate RR and 95% CI in the exponential scale
dir_df <- dir_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

dir_df$label <- with(dir_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Create the intervention column by extracting the text between 'd_dir[' and ' vs. Placebo]'
dir_df$intervention <- sub("d_dir\\[(.*?) vs\\. Placebo\\]", "\\1", dir_df$parameter)

# Perform a left join to add only the "label" field from dir_df to intervention_df
intervention_df <- intervention_df %>%
  left_join(dir_df %>% select(intervention, label), by = c("parameter" = "intervention"))


# Calculate RR and 95% CI in the exponential scale
ind_df <- ind_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

ind_df$label <- with(ind_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Create the intervention column by extracting the text between 'd_dir[' and ' vs. Placebo]'
ind_df$intervention <- sub("d_ind\\[(.*?) vs\\. Placebo\\]", "\\1", ind_df$parameter)

# Perform a left join to add only the "label" field from dir_df to intervention_df
intervention_df <- intervention_df %>%
  left_join(ind_df %>% select(intervention, label), by = c("parameter" = "intervention"))


# Rename labels
intervention_df <- intervention_df %>%
  rename(label_net = label.x)


intervention_df <- intervention_df %>%
  rename(label_dir = label.y)

intervention_df <- intervention_df %>%
  rename(label_ind = label)

# Create a data frame with 'trt2' and 'p_value'
p_value_df <- data.frame(trt2 = placebo_comparisons$trt2, p_value = placebo_comparisons$p_value)

# Ensure that the columns have the correct names for the join
intervention_df <- intervention_df %>%
  left_join(p_value_df, by = c("parameter" = "trt2"))

intervention_df <- intervention_df %>%
  mutate(p_value = round(p_value, 2))


# RE model
mix_fit_RE_nodesplit <- nma(mix_net,
                            consistency = "nodesplit",
                            seed = 437219664,
                            trt_effects = "random",
                            prior_intercept = normal(0, scale = 10),
                            prior_trt = normal(0, scale = 10),
                            prior_het = half_normal(scale = 5),
                            control = list(max_treedepth = 12),
                            adapt_delta = 0.999)

# Extract the summary of node-splitting results
nodesplit_summary <- summary(mix_fit_RE_nodesplit)

# Extract rows where comparisons involve placebo
placebo_comparisons <- nodesplit_summary[nodesplit_summary$trt1 == "Placebo" | nodesplit_summary$trt2 == "Placebo", ]

# Initialize empty lists to store the extracted data
net_data <- list()
dir_data <- list()
ind_data <- list()

# Loop over each comparison in placebo_comparisons
for (i in seq_along(placebo_comparisons$summary)) {
  # Access the summary table for each comparison
  summary_df <- placebo_comparisons$summary[[i]]$summary
  
  # Extract the relevant rows and columns for each parameter type
  net_row <- summary_df[grep("^d_net", summary_df$parameter), c("parameter", "mean", "2.5%", "97.5%")]
  dir_row <- summary_df[grep("^d_dir", summary_df$parameter), c("parameter", "mean", "2.5%", "97.5%")]
  ind_row <- summary_df[grep("^d_ind", summary_df$parameter), c("parameter", "mean", "2.5%", "97.5%")]
  
  # Append to the lists
  net_data[[i]] <- net_row
  dir_data[[i]] <- dir_row
  ind_data[[i]] <- ind_row
}

# Combine lists into dataframes
net_df <- do.call(rbind, net_data)
dir_df <- do.call(rbind, dir_data)
ind_df <- do.call(rbind, ind_data)

# Print the resulting dataframes
net_df
dir_df
ind_df

# Calculate RR and 95% CI in the exponential scale
dir_df <- dir_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

dir_df$label <- with(dir_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Create the intervention column by extracting the text between 'd_dir[' and ' vs. Placebo]'
dir_df$intervention <- sub("d_dir\\[(.*?) vs\\. Placebo\\]", "\\1", dir_df$parameter)

# Perform a left join to add only the "label" field from dir_df to intervention_df
intervention_df_RE <- intervention_df_RE %>%
  left_join(dir_df %>% select(intervention, label), by = c("parameter" = "intervention"))


# Calculate RR and 95% CI in the exponential scale
ind_df <- ind_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

ind_df$label <- with(ind_df, sprintf("%.2f (%.2f, %.2f)", HR, LCI, UCI))

# Create the intervention column by extracting the text between 'd_dir[' and ' vs. Placebo]'
ind_df$intervention <- sub("d_ind\\[(.*?) vs\\. Placebo\\]", "\\1", ind_df$parameter)

# Perform a left join to add only the "label" field from dir_df to intervention_df
intervention_df_RE <- intervention_df_RE %>%
  left_join(ind_df %>% select(intervention, label), by = c("parameter" = "intervention"))


# Rename labels
intervention_df_RE <- intervention_df_RE %>%
  rename(label_net_RE = label.x)


intervention_df_RE <- intervention_df_RE %>%
  rename(label_dir_RE = label.y)

intervention_df_RE <- intervention_df_RE %>%
  rename(label_ind_RE = label)

# Create a data frame with 'trt2' and 'p_value'
p_value_df_RE <- data.frame(trt2 = placebo_comparisons$trt2, p_value = placebo_comparisons$p_value)

# Ensure that the columns have the correct names for the join
intervention_df_RE <- intervention_df_RE %>%
  left_join(p_value_df_RE, by = c("parameter" = "trt2"))

intervention_df_RE <- intervention_df_RE %>%
  mutate(p_value = round(p_value, 2))
