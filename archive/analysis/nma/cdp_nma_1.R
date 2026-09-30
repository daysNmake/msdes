# NMA of CDP3 and CDP6 on RRMS NICE MTA
# Catalina Lopez 14-Oct-2024
# Largely follows multinma vignette of David Phillippo
# https://dmphillippo.github.io/multinma/articles/example_parkinsons.html

library(xlsx)
library(multinma)
library(dplyr)



#CDP3 Network

# Load the raw CDP3 from the SLR
# This is a mixture of hazard ratio and dichotomous raw data
CDP3_data <- read.xlsx(file = "data/nma/CDP3.xlsx",
                              sheetIndex = 1)


# Simplify the column names
colnames(CDP3_data) <- gsub("CDP3..", "", colnames(CDP3_data))

# Change from arm to treatment name for simplicity
colnames(CDP3_data)[colnames(CDP3_data) == "Arm.label"] <- "Treatment"

# Delete blank rows
CDP3_data <- CDP3_data %>%
  filter(Treatment != "")

# Modify OPERA I and II labels
CDP3_data <- CDP3_data %>%
  mutate(Study.Name = ifelse(Study.Name == "OPERA I/II" & N %in% c(410, 411), "OPERA I", Study.Name))

CDP3_data <- CDP3_data %>%
  mutate(Study.Name = ifelse(Study.Name == "OPERA I/II" & N %in% c(417, 418), "OPERA II", Study.Name))

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
                           sample_size = N)

# Create network for hazard ratio data
CDP3_mix_contrast_net <- set_agd_contrast(CDP3_contrast, 
                                     study = Study.Name,
                                     trt = Treatment,
                                     y = diff, 
                                     se = se_diff,
                                     sample_size = N)

# Combine the networks together
CDP3_mix_net <- combine_network(CDP3_mix_event_net, CDP3_mix_contrast_net)

# Export a summary of the network
CDP3_mix_net

# Generate a network plot with nodes weighted by sample size
# and edges by number of studies
plot(CDP3_mix_net, weight_edges = TRUE, weight_nodes = TRUE)


# Can look at estimates of basic log hazard ratios relative to placebo
CDP3_mix_fit_FE <- nma(CDP3_mix_net, 
                  trt_effects = "fixed",
                  prior_intercept = normal(scale = 100),
                  prior_trt = normal(scale = 100))


# Create dataframe with results summary
CDP3_mix_fit_FE_summary <- summary(CDP3_mix_fit_FE)
CDP3_mix_fit_FE_summary_df <- as.data.frame(CDP3_mix_fit_FE_summary)


#Calculate HR and 95% CI in the exponential scale
CDP3_mix_fit_FE_summary_df <- CDP3_mix_fit_FE_summary_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

par(mar = c(5, 14, 4, 2))
plot(CDP3_mix_fit_FE_summary_df$HR, 1:nrow(CDP3_mix_fit_FE_summary_df),  # HR on x-axis, treatments on y-axis
     xlim = range(c(CDP3_mix_fit_FE_summary_df$LCI, CDP3_mix_fit_FE_summary_df$UCI)),  # Set x-axis limits to include CIs
     pch = 16,  # Use filled circles for points
     yaxt = "n",  # Suppress default y-axis
     xlab = "Hazard Ratio (HR)",  # X-axis label
     ylab = "Treatments")  # Y-axis label

# Add custom y-axis labels with treatment names
axis(2, at = 1:nrow(CDP3_mix_fit_FE_summary_df), labels = CDP3_mix_fit_FE_summary_df$parameter, las = 1, cex.axis = 0.8)

# Add error bars to represent the confidence intervals
arrows(CDP3_mix_fit_FE_summary_df$LCI, 1:nrow(CDP3_mix_fit_FE_summary_df),  # Lower bound (LCI)
       CDP3_mix_fit_FE_summary_df$UCI, 1:nrow(CDP3_mix_fit_FE_summary_df),  # Upper bound (UCI)
       angle = 90, code = 3, length = 0.05)

# Add a reference line at x = 1 (no effect)
abline(v = 1, col = "red", lty = 2)  # Vertical dashed red line at HR = 1


# Random effects NMA
# Note that we're using a non-informative prior. This leads to poor convergence
# May want to use informative prior
CDP3_mix_fit_RE <- nma(CDP3_mix_net, 
                  seed = 437219664,
                  trt_effects = "random",
                  prior_intercept = normal(scale = 100),
                  prior_trt = normal(scale = 100),
                  prior_het = half_normal(scale = 5),
                  adapt_delta = 0.99)

# Create dataframe with results summary
CDP3_mix_fit_RE_summary <- summary(CDP3_mix_fit_RE)
CDP3_mix_fit_RE_summary_df <- as.data.frame(CDP3_mix_fit_RE_summary)

#Calculate HR and 95% CI in the exponential scale
CDP3_mix_fit_RE_summary_df <- CDP3_mix_fit_RE_summary_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

par(mar = c(5, 14, 4, 2))
plot(CDP3_mix_fit_RE_summary_df$HR, 1:nrow(CDP3_mix_fit_RE_summary_df),  # HR on x-axis, treatments on y-axis
     xlim = range(c(CDP3_mix_fit_RE_summary_df$LCI, CDP3_mix_fit_RE_summary_df$UCI)),  # Set x-axis limits to include CIs
     pch = 16,  # Use filled circles for points
     yaxt = "n",  # Suppress default y-axis
     xlab = "Hazard Ratio (HR)",  # X-axis label
     ylab = "Treatments")  # Y-axis label

# Add custom y-axis labels with treatment names
axis(2, at = 1:nrow(CDP3_mix_fit_RE_summary_df), labels = CDP3_mix_fit_RE_summary_df$parameter, las = 1, cex.axis = 0.8)

# Add error bars to represent the confidence intervals
arrows(CDP3_mix_fit_RE_summary_df$LCI, 1:nrow(CDP3_mix_fit_RE_summary_df),  # Lower bound (LCI)
       CDP3_mix_fit_RE_summary_df$UCI, 1:nrow(CDP3_mix_fit_RE_summary_df),  # Upper bound (UCI)
       angle = 90, code = 3, length = 0.05)

# Add a reference line at x = 1 (no effect)
abline(v = 1, col = "red", lty = 2)  # Vertical dashed red line at HR = 1


# Model comparison via DIC
# Although noting random effects haven't converged there isn't much 
# difference between the two models
(CDP3_mix_dic_FE <- dic(CDP3_mix_fit_FE))
(CDP3_mix_dic_RE <- dic(CDP3_mix_fit_RE))
plot(CDP3_mix_dic_FE)


# Treatment effects relative to natalizumab IV300
# Values >= 0 favour natalizumab
(CDP3_mix_releff_FE <- relative_effects(CDP3_mix_fit_FE, trt_ref = "Natalizumab IV300"))

# Extract the summary of the relative effects analysis
CDP3_mix_releff_summary <- CDP3_mix_releff_FE$summary

#Convert to a dataframe
CDP3_mix_releff_summary_df <- as.data.frame(CDP3_mix_releff_summary)

#Calculate HR and 95% CI in the exponential scale
CDP3_mix_releff_summary_df <- CDP3_mix_releff_summary_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

par(mar = c(5, 14, 4, 2))
plot(CDP3_mix_releff_summary_df$HR, 1:nrow(CDP3_mix_releff_summary_df),  # HR on x-axis, treatments on y-axis
     xlim = range(c(CDP3_mix_releff_summary_df$LCI, CDP3_mix_releff_summary_df$UCI)),  # Set x-axis limits to include CIs
     pch = 16,  # Use filled circles for points
     yaxt = "n",  # Suppress default y-axis
     xlab = "Hazard Ratio (HR)",  # X-axis label
     ylab = "Treatments")  # Y-axis label

# Add custom y-axis labels with treatment names
axis(2, at = 1:nrow(CDP3_mix_releff_summary_df), labels = CDP3_mix_releff_summary_df$parameter, las = 1, cex.axis = 0.8)

# Add error bars to represent the confidence intervals
arrows(CDP3_mix_releff_summary_df$LCI, 1:nrow(CDP3_mix_releff_summary_df),  # Lower bound (LCI)
       CDP3_mix_releff_summary_df$UCI, 1:nrow(CDP3_mix_releff_summary_df),  # Upper bound (UCI)
       angle = 90, code = 3, length = 0.05)

# Add a reference line at x = 1 (no effect)
abline(v = 1, col = "red", lty = 2)  # Vertical dashed red line at HR = 1






##################################################################################################
##################################################################################################



#CDP6 Network

# Load the raw CDP6 from the SLR
# This is a mixture of hazard ratio and dichotomous raw data
CDP6_data <- read.xlsx(file = "data/nma/CDP6.xlsx",
                       sheetIndex = 1)


# Simplify the column names
colnames(CDP6_data) <- gsub("CDP6..", "", colnames(CDP6_data))

# Change from arm to treatment name for simplicity
colnames(CDP6_data)[colnames(CDP6_data) == "Arm.label"] <- "Treatment"

# Delete blank rows
CDP6_data <- CDP6_data %>%
  filter(Treatment != "")

# Modify OPERA I and II labels
CDP6_data <- CDP6_data %>%
  mutate(Study.Name = ifelse(Study.Name == "OPERA I/II" & N %in% c(410, 411), "OPERA I", Study.Name))

CDP6_data <- CDP6_data %>%
  mutate(Study.Name = ifelse(Study.Name == "OPERA I/II" & N %in% c(417, 418), "OPERA II", Study.Name))

# Remove disconnected studies
CDP6_data <- CDP6_data %>%
  filter(!(Study.Name %in% c("ASCLEPIOS I", "ASCLEPIOS II", "OPTIMUM")))


#####################################################################################


# Extract the unique study names
CDP6_study_ids <- unlist(lapply(unique(CDP6_data$EndNote.ID), toString))
cdp6_study_nameS <- unlist(lapply(unique(CDP6_data$Study.Name), toString))

# Hazard ratio data
CDP6_contrast <- as.data.frame(CDP6_data[, c("Study.Name", "EndNote.ID","Time.point.reported..Months.", "Treatment", "Reference", "Estimate", "Effect.Estimate.Type", "LCI", "UCI", "N")])

#Convert "NA" and empty values in Reference to NA
CDP6_contrast <- CDP6_contrast %>%
  mutate(Reference = ifelse(Reference == "" | Reference == "NA", NA, Reference))

# Remove studies with no effect estimate and no reporting rate ratio
CDP6_contrast <- CDP6_contrast %>%
  group_by(Study.Name) %>%
  filter(any(Estimate != "" & Estimate != "NR")) %>%
  ungroup()

# Remove studies with effect estimates different to HR
CDP6_contrast <- CDP6_contrast %>%
  group_by(Study.Name) %>%
  filter(any(Effect.Estimate.Type == "HR")) %>%
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


# Raw dichotomous data
CDP6_event <- as.data.frame(CDP6_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment","Reference", "N", "No..of.pt.s.with.events")])

# Simplify variable names
CDP6_event <- CDP6_event %>%
  rename(Follow.Up.Time = Time.point.reported..Months.,
         Events = No..of.pt.s.with.events)

# Remove studies that don't report number of events
CDP6_event <- CDP6_event[CDP6_event$Events != "NR" & CDP6_event$Event != "" , ]

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
CDP6_event <- CDP6_event %>%
  mutate(Reference = ifelse(Reference == "" | Reference == "NA", NA, Reference))

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
                                  sample_size = N)

# Create network for hazard ratio data
CDP6_mix_contrast_net <- set_agd_contrast(CDP6_contrast, 
                                     study = Study.Name,
                                     trt = Treatment,
                                     y = diff, 
                                     se = se_diff,
                                     sample_size = N)

# Combine the networks together
CDP6_mix_net <- combine_network(CDP6_mix_event_net, CDP6_mix_contrast_net)

# Export a summary of the network
CDP6_mix_net

# Generate a network plot with nodes weighted by sample size
# and edges by number of studies
plot(CDP6_mix_net, weight_edges = TRUE, weight_nodes = TRUE)

# Fixed effects NMA
CDP6_mix_fit_FE <- nma(CDP6_mix_net, 
                  trt_effects = "fixed",
                  prior_intercept = normal(scale = 100),
                  prior_trt = normal(scale = 100))

# Can look at estimates of basic log hazard ratios relative to placebo

# Create dataframe with results summary
CDP6_mix_fit_FE_summary <- summary(CDP6_mix_fit_FE)
CDP6_mix_fit_FE_summary_df <- as.data.frame(CDP6_mix_fit_FE_summary)

#Calculate HR and 95% CI in the exponential scale
CDP6_mix_fit_FE_summary_df <- CDP6_mix_fit_FE_summary_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

par(mar = c(5, 14, 4, 2))
plot(CDP6_mix_fit_FE_summary_df$HR, 1:nrow(CDP6_mix_fit_FE_summary_df),  # HR on x-axis, treatments on y-axis
     xlim = range(c(CDP6_mix_fit_FE_summary_df$LCI, CDP6_mix_fit_FE_summary_df$UCI)),  # Set x-axis limits to include CIs
     pch = 16,  # Use filled circles for points
     yaxt = "n",  # Suppress default y-axis
     xlab = "Hazard Ratio (HR)",  # X-axis label
     ylab = "Treatments")  # Y-axis label

# Add custom y-axis labels with treatment names
axis(2, at = 1:nrow(CDP6_mix_fit_FE_summary_df), labels = CDP6_mix_fit_FE_summary_df$parameter, las = 1, cex.axis = 0.8)

# Add error bars to represent the confidence intervals
arrows(CDP6_mix_fit_FE_summary_df$LCI, 1:nrow(CDP6_mix_fit_FE_summary_df),  # Lower bound (LCI)
       CDP6_mix_fit_FE_summary_df$UCI, 1:nrow(CDP6_mix_fit_FE_summary_df),  # Upper bound (UCI)
       angle = 90, code = 3, length = 0.05)

# Add a reference line at x = 1 (no effect)
abline(v = 1, col = "red", lty = 2)  # Vertical dashed red line at HR = 1



# Random effects NMA
# Note that we're using a non-informative prior. This leads to poor convergence
# May want to use informative prior
CDP6_mix_fit_RE <- nma(CDP6_mix_net, 
                  seed = 437219664,
                  trt_effects = "random",
                  prior_intercept = normal(scale = 100),
                  prior_trt = normal(scale = 100),
                  prior_het = half_normal(scale = 5),
                  adapt_delta = 0.995)


# Create dataframe with results summary
CDP6_mix_fit_RE_summary <- summary(CDP6_mix_fit_RE)
CDP6_mix_fit_RE_summary_df <- as.data.frame(CDP6_mix_fit_RE_summary)

#Need to insert code for dataframe and plot

# Treatment effects relative to natalizumab IV300
# Values >= 0 favour natalizumab
(CDP6_mix_releff_FE <- relative_effects(CDP6_mix_fit_FE, trt_ref = "Natalizumab IV300"))

# Extract the summary of the relative effects analysis
CDP6_mix_releff_summary <- CDP6_mix_releff_FE$summary

#Convert to a dataframe
CDP6_mix_releff_summary_df <- as.data.frame(CDP6_mix_releff_summary)

#Calculate HR and 95% CI in the exponential scale
CDP6_mix_releff_summary_df <- CDP6_mix_releff_summary_df %>%
  mutate(
    HR = exp(mean),          # Exponentiate the mean values
    LCI = exp(`2.5%`),        # Exponentiate the 2.5% CI lower bound
    UCI = exp(`97.5%`)        # Exponentiate the 97.5% CI upper bound
  )

par(mar = c(5, 14, 4, 2))
plot(CDP6_mix_releff_summary_df$HR, 1:nrow(CDP6_mix_releff_summary_df),  # HR on x-axis, treatments on y-axis
     xlim = range(c(CDP6_mix_releff_summary_df$LCI, CDP6_mix_releff_summary_df$UCI)),  # Set x-axis limits to include CIs
     pch = 16,  # Use filled circles for points
     yaxt = "n",  # Suppress default y-axis
     xlab = "Hazard Ratio (HR)",  # X-axis label
     ylab = "Treatments")  # Y-axis label

# Add custom y-axis labels with treatment names
axis(2, at = 1:nrow(CDP6_mix_releff_summary_df), labels = CDP6_mix_releff_summary_df$parameter, las = 1, cex.axis = 0.8)

# Add error bars to represent the confidence intervals
arrows(CDP6_mix_releff_summary_df$LCI, 1:nrow(CDP6_mix_releff_summary_df),  # Lower bound (LCI)
       CDP6_mix_releff_summary_df$UCI, 1:nrow(CDP6_mix_releff_summary_df),  # Upper bound (UCI)
       angle = 90, code = 3, length = 0.05)

# Add a reference line at x = 1 (no effect)
abline(v = 1, col = "red", lty = 2)  # Vertical dashed red line at HR = 1






###################################################################################
