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

# Perform a full outer join
merged_data <- dplyr::full_join(CDP6_data, CDP3_data, by = "Study.Name")

result_data <- bind_rows(
  CDP6_data,
  CDP3_data %>% filter(!Study.Name %in% CDP6_data$Study.Name)
)


# Remove disconnected studies
result_data <- result_data %>%
  filter(!(Study.Name %in% c("ASCLEPIOS I", "ASCLEPIOS II", "OPTIMUM")))
##########################################################################


# result network

# Extract the unique study names
result_study_ids <- unlist(lapply(unique(result_data$EndNote.ID), toString))
result_study_names <- unlist(lapply(unique(result_data$Study.Name), toString))

# Hazard ratio data
result_contrast <- as.data.frame(result_data[, c("Study.Name", "EndNote.ID","Time.point.reported..Months.", "Treatment", "Reference", "Estimate", "Effect.Estimate.Type", "LCI", "UCI", "N")])

# Remove studies with no effect estimate and no reporting rate ratio
result_contrast <- result_contrast %>%
  group_by(Study.Name) %>%
  filter(any(Estimate != "" & Estimate != "NR")) %>%
  ungroup()

#Ensure all relevant data data is numeric
result_contrast$N <- as.numeric(result_contrast$N)
result_contrast$Estimate <- as.numeric(result_contrast$Estimate)
result_contrast$LCI <- as.numeric(result_contrast$LCI)
result_contrast$UCI <- as.numeric(result_contrast$UCI)

# Don't need code to remove those without 95% CI as there are none
# All studies have a contrast row so don't need to include the blank row from each study
# All rows have intervention names so don't need to remove rows without intervention names
# No studies with single rows, so don't need to remove studies with only a single row (After the above removal)

# Calculate the log mean and log SE for hazard ratio data
result_contrast$diff <- log(as.numeric(result_contrast$Estimate))
result_contrast$se_diff <- (log(as.numeric(result_contrast$UCI)) - 
                            log(as.numeric(result_contrast$LCI))) / (2 * 1.96)

# Calculate a small positive standard error based on non-baseline arms
small_positive_se <- result_contrast %>%
  filter(!is.na(se_diff)) %>%
  summarise(min_se = min(se_diff[se_diff > 0], na.rm = TRUE)) %>%
  pull(min_se) * 0.1  # Use 1% of the smallest positive SE

# Assign this small positive SE to baseline arms where se_diff is NA
result_contrast <- result_contrast %>%
  mutate(
    se_diff = ifelse(is.na(se_diff), small_positive_se, se_diff)
  )

#Convert "NA" values in Reference to NA
result_contrast$Reference[result_contrast$Reference == "NA"] <- NA
result_contrast$Reference[result_contrast$Reference == ""] <- NA

# Raw dichotomous data
result_event <- as.data.frame(result_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment","Reference", "N", "No..of.pt.s.with.events")])

# Simplify variable names
result_event <- result_event %>%
  rename(Follow.Up.Time = Time.point.reported..Months.,
         Events = No..of.pt.s.with.events)

# Remove studies that don't report number of events
result_event <- result_event[result_event$Events != "NR", ]

#Ensure all relevant data data is numeric
result_event$N <- as.numeric(result_event$N)
result_event$Events <- as.numeric(result_event$Events)
result_event$Follow.Up.Time <- as.numeric(result_event$Follow.Up.Time)

#Calculate person-years at risk
result_event <- result_event %>%
  mutate(T = (Follow.Up.Time * N) / 12)

#Calculate hazard rates
result_event <- result_event %>%
  mutate(HR = Events/T)

#Convert "NA" values in Reference to NA
result_event$Reference[result_event$Reference == "NA"] <- NA

#Calculate hazard Ratios from Rates
result_event <- result_event %>%
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
result_event$diff <- log(as.numeric(result_event$Hazard.Ratio))


# Calculate the log SE for events data
result_event <- result_event %>%
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
result_treatment_names <- sort(unique(c(result_event$Treatment, result_contrast$Treatment)))
result_treatment_names <- c("Placebo", result_treatment_names[-which(result_treatment_names == "Placebo")])


# Remove studies from the events dataset that are in the contrast dataset
result_event <- result_event[-which(is.element(result_event$EndNote.ID, result_contrast$EndNote.ID)), ]

# Remove studies that don't report number of events
result_event <- result_event[!is.na(result_event$Events), ]



# Create network for events data
result_mix_event_net <- set_agd_contrast(result_event, 
                                       study = Study.Name,
                                       trt = Treatment,
                                       y = diff, 
                                       se = se_diff,
                                       trt_ref = "Placebo",
                                       sample_size = N)

result_mix_event_net

# Create network for hazard ratio data
result_mix_contrast_net <- set_agd_contrast(result_contrast, 
                                          study = Study.Name,
                                          trt = Treatment,
                                          y = diff, 
                                          se = se_diff,
                                          trt_ref = "Placebo",
                                          sample_size = N)
result_mix_contrast_net

# Combine the networks together
result_mix_net <- combine_network(result_mix_event_net, result_mix_contrast_net)

# Export a summary of the network
result_mix_net

mix_net <- result_mix_net

# Generate a network plot with nodes weighted by sample size
# and edges by number of studies
plot(result_mix_net, weight_edges = TRUE, weight_nodes = TRUE, nudge = 0.1) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing = ggplot2::unit(0.5, "lines")
  )

mix_fit_RE_nodesplit <- nma(result_mix_net,
                            consistency = "nodesplit",
                            seed = 437219664,
                            trt_effects = "random",
                            prior_intercept = normal(0, scale = 10),
                            prior_trt = normal(0, scale = 10),
                            prior_het = half_normal(scale = 2),
                            adapt_delta = 0.99
                            )



  





