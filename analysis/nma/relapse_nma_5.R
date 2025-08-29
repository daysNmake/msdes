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
rrms_1 <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID","Time.point.reported..Months.", "Treatment", "Reference", "Effect.estimate", "Effect.Estimate.Type", "LCI.1", "UCI.1", "N")])
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

# Rate data with 95%CI - rrms_2
rrms_2 <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment", "Mean", "LCI", "UCI", "N")])

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

# Rate data without 95%CI rrms_3
rrms_3 <- as.data.frame(raw_relapse_data[, c("Study.Name", "EndNote.ID", "Time.point.reported..Months.", "Treatment","Reference", "Mean", "N")])

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

