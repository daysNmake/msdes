#######################################

#Suggested new method for vectorising summarising results
######################################
# Pre-allocate
dim_results <- c(n_samples, n_treatments, n_patients)

n_spms_ipd_new<-
  n_relapse_ipd_new<-
  n_edssincrease_ipd_new<-
  n_edssdecrease_ipd_new<-
  n_sae_ipd_new<-
  final_edss_value_ipd_new<-
  array(
    NA_real_,
    dim = dim_results,
    dimnames = list(NULL, v_treatment_names, NULL)
  )

trt_idx <- setNames(seq_along(v_treatment_names), v_treatment_names)

for (i_sample in seq_len(n_samples)) {
  
  df <- results[[1]][[i_sample]]$merged_df
  
  # Keep only final ("death") rows
  df <- df[df$evtname == "death", ]
  
  idx <- cbind(
    i_sample,
    trt_idx[df$arm],
    df$pat_id
  )
  
  n_relapse_ipd_new[idx]         <- df$n_relapse
  n_spms_ipd_new[idx]            <- df$spms_flag
  n_edssincrease_ipd_new[idx]    <- df$n_edssinc_evts
  n_edssdecrease_ipd_new[idx]    <- df$n_edssdec_evts
  n_sae_ipd_new[idx]             <- df$n_sae_evts
  final_edss_value_ipd_new[idx]  <- df$edss_value
}

O(n_samples * n_treatments * n_patients * n_rows)

O(n_samples * n_rows)

# Mean across patients
total_n_relapse_new         <- apply(n_relapse_ipd_new,        c(1, 2), mean, na.rm = TRUE)
total_n_spms_new            <- apply(n_spms_ipd_new,           c(1, 2), mean, na.rm = TRUE)
total_edss_increases_new    <- apply(n_edssincrease_ipd_new,   c(1, 2), mean, na.rm = TRUE)
total_edss_decreases_new    <- apply(n_edssdecrease_ipd_new,   c(1, 2), mean, na.rm = TRUE)
total_saes_new              <- apply(n_sae_ipd_new,            c(1, 2), mean, na.rm = TRUE)
total_final_edss_value_new  <- apply(final_edss_value_ipd_new, c(1, 2), mean, na.rm = TRUE)

key_events_trt_grps_new <- data.frame(
  n_spms_trt_new            = apply(total_n_spms_new,           2, summarise_samples),
  n_rel_trt_new             = apply(total_n_relapse_new,        2, summarise_samples),
  n_edssincrease_trt_new    = apply(total_edss_increases_new,   2, summarise_samples),
  n_edssdecrease_trt_new    = apply(total_edss_decreases_new,   2, summarise_samples),
  n_saes_trt_new            = apply(total_saes_new,            2, summarise_samples),
  n_final_edss_value_new    = apply(total_final_edss_value_new, 2, summarise_samples)
)

rownames(key_events_trt_grps_new) <- v_treatment_names

# test equivilence
all.equal(n_relapse_ipd, n_relapse_ipd_new)
all.equal(n_spms_ipd, n_spms_ipd_new)
all.equal(total_n_relapse, total_n_relapse_new)
all.equal(key_events_trt_grps, key_events_trt_grps_new)

#or :
max(abs(total_n_relapse- total_n_relapse_new), na.rm = TRUE)

#######################################



###############################################################################
# TEST: ARRAY APPROACH VS DIRECT SUMMARY APPROACH
#
# Assumes the following objects already exist:
#   results
#   n_samples
#   v_treatment_names
#   summarise_samples()
#
# Purpose:
#   Compare treatment-level summaries obtained from:
#     1. Existing array method
#     2. Direct sample-level aggregation method
###############################################################################

###############################################################################
# EXISTING RESULTS
#
# These should already have been created by your current code:
#
# total_n_relapse
# total_n_spms
# total_edss_increases
# total_edss_decreases
# total_saes
# total_final_edss_value
#
###############################################################################

###############################################################################
# DIRECT SUMMARY APPROACH (NO PATIENT ARRAYS)
###############################################################################

n_trt <- length(v_treatment_names)

total_n_relapse_direct <-
  matrix(NA_real_, nrow = n_samples, ncol = n_trt,
         dimnames = list(NULL, v_treatment_names))

total_n_spms_direct <-
  matrix(NA_real_, nrow = n_samples, ncol = n_trt,
         dimnames = list(NULL, v_treatment_names))

total_edss_increases_direct <-
  matrix(NA_real_, nrow = n_samples, ncol = n_trt,
         dimnames = list(NULL, v_treatment_names))

total_edss_decreases_direct <-
  matrix(NA_real_, nrow = n_samples, ncol = n_trt,
         dimnames = list(NULL, v_treatment_names))

total_saes_direct <-
  matrix(NA_real_, nrow = n_samples, ncol = n_trt,
         dimnames = list(NULL, v_treatment_names))

total_final_edss_value_direct <-
  matrix(NA_real_, nrow = n_samples, ncol = n_trt,
         dimnames = list(NULL, v_treatment_names))

###############################################################################
# LOOP OVER PSA SAMPLES ONLY
###############################################################################

for (i_sample in seq_len(n_samples)) {
  
  df <- results[[1]][[i_sample]]$merged_df
  
  # Keep final outcomes only
  df <- df[df$evtname == "death", ]
  
  for (trt in v_treatment_names) {
    
    temp <- df[df$arm == trt, ]
    
    total_n_relapse_direct[i_sample, trt] <-
      mean(temp$n_relapse, na.rm = TRUE)
    
    total_n_spms_direct[i_sample, trt] <-
      mean(temp$spms_flag, na.rm = TRUE)
    
    total_edss_increases_direct[i_sample, trt] <-
      mean(temp$n_edssinc_evts, na.rm = TRUE)
    
    total_edss_decreases_direct[i_sample, trt] <-
      mean(temp$n_edssdec_evts, na.rm = TRUE)
    
    total_saes_direct[i_sample, trt] <-
      mean(temp$n_sae_evts, na.rm = TRUE)
    
    total_final_edss_value_direct[i_sample, trt] <-
      mean(temp$edss_value, na.rm = TRUE)
  }
}

###############################################################################
# EQUIVALENCE TESTS
###############################################################################

cat("\n")
cat("=============================================\n")
cat("COMPARING ARRAY VS DIRECT AGGREGATION\n")
cat("=============================================\n")

cat("\nRelapses:\n")
print(all.equal(total_n_relapse,
                total_n_relapse_direct))

cat("\nSPMS:\n")
print(all.equal(total_n_spms,
                total_n_spms_direct))

cat("\nEDSS increases:\n")
print(all.equal(total_edss_increases,
                total_edss_increases_direct))

cat("\nEDSS decreases:\n")
print(all.equal(total_edss_decreases,
                total_edss_decreases_direct))

cat("\nSAEs:\n")
print(all.equal(total_saes,
                total_saes_direct))

cat("\nFinal EDSS:\n")
print(all.equal(total_final_edss_value,
                total_final_edss_value_direct))

###############################################################################
# MAXIMUM ABSOLUTE DIFFERENCES
###############################################################################

cat("\n")
cat("Maximum absolute differences\n")
cat("---------------------------\n")

cat("Relapses: ",
    max(abs(total_n_relapse -
              total_n_relapse_direct),
        na.rm = TRUE), "\n")

cat("SPMS: ",
    max(abs(total_n_spms -
              total_n_spms_direct),
        na.rm = TRUE), "\n")

cat("EDSS increases: ",
    max(abs(total_edss_increases -
              total_edss_increases_direct),
        na.rm = TRUE), "\n")

cat("EDSS decreases: ",
    max(abs(total_edss_decreases -
              total_edss_decreases_direct),
        na.rm = TRUE), "\n")

cat("SAEs: ",
    max(abs(total_saes -
              total_saes_direct),
        na.rm = TRUE), "\n")

cat("Final EDSS: ",
    max(abs(total_final_edss_value -
              total_final_edss_value_direct),
        na.rm = TRUE), "\n")

###############################################################################
# FINAL TREATMENT SUMMARIES USING DIRECT APPROACH
###############################################################################

key_events_trt_grps_direct <- data.frame(
  n_spms_trt =
    apply(total_n_spms_direct, 2, summarise_samples),
  
  n_rel_trt =
    apply(total_n_relapse_direct, 2, summarise_samples),
  
  n_edssincrease_trt =
    apply(total_edss_increases_direct, 2, summarise_samples),
  
  n_edssdecrease_trt =
    apply(total_edss_decreases_direct, 2, summarise_samples),
  
  n_saes_trt =
    apply(total_saes_direct, 2, summarise_samples),
  
  n_final_edss_value =
    apply(total_final_edss_value_direct, 2, summarise_samples)
)

print(key_events_trt_grps_direct)

###############################################################################
# COMPARE FINAL TABLES
###############################################################################

cat("\n")
cat("Final table comparison:\n")
print(all.equal(
  as.matrix(key_events_trt_grps),
  as.matrix(key_events_trt_grps_direct)
))