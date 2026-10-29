#' Plot Predicted Mortality Over Time by Treatment Arm
#'
#' Extracts patient death times from WARDEN DES simulation output, fits a
#' Kaplan-Meier survival curve per treatment arm pooled across all PSA samples,
#' and returns a \code{ggplot} step plot of predicted overall survival.
#'
#' @param runresults  List of length \code{n_samples}. Each element is a WARDEN
#'   simulation result containing a \code{merged_df} data frame with columns
#'   \code{pat_id}, \code{arm}, \code{evtname}, and \code{evttime}.
#' @param n_samples   Integer. Number of PSA samples run.
#' @param n_patients  Integer. Number of simulated patients per sample.
#' @param n_treatments Integer. Number of treatment arms.
#' @param trt_names   Character vector of length \code{n_treatments} giving
#'   treatment arm names. Must match values in \code{merged_df$arm}.
#'
#' @details
#' Death times are extracted by filtering \code{merged_df} to rows where
#' \code{evtname == "death"} for each combination of sample, treatment arm,
#' and patient. Times are stored in a three-dimensional array
#' \code{[n_samples x n_treatments x n_patients]} and sorted within each
#' \code{[sample, treatment]} slice before KM fitting.
#'
#' KM curves are fitted using \code{\link[survival]{survfit}} with all patients
#' pooled across PSA samples for each treatment arm, treating every patient as
#' experiencing the event (no censoring). Linetype cycles through
#' \code{"solid"}, \code{"dashed"}, \code{"dotted"}, \code{"dotdash"} if there
#' are more than four treatment arms.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object. The plot shows survival
#'   probability (y-axis, 0 to 1) against time in years (x-axis, 0 to 65),
#'   with one step line per treatment arm distinguished by colour and linetype.
#'   The legend is positioned at the bottom.
#'
#' @seealso \code{\link[msdes]{rrmsdesmodel}}, \code{\link[survival]{survfit}}
#'
#' @importFrom survival survfit Surv
#' @importFrom ggplot2 ggplot aes geom_step scale_linetype_manual labs
#'   xlim ylim theme_bw theme
#'
#' @examples
#' \dontrun{
#' msdes::rrmsdessetup(drc = 0.035, drq = 0.035, time_horizon = 70,
#'                     p_eid = 0, jcvtest_cost = 0)
#' msdes::rrmsdesmodel(n_patients = 10, n_samples = 100)
#'
#' msdes::plot_mortality_curves(
#'   runresults   = results[[1]],
#'   n_samples    = 100,
#'   n_patients   = 10,
#'   n_treatments = n_treatments,
#'   trt_names    = v_treatment_names
#' )
#' }
#'
#' @export
plot_mortality_curves <- function(runresults, n_samples, n_patients,
                                  n_treatments, trt_names) {
  
  # Collect death times into a 3-D array [sample x treatment x patient] -------
  death_ipd <- array(
    NA_real_,
    dim      = c(n_samples, n_treatments, n_patients),
    dimnames = list(NULL, trt_names, NULL)
  )
  
  for (trt in trt_names) {
    for (i_sample in seq_len(n_samples)) {
      for (i_patient in seq_len(n_patients)) {
        idx <- (runresults[[i_sample]]$merged_df$pat_id  == i_patient &
                  runresults[[i_sample]]$merged_df$arm     == trt       &
                  runresults[[i_sample]]$merged_df$evtname == "death")
        death_ipd[i_sample, trt, i_patient] <-
          runresults[[i_sample]]$merged_df$evttime[idx]
      }
    }
  }
  
  # Sort death times within each [sample, treatment] slice --------------------
  for (i_sample in seq_len(n_samples)) {
    for (trt in trt_names) {
      death_ipd[i_sample, trt, ] <- sort(death_ipd[i_sample, trt, ])
    }
  }
  
  # Build tidy long-format survival data and fit KM per treatment -------------
  km_data <- lapply(seq_len(n_treatments), function(i) {
    df <- data.frame(
      time  = as.vector(death_ipd[, i, ]),
      event = 1L
    )
    df <- df[order(df$time), ]
    
    fit <- survival::survfit(survival::Surv(time, event) ~ 1,
                             data = df, type = "kaplan-meier")
    data.frame(time = fit$time, surv = fit$surv, treatment = trt_names[i])
  })
  
  km_df <- do.call(rbind, km_data)
  
  # Linetypes cycle if more than 4 treatment arms -----------------------------
  ltypes <- rep(c("solid", "dashed", "dotted", "dotdash"), length.out = n_treatments)
  names(ltypes) <- trt_names
  
  # ggplot --------------------------------------------------------------------
  ggplot2::ggplot(km_df, ggplot2::aes(x = time, y = surv,
                                      colour   = treatment,
                                      linetype = treatment)) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::scale_linetype_manual(values = ltypes) +
    ggplot2::labs(
      title    = "Predicted mortality over time \u2014 HARRMS Model",
      y        = "Survival (probability)",
      x        = "Time (years)",
      colour   = NULL,
      linetype = NULL
    ) +
    ggplot2::xlim(0, 65) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}