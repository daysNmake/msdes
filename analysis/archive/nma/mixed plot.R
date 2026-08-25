# Reorder the 'parameter' factor levels in alphabetical order
intervention_RE_df$parameter <- factor(intervention_RE_df$parameter, 
                                    levels = rev(unique(intervention_RE_df$parameter)))


ggplot() +
  # Plot intervention_df data with #0072B2 points and error bars
  geom_point(data = intervention_RE_df, aes(x = parameter, y = HR), color = "#0072B2") +
  geom_errorbar(data = intervention_RE_df, aes(x = parameter, ymin = LCI, ymax = UCI), 
                width = 0.2, color = "#0072B2") +
  
  # Plot dir_df data with #009E73 points and error bars, slightly offset on the x-axis
  geom_point(data = HA_df, aes(x = parameter, y = HR), color = "#009E73", position = position_nudge(x = -0.2)) +
  geom_errorbar(data = HA_df, aes(x = parameter, ymin = LCI, ymax = UCI), 
                width = 0.2, color = "#009E73", position = position_nudge(x = -0.2)) +
  
  
  # Add reference line at y = 1 with red color #D55E00
  geom_hline(yintercept = 1, linetype = "dashed", color = "#D55E00") +
  
  # Customize theme and labels
  theme_minimal() +
  labs(x = NULL, y = "Hazard Ratio (HR)", title = NULL) +
  coord_flip() +
  
  # Annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(intervention_RE_df$parameter)) + 1.5, y = 0.5, 
           label = "Favours\nIntervention", color = "black", size = 3, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_RE_df$parameter)) + 1.5, y = 1.2, 
           label = "Favours\nPlacebo", color = "black", size = 3, hjust = 0) +
  
  # Adding column headings with the new color palette
  annotate("text", x = length(unique(intervention_RE_df$parameter)) + 1.5, y = 2, 
           label = "RRMS\n(RR 95% CrI)", color = "#0072B2", size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = length(unique(intervention_RE_df$parameter)) + 1.5, y = 2.5, 
           label = "Highly active\n(RR 95% CrI)", color = "#009E73", size = 3, fontface = "bold", hjust = 0) +

  
  # Expand y-axis and x-axis limits
  expand_limits(y = max(c(intervention_RE_df$UCI, HA_df$UCI)) * 2) +
  expand_limits(x = length(unique(intervention_RE_df$parameter)) + 2) +
  
  # Adjust plot margins and theme elements
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
  theme(axis.text.y = element_text(hjust = 0)) + 
  
  # Add labels for label_net, label_dir, label_ind, and p_value as separate columns with new colors
  geom_text(data = intervention_RE_df, aes(x = parameter, y = 2, label = label), 
            hjust = 0, color = "#0072B2", size = 3) +
  geom_text(data = HA_df, aes(x = parameter, y = 2.5, label = label), 
            hjust = 0, color = "#009E73", size = 3) 
