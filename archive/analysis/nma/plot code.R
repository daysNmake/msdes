
ggplot() +
  # Plot intervention_df data with #0072B2 points and error bars
  geom_point(data = intervention_df, aes(x = parameter, y = HR), color = "#0072B2") +
  geom_errorbar(data = intervention_df, aes(x = parameter, ymin = LCI, ymax = UCI), 
                width = 0.2, color = "#0072B2") +
  
  # Plot dir_df data with #009E73 points and error bars, slightly offset on the x-axis
  geom_point(data = dir_df, aes(x = intervention, y = HR), color = "#009E73", position = position_nudge(x = -0.2)) +
  geom_errorbar(data = dir_df, aes(x = intervention, ymin = LCI, ymax = UCI), 
                width = 0.2, color = "#009E73", position = position_nudge(x = -0.2)) +
  
  # Plot ind_df data with #CC79A7 points and error bars, further offset on the x-axis
  geom_point(data = ind_df, aes(x = intervention, y = HR), color = "#CC79A7", position = position_nudge(x = 0.2)) +
  geom_errorbar(data = ind_df, aes(x = intervention, ymin = LCI, ymax = UCI), 
                width = 0.2, color = "#CC79A7", position = position_nudge(x = 0.2)) +
  
  # Add reference line at y = 1 with red color #D55E00
  geom_hline(yintercept = 1, linetype = "dashed", color = "#D55E00") +
  
  # Customize theme and labels
  theme_minimal() +
  labs(x = NULL, y = "Hazard Ratio (HR)", title = NULL) +
  coord_flip() +
  
  # Annotations for "Favours Placebo" and "Favours Intervention"
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 0.25, 
           label = "Favours Intervention", color = "black", size = 3, hjust = 0.5) +
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 1.1, 
           label = "Favours Placebo", color = "black", size = 3, hjust = 0) +
  
  # Adding column headings with the new color palette
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 10, 
           label = "Network Evidence\n(HR 95% CrI)", color = "#0072B2", size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 15, 
           label = "Direct Evidence\n(HR 95% CrI", color = "#009E73", size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 20, 
           label = "Indirect Evidence\n(HR 95% CrI", color = "#CC79A7", size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = length(unique(intervention_df$parameter)) + 1.5, y = 25, 
           label = "p-value", color = "black", size = 3, fontface = "bold", hjust = 0) +
  
  # Expand y-axis and x-axis limits
  expand_limits(y = max(c(intervention_df$UCI, dir_df$UCI, ind_df$UCI)) * 1.7) +
  expand_limits(x = length(unique(intervention_df$parameter)) + 2) +
  
  # Adjust plot margins and theme elements
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
  theme(axis.text.y = element_text(hjust = 0)) +
  
  # Add labels for label_net, label_dir, label_ind, and p_value as separate columns with new colors
  geom_text(data = intervention_df, aes(x = parameter, y = 10, label = label_net), 
            hjust = 0, color = "#0072B2", size = 3) +
  geom_text(data = intervention_df, aes(x = parameter, y = 15, label = label_dir), 
            hjust = 0, color = "#009E73", size = 3) +
  geom_text(data = intervention_df, aes(x = parameter, y = 20, label = label_ind), 
            hjust = 0, color = "#CC79A7", size = 3) +
  geom_text(data = intervention_df, aes(x = parameter, y = 25, label = p_value), 
            hjust = 0, color = "black", size = 3)
