#######################################################################################################################
   # Set up environment #
#######################################################################################################################
options(scipen = 999)
install.packages("reticulate")
install.packages('ggdist')
library(reticulate)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggdist)
library(ggthemes)
library(ggdist)
pd <- import("pandas")

#######################################################################################################################
   # Import Python Pickles #
#######################################################################################################################

full_data <- as.list(pd$read_pickle("full_data.pkl"))
baseline_results_dict <- as.list(pd$read_pickle("baseline_results.pkl"))
stratifications_results_dict <- as.list(pd$read_pickle("stratification_results_2.pkl"))
annotated_data <- as.list(pd$read_pickle("annotated_data.pkl"))

#####################################################################################################################
  # Set Custom Theme #
#####################################################################################################################

custom_theme <- theme_bw(base_size = 16) +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray", size = 0.2), 
        panel.grid.major.y = element_line(color = "gray"), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        axis.line.x = element_line(color = "black", size = 0.2),
        axis.line.y = element_line(color = "black", size = 0.2),
        axis.ticks = element_line(size = 1),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 15, face = "bold"),
        legend.position = "right",
        legend.box = "none", 
        plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))

#####################################################################################################################
  # Frequency Plots #
#####################################################################################################################

plot_frequency <- function(data1, data2, total_data1, total_data2, title1, title2) {
  library(scales) 
  datasets <- list(data1, data2)
  total_datasets <- list(total_data1, total_data2)
  titles <- c(title1, title2)
  
  plot_list <- list()
  
  for (i in 1:2) {
    plot_data_final <- bind_rows(lapply(names(datasets[[i]]), function(key) {
      df <- datasets[[i]][[key]]
      df$created_at <- as.Date(df$created_at)
      count_data <- df %>%
        group_by(created_at) %>%
        tally(name = 'count')
      
      total_datasets[[i]]$created_at <- as.Date(total_datasets[[i]]$created_at)
      total_counts <- total_datasets[[i]] %>%
        group_by(created_at) %>%
        tally(name = 'total_count')
      
      plot_data <- left_join(count_data, total_counts, by = 'created_at') %>%
        mutate(percentage = count / total_count * 100,
               credibility_type = ifelse(grepl("low_cred", key), "low_credibility", "high_credibility"))
      
      return(plot_data)
    }))
    
  plot <- ggplot(plot_data_final, aes(x = created_at, y = percentage, fill = credibility_type)) +
      geom_area(alpha = 0.9, position = 'identity') +
      scale_fill_manual(values = c("low_credibility" = "#5B6B8F", "high_credibility" = "#D4AF37")) +
      labs(title = titles[i], x = "Date", y = "% of Full Data") +
      custom_theme +
      scale_x_date(breaks = "2 days", labels = date_format("%b %d")) +  #
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
    plot_list[[i]] <- plot
  }
  
  grid.arrange(grobs = plot_list, nrow = 2)
}


covid_cred_data <- annotated_data[grep('covid', names(annotated_data))]
climate_cred_data <- annotated_data[grep('climate', names(annotated_data))]

covid_full_data <- full_data[['covid']]
climate_full_data <- full_data[['climate']]

fig_1 = plot_frequency(covid_cred_data, climate_cred_data, covid_full_data, climate_full_data, 'COVID-19', 'Climate Change')

ggsave("fig_1.jpg", plot = fig_1, width = 9, height = 9, dpi = 300)

#######################################################################################################################
   # Raincloud Plots   #
#######################################################################################################################
covid_percentage_data <- baseline_results_dict[["covid_results_percentage"]]
climate_percentage_data <- baseline_results_dict[["climate_results_percentage"]]
covid_percentage_data$
covid_percentage_data$group <- 'covid'
climate_percentage_data$group <- 'climate'

# Merge the percentage data
boot_differences_percentage_merged <- rbind(covid_percentage_data, climate_percentage_data)

raincloud_plot = ggplot(boot_differences_percentage_merged, aes(x = mean_difference_perc, y = group)) +
  stat_halfeye(
    aes(fill = group),
    point_color = NA, .width = 0, height = 0.6,
    position = position_nudge(y = 0.3)
  ) +
  geom_boxplot(
    aes(color = group),
    position = position_nudge(y = 0.2),
    width = 0.1, outlier.shape = NA
  ) +
  geom_point(
    aes(color = group),
    position = position_jitter(width = 0, height = 0.1, seed = 1)
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", size = 0.7) + 
  coord_flip() +
  scale_color_manual(values = c("covid" = "#5B6B8F", "climate" = "#D4AF37")) + 
  scale_fill_manual(values = c("covid" = "#5B6B8F", "climate" = "#D4AF37")) +
  scale_x_continuous(limits = c(-100, 250)) +
  scale_y_discrete(labels = c("covid" = "COVID-19", "climate" = "Climate Change")) +
  labs(
    x = "Mean % Difference in Impressions)",  
    y = "Dataset"
  ) +
  custom_theme +
  theme(legend.position = "none")

ggsave("fig_2.jpg", plot = raincloud_plot, width = 11, height = 8, dpi = 300)

#####################################################################################################################
  # Heatmaps #
#####################################################################################################################

covid_results_stratum_percentage = baseline_results_dict$covid_results_stratum_percentage
climate_results_stratum_percentage = baseline_results_dict$climate_results_stratum_percentage

plot_heatmap <- function(data,title,breaks_values) {
  
  data$followers_cluster <- sapply(data$stratum, `[[`, 1)
  data$engagement_cluster <- sapply(data$stratum, `[[`, 2)
  
breakpoints <- c(min(data$mean_difference_perc), 0, 5, max(data$mean_difference_perc))
colors <- c("#3B4B6F", "white", "#e9dc91", "#d6ac23")  # Colors for each breakpoint

color_mapping <- scale_colour_gradientn(
    "Mean % Difference",
    colours = colors,
    values = scales::rescale(breakpoints)
)
  
  # Create plot
  p <- ggplot(data) + 
    geom_tile(aes(x = as.factor(followers_cluster), y = as.factor(engagement_cluster)), fill = "white", color = "black") + 
    geom_jitter(aes(
      x = as.factor(followers_cluster), 
      y = as.factor(engagement_cluster), 
      color = mean_difference_perc, 
      size = total_sample_size
    ), width = 0, height = 0) +
      labs(
      title = title, 
      x = "Followers Cluster", 
      y = "Engagement Cluster"
    ) + 
    custom_theme +
    color_mapping + 
    scale_size_continuous(
      name = "Total Sample Size", 
      breaks = breaks_values, 
      range = c(2, 20)
    )  +
    guides(
      size = guide_legend(override.aes = list(fill = "transparent", color = "black", shape = 21)),
      color = guide_colourbar(order = 1),
      size = guide_legend(order = 2)
    )
    
}

covid_heatmap = plot_heatmap(covid_results_stratum_percentage, 'COVID-19', c(10000, 15000, 30000))
climate_heatmap = plot_heatmap(climate_results_stratum_percentage, 'Climate Change', c(1000, 2500, 5000))

fig_3 = grid.arrange(covid_heatmap, climate_heatmap, nrow = 2)

ggsave("fig_3.png", plot = fig_3, width = 11, height = 11, dpi = 300)

#####################################################################################################################
  # Stratification Plots #
#####################################################################################################################

strat_data_covid <- stratifications_results_dict[["covid_stratifications"]]
strat_data_climate <- stratifications_results_dict[["climate_stratifications"]]

create_strat_plot <- function(data, plot_title, lower_limit = -100, upper_limit = 250) {
  
  # Convert to factor with desired order
  data$stratum <- factor(data$stratum, levels = c("political_bias_left", "political_bias_right", "verified_false", "verified_true", "toxicity_low", "toxicity_mid", "toxicity_high"))
  
  # Group strata
  data$stratum_group <- ifelse(grepl("toxicity", data$stratum), "toxicity",
                               ifelse(grepl("political_bias", data$stratum), "bias", "verified"))
  
  # Define shapes
  shape_map <- c("toxicity" = 15, "bias" = 19, "verified" = 17)
  
  # Plotting
  p <- ggplot(data, aes(x = impact, y = stratum)) + 
    geom_point(aes(shape = stratum_group, color = ifelse(impact < 0, "#5B6B8F", "#D4AF37")), size = 6) + 
    scale_shape_manual(values = shape_map) +
    scale_color_identity(guide = "none") + # Ensures the colors are used as specified and removes legend
    geom_vline(xintercept = 0, linetype = "dotted") + 
    theme_light() + 
    labs(title = plot_title, y = "Stratum Variable", x = " Raw Change Over Baseline Amplification", y = "Stratum") +
    custom_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none"  # This will remove the legend
    ) +
    scale_x_continuous(limits = c(lower_limit, upper_limit))
  
  return(p)
}


fig_4_1 <- create_strat_plot(strat_data_covid, "COVID-19", lower_limit = -10, upper_limit = 160)
print(fig_4_1)

fig_4_2<- create_strat_plot(strat_data_climate, "Climate Change", lower_limit = -10, upper_limit = 160)
print(fig_4_2)

fig_4_merged = grid.arrange(plfig_4_1ot_result, fig_4_2, nrow = 2)

ggsave("fig_4.jpg", plot = fig_4_merged, width = 12, height = 12, dpi = 300)
