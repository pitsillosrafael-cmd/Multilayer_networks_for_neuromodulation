###############################################################################
# 1. SETUP
###############################################################################

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(igraph)
library(pheatmap)
library(lme4)
library(lmerTest)
library(emmeans)

connectome_dir <-
  "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse"

metric_dir <-
  "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/Network_metrics"

session_levels <- c(
  "ses-preop","ses-postop01m","ses-postop03m","ses-postop06m","ses-postop12m")


###############################################################################
# GLOBAL NETWORK METRICS
# Longitudinal trajectories for a single subject
###############################################################################

# Example sub-05
subject <- "sub-DBS05"
metric_dir <- file.path(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/Network_metrics",
  subject)

# Read all sessions
global_metrics <- list.files(
  metric_dir,
  pattern = "global_metrics.csv$",
  full.names = TRUE) %>%
  lapply(function(f){
    
    df <- read.csv(f)
    
    df$Session <- sub(
      ".*_(ses-[^_]+)_global_metrics\\.csv",
      "\\1",
      basename(f))
    df
  }) %>%
  bind_rows()


# have them in right order
global_metrics$Session <- factor(
  global_metrics$Session,
  levels = c(
    "ses-preop", "ses-postop01m", "ses-postop03m",  "ses-postop06m", "ses-postop12m"))
global_metrics <- global_metrics[
  order(global_metrics$Session),]

# long format
global_long <- pivot_longer(
  global_metrics,
  
  cols = c(
    Density,
    Clustering,
    PathLength,
    GlobalEfficiency,
    Modularity,
    Assortativity,
    NumberModules),
  
  names_to = "Metric",
  values_to = "Value")

# Plot
ggplot(
  global_long,
  aes(
    x = Session,
    y = Value,
    group = 1)) +
  geom_line(
    linewidth = 1) +
  geom_point(
    size = 3) +
  facet_wrap(
    ~Metric,
    scales = "free_y",
    ncol = 3) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1)) +
  labs(title = "DBS05 Global Network Metrics",
    x = "",
    y = "")


###############################################################################
# NODE-LEVEL METRICS
# Longitudinal trajectories of the top 15 hubs for each subject
###############################################################################

# Input directory
# Example subjects
subjects <- c("sub-DBS02", "sub-DBS03")


# PROCESS EACH SUBJECT
for(subject in subjects){
  
  cat("Processing", subject, "...\n")
  
  # Read node metrics
  metric_dir <- file.path(
    metric_dir,
    subject)
  
  files <- list.files(
    metric_dir,
    pattern = "node_metrics.csv$",
    full.names = TRUE)
  
  node_metrics <- bind_rows(
    lapply(files, function(f){
      
      df <- read.csv(f)
      
      df$Session <- sub(
        ".*_(ses-[^_]+)_node_metrics\\.csv",
        "\\1",
        basename(f))
      
      df$Subject <- subject
      
      df
    }))
  
  # Order sessions
  node_metrics$Session <- factor(
    node_metrics$Session,
    levels = c(
      "ses-preop", "ses-postop01m", "ses-postop03m", "ses-postop06m", "ses-postop12m"))
  
# Identify the top 15 hubs
  top_hubs <- node_metrics %>%
    group_by(Region) %>%
    summarise(
      MeanStrength = mean(Strength),
      .groups = "drop"
    ) %>%
    arrange(desc(MeanStrength)) %>%
    slice(1:15)
  
  hub_data <- node_metrics %>%
    filter(
      Region %in% top_hubs$Region)
  
  # Plot longitudinal hub trajectories
  p <- ggplot(
    hub_data,
    aes(
      x = Session,
      y = Strength,
      colour = Region,
      group = Region
    )
  ) +
    geom_point(size = 3) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 1.2
    ) +
    theme_bw(base_size = 14) +
    labs(
      title = paste(subject, "- Top 15 hub trends"),
      x = "",
      y = "Node strength")
  print(p)
  
  # Save figure
  ggsave(
    filename = file.path(
      metric_dir,
      paste0(subject, "_hub_trends.pdf")
    ),
    plot = p,
    width = 10,
    height = 7)}

cat("Finished!\n")




#############################################################

# Boxplots for the strength of all nodes from the 14 subjects

#############################################################

# Statistical tests for boxplots
comparisons <- list(
  c("ses-preop", "ses-postop01m"),
  c("ses-preop", "ses-postop03m"),
  c("ses-preop", "ses-postop06m"),
  c("ses-preop", "ses-postop12m"))

wilcox_results <- data.frame()

for(region in unique(all_nodes_boxplots$Region)){
  
  dat <- all_nodes_boxplots %>%
    filter(Region == region)
  
  for(comp in comparisons){
    
    tmp <- dat %>%
      filter(Session %in% comp) %>%
      select(Subject, Session, Strength) %>%
      pivot_wider(
        names_from = Session,
        values_from = Strength)
    
    # remove incomplete pairs
    tmp <- na.omit(tmp)
    
    wt <- wilcox.test(
      tmp[[comp[1]]],
      tmp[[comp[2]]],
      paired = TRUE,
      exact = FALSE)
    
    wilcox_results <- rbind(
      wilcox_results,
      data.frame(
        Region = region,
        Comparison = paste(comp, collapse = " vs "),
        N = nrow(tmp),
        W = wt$statistic,
        P = wt$p.value
      ))
  }
}

wilcox_results$P_FDR <- p.adjust(
  wilcox_results$P,
  method = "fdr")


# Boxplots for each region across timepoints
subjects <- list.dirs(metric_dir, recursive = FALSE, full.names = FALSE)

all_nodes_boxplots <- data.frame()

for (sub in subjects) {
  files <- list.files(
    file.path(base_dir, sub),
    pattern = "node_metrics.csv",
    full.names = TRUE)
  
  for(f in files) {
    df <- read.csv(f)
    df$Subject <- sub
    df$Session <-sub(
      ".*_(ses-[^_]+)_node_metrics\\.csv",
      "\\1",
      basename(f))
    
    all_nodes_boxplots <- bind_rows(all_nodes_boxplots, df)}}
    
    all_nodes_boxplots$Session <- factor(
      all_nodes_boxplots$Session, levels = c( "ses-preop",
                                              "ses-postop01m",
                                              "ses-postop03m",
                                              "ses-postop06m",
                                              "ses-postop12m"))
    
    pdf(file.path(base_dir,
        "Regional_NodeStrength_Boxplots.pdf"),
      width = 8,
      height = 6)
    
    for(region in unique(all_nodes_boxplots$Region)){
      dat <- all_nodes_boxplots %>%
        filter(Region == region)
      p <- ggplot(
        dat,
        aes(
          x = Session,
          y = Strength,
          fill = Session)) +
        
        geom_boxplot(
          alpha = 0.7,
          outlier.shape = NA) +
        
        geom_jitter(
          aes(color = Subject),
          width = 0.12,
          size = 2) +
        
        stat_compare_means(
          comparisons = comparisons,
          method = "wilcox.test",
          paired = TRUE,
          label = "p.format") +
        
        theme_bw(base_size = 14) +
        
        labs(
          title = region,
          y = "Node strength",
          x = ""
        ) +
        
        theme(
          legend.position = "none",
          axis.text.x = element_text(
            angle = 45,
            hjust = 1))
      
      print(p)}
    
    dev.off()