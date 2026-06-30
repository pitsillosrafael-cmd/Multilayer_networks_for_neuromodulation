# Compare connectivity matrices pre to postop12m
library(igraph)
library(dplyr)
library(pheatmap)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Delta postop12m - preop heatmaps
# Example for subject 05
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"
subject <- "sub-DBS05"

# find file (preop)
file_05_pre <- file.path(
  base_dir, subject,
  "ses-preop",
  "connectome",
  paste0(
    "labeled_",
    subject,
    "_ses-preop_connectome.csv"))
# prepare the matrix
mat_05_pre <- as.matrix(
  read.csv(
    file_05_pre,
    row.names = 1,
    check.names = FALSE))

# postop12m
file_05_12m <- file.path(
  base_dir, subject,
  "ses-postop12m",
  "connectome",
  paste0(
    "labeled_",
    subject,
    "_ses-postop12m_connectome.csv"))
mat_07_12m <- as.matrix(
  read.csv(
    file_07_12m,
    row.names = 1,
    check.names = FALSE))

storage.mode(mat_05_pre) <- "numeric"
storage.mode(mat_05_12m) <- "numeric"

# Their difference
delta_mat_05 <- mat_05_pre - mat_05_12m

# Heatmap
pheatmap(
  delta_mat_05,
  color = colorRampPalette(
    c("blue", "white", "red")
  )(200),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "DBS05 rewiring (12m - preop)")

# Only the most rewired ones
edges <- which(
  upper.tri(delta_mat_05),
  arr.ind = TRUE)

rewiring <- data.frame(
  Region1 = rownames(delta_mat_05)[edges[,1]],
  Region2 = colnames(delta_mat_05)[edges[,2]],
  Delta = delta_mat_05[edges])

rewiring <- rewiring[
  order(abs(rewiring$Delta), decreasing = TRUE),]

rewiring_gain <- rewiring[
  order(-rewiring$Delta),]

head(rewiring_gain, 20)




# Global metrics comparison
metric_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/Network_metrics/sub-DBS05"

files <- list.files(
  metric_dir,
  pattern = "global_metrics.csv$",
  full.names = TRUE)

global_list <- lapply(
  files,
  function(f){
    
    df <- read.csv(f)
    session <- gsub(
      "sub-DBS05_|_global_metrics.csv",
      "",
      basename(f))
    df$Session <- session
    return(df)})

global_metrics <- bind_rows(
  global_list)

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


base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/Network_metrics"
subjects <- c("sub-DBS05", "sub-DBS06", "sub-DBS07")

# Table for the global changes
change_pct <- function(pre, post){
  round(
    100 * (post - pre) / pre,
    1)}

results <- data.frame()

for(sub in subjects){
  
  metric_dir <- file.path(
    base_dir,
    sub)
  
  files <- list.files(
    metric_dir,
    pattern = "global_metrics.csv$",
    full.names = TRUE)
  
  all_metrics <- bind_rows(
    lapply(files, read.csv))
  
  pre <- all_metrics[
    all_metrics$Session == "ses-preop",]
  
  post <- all_metrics[
    all_metrics$Session == "ses-postop12m",]
  
  results <- rbind(
    results,
    data.frame(
      Subject = sub,
      Efficiency =
        change_pct(
          pre$GlobalEfficiency,
          post$GlobalEfficiency
        ),
      PathLength =
        change_pct(
          pre$PathLength,
          post$PathLength
        ),
      Modularity =
        change_pct(
          pre$Modularity,
          post$Modularity
        ),
      Density =
        change_pct(
          pre$Density,
          post$Density
        ),
      Clustering =
        change_pct(
          pre$Clustering,
          post$Clustering
        )))}

results


# Local node metrics
# Line plots for each
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/Network_metrics"
subjects <- c("sub-DBS05", "sub-DBS06", "sub-DBS07", "sub-DBS08", "sub-DBS09", "sub-DBS10", 'sub-DBS12', "sub-DBS13", "sub-DBS14")

for(subject in subjects){
  
  cat("Processing", subject, "...\n")
  
  metric_dir <- file.path(
    base_dir,
    subject)
  
  files <- list.files(
    metric_dir,
    pattern = "node_metrics.csv$",
    full.names = TRUE)
  
  # Read all node metrics
  node_metrics <- bind_rows(
    lapply(files, function(f){
      
      df <- read.csv(f)
      
      df$Session <- sub(
        ".*_(ses-[^_]+)_node_metrics\\.csv",
        "\\1",
        basename(f))
      
      df$Subject <- subject
      
      df}))
  
  # Order sessions
  node_metrics$Session <- factor(
    node_metrics$Session,
    levels = c("ses-preop", "ses-postop01m",  "ses-postop03m", "ses-postop06m", "ses-postop12m"))
  
  # Top 15 hubs
  top_hubs <- node_metrics %>%
    group_by(Region) %>%
    summarise(
      MeanStrength = mean(Strength),
      .groups = "drop") %>%
    arrange(desc(MeanStrength)) %>%
    slice(1:15)
  
  hub_data <- node_metrics %>%
    filter(
      Region %in% top_hubs$Region)
  
  # Plot
  p <- ggplot(
    hub_data,
    aes(
      x = Session,
      y = Strength,
      colour = Region,
      group = Region)) +
    geom_point(size = 3) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 1.2) +
    theme_bw(base_size = 14) +
    labs(
      title = paste(subject, "- Top 15 hub trends"),
      x = "",
      y = "Node strength")
  
  print(p)
  
  ggsave(
    filename = file.path(
      metric_dir,
      paste0(subject, "_hub_trends.pdf")),
    plot = p,
    width = 10,
    height = 7)}

cat("Finished!\n")


# Boxplots for the strength of all nodes from the 14 subjects
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/Network_metrics"

subjects <- list.dirs(base_dir, recursive = FALSE, full.names = FALSE)

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




