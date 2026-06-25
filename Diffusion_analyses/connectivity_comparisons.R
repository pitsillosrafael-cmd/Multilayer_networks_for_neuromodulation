# Compare connectivity matrices pre to postop12m
library(igraph)
library(dplyr)
library(pheatmap)
library(tidyverse)
library(tidyr)
library(ggplot2)

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














