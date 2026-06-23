library(gganimate)
library(ggplot2)
library(patchwork)
library(graphlayouts)

# Try the "dynamic networks" example from the page https://schochastics.github.io/netVizR/
# Create the graphs
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"
output_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/"
subject <- "sub-DBS07"
sessions <- c("ses-preop", "ses-postop01m", "ses-postop03m", "ses-postop06m", "ses-postop12m")

# LOAD ALL NETWORKS
graphs <- list()

for(session in sessions){
  
  file <- file.path(
    base_dir,
    subject,
    session,
    "connectome",
    paste0(
      "labeled_",
      subject,
      "_",
      session,
      "_connectome.csv"))
  
  mat <- as.matrix(
    read.csv(
      file,
      row.names = 1,
      check.names = FALSE))
  
  storage.mode(mat) <- "numeric"
  
  # Threshold at top 10%
  weights <- mat[upper.tri(mat)]
  
  thr <- quantile(
    weights[weights > 0],
    0.90)
  
  mat[mat < thr] <- 0
  
  g <- graph_from_adjacency_matrix(
    mat,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE)
  
  graphs[[session]] <- g}

# DYNAMIC LAYOUT
xy <- layout_as_dynamic(
  graphs,
  alpha = 0.2)

# save
pdf(
  file.path(
    output_dir,
    paste0(
      subject,
      "_dynamic_community_networks.pdf")),
  width = 18,
  height = 10)

par(
  mfrow = c(2,3),
  mar = c(1,1,3,1))

for(i in seq_along(sessions)){
  
  g <- graphs[[i]]
  
  # Communities
  community <- cluster_louvain(
    g,
    weights = E(g)$weight)
  
  groups <- split(
    V(g),
    membership(community))
  
  # Node size = strength
  V(g)$size <- rescale(
    strength(g),
    to = c(6,20))
  
  # Top hubs
  hub_idx <- order(
    strength(g),
    decreasing = TRUE
  )[1:15]
  
  V(g)$label <- ""
  
  V(g)$label[hub_idx] <-
    V(g)$name[hub_idx]
  
  # Community colors
  V(g)$color <- membership(
    community)
  
  # Edge widths
  E(g)$width <- rescale(
    E(g)$weight,
    to = c(0.5,6))
  
  plot(
    g,
    
    layout = xy[[i]],
    
    vertex.size = V(g)$size,
    vertex.color = V(g)$color,
    
    vertex.label.cex = 0.7,
    vertex.label.color = "black",
    
    edge.width = E(g)$width,
    
    edge.color = adjustcolor(
      "grey40",
      alpha.f = 0.25
    ),
    
    mark.groups = groups,
    
    mark.col = rainbow(
      length(groups),
      alpha = 0.12
    ),
    
    mark.border = NA,
    
    main = gsub(
      "ses-",
      "",
      sessions[i]))}

dev.off()

cat(
  "Saved:",
  file.path(
    output_dir,
    paste0(
      subject,
      "_dynamic_community_networks.pdf")))