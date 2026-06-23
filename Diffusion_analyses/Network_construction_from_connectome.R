library(igraph)
install.packages("networkD3")
library(networkD3)

library(igraph)
library(dplyr)

base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"
subjects <- c("sub-DBS05", "sub-DBS06", "sub-DBS07")
sessions <- c("ses-preop", "ses-postop01m", "ses-postop03m", "ses-postop06m", "ses-postop12m")

for(sub in subjects){
  
  for(ses in sessions){
    
    cat("\n====================\n")
    cat(sub, ses, "\n")
    
    file <- file.path(
      base_dir,
      sub,
      ses,
      "connectome",
      paste0(
        "labeled_",
        sub,
        "_",
        ses,
        "_connectome.csv"))
    
    if(!file.exists(file)){
      cat("Missing:", file, "\n")
      next}
    
    # Load connectome
    mat <- as.matrix(
      read.csv(
        file,
        row.names = 1,
        check.names = FALSE))
    
    storage.mode(mat) <- "numeric"
    
    # Threshold top 10%
    weights <- mat[upper.tri(mat)]
    
    thr <- quantile(
      weights[weights > 0],
      0.90)
    
    mat_thr <- mat
    mat_thr[mat_thr < thr] <- 0
    
    # Build graph
    g <- graph_from_adjacency_matrix(
      mat_thr,
      mode = "undirected",
      weighted = TRUE,
      diag = FALSE)
    
    # -------------------------
    # NODE METRICS
    # -------------------------
    
    community <- cluster_louvain(
      g,
      weights = E(g)$weight)
    
    node_metrics <- data.frame(
      Region = V(g)$name,
      
      Strength = strength(
        g,
        weights = E(g)$weight),
      
      Betweenness = betweenness(
        g,
        weights = 1/E(g)$weight),
      
      Eigenvector = eigen_centrality(
        g,
        weights = E(g)$weight
      )$vector,
      
      Closeness = closeness(
        g,
        weights = 1/E(g)$weight),
      
      Community = membership(
        community))
    
    # Local efficiency
    
    local_eff <- sapply(
      V(g),
      function(v){
        
        neigh <- neighbors(
          g,
          v)
        
        if(length(neigh) < 2){
          return(NA)}
        
        subg <- induced_subgraph(
          g,
          neigh)
        
        if(ecount(subg) == 0){
          return(NA)}
        
        Dsub <- distances(
          subg,
          weights = 1/E(subg)$weight)
        
        diag(Dsub) <- Inf
        
        mean(
          1/Dsub[is.finite(Dsub)])})
    
    node_metrics$LocalEfficiency <- local_eff
    
    write.csv(
      node_metrics,
      file.path(
        base_dir,
        sub,
        ses,
        paste0(
          sub,
          "_",
          ses,
          "_node_metrics.csv")),
      row.names = FALSE)
    
    # -------------------------
    # GLOBAL METRICS
    # -------------------------
    
    g_dist <- g
    E(g_dist)$distance <- 1/E(g_dist)$weight
    
    D <- distances(
      g_dist,
      weights = E(g_dist)$distance)
    
    diag(D) <- Inf
    
    global_metrics <- data.frame(
      Subject = sub,
      Session = ses,
      
      Nodes = vcount(g),
      Edges = ecount(g),
      
      Density = edge_density(g),
      
      Clustering = transitivity(
        g,
        type = "global"),
      
      PathLength = mean_distance(
        g_dist,
        directed = FALSE,
        weights = E(g_dist)$distance),
      
      GlobalEfficiency = mean(
        1/D[is.finite(D)]),
      
      Modularity = modularity(
        community),
      
      Assortativity = assortativity_degree(
        g,
        directed = FALSE),
      
      NumberModules = length(
        unique(
          membership(
            community))))
    
    write.csv(
      global_metrics,
      file.path(
        base_dir,
        sub,
        ses,
        paste0(
          sub,
          "_",
          ses,
          "_global_metrics.csv")),
      row.names = FALSE)
    
    cat(
      "Saved:",
      sub,
      ses,
      "\n"
    )}}






# visualize the graphs
set.seed(123)
layout_fixed <- layout_with_fr(g)

plot(
  g,
  layout = layout_fixed,
  vertex.size = scales::rescale(
    node_strength,
    to = c(5,20)
  ),
  vertex.label = NA,
  edge.width = scales::rescale(
    E(g)$weight,
    to = c(0.5,3)
  ),
  edge.color = "grey70",
  main = "sub-DBS05 preop")








subject <- "sub-DBS05"
sessions <- c("ses-preop", "ses-postop01m",
              "ses-postop03m", "ses-postop06m",
              "ses-postop12m")


graphs <- list()

# loop for networks
for (ses in sessions) {
  
  file <- file.path(
    base_dir,
    subject,
    ses,
    "connectome",
    paste0(
      "labeled_",
      subject,
      "_",
      ses,
      "_connectome.csv"))

  cat("Loading:", basename(file), "\n")
  
  mat <- as.matrix(
    read.csv(
      file,
      row.names = 1,
      check.names = FALSE))
  
  storage.mode(mat) <- "numeric"
  
  g <- graph_from_adjacency_matrix(
    mat,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE)
  
  graphs[[ses]] <- g
  
  cat(
    ses,
    "\nNodes:", vcount(g),
    "\nEdges:", ecount(g),
    "\nDensity:", round(edge_density(g), 3),
    "\n\n")}


# Fixed layout from preop graph
layout_fixed <- layout_with_fr(
  graphs[["ses-preop"]]
)

# Save plots
pdf(
  file.path(
    output_dir,
    paste0(subject, "-networks.pdf")
  ),
  width = 12,
  height = 8
)

par(mfrow = c(2,3))

for (ses in sessions) {
  
  plot(
    graphs[[ses]],
    layout = layout_fixed,
    vertex.size = 4,
    vertex.label = NA,
    edge.width = 1,
    edge.arrow.mode = 0,
    main = ses)}

dev.off()

cat("PDF saved.\n")

# Individual networks
# Sub-DBS05 (preop)
file <- file.path(base_dir, "sub-DBS06", "ses-postop12m", "connectome",
                  "labeled_sub-DBS06_ses-postop12m_connectome.csv")

mat <- as.matrix(read.csv(file, row.names = 1, check.names = FALSE))
storage.mode(mat) <- "numeric"

# Threshold the network
weights <- mat[upper.tri(mat)]
thr <- quantile(weights[weights > 0],
                0.90)

mat_thr <- mat
mat_thr[mat_thr < thr] <- 0

# For check
sum(mat > 0)
sum(mat_thr > 0)

# Construct a network
g <- graph_from_adjacency_matrix(mat_thr, mode = "undirected",
                                 weighted = TRUE, diag = FALSE)


# Node strength (hub or not | larger nodes = stronger hubs)
node_strength <- strength(g)
# V(g)$size <- scales::rescale(node_strength, to = c(5,20))

# Strongest regions
V(g)$label <- ""

hub_idx <- order(
  node_strength, decreasing = TRUE)[1:15]

V(g)$label[hub_idx] <-
  V(g)$name[hub_idx]

E(g)$width <- scales::rescale(
  E(g)$weight,
  to = c(1,6)
)

# plot
set.seed(123)

layout_fixed <- layout_with_fr(g)

plot(
  g, layout = layout_fixed,
  vertex.label.cex = 0.7,
  vertex.label.dist = 0.5,
  edge.color = "grey70",
  main = "DBS06 Postop12m"
)


