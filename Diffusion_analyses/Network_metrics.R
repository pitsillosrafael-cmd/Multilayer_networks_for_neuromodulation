library(igraph)
install.packages("networkD3")
library(networkD3)
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


