###############################################
# Metrics for mean graphs across all timepoints
###############################################
graphs <- list(preop = g_preop,
               postop01m = g_postop01m,  postop03m = g_postop03m,
               postop06m = g_postop06m,postop12m = g_postop12m)

# Create the loop
global_efficiency <- function(g){
  
  d <- distances(g,
                 weights = 1 / E(g)$weight)
  
  diag(d) <- Inf
  
  mean(1 / d[is.finite(d)])}

global_metrics <- list()
node_metrics <- list()

for(tp in names(graphs)){
  
  cat("Processing", tp, "...\n")
  
  g <- graphs[[tp]]
  
  comm <- cluster_louvain(g,
                          weights = E(g)$weight)
  ############################
  # Global metrics
  ############################
  
  global_metrics[[tp]] <- data.frame(
    
    Timepoint = tp,
    
    Nodes = gorder(g),
    
    Edges = gsize(g),
    
    Density = edge_density(g),
    
    MeanStrength = mean(
      strength(g, weights = E(g)$weight)),
    
    MeanEdgeWeight = mean(E(g)$weight),
    
    Clustering = transitivity(
      g,
      type = "global"),
    
    PathLength = mean_distance(
      g,
      directed = FALSE,
      weights = 1 / E(g)$weight),
    
    GlobalEfficiency = global_efficiency(g),
    
    Modularity = modularity(comm),
    
    Assortativity = assortativity_degree(
      g,
      directed = FALSE),
    
    NumberModules = length(unique(membership(comm))))
  
  ############################
  # Node metrics
  ############################
  
  node_metrics[[tp]] <- data.frame(
    
    Timepoint = tp,
    
    Region = V(g)$name,
    
    Degree = degree(g),
    
    Strength = strength(
      g,
      weights = E(g)$weight),
    
    Betweenness = betweenness(
      g,
      directed = FALSE,
      weights = 1 / E(g)$weight),
    
    Closeness = closeness(
      g,
      weights = 1 / E(g)$weight),
    
    Eigenvector = eigen_centrality(
      g,
      weights = E(g)$weight)$vector,
    
    PageRank = page_rank(
      g,
      weights = E(g)$weight)$vector,
    
    Clustering = transitivity(
      g,
      type = "local",
      isolates = "zero"),
    
    Community = membership(comm))}

global_metrics <- bind_rows(global_metrics)
node_metrics <- bind_rows(node_metrics)

# save 
metrics_dir <- file.path(graphs_dir, "Network_metrics")

write.csv(
  global_metrics,
  file = file.path(metrics_dir, "global_metrics.csv"),
  row.names = FALSE)

write.csv(
  node_metrics,
  file = file.path(metrics_dir, "node_metrics.csv"),
  row.names = FALSE)

cat("Metrics saved to:", metrics_dir, "\n")


