library(gganimate)
library(ggplot2)
library(patchwork)
library(graphlayouts)

# Try the "dynamic networks" example from the page https://schochastics.github.io/netVizR/
# Create the graphs
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"
output_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/"
subject <- "sub-DBS05"
sessions <- c("ses-preop", "ses-postop01m", "ses-postop03m", "ses-postop06m", "ses-postop12m")

# LOAD ALL NETWORKS
graphs <- list()

# Create a table containing the modules and strengths of each region
membership_table <- data.frame(
  Region = V(graphs[[1]])$name)

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
  
  # ------------------------------
  # COMMUNITY DETECTION
  # ------------------------------
  
  set.seed(123)
  
  community <- cluster_louvain(
    g,
    weights = E(g)$weight
  )
  
  memberships <- membership(
    community
  )
  
  # save to master table
  membership_table[[sessions[i]]] <-
    memberships[
      membership_table$Region
    ]
  
  # ------------------------------
  # MODULE COMPOSITION
  # ------------------------------
  
  module_df <- data.frame()
  
  for(m in sort(unique(memberships))){
    
    regions <- names(
      memberships[memberships == m]
    )
    
    module_df <- rbind(
      module_df,
      data.frame(
        Module = m,
        N_regions = length(regions),
        Regions = paste(
          regions,
          collapse = "; ")))}
  
  write.csv(
    module_df,
    file.path(
      output_dir,
      paste0(
        subject,
        "_",
        sessions[i],
        "_modules.csv")),
    row.names = FALSE)
  
  # ------------------------------
  # HUBS WITHIN EACH MODULE
  # ------------------------------
  
  strength_vals <- strength(
    g,
    weights = E(g)$weight)
  
  module_hubs <- data.frame()
  
  for(m in sort(unique(memberships))){
    
    idx <- memberships == m
    
    tmp <- data.frame(
      Region = names(
        strength_vals[idx]),
      Strength = strength_vals[idx])
    
    tmp <- tmp[
      order(
        -tmp$Strength),]
    
    module_hubs <- rbind(
      module_hubs,
      data.frame(
        Module = m,
        Hub = tmp$Region[1],
        Strength = round(
          tmp$Strength[1],
          2)))}
  
  write.csv(
    module_hubs,
    file.path(
      output_dir,
      paste0(
        subject,
        "_",
        sessions[i],
        "_module_hubs.csv")),
    row.names = FALSE)
  
  # ------------------------------
  # GRAPH VISUALIZATION
  # ------------------------------
  
  groups <- split(
    V(g),
    memberships)
  
  V(g)$size <- scales::rescale(
    strength_vals,
    to = c(6,20))
  
  hub_idx <- order(
    strength_vals,
    decreasing = TRUE)[1:15]
  
  V(g)$label <- ""
  
  V(g)$label[hub_idx] <-
    V(g)$name[hub_idx]
  
  V(g)$color <- memberships
  
  E(g)$width <- scales::rescale(
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
      alpha.f = 0.25),
    
    mark.groups = groups,
    
    mark.col = rainbow(
      length(groups),
      alpha = 0.12),
    
    mark.border = NA,
    
    main = gsub(
      "ses-",
      "",
      sessions[i]))}

dev.off()

# Save modules and strength for each node
cat(
  "Saved:",
  file.path(
    output_dir,
    paste0(
      subject,
      "_dynamic_community_networks.pdf")))

write.csv(
  membership_table,
  file.path(
    output_dir,
    paste0(
      subject,
      "_module_membership_across_sessions.csv")),
  row.names = FALSE)

cat(
  "\nSaved:\n",
  paste0(
    subject,
    "_module_membership_across_sessions.csv\n"))