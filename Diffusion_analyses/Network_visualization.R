library(networkD3)
library(circlize)
library(igraph)

# Playing with networks
# Load connectome
file <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07/sub-DBS05/ses-postop12m/connectome/labeled_sub-DBS05_ses-postop12m_connectome.csv"
mat <- as.matrix(
  read.csv(
    file,
    row.names = 1,
    check.names = FALSE))

storage.mode(mat) <- "numeric"

weights <- mat[upper.tri(mat)]
thr <- quantile(
  weights[weights > 0],
  0.9)

mat_thr <- mat
mat_thr[mat_thr < thr] <- 0

# graph
g <- graph_from_adjacency_matrix(
  mat_thr,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE)
g

adj <- as_adjacency_matrix(
  g,
  attr = "weight",
  sparse = FALSE)

edges <- which(
  adj > 0,
  arr.ind = TRUE)

links <- data.frame(
  source = edges[,1]-1,
  target = edges[,2]-1,
  value = adj[edges])

links <- links[
  links$source < links$target,]
head(links)


community <- cluster_louvain(
  g,
  weights = E(g)$weight)

nodes <- data.frame(
  name = V(g)$name,
  group = membership(community))

# Play with NetworkD3
forceNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  Group = "group",
  opacity = 0.8,
  zoom = TRUE,
  fontSize = 12,
  charge = -300,
  linkDistance = 60)

# Circled
mat_plot <- mat_thr

# Remove empty rows/columns
keep <- rowSums(mat_plot) > 0

mat_plot <- mat_plot[
  keep,
  keep]

chordDiagram(
  mat_plot,
  transparency = 0.8,
  annotationTrack = "grid",
  directional = FALSE)


# colour by community
table(membership(community))

V(g)$color <- membership(community)
plot(g,
     layout = layout_with_fr(g),
     vertex.size = 8,
     vertex.label = NA,
     edge.color = adjustcolor("grey50", alpha.f = 0.2),
     main = "sub-DBS06 postop12m")



# Loop to create for all subjects weighted, community and by strength networks
library(igraph)
library(scales)

base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"

output_dir <- "C:/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/weighted_communities_hub-strength"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE)

subjects <- c("sub-DBS05", "sub-DBS06", "sub-DBS07")
sessions <- c("ses-preop", "ses-postop01m", "ses-postop03m", "ses-postop06m", "ses-postop12m")

for(subject in subjects){
  
  for(session in sessions){
    
    cat(
      "Processing:",
      subject,
      session,
      "\n")
    
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
    
    if(!file.exists(file)){
      cat("Missing:", file, "\n")
      next}
    
    mat <- as.matrix(
      read.csv(
        file,
        row.names = 1,
        check.names = FALSE))
    
    storage.mode(mat) <- "numeric"
    
    # 90th percentile threshold
    weights <- mat[upper.tri(mat)]
    
    thr <- quantile(
      weights[weights > 0],
      0.90)
    
    mat_thr <- mat
    mat_thr[mat_thr < thr] <- 0
    
    # graph
    g <- graph_from_adjacency_matrix(
      mat_thr,
      mode = "undirected",
      weighted = TRUE,
      diag = FALSE)
    
    # community detection
    community <- cluster_louvain(
      g,
      weights = E(g)$weight)
    
    groups <- split(
      V(g),
      membership(community))
    
    # node size = hub strength
    V(g)$size <- rescale(
      strength(g),
      to = c(5,20))
    
    # label only top hubs
    hub_idx <- order(
      strength(g),
      decreasing = TRUE
    )[1:15]
    
    V(g)$label <- ""
    
    V(g)$label[hub_idx] <-
      V(g)$name[hub_idx]
    
    # node color = community
    V(g)$color <- membership(
      community)
    
    # edge width = connection strength
    E(g)$width <- rescale(
      E(g)$weight,
      to = c(0.5,8))
    
    # fixed seed for reproducibility
    set.seed(123)
    
    layout_fixed <- layout_with_fr(g)
    
    pdf(
      file.path(
        output_dir,
        paste0(
          subject,
          "_",
          session,
          "_weighted_community.pdf")),
      width = 12,
      height = 10)
    
    plot(
      g,
      layout = layout_fixed,
      
      vertex.size = V(g)$size,
      vertex.color = V(g)$color,
      
      vertex.label.cex = 0.8,
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
      
      main = paste(
        subject,
        session,
        "\nCommunity structure (weighted)"))
    
    dev.off()}}

cat("All plots saved.\n")

