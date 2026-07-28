###############################################################################

# Start building usual graphs with communities between the most connected nodes

###############################################################################
library(igraph)
library(tidygraph)
library(ggraph)
library(graphlayouts)

# Threshold the graphs
thr <- quantile(E(g_preop)$weight, 0.90)

##############################
# Preop
##############################

g_preop <- graph_from_adjacency_matrix(
  mean_preop,
  mode = "undirected",
  weighted = T,
  diag = F)

# Attribute node size
morph_preop <- mean_multimodal %>%
  filter(Timepoint == "preop")

morph_preop <- morph_preop[
  match(V(g_preop)$name, morph_preop$Region),]

V(g_preop)$Morphology <- morph_preop$Morphology

# Build communities 
communities <- cluster_louvain(
  g_preop, weights = E(g_preop)$weight)

V(g_preop)$community <- membership(communities)

# inspect them
sizes(communities)

###########Visualize the graph###########

g_preop_thr <- delete_edges(
  g_preop, E(g_preop)[weight < thr])

# plot it
g_tbl <- as_tbl_graph(g_preop_thr)

ggraph(g_tbl, layout = "stress") +
  
  geom_edge_link(
    aes(width = weight),
    alpha = 0.15,
    colour = "grey10") +
  
  geom_node_point(aes(size = Morphology, colour = factor(community))) +
  
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  
  scale_edge_width(range = c(0.2, 2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  plot.margin = margin(30, 30, 30, 30))  
  theme_graph()

ggsave(file = file.path(graphs_dir,"mean_preop_graph.pdf"),
  width = 14,
  height = 14,
  units = "in")



##############################
# Postop12m
##############################
g_postop12m <- graph_from_adjacency_matrix(
  mean_post12m,
  mode = "undirected",
  weighted = T,
  diag = F)

# Attribute node size
morph_postop12m <- mean_multimodal %>%
  filter(Timepoint == "postop12m")

morph_postop12m <- morph_postop12m[
  match(V(g_postop12m)$name, morph_postop12m$Region),]

V(g_postop12m)$Morphology <- morph_postop12m$Morphology

# Build communities 
communities <- cluster_louvain(
  g_postop12m, weights = E(g_postop12m)$weight)

V(g_postop12m)$community <- membership(communities)

# inspect them
sizes(communities)

###########Visualize the graph###########

# Threshold the graph
g_postop12m_thr <- delete_edges(
  g_postop12m, E(g_postop12m)[weight < thr])

# plot it
g_tbl_postop12m <- as_tbl_graph(g_postop12m_thr)

ggraph(g_tbl_postop12m, layout = "stress") +
  
  geom_edge_link(
    aes(width = weight),
    alpha = 0.15,
    colour = "grey10") +
  
  geom_node_point(aes(size = Morphology, colour = factor(community))) +
  
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  
  scale_edge_width(range = c(0.2, 2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  plot.margin = margin(30, 30, 30, 30))
  
theme_graph()
  
  
##############################
# Postop01m
##############################
g_postop01m <- graph_from_adjacency_matrix(
  mean_post01m,
  mode = "undirected",
  weighted = T,
  diag = F)

# Attribute node size
morph_postop01m <- mean_multimodal %>%
  filter(Timepoint == "postop01m")

morph_postop01m <- morph_postop01m[
  match(V(g_postop01m)$name, morph_postop01m$Region),]

V(g_postop01m)$Morphology <- morph_postop01m$Morphology

# Build communities 
communities <- cluster_louvain(
  g_postop01m, weights = E(g_postop01m)$weight)

V(g_postop01m)$community <- membership(communities)

# inspect them
sizes(communities)

###########Visualize the graph###########

# Threshold the graph
g_postop01m_thr <- delete_edges(
  g_postop01m, E(g_postop01m)[weight < thr])

# plot it
g_tbl_postop01m <- as_tbl_graph(g_postop01m_thr)

ggraph(g_tbl_postop01m, layout = "stress") +
  
  geom_edge_link(
    aes(width = weight),
    alpha = 0.15,
    colour = "grey10") +
  
  geom_node_point(aes(size = Morphology, colour = factor(community))) +
  
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  
  scale_edge_width(range = c(0.2, 2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(30, 30, 30, 30))

theme_graph()
  
##############################
# Postop03m
##############################
g_postop03m <- graph_from_adjacency_matrix(
  mean_post03m,
  mode = "undirected",
  weighted = T,
  diag = F)

# Attribute node size
morph_postop03m <- mean_multimodal %>%
  filter(Timepoint == "postop03m")

morph_postop03m <- morph_postop03m[
  match(V(g_postop03m)$name, morph_postop03m$Region),]

V(g_postop03m)$Morphology <- morph_postop03m$Morphology

# Build communities 
communities <- cluster_louvain(
  g_postop03m, weights = E(g_postop03m)$weight)

V(g_postop03m)$community <- membership(communities)

# inspect them
sizes(communities)

###########Visualize the graph###########

# Threshold the graph
g_postop03m_thr <- delete_edges(
  g_postop03m, E(g_postop03m)[weight < thr])

# plot it
g_tbl_postop03m <- as_tbl_graph(g_postop03m_thr)

ggraph(g_tbl_postop03m, layout = "stress") +
  
  geom_edge_link(
    aes(width = weight),
    alpha = 0.15,
    colour = "grey10") +
  
  geom_node_point(aes(size = Morphology, colour = factor(community))) +
  
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  
  scale_edge_width(range = c(0.2, 2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(30, 30, 30, 30))

theme_graph()


##############################
# Postop06m
##############################
g_postop06m <- graph_from_adjacency_matrix(
  mean_post06m,
  mode = "undirected",
  weighted = T,
  diag = F)

# Attribute node size
morph_postop06m <- mean_multimodal %>%
  filter(Timepoint == "postop06m")

morph_postop06m <- morph_postop06m[
  match(V(g_postop06m)$name, morph_postop06m$Region),]

V(g_postop06m)$Morphology <- morph_postop06m$Morphology

# Build communities 
communities <- cluster_louvain(
  g_postop06m, weights = E(g_postop06m)$weight)

V(g_postop06m)$community <- membership(communities)

# inspect them
sizes(communities)

###########Visualize the graph###########

# Threshold the graph
g_postop06m_thr <- delete_edges(
  g_postop06m, E(g_postop06m)[weight < thr])

# plot it
g_tbl_postop06m <- as_tbl_graph(g_postop06m_thr)

ggraph(g_tbl_postop06m, layout = "stress") +
  
  geom_edge_link(
    aes(width = weight),
    alpha = 0.15,
    colour = "grey10") +
  
  geom_node_point(aes(size = Morphology, colour = factor(community))) +
  
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  
  scale_edge_width(range = c(0.2, 2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(30, 30, 30, 30))

theme_graph()

