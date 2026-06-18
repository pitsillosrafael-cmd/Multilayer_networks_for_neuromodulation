library(networkD3)
library(igraph)

adj <- as_adjacency_matrix(
  g,
  attr = "weight",
  sparse = FALSE)

edges <- which(
  adj > 0,
  arr.ind = TRUE
)

links <- data.frame(
  source = edges[,1]-1,
  target = edges[,2]-1,
  value = adj[edges]
)

links <- links[
  links$source < links$target,
]

nodes <- data.frame(
  name = V(g)$name
)

forceNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  zoom = TRUE,
  opacity = 0.8
)