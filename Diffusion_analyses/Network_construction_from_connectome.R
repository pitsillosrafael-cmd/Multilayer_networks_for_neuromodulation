library(igraph)


# Get the networks for each subject
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"
output_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks"
subject <- "sub-DBS05"
sessions <- c("ses-preop", "ses-postop01m",
              "ses-postop03m", "ses-postop06m",
              "ses-postop12m")

graphs <- list()

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



