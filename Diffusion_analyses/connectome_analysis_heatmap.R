library(pheatmap)

# Load connectome
mat <- as.matrix(
  read.csv("",
    header = FALSE))

blue_colors <- colorRampPalette(
  c("black", "navy", "blue", "skyblue", "white")
)(100)

pheatmap(
  log1p(mat),
  color = blue_colors,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  show_rownames = FALSE,
  show_colnames = FALSE
)