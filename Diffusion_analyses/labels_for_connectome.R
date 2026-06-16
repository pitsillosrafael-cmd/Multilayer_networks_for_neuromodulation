library(pheatmap)

# Assign labels to connectomes
# Set base directory
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07/sub-DBS07"

# Label the 64 regions based on DSK and aseg FS atlases
labels_raw <- read.table(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07/fs_default.txt",
  stringsAsFactors = FALSE
)

# Remove unknown
labels_clean <- labels_raw[
  labels_raw$V2 != "???" &
    !duplicated(labels_raw$V1),
]

labels <- labels_clean$V2

# Find all connectomes
connectome_files <- list.files(
  base_dir,
  pattern = "_connectome\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Verify files
length(connectome_files)


for (file in connectome_files) {
  cat('Processing:', basename(file), "\n")
  mat <- read.csv(
    file,
    header=FALSE
  )
  
  mat <- as.matrix(mat)
  storage.mode(mat) <- "numeric"

  cat("Matrix dimensions:", dim(mat), "\n")
  cat("Number of labels:", length(labels), "\n")
  
  # Assign labels
  rownames(mat) <- labels
  colnames(mat) <- labels
  
  # QC heatmap
  print(pheatmap(
    mat,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    color = colorRampPalette(
      c("black", "blue", "cyan", "white")
    )(100),
    border_color = NA,
    show_rownames = FALSE,
    show_colnames = FALSE,
    main = basename(file)
  ))
  # readline("Press ENTER for next connectome...")
  
  # Save labeled connectome
  outfile <- file.path(
    dirname(file),
    paste0(
      "labeled_",
      basename(file)
    )
  )
  
  write.csv(
    mat,
    outfile,
    row.names = TRUE
  )
  
  cat("Saved:", outfile, "\n")
}
