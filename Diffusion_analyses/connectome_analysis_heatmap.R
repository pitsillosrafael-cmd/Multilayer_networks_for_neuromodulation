library(pheatmap)

# Load connectome
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"
subjects <- c("sub-DBS05", "sub-DBS06", "sub-DBS07")
sessions <- c("ses-preop", "ses-postop01m", "ses-postop03m", "ses-postop06m", "ses-postop12m")

# Colour assignment
red_colors <- colorRampPalette(
  c("black", "darkred", "red", "orange", "white"))(100)

# loop for all subjects
for(sub in subjects){
  
  pdf(file.path(
      base_dir,
      sub,
      paste0(sub, "_connectome_heatmaps.pdf")),
    width = 8,
    height = 8)
  
  for(ses in sessions){
    
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
    
    mat <- as.matrix(
      read.csv(
        file,
        row.names = 1,
        check.names = FALSE))
# save them
    pdf(
      file.path(
        base_dir,
        sub,
        ses,
        paste0(
          "heatmap_",
          ses,
          ".pdf")),
      width = 8,
      height = 8)
    
    pheatmap(
      log1p(mat),
      color = red_colors,
      cluster_rows = FALSE,
      cluster_cols = FALSE,
      show_rownames = TRUE,
      show_colnames = TRUE,
      fontsize_row = 5,
      fontsize_col = 5,
      angle_col = 90,
      border_color = NA,
      main = paste(sub, ses))}
  
  dev.off()
  
  cat("Saved:", paste0(sub, "_connectome_heatmaps.pdf"),"\n")}

###############################################################################
# CONNECTOME DIFFERENCE HEATMAP
# Example: sub-DBS05 (12 months - preoperative)
###############################################################################

base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse"

subject <- "sub-DBS05"

# Read connectivity matrices
mat_pre <- as.matrix(
  read.csv(
    file.path(
      base_dir, subject,
      "ses-preop",
      "connectome",
      paste0("labeled_", subject, "_ses-preop_connectome.csv")
    ),
    row.names = 1,
    check.names = FALSE
  )
)

mat_12m <- as.matrix(
  read.csv(
    file.path(
      base_dir, subject,
      "ses-postop12m",
      "connectome",
      paste0("labeled_", subject, "_ses-postop12m_connectome.csv")
    ),
    row.names = 1,
    check.names = FALSE
  )
)

storage.mode(mat_pre) <- "numeric"
storage.mode(mat_12m) <- "numeric"

# Connectivity change (12m - preop)
delta_mat <- mat_12m - mat_pre

# Heatmap
pheatmap(
  delta_mat,
  color = colorRampPalette(c("blue", "white", "red"))(200),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = paste(subject, "Connectivity change (12m - preop)")
)

# Largest connectivity changes
edges <- which(upper.tri(delta_mat), arr.ind = TRUE)

rewiring <- data.frame(
  Region1 = rownames(delta_mat)[edges[, 1]],
  Region2 = colnames(delta_mat)[edges[, 2]],
  Delta = delta_mat[edges]
) %>%
  arrange(desc(abs(Delta)))

head(rewiring, 20)
