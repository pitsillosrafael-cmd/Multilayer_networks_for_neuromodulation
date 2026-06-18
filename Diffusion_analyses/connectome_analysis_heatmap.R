library(pheatmap)

# Load connectome
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse_sub_05_06_07"
subjects <- c(
  "sub-DBS05", "sub-DBS06", "sub-DBS07")
sessions <- c(
  "ses-preop", "ses-postop01m", "ses-postop03m", "ses-postop06m", "ses-postop12m")

# Colour assignment
red_colors <- colorRampPalette(
  c("black", "darkred", "red", "orange", "white")
)(100)

# loop for all subjects
for(sub in subjects){
  
  pdf(
    file.path(
      base_dir,
      sub,
      paste0(sub, "_connectome_heatmaps.pdf")
    ),
    width = 8,
    height = 8
  )
  
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
        "_connectome.csv"
      )
    )
    
    mat <- as.matrix(
      read.csv(
        file,
        row.names = 1,
        check.names = FALSE
      )
    )
# save them
    pdf(
      file.path(
        base_dir,
        sub,
        ses,
        paste0(
          "heatmap_",
          ses,
          ".pdf"
        )
      ),
      width = 8,
      height = 8
    )
    
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
      main = paste(sub, ses)
    )
  }
  
  dev.off()
  
  cat(
    "Saved:",
    paste0(sub, "_connectome_heatmaps.pdf"),
    "\n"
  )
}
