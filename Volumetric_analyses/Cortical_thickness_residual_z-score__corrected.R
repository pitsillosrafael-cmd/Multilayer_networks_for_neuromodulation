library(pheatmap)
library(ComplexHeatmap)
library(circlize)
library(reshape2)
library(dplyr)

# ICV, sex and age correction in aparc (DSK) & aseg
# Import the data
aparc_stats <- read.csv("/home/rafaelp/META-BRAIN/open-DBS/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aparc_thickness.csv")
colnames(aparc_stats)

regions_aparc <- grep("thickness", colnames(aparc_stats), value = TRUE)
aparc_stats$Sex <- as.factor(aparc_stats$Sex)
aparc_stats$Timepoint <- sub("^.*_", "", aparc_stats$Subjects)

# Check the distributions
for (r in regions_aparc) {
  hist(aparc_stats[[r]],
       main = r,
       xlab = "Thickness",
       col = "Lightblue")
}
head(aparc_stats)

# Residuals for each timepoint
aparc_corrected_time <- data.frame(
  Subjects = aparc_stats$Subjects,
  Timepoint = sub("^.*_", "", aparc_stats$Subjects)
)

# Corrected for age and sex for each region based on each tp 
for (tp in unique(aparc_stats$Timepoint)) {
  
  idx <- aparc_stats$Timepoint == tp
  data_tp <- aparc_stats[idx, ]
  
  for (r in regions_aparc) {
    
    model <- lm(as.formula(paste(r, "~ Age + Sex")),
                data = data_tp)
    
    aparc_corrected_time[idx, r] <- resid(model)
  }
}

# z-scores for each region and tp
aparc_corrected_z_time <- aparc_corrected_time

for (tp in unique(aparc_corrected_time$Timepoint)) {
  
  idx <- aparc_corrected_time$Timepoint == tp
  
  aparc_corrected_z_time[idx, regions_aparc] <-
    scale(aparc_corrected_time[idx, regions_aparc])
}


# Vizualize with heatmap
tps <- c("preop", "postop01m", "postop03m", "postop06m", "postop12m")

# Set as factors in ordr to respect the order
aparc_corrected_z_time$Timepoint <- factor(
  aparc_corrected_z_time$Timepoint,
  levels = tps
)


for (tp in unique(aparc_corrected_z_time$Timepoint)) {
  
  # subset data for this timepoint
  idx <- aparc_corrected_z_time$Timepoint == tp
  data_aparc_tp <- aparc_corrected_z_time[idx,
                                          setdiff(regions_aparc,
                                                  c("lh_MeanThickness_thickness",
                                                    "rh_MeanThickness_thickness"))]
  
  # set rownames (subjects)
  rownames(data_aparc_tp) <- aparc_corrected_z_time$Subjects[idx]
  
  # plot heatmap
  pheatmap(as.matrix(data_aparc_tp),
           main = paste("Heatmap -", tp),
           cluster_rows = F,
           cluster_cols = F,
           scale = "none")
}

regions_aparc_clean <- setdiff(regions_aparc, c("lh_MeanThickness_thickness", "rh_MeanThickness_thickness"))
regions_aparc_clean <- cbind(Subjects = aparc_corrected_time$Subjects, Timepoint = aparc_corrected_time$Timepoint, aparc_corrected_time[, regions_aparc_clean])
aparc_zscores_export <- aparc_corrected_z_time[ ,c("Subjects", "Timepoint", setdiff(regions_aparc, c("lh_MeanThickness_thickness","rh_MeanThickness_thickness")))]

write.csv(regions_aparc_clean, "/Users/rafaelpitsillos/Desktop/aparc_corrected/aparc_corrected.csv", row.names = FALSE)
write.csv(aparc_zscores_export, "/Users/rafaelpitsillos/Desktop/aparc_corrected/aparc_z_corrected.csv", row.names = FALSE)
