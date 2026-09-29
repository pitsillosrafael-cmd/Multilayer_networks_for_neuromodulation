install.packages("pheatmap")
install.packages("ComplexHeatmap")
install.packages("circlize")
install.packages("reshape2")
library(pheatmap)
library(ComplexHeatmap)
library(circlize)
library(reshape2)

# ICV, sex and age correction in aparc (DSK) & aseg
# Import the data
aseg_stats_PD <- read.csv("/mnt/shared_data/rafaelp/META-BRAIN/PPMI/Network_analyses/Volumetry/PD_aseg_volume.csv")
colnames(aseg_stats_PD)

# Check if the estimated ICV is in normal range
aseg_stats_PD$EstimatedTotalIntraCranialVol
# Check the distribution of regions
hist(aseg_stats_PD$Right.Pallidum)

# Fix the variables
aseg_stats_PD$Sex <- as.factor(aseg_stats_PD$Sex)

# Set the regions
regions_aseg <- c(
  "Left.Cerebellum.Cortex", "Left.Thalamus", "Left.Caudate",
  "Left.Putamen", "Left.Pallidum",
  "Left.Hippocampus", "Left.Amygdala", "Left.Accumbens.area",
  "Right.Cerebellum.Cortex", "Right.Thalamus", "Right.Caudate",
  "Right.Putamen", "Right.Pallidum", "Brain.Stem",
  "Right.Hippocampus", "Right.Amygdala", "Right.Accumbens.area")

# For each brain region column convert cells into numeric values
aseg_stats_PD[regions_aseg] <- lapply(aseg_stats_PD[regions_aseg], function(x) as.numeric(as.character(x)))

head(regions_aseg)
length(regions_aseg)

# Correct the regions
# Forcing numeric correction
aseg_stats_PD_corrected <- data.frame(matrix(nrow = nrow(aseg_stats_PD), ncol = 0))

for (r in regions_aseg) {
  model <- lm(as.formula(paste(r, "~ Age + Sex + EstimatedTotalIntraCranialVol")),
              data = aseg_stats_PD)
  aseg_stats_PD_corrected[[r]] <- resid(model)}

aseg_stats_corrected_PD_full <- cbind('Subjects' = aseg_stats_PD$Subjects, aseg_stats_PD_corrected)

# Check the differences between residuals and pre-correction
boxplot(aseg_stats_PD_corrected$Left.Thalamus)
boxplot(aseg_stats_PD$Left.Thalamus)



# ICV ONLY correction (for boxplots + line plots in order to avoid residuals and negative values)
# Create a separate copy of the original dataset
aseg_stats_PD_only_ICV_corrected <- aseg_stats_PD

# Apply proportional ICV correction to the selected regions
stopifnot(
  all(is.finite(aseg_stats_PD[[icv_col]])),
  all(aseg_stats_PD[[icv_col]] > 0))

aseg_stats_PD_only_ICV_corrected[regions_aseg] <- lapply(
  aseg_stats_PD[regions_aseg],
  function(x) x / aseg_stats_PD[[icv_col]] * 100000)

# Keep only Subject, Timepoint, and the selected ASEG regions
PD_aseg_ICV_final <- aseg_stats_PD_only_ICV_corrected[
  , c("Subject", "Timepoint", regions_aseg)]

# Save the dataset
write.csv(
  PD_aseg_ICV_final,
  "/mnt/shared_data/rafaelp/META-BRAIN/PPMI/Network_analyses/Volumetry/aparc_aseg_volumes/PD_aseg_volumes_ICV_only.csv",
  row.names = FALSE)

# Verify
dim(PD_aseg_ICV_final)
head(PD_aseg_ICV_final)

# # Create z-scores for each time point
# aseg_stats_corrected_full$Timepoint <- sub("^.*_", "", aseg_stats_corrected_full$Subjects)
# table(aseg_stats_corrected_full$Timepoint)
# 
# # Empty output for each time point
# aseg_corrected_z_time <- aseg_stats_corrected_full
# 
# # Loop to get a z score for each timepoint 
# for (tp in unique(aseg_corrected_z_time$Timepoint)) {
#   
#   idx <- aseg_corrected_z_time$Timepoint == tp
#   
#   aseg_corrected_z_time[idx, regions_aseg] <-
#     scale(aseg_corrected_z_time[idx, regions_aseg])
# }
# 
# # Sanity check 
# tapply(aseg_corrected_z_time$Left.Thalamus,
#        aseg_corrected_z_time$Timepoint,
#        mean)

# Add subject, age and sex in aseg_stats_PD_corrected
aseg_stats_PD_corrected <- cbind(
  aseg_stats_PD[, c("Subject", "Timepoint", "Age", "Sex")],
  aseg_stats_PD_corrected)


# Set the variables
tps <- c("Baseline", "12m")

# Set as factors in ordr to respect the order
aseg_stats_PD_corrected$Timepoint <- factor(
  aseg_stats_PD_corrected$Timepoint,
  levels = tps)

# Save the existing corrected dataset
write.csv(
  aseg_stats_PD_corrected,
  file = "PD_aseg_volumes_age_sex_ICV_corrected.csv",
  row.names = FALSE)


# Check heatmap for each region in each subject and each tp
for (tp in unique(aseg_stats_PD_corrected$Timepoint)) {
  
  # subset data for this timepoint
  idx <- aseg_stats_PD_corrected$Timepoint == tp
  data_tp <- aseg_stats_PD_corrected[idx, regions_aseg, drop = FALSE]
  
  # set rownames (subjects)
  rownames(data_tp) <- aseg_stats_PD_corrected$Subject[idx]
  
  # plot heatmap
  pheatmap(data_tp,
           main = paste("Heatmap -", tp),
           cluster_rows = F,
           cluster_cols = F,
           scale = "none")
}

# For line-plots
# Only for basal ganglia
bg_regions <- c("Left.Caudate", "Left.Putamen", "Left.Pallidum", "Left.Accumbens.area",
                "Right.Caudate", "Right.Putamen", "Right.Pallidum", "Right.Accumbens.area")
long_bg <- melt(aseg_corrected_z_time,
                id.vars = c("Subjects", "Timepoint"),
                measure.vars = bg_regions,
                variable.name = "Region",
                value.name = "Zscore")

# Clean names
long_bg$SubjectID <- sub("_(preop|postop.*)", "", long_bg$Subjects)

ggplot(long_bg,
       aes(x = Timepoint,
           y = Zscore,
           group = SubjectID,
           color = SubjectID)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1) +
  facet_wrap(~ Region, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Trajectory of brain regions across time",
       x = "Timepoint",
       y = "Z-score")

 # Write the files
residuals_df <- cbind(Subjects= aseg_stats$Subjects, aseg_stats_corrected)
write.csv(residuals_df, "/Users/rafaelpitsillos/Desktop/aseg_zscores/aseg_residuals_corrected.csv", row.names = FALSE)
write.csv(aseg_corrected_z_time, "/Users/rafaelpitsillos/Desktop/aseg_corrected//aseg_z-score.csv", row.names = FALSE)

# # Corrected and standardized to match the cortical thickness values (z-score normalization)
# aseg_corrected_z <- scale(aseg_stats_corrected)
# 
# # Plot to check
# hist(as.vector(aseg_corrected_z),
#      breaks = 50,
#      main = "Distribution of Z-scored volumes",
#      xlab = "Z-score")
# 
# boxplot(aseg_corrected_z,
#         las = 2,
#         main = "Z-scored volumes per region")
# 
# image(as.matrix(aseg_corrected_z),
#       xlab = "Regions",
#       ylab = "Subjects",
#       main = "Morphological variation (z-scored)")



