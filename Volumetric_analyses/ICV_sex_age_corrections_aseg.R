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
aseg_stats <- read.csv("/home/rafaelp/META-BRAIN/open-DBS/DBS_longitudinal_aseg_volumes1_mac.csv", sep = ",")
colnames(aseg_stats)

# Check if the estimated ICV is in normal range
aseg_stats$EstimatedTotalIntraCranialVol
# Check the distribution of regions
hist(aseg_stats$Brain.Stem)

# Fix the variables
aseg_stats$Sex <- as.factor(aseg_stats$Sex)

# Set the regions
regions_aseg <- c(
  "Left.Cerebellum.Cortex", "Left.Thalamus", "Left.Caudate",
  "Left.Putamen", "Left.Pallidum",
  "Left.Hippocampus", "Left.Amygdala", "Left.Accumbens.area",
  "Right.Cerebellum.Cortex", "Right.Thalamus", "Right.Caudate",
  "Right.Putamen", "Right.Pallidum", "Brain.Stem",
  "Right.Hippocampus", "Right.Amygdala", "Right.Accumbens.area"
)

# For each brain region column convert cells into numeric values
aseg_stats[regions_aseg] <- lapply(aseg_stats[regions_aseg], function(x) as.numeric(as.character(x)))

head(regions_aseg)
length(regions_aseg)

# Correct the regions
# Forcing numeric correction
aseg_stats_corrected <- data.frame(matrix(nrow = nrow(aseg_stats), ncol = 0))

for (r in regions_aseg) {
  model <- lm(as.formula(paste(r, "~ Age + Sex + EstimatedTotalIntraCranialVol")),
              data = aseg_stats)
  aseg_stats_corrected[[r]] <- resid(model)
}

aseg_stats_corrected_full <- cbind('Subjects' = aseg_stats$Subjects, aseg_stats_corrected)

# Check the differences between residuals and pre-correction
boxplot(aseg_stats_corrected$Left.Thalamus)
boxplot(aseg_stats$Left.Thalamus)

# Create z-scores for each time point
aseg_stats_corrected_full$Timepoint <- sub("^.*_", "", aseg_stats_corrected_full$Subjects)
table(aseg_stats_corrected_full$Timepoint)

# Empty output for each time point
aseg_corrected_z_time <- aseg_stats_corrected_full

# Loop to get a z score for each timepoint 
for (tp in unique(aseg_corrected_z_time$Timepoint)) {
  
  idx <- aseg_corrected_z_time$Timepoint == tp
  
  aseg_corrected_z_time[idx, regions_aseg] <-
    scale(aseg_corrected_z_time[idx, regions_aseg])
}

# Sanity check 
tapply(aseg_corrected_z_time$Left.Thalamus,
       aseg_corrected_z_time$Timepoint,
       mean)


# Set the variables
tps <- c("preop", "postop01m", "postop03m", "postop06m", "postop12m")

# Set as factors in ordr to respect the order
aseg_corrected_z_time$Timepoint <- factor(
  aseg_corrected_z_time$Timepoint,
  levels = tps
)


# Check heatmap for each region in each subject and each tp
for (tp in unique(aseg_corrected_z_time$Timepoint)) {
  
  # subset data for this timepoint
  idx <- aseg_corrected_z_time$Timepoint == tp
  data_tp <- aseg_corrected_z_time[idx, regions_aseg]
  
  # set rownames (subjects)
  rownames(data_tp) <- aseg_corrected_z_time$Subjects[idx]
  
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



