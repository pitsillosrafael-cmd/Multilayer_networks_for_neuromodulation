library (tidyverse)
library(igraph)
library(dplyr)
library(tidyr)
library(ggpubr)
library(Hmisc)

aparc_stats <- read.csv("/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aparc_thickness.csv")
colnames(aparc_stats)

# Exclude non-brain columnms
cortical_rois <- names(aparc_stats)[
  grepl("_thickness$", names(aparc_stats))]

cortical_rois <- cortical_rois[
  !grepl("MeanThickness", cortical_rois)]
length(cortical_rois)

# Separate subject from their timepoints
data_aparc_subj_timepoint <- aparc_stats %>%
  rename(
    Subject_Timepoint = Subjects) %>%
  separate(
    Subject_Timepoint,
    into = c(
      "Subjects",
      "Timepoint"),
    sep = "_",
    extra = "merge")

# Long format
data_long_cortical <- data_aparc_subj_timepoint %>%
  select(
    Subjects,
    Timepoint,
    all_of(cortical_rois)) %>%
  pivot_longer(
    cols = all_of(cortical_rois),
    names_to = "Region",
    values_to = "Thickness")

# Set timepoint as a factor
data_long_cortical <- data_long_cortical %>%
  mutate(
    Timepoint = factor(
      Timepoint,
      levels = c("preop", "postop01m", "postop03m", "postop06m", "postop12m")))

# Separate by hemisphere
data_long_cortical <- data_long_cortical %>%
  mutate(
    Hemisphere = ifelse(
      grepl("^lh_", Region),
      "Left",
      "Right"),
    RegionBase = Region %>%
      gsub("^lh_", "", .) %>%
      gsub("^rh_", "", .) %>%
      gsub("_thickness$", "", .))

# Numeric time variable
data_long_cortical$Months <- c(
  0, 1, 3, 6, 12)[match(
  data_long_cortical$Timepoint,
  c("preop", "postop01m", "postop03m", "postop06m", "postop12m"))]

# Subject-specific slopes
slopes_cortical <- data_long_cortical %>%
  group_by(Subjects, Region) %>%
  summarise(
    Slope = coef(lm(Thickness ~ Months))[2],
    .groups = "drop")
write.csv(slopes_cortical, "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/aparc_statistics/cortical_slopes_all.csv",
          row.names = FALSE)

# Mean slope per cortical region to identify the most significant regions and plot them
mean_slopes_cortical <- slopes_cortical %>%
  group_by(Region) %>%
  summarise(
    MeanSlope = mean(Slope, na.rm = TRUE),
    SD = sd(Slope, na.rm = TRUE),
    .groups = "drop"
  )


# Most increasing regions
top_increasing_cortical <- mean_slopes_cortical %>%
  arrange(desc(MeanSlope)) %>%
  slice(1:10)

top_increasing_cortical

# Top 20 regions by absolute slope magnitude
top_regions_cortical <- mean_slopes_cortical %>%
  arrange(desc(abs(MeanSlope))) %>%
  slice(1:20)

top_regions_cortical

# Vector of selected regions
selected_regions_cortical <- top_regions_cortical$Region
selected_regions_cortical

# Filter cortical data to selected regions
data_long_cortical_top <- data_long_cortical %>%
  filter(
    Region %in% selected_regions_cortical
  )
top_regions_cortical

# Save the most increasing and decreasing thickness
write.csv(
  mean_slopes_cortical,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/cortical_slope_ranking.csv",
  row.names = FALSE
)

# Graphs showing the most significant regions changing from preop to postop
ggplot(
  data_long_cortical_top,
  aes(
    x = Timepoint,
    y = Thickness,
    colour = RegionBase,
    group = Region
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  facet_wrap(
    ~Subjects,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Longitudinal cortical thickness progression per Subject",
    x = "Timepoint",
    y = "Thickness (mm)",
    color = "Region"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 6
    ),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 7),
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8))

# Boxplots showing the change between postop01m to poatop12m
# Postop01m vs 12m cortical thickness
boxplot_cortical <- data_long_cortical_top %>%
  filter(
    !Subjects %in% excluded_subjects,
    Timepoint %in% c("postop01m", "postop12m")
  ) %>%
  mutate(
    Timepoint = factor(
      Timepoint,
      levels = c("postop01m", "postop12m")))

# Paired Wilcoxon test
wilcox_cortical <- boxplot_cortical %>%
  group_by(Region) %>%
  summarise(
    p_value = wilcox.test(
      Thickness[Timepoint == "postop01m"],
      Thickness[Timepoint == "postop12m"],
      paired = TRUE,
      exact = FALSE
    )$p.value,
    .groups = "drop"
  )

# Labels
sig_labels_cortical <- wilcox_cortical %>%
  mutate(
    label = paste0(
      "p = ",
      signif(p_value, 2)))

# Plot
ggplot(
  boxplot_cortical,
  aes(
    x = Timepoint,
    y = Thickness)) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA
  ) +
  geom_line(
    aes(group = Subjects),
    alpha = 0.4) +
  geom_point(size = 2) +
  facet_wrap(
    ~Region,
    scales = "free_y") +
  geom_text(
    data = sig_labels_cortical,
    aes(
      x = 1.5,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    vjust = 1.2,
    size = 3) +
  theme_minimal() +
  labs(
    title = "Cortical thickness change (1m → 12m)",
    x = "",
    y = "Thickness (mm)")



# Check the percentage change to decide the regions that need to be plot
# Only include the regions with both pre and postop12m
valid_subjects <- data_long_cortical %>%
  filter(
    Timepoint %in% c(
      "postop01m",
      "postop12m")) %>%
  group_by(Subjects) %>%
  summarise(n_tp = n_distinct(Timepoint)) %>%
  filter(n_tp == 2) %>%
  pull(Subjects)

# Percentage of change
pct_change_cortical <- data_long_cortical %>%
  filter(
    Subjects %in% valid_subjects,
    Timepoint %in% c(
      "postop01m",
      "postop12m")) %>%
  select(
    Subjects,
    Region,
    Timepoint,
    Thickness) %>%
  pivot_wider(
    names_from = Timepoint,
    values_from = Thickness) %>%
  mutate(
    PercentChange =
      100 * (
        postop12m - postop01m
      ) / postop01m)

# save the percentages
write.csv(pct_change_cortical,
          "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/aparc_statistics/percent_change_aparc.csv",
          row.names = FALSE)

# Shapiro for normality of these percentages
normality_pct_cortical <- pct_change_cortical %>%
  group_by(
    Region) %>%
  summarise(
    Shapiro_p =
      shapiro.test(
        PercentChange
      )$p.value,
    .groups = "drop")

normality_pct_cortical
write.csv(normality_pct_cortical,
          "/home/rafaelp/META-BRAIN/open-DBS/Network_analyses/Volumetry/aparc_aseg_volumes/normality_pct_change_aparc.csv",
          row.names = FALSE)

# Correlation mtrx
pct_wide_cortical <- pct_change_cortical %>%
  select(
    Subjects,
    Region,
    PercentChange
  ) %>%
  pivot_wider(
    names_from = Region,
    values_from = PercentChange
  )

# Do spearman as some of them are in non normal distrb
corr_res_cortical <- rcorr(
  as.matrix(
    pct_wide_cortical[, -1]
  ),
  type = "spearman")
corr_mat_cortical <- corr_res_cortical$r
p_mat_cortical <- corr_res_cortical$P


# SAelect the cortical regions of interest (the ones with top change) instead of all 68
pct_wide_cortical_top <- pct_change_cortical %>%
  filter(
    Region %in% selected_regions_cortical
  ) %>%
  select(
    Subjects,
    Region,
    PercentChange
  ) %>%
  pivot_wider(
    names_from = Region,
    values_from = PercentChange)

# check the top changed regions
corr_res_cortical_top <- rcorr(
  as.matrix(
    pct_wide_cortical_top[, -1]
  ),
  type = "spearman")

corr_mat_cortical_top <- corr_res_cortical_top$r
p_mat_cortical_top <- corr_res_cortical_top$P

# plot the cor matrix
corrplot(
  corr_mat_cortical_top,
  method = "color",
  type = "upper",
  order = "hclust",
  
  p.mat = p_mat_cortical_top,
  insig = "label_sig",
  sig.level = c(0.001, 0.01, 0.05),
  pch.cex = 0.5,
  pch.col = "black",
  
  tl.col = "black",
  tl.cex = 0.6,
  
  addCoef.col = NULL,
  
  col = colorRampPalette(
    c("blue", "white", "red")
  )(200)
)


# Scatter plot cortical regions between preoperative and postoperative
scatter_cortical <- data_long_cortical %>%
  filter(!Subjects %in% excluded_subjects,
    Timepoint %in% c("preop", "postop12m")) %>%
  select(
    Subjects,
    Region,
    Timepoint,
    Thickness) %>%
  pivot_wider(
    names_from = Timepoint,
    values_from = Thickness) %>%
  mutate(
    Hemisphere = ifelse(
      grepl("^lh_", Region),
      "Left",
      "Right"),
    RegionBase = Region %>%
      gsub("^lh_", "", .) %>%
      gsub("^rh_", "", .) %>%
      gsub("_thickness$", "", .))

# plot the scatter 
ggplot(
  scatter_cortical,
  aes(
    x = preop,
    y = postop12m,
    colour = RegionBase,
    shape = Hemisphere)) +
  geom_point(
    size = 3,
    alpha = 0.8) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.8) +
  scale_shape_manual(
    values = c(
      "Left" = 16,   # circle
      "Right" = 17   # triangle
    )) +
  theme_minimal() +
  labs(title = "Preoperative vs 12-month postoperative cortical thickness",
    x = "Preoperative thickness (mm)",
    y = "Postoperative thickness at 12 months (mm)",
    colour = "Region",
    shape = "Hemisphere")


# Identify regions with greatest change
cortical_stats <- pct_change_cortical %>%
  group_by(Region) %>%
  summarise(
    MeanPercentChange = mean(PercentChange, na.rm = TRUE),
    MeanAbsChange = mean(abs(PercentChange), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanAbsChange))

top_cortical_region_change <- cortical_stats %>%
  slice_head(n = 14) %>%
  pull(Region)



# Left hemisphere
# Consistent colors for all plots
region_cols <- c("Accumbens.area" = "#F8766D", "Amygdala" = "#C49A00", "Caudate" = "#53B400",
                 "Hippocampus" = "#00C094", "Pallidum" = "#00B6EB", "Putamen" = "#A58AFF", "Thalamus" = "#FB61D7")

# Left hemisphere
ROIS_aseg_lh <- c("Left.Putamen", "Left.Pallidum", "Left.Thalamus",
                  "Left.Caudate", "Left.Hippocampus", "Left.Amygdala",
                  "Left.Accumbens.area")

# Separate columns (from sub-DBS01_preop to "sub-DBS.." and "Timepoint")
data_subj_timepoint_lh <- data_aseg_lh %>%
  rename(Subject_Timepoint = Subjects) %>%  # rename if needed
  separate(Subject_Timepoint, into = c("Subjects", "Timepoint"), sep = "_", extra = "merge")  # split first _ as separator

data_long_lh <- data_subj_timepoint_lh %>%
  select(Subjects, Timepoint, all_of(ROIS_aseg_lh)) %>%
  pivot_longer(
    cols = all_of(ROIS_aseg_lh),
    names_to = "Region",
    values_to = "Volume"
  )

# Timepoint as a factor
data_long_lh <- data_long_lh %>%
  mutate(Timepoint = factor(Timepoint,
                            levels = c("preop", "postop01m", "postop03m", "postop06m", "postop12m")))

# Set colors
data_long_lh <- data_long_lh %>%
  mutate(
    RegionBase = sub("^Left\\.", "", Region)
  )


# For mean volumes
data_mean_aseg_lh <- data_long_lh %>%
  group_by(Timepoint, Region) %>%
  summarise(
    MeanVolume = mean(Volume, na.rm = TRUE),
    .groups = "drop"
  )

# For the range of the values
data_range_aseg_lh <- data_long_lh %>%
  group_by(Timepoint, Region) %>%
  summarise(
    Mean = mean(Volume, na.rm = TRUE),
    Min = min(Volume, na.rm = TRUE),
    Max = max(Volume, na.rm = TRUE),
    .groups = "drop"
  )

# plot all regions
ggplot(
  data_long_lh,
  aes(
    x = Timepoint,
    y = Volume,
    colour = Region,
    group = Region
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  facet_wrap(~Subjects, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Subcortical left volumes progression per Subject",
    x = "Timepoint",
    y = "Volume (mm3)",
    color = "Region") +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 6),
    axis.text.y = element_text(
      size = 6),
    strip.text = element_text(
      size = 7),
    legend.text = element_text(
      size = 7),
    legend.title = element_text(
      size = 8))

# To get the slopes
data_long_lh$Months <- c(
  0, 1, 3, 6, 12
)[match(
  data_long_lh$Timepoint,
  c("preop", "postop01m", "postop03m", "postop06m", "postop12m"))]

slopes_lh <- data_long_lh %>%
  group_by(Subjects, Region) %>%
  summarise(
    Slope = coef(lm(Volume ~ Months))[2],
    .groups = "drop")




# Check which regions change the most dramatically to plot (as there are 64 areas)
cortical_stats <- pct_change_cortical %>%
  group_by(Region) %>%
  summarise(
    MeanPercentChange = mean(PercentChange, na.rm = TRUE),
    SDPercentChange = sd(PercentChange, na.rm = TRUE),
    MedianPercentChange = median(PercentChange, na.rm = TRUE),
    MinPercentChange = min(PercentChange, na.rm = TRUE),
    MaxPercentChange = max(PercentChange, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(MeanPercentChange)