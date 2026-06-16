library (tidyverse)
library(igraph)
library(dplyr)
library(tidyr)
library(ggpubr)

# Left hemisphere
data_aseg <- read.csv("/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aseg_volumes_age_sex.csv")
colnames(data_aseg)

# Consistent colors for all plots
region_cols <- c("Accumbens.area" = "#F8766D", "Amygdala" = "#C49A00", "Caudate" = "#53B400",
                 "Hippocampus" = "#00C094", "Pallidum" = "#00B6EB", "Putamen" = "#A58AFF", "Thalamus" = "#FB61D7", "Cerebellum.Cortex" = "brown")

# Left hemisphere
ROIS_aseg_lh <- c("Left.Putamen", "Left.Pallidum", "Left.Thalamus",
                  "Left.Caudate", "Left.Hippocampus", "Left.Amygdala",
                  "Left.Accumbens.area")
# Right volumes
ROIS_aseg_rh <- c(
  "Right.Putamen", "Right.Pallidum", "Right.Thalamus", "Right.Caudate",
  "Right.Hippocampus", "Right.Amygdala", "Right.Accumbens.area")


# ICV normalization for boxplots
# ROIs to normalize
ROIs_aseg <- c("Left.Putamen", "Left.Pallidum", "Left.Thalamus", "Left.Caudate", "Left.Hippocampus", "Left.Amygdala",
  "Left.Accumbens.area", "Right.Putamen", "Right.Pallidum", "Right.Thalamus",
  "Right.Caudate", "Right.Hippocampus", "Right.Amygdala", "Right.Accumbens.area")

# Create normalized dataset
data_aseg_icv <- data_aseg

for(region in ROIs_aseg){
  
  data_aseg_icv[[region]] <-
    data_aseg_icv[[region]] /
    data_aseg_icv$ICV * 100000}

data_subj_timepoint_icv <- data_aseg_icv %>%
  rename(Subject_Timepoint = Subjects) %>%
  separate(
    Subject_Timepoint,
    into = c("Subjects", "Timepoint"),
    sep = "_",
    extra = "merge")

data_long_lh_icv <- data_subj_timepoint_icv %>%
  select(
    Subjects,
    Timepoint,
    all_of(ROIS_aseg_lh)
  ) %>%
  pivot_longer(
    cols = all_of(ROIS_aseg_lh),
    names_to = "Region",
    values_to = "Volume")

# Rright
data_long_rh_icv <- data_subj_timepoint_icv %>%
  select(
    Subjects,
    Timepoint,
    all_of(ROIS_aseg_rh)
  ) %>%
  pivot_longer(
    cols = all_of(ROIS_aseg_rh),
    names_to = "Region",
    values_to = "Volume")





# Separate columns (from sub-DBS01_preop to "sub-DBS.." and "Timepoint")
data_subj_timepoint_lh <- data_aseg %>%
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
data_long_lh <- data_long_lh %>% mutate(Timepoint = factor(Timepoint,
                            levels = c("preop", "postop01m", "postop03m", "postop06m", "postop12m")))

# Set colors
data_long_lh <- data_long_lh %>%
  mutate(RegionBase = sub("^Left\\.", "", Region))


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
    title = "Longitudinal subcortical left volumes progression per Subject",
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

# Slopes excluding sub-DBS01 and 03 as they only have 2 timepoints
slopes_lh <- data_long_lh %>%
  filter(!Subjects %in% excluded_subjects) %>%
  group_by(Subjects, Region) %>%
  summarise(
    Slope = coef(lm(Volume ~ Months))[2],
    .groups = "drop")



# Separate subject and timepoint
data_subj_timepoint_rh <- data_aseg %>%
  rename(Subject_Timepoint = Subjects) %>%
  separate(
    Subject_Timepoint,
    into = c("Subjects", "Timepoint"),
    sep = "_",
    extra = "merge")

# Long format
data_long_rh <- data_subj_timepoint_rh %>%
  select(
    Subjects,
    Timepoint,
    all_of(ROIS_aseg_rh)
  ) %>%
  pivot_longer(
    cols = all_of(ROIS_aseg_rh),
    names_to = "Region",
    values_to = "Volume")

# Timepoint as factor
data_long_rh <- data_long_rh %>%
  mutate(Timepoint = factor(Timepoint,
      levels = c("preop", "postop01m", "postop03m", "postop06m", "postop12m")))

# Mean volumes
data_mean_aseg_rh <- data_long_rh %>%
  group_by(Timepoint, Region) %>%
  summarise(
    MeanVolume = mean(Volume, na.rm = TRUE),
    .groups = "drop")

# For color
data_long_rh <- data_long_rh %>%
  mutate(RegionBase = sub("^Right\\.", "", Region))

# Range
data_range_aseg_rh <- data_long_rh %>%
  group_by(Timepoint, Region) %>%
  summarise(
    Mean = mean(Volume, na.rm = TRUE),
    Min = min(Volume, na.rm = TRUE),
    Max = max(Volume, na.rm = TRUE),
    .groups = "drop")

# Plot trajectories + linear trend
ggplot(data_long_rh,
  aes(
    x = Timepoint,
    y = Volume,
    colour = RegionBase,
    group = Region
  )) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1) +
  scale_colour_manual(values = region_cols) +
  facet_wrap(~Subjects, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Subcortical right volume progression per Subject",
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

# Numeric time variable
data_long_rh$Months <- c(
  0, 1, 3, 6, 12
)[match(
  data_long_rh$Timepoint,
  c("preop", "postop01m", "postop03m", "postop06m", "postop12m"))]

# Slopes
slopes_rh <- data_long_rh %>%
  filter(!Subjects %in% excluded_subjects) %>%
  group_by(Subjects, Region) %>%
  summarise(
    Slope = coef(lm(Volume ~ Months))[2],
    .groups = "drop")

# Most increasing regions
slopes_lh %>%
  arrange(desc(Slope))
# Most decreasing regions
slopes_lh %>%
  arrange(Slope)
# Most increasing regions
slopes_rh %>%
  arrange(desc(Slope))
# Most decreasing regions
slopes_rh %>%
  arrange(Slope)

write.csv(slopes_lh,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/aseg_statistics/slopes_lh.csv",
  row.names = FALSE)

write.csv(slopes_rh,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/aseg_statistics/slopes_rh.csv",
  row.names = FALSE)


# Check if there is a trend
# Scatter plot
# Combine left and right
data_long_all <- bind_rows(
  data_long_lh,
  data_long_rh)

# Keep only preop and 12m
excluded_subjects <- c(
  "sub-DBS01", "sub-DBS03", "sub-DBS08")

scatter_subcortical <- data_long_all %>%
  filter(!Subjects %in% excluded_subjects,
    Timepoint %in% c("postop01m", "postop12m")) %>%
  select(Subjects, Region, Timepoint, Volume) %>%
  pivot_wider(
    names_from = Timepoint,
    values_from = Volume)

# same color left|right
scatter_subcortical <- scatter_subcortical %>%
  mutate(Hemisphere = ifelse(
      grepl("^Left", Region),
      "Left","Right"),
    RegionBase = sub(
      "^(Left|Right)\\.",
      "",
      Region))

# plot the scatter plot
ggplot(
  scatter_subcortical,
  aes(x = postop01m,
    y = postop12m,
    colour = RegionBase,
    shape = Hemisphere)) +
  geom_point(
    size = 3,
    alpha = 0.8) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed") +
  theme_minimal() +
  labs(title = "1-month vs 12-month postoperative volumes",
    x = "1st month postoperation volume (mm³)",
    y = "12th month postoperation volume (mm³)",
    colour = "Structure",
    shape = "Hemisphere") +
  theme(legend.text = element_text(size = 7),
    legend.title = element_text(size = 8))


# Check if difference of region preop to postop is significant 
# before go to change correlation between regions
boxplot_lh <- data_long_lh_icv %>%
  filter(
    !Subjects %in% excluded_subjects,
    Timepoint %in% c("postop01m", "postop12m")) %>%
  mutate(Timepoint = factor(Timepoint, levels = c("postop01m", "postop12m")))

ttest_lh <- boxplot_lh %>%
  group_by(Region) %>%
  summarise(
    p_value = t.test(
      Volume[Timepoint == "postop01m"],
      Volume[Timepoint == "postop12m"],
      paired = TRUE
    )$p.value,
    .groups = "drop")

# labels of t-test
sig_labels_lh <- ttest_lh %>%
  mutate(
    label = paste0(
      "p = ",
      signif(p_value, 2)))

# boxplots for change of regions
ggplot(
  boxplot_lh,
  aes(
    x = Timepoint,
    y = Volume)) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA) +
  geom_line(
    aes(group = Subjects),
    alpha = 0.4) +
  geom_point(size = 2) +
  facet_wrap(
    ~Region,
    scales = "free_y") +
  geom_text(
    data = sig_labels_lh,
    aes(
      x = 1.5,
      y = Inf,
      label = label),
    inherit.aes = FALSE,
    vjust = 1.5) +
  theme_minimal()


# Check for normality and do the correlation analysis of the change (Delta percentage)
# Keep only the ones with preop and postop12m
library(Hmisc)
valid_subjects <- data_long_all %>%
  filter(Timepoint %in% c("postop01m", "postop12m")) %>%
  group_by(Subjects) %>%
  summarise(
    n_tp = n_distinct(Timepoint)) %>%
  filter(n_tp == 2) %>%
  pull(Subjects)

# percentage of change
pct_change_df_01_12m <- data_long_all %>%
  filter(
    Subjects %in% valid_subjects,
    Timepoint %in% c("postop01m", "postop12m")) %>%
  select(Subjects, Region, Timepoint, Volume) %>%
  pivot_wider(names_from = Timepoint,
    values_from = Volume) %>%
  mutate(PercentChange =
      100 * (postop12m - postop01m) / postop01m)

head(pct_change_df_01_12m)
# save it
write.csv(pct_change_df_01_12m,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/aseg_statistics/percent_of_change_01m_12m.csv",
  row.names = FALSE)

normality_pct_01m_12m <- pct_change_df_01_12m %>%
  group_by(Region) %>%
  summarise(
    Shapiro_p = shapiro.test(PercentChange)$p.value,
    .groups = "drop")
normality_pct_01m_12m


# Subjects with Delta change percentage for all regions across time
pct_wide_01m_12m <- pct_change_df_01_12m %>%
  select(
    Subjects,
    Region,
    PercentChange) %>%
  pivot_wider(
    names_from = Region,
    values_from = PercentChange)

# Normality checked -> go with Pearson
corr_res_01m_12m <- rcorr(
  as.matrix(pct_wide_01m_12m[, -1]),
  type = "pearson")

corr_mat_01m_12m <- corr_res_01m_12m$r
p_mat_01m_12m <- corr_res_01m_12m$P

library(corrplot)

corrplot(
  corr_res_01m_12m$r,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.7,
  number.cex = 0.5,
  col = colorRampPalette(
    c("blue", "white", "red")
  )(200))

# t-test showing if they have significant difference in change percentage or they change similarly
pairwise_res_percentage_of_change_01m_12m <- pairwise.t.test(
  pct_change_df_01_12m$PercentChange,
  pct_change_df_01_12m$Region,
  paired = TRUE,
  p.adjust.method = "fdr")

# save it
write.csv(
  pairwise_res_percentage_of_change_01m_12m$p.value,
  "/home/rafaelp/META-BRAIN/open-DBS/Network_analyses/Volumetry/aparc_aseg_volumes/pairwise_subcortical_region_pvalues_01m_12m.csv",
  row.names = TRUE)

# check if they change similarly
ttest_pct <- pct_change_df_01_12m %>%
  group_by(Region) %>%
  summarise(
    p_value = t.test(
      PercentChange,
      mu = 0
    )$p.value,
    .groups = "drop"
  )

# label for boxplot of change
sig_labels <- ttest_pct %>%
  mutate(
    label = paste0(
      "p = ",
      signif(p_value, 2)))

# boxplot
ggplot(
  pct_change_df_01_12m,
  aes(
    x = Region,
    y = PercentChange,
    fill = Region)) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.7) +
  geom_jitter(
    width = 0.15,
    size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_text(
    data = sig_labels,
    aes(
      x = Region,
      y = max(pct_change_df_01_12m$PercentChange) + 5,
      label = label),
    inherit.aes = FALSE,
    size = 3) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    legend.position = "none") +
  labs(
    title = "Percentage volume change (Preop → 12m)",
    y = "% Change",
    x = "Region")
