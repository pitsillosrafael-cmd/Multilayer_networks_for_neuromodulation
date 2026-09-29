library (tidyverse)
library(igraph)
library(dplyr)
library(tidyr)
library(ggpubr)

# Use PD_aseg_ICV_final (contains both ICV corrected and only the significant columns -16 subcortical regions)
setwd("/mnt/shared_data/rafaelp/META-BRAIN/PPMI/Network_analyses/Volumetry/aparc_aseg_volumes")

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

# Reshape the data and match the participants
PD_aseg_boxplot <- PD_aseg_ICV_final %>%
  filter(Timepoint %in% c("Baseline", "12m")) %>%
  select(Subject, Timepoint, all_of(ROIS_aseg_rh)) %>%
  pivot_longer(cols = all_of(ROIS_aseg_rh),
               names_to = "Region",
               values_to = "Volume"
               ) %>%
  mutate(Subject = as.character(Subject),
         Timepoint = factor(
           Timepoint, levels = c("Baseline", "12m")
         ), Region = factor(Region, levels = ROIS_aseg_rh))

# One row per participant and per subject
# Create one row per participant and region
paired_data <- PD_aseg_boxplot %>%
  pivot_wider(
    names_from = Timepoint,
    values_from = Volume
  ) %>%
  filter(
    !is.na(Baseline),
    !is.na(`12m`))

# Check the number of paired observations per region
paired_data %>% group_by(Region) %>% summarise(n_pairs = n(), .groups = "drop")

# Normality test
normality_results <- paired_data %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    shapiro_p = if (n() >= 3 && n() <= 5000) {
      shapiro.test(`12m` - Baseline)$p.value
    } else {
      NA_real_},
    .groups = "drop")

# Calculate the p-values for the box plot based on the results from normality test
test_results <- paired_data %>%
  group_by(Region) %>%
  group_modify(~ {
    if (as.character(.y$Region) %in%
        c("Left.Pallidum", "Left.Thalamus")) {
      
      p <- wilcox.test(
        .x$Baseline, .x$`12m`,
        paired = TRUE,
        exact = FALSE
      )$p.value
      
    } else {
      
      p <- t.test(
        .x$Baseline, .x$`12m`,
        paired = TRUE
      )$p.value
    }
    
    tibble(p_value = p)
  }) %>%
  ungroup()
      

# place them above the graphs
# Position p-values above each boxplot
label_positions <- PD_aseg_boxplot %>%
  group_by(Region) %>%
  summarise(
    y_position = max(Volume, na.rm = TRUE) +
      0.10 * diff(range(Volume, na.rm = TRUE)),
    .groups = "drop")

test_results <- test_results %>%
  left_join(label_positions, by = "Region") %>%
  mutate(p_label = paste0("p = ", format.pval(
    p_value, digits = 2, eps = 0.001
  )))

# Draw the boxplot
Baseline_12m_plot <- ggplot(
  PD_aseg_boxplot,
  aes(x = Timepoint, y = Volume)) +
  geom_boxplot(
    width = 0.45,
    fill = "white",
    colour = "black",
    outlier.shape = NA) +
  geom_line(aes(group = Subject),
    colour = "grey60",
    alpha = 0.8,
    linewidth = 0.4) +
  geom_point(
    aes(group = Subject),
    colour = "black",
    size = 2) +
  geom_text(
    data = test_results,
    aes(
      x = 1.5,
      y = y_position,
      label = p_label),
    inherit.aes = FALSE,
    size = 3.5) +
  facet_wrap(
    ~ Region,
    scales = "free_y",
    ncol = 3) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.20))) +
  labs(
    x = NULL,
    y = "Volume") +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title.y = element_text(size = 11))
       
# save
ggsave(
  filename = "Box_plots_right_ICV_corrected_Baseline_12m.pdf",
  plot = Baseline_12m_plot,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in")
