# Create long format for the analyses
library(tidyr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

# Read
morph <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/fs_default_scaled/DBS_longitudinal_morphology_84regions_scaled.csv",
  check.names = FALSE)

strength <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/Network_metrics/All_node_strength_scaled.csv",
  check.names = FALSE)

# Long format morphology
morph_long <- morph %>%
  pivot_longer(
    cols = -c(Subjects, Timepoint),
    names_to = "Region",
    values_to = "Morphology")

# Long format strength
strength$Timepoint <- sub("^ses-", "", strength$Timepoint)

strength_long <- strength %>%
  pivot_longer(
    cols = -c(Subjects, Timepoint),
    names_to = "Region",
    values_to = "Strength")

# Merge
multimodal_long <- morph_long %>%
  left_join(
    strength_long,
    by = c("Subjects", "Timepoint", "Region"))

head(multimodal_long)

write.csv(
  multimodal_long,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Multimodal_long.csv",
  row.names = FALSE)

# Calculated Integrated value for all regions acorss subjects and sessions
w_morph <- 0.5
w_conn  <- 0.5

multimodal_long <- multimodal_long %>%
  mutate(I = ifelse(
      is.na(Morphology) | is.na(Strength),
      NA,
      w_morph * Morphology + w_conn * Strength))

# Now line plot
# Order sessions
multimodal_long$Timepoint <- factor(
  multimodal_long$Timepoint,
  levels = c(
    "preop",
    "postop01m",
    "postop03m",
    "postop06m",
    "postop12m"))

df_integrated <- multimodal_long %>%
  group_by(Region, Timepoint) %>%
  summarise(
    Mean_I = mean(I, na.rm = TRUE),
    SD_I   = sd(I, na.rm = TRUE),
    n      = sum(!is.na(I)),
    .groups = "drop")

# Plot
# Create numeric version for fitting
multimodal_long$TimeNum <- c(0,1,3,6,12)[match(
  multimodal_long$Timepoint,
  c("preop","postop01m","postop03m","postop06m","postop12m"))]

# Output folder
outdir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Integrated/integrated_line_plots_all_DTIs"
dir.create(outdir, showWarnings = FALSE)

regions <- unique(multimodal_long$Region)

trend_results <- data.frame()

for(reg in regions){
  
  df <- multimodal_long %>%
    filter(
      Region == reg,
      Timepoint %in% c("preop", "postop01m", "postop03m", "postop06m", "postop12m"),
      !is.na(I))
  
  fit <- lm(I ~ TimeNum, data = df)
  
  slope <- coef(fit)[2]
  r2 <- summary(fit)$r.squared
  p_lm <- coef(summary(fit))[2, 4]
  
  sp <- cor.test(
    df$TimeNum,
    df$I,
    method = "spearman",
    exact = FALSE)
  
  rho <- unname(sp$estimate)
  p_spear <- sp$p.value
  
  trend_results <- rbind(
    trend_results,
    data.frame(
      Region = reg,
      Slope = slope,
      R2 = r2,
      LM_p = p_lm,
      Spearman_rho = rho,
      Spearman_p = p_spear))
  
  if(nrow(df) < 3) next
  
  p <- ggplot(df, aes(x = TimeNum, y = I)) +
    
    geom_point(
      colour = "#69b3a2",
      size = 2,
      alpha = 0.7) +
    
    geom_smooth(
      method = "lm",
      colour = "#2C7FB8",
      fill = "#69b3a2",
      se = TRUE,
      linewidth = 1) +
    
    scale_x_continuous(
      breaks = c(0,1,3,6,12),
      labels = c("Pre","1m","3m","6m","12m")) +
    
    coord_cartesian(ylim = c(0,1)) +
    
    labs(
      title = reg,
      subtitle = paste0(
        "Slope = ", round(slope, 3),
        "   |   R² = ", round(r2, 2),
        "   |   p(LM) = ", signif(p_lm, 2),
        "\n",
        "Spearman ρ = ", round(rho, 2),
        "   |   p = ", signif(p_spear, 2)),
      x = "Session",
      y = "Integrated Index (I)") +
    
    theme_minimal()
  
  print(p)
  
  ggsave(
    file.path(outdir, paste0(reg, ".pdf")), p, width = 6, height = 5)}

write.csv(trend_results,
  file.path(outdir, "IntegratedIndex_Trends.csv"), row.names = FALSE)



# Identify the 10 most increasing and most decreasing regions
region_change <- multimodal_long %>%
  filter(Timepoint %in% c("preop", "postop12m")) %>%
  group_by(Region, Timepoint) %>%
  summarise(
    Mean_I = mean(I, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = Timepoint,
    values_from = Mean_I
  ) %>%
  mutate(
    Change = postop12m - preop
  ) %>%
  arrange(desc(Change))

# increasing
top15_increasing <- region_change %>%
  slice_max(Change, n = 15)
top15_increasing

# decreasing
top10_decreasing <- region_change %>%
  slice_min(Change, n = 10)
top10_decreasing


write.csv(
  top10_increasing,
  file = paste0(outdir, "/Top10_increasing_regions.csv"),
  row.names = FALSE)

write.csv(
  top10_decreasing,
  file = paste0(outdir, "/Top10_decreasing_regions.csv"),
  row.names = FALSE)


# Check correlation between integrated index across regions
I_wide <- multimodal_long %>%
  filter(!is.na(I)) %>%
  select(Subjects, Timepoint, Region, I) %>%
  pivot_wider(
    names_from = Region,
    values_from = I)

# Calculate pval for every combination
mat <- I_wide %>%
  select(-Subjects, -Timepoint) %>%
  as.matrix()

corr <- rcorr(mat, type = "spearman")

cor_mat <- corr$r
p_mat   <- corr$P

cor_mat <- I_wide %>%
  select(-Subjects, -Timepoint) %>%
  cor(use = "pairwise.complete.obs",
    method = "spearman")

pdf("IntegratedIndex_RegionalCorrelationMatrix.pdf",
    width = 10,
    height = 10)

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.cex = 0.5,
  tl.col = "black")

dev.off()