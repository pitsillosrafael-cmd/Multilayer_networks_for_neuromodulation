######################################################

# Run shapiro tests to test normality of distributions
# in multimodal_long (strength & morphology)

######################################################
library(dplyr)

# Morphology
shapiro_morphology <- multimodal_long %>%
  group_by(Timepoint, Region) %>%
  summarise(
    N = sum(!is.na(Morphology)),
    W = ifelse(N >= 3,
               shapiro.test(Morphology)$statistic,
               NA),
    p = ifelse(N >= 3,
               shapiro.test(Morphology)$p.value,
               NA),
    .groups = "drop")

shapiro_morphology
sum(shapiro_morphology$p < 0.05, na.rm = TRUE)


# Strength
shapiro_strength <- multimodal_long %>%
  group_by(Timepoint, Region) %>%
  summarise(
    N = sum(!is.na(Strength)),
    W = ifelse(N >= 3,
               shapiro.test(Strength)$statistic,
               NA),
    p = ifelse(N >= 3,
               shapiro.test(Strength)$p.value,
               NA),
    .groups = "drop")

shapiro_strength
sum(shapiro_strength$p < 0.05, na.rm = TRUE)


# Integrated index
shapiro_I <- multimodal_long %>%
  group_by(Timepoint, Region) %>%
  summarise(
    N = sum(!is.na(I)),
    W = ifelse(N >= 3,
               shapiro.test(I)$statistic,
               NA),
    p = ifelse(N >= 3,
               shapiro.test(I)$p.value,
               NA),
    .groups = "drop")

shapiro_I
sum(shapiro_I$p < 0.05, na.rm = TRUE)



# Gather all shapiro in one df
shapiro_summary <- shapiro_morphology %>%
  select(Timepoint, Region, Morphology = p) %>%
  left_join(
    shapiro_strength %>%
      select(Timepoint, Region, strength = p),
    by = c("Timepoint", "Region")
  ) %>%
  left_join(shapiro_I %>%
              select(Timepoint, Region, I = p),
            by = c("Timepoint", "Region"))

# Save all Pval from Shapiro
write.csv(shapiro_summary, file = file.path(shapiro_dir, "Shapiro_summary.csv"),
          row.names = FALSE)
