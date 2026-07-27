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
