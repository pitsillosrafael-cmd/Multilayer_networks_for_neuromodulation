# Integrated networks
library(dplyr)

# Read the CSV
aparc_df <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aparc_thickness.csv",
  check.names = FALSE)

# Mapping
name_map <- c(
  "lh_bankssts_thickness"="L.BSTS",
  "lh_caudalanteriorcingulate_thickness"="L.CACG",
  "lh_caudalmiddlefrontal_thickness"="L.CMFG",
  "lh_cuneus_thickness"="L.CU",
  "lh_entorhinal_thickness"="L.ENT",
  "lh_fusiform_thickness"="L.FG",
  "lh_inferiorparietal_thickness"="L.IPG",
  "lh_inferiortemporal_thickness"="L.ITG",
  "lh_isthmuscingulate_thickness"="L.ICG",
  "lh_lateraloccipital_thickness"="L.LOG",
  "lh_lateralorbitofrontal_thickness"="L.LOFG",
  "lh_lingual_thickness"="L.LG",
  "lh_medialorbitofrontal_thickness"="L.MOFG",
  "lh_middletemporal_thickness"="L.MTG",
  "lh_parahippocampal_thickness"="L.PHG",
  "lh_paracentral_thickness"="L.PaCG",
  "lh_parsopercularis_thickness"="L.POP",
  "lh_parsorbitalis_thickness"="L.PORB",
  "lh_parstriangularis_thickness"="L.PTRI",
  "lh_pericalcarine_thickness"="L.PCAL",
  "lh_postcentral_thickness"="L.PoCG",
  "lh_posteriorcingulate_thickness"="L.PCG",
  "lh_precentral_thickness"="L.PrCG",
  "lh_precuneus_thickness"="L.PCU",
  "lh_rostralanteriorcingulate_thickness"="L.RACG",
  "lh_rostralmiddlefrontal_thickness"="L.RMFG",
  "lh_superiorfrontal_thickness"="L.SFG",
  "lh_superiorparietal_thickness"="L.SPG",
  "lh_superiortemporal_thickness"="L.STG",
  "lh_supramarginal_thickness"="L.SMG",
  "lh_frontalpole_thickness"="L.FP",
  "lh_temporalpole_thickness"="L.TP",
  "lh_transversetemporal_thickness"="L.TTG",
  "lh_insula_thickness"="L.IN",
  
  "rh_bankssts_thickness"="R.BSTS",
  "rh_caudalanteriorcingulate_thickness"="R.CACG",
  "rh_caudalmiddlefrontal_thickness"="R.CMFG",
  "rh_cuneus_thickness"="R.CU",
  "rh_entorhinal_thickness"="R.ENT",
  "rh_fusiform_thickness"="R.FG",
  "rh_inferiorparietal_thickness"="R.IPG",
  "rh_inferiortemporal_thickness"="R.ITG",
  "rh_isthmuscingulate_thickness"="R.ICG",
  "rh_lateraloccipital_thickness"="R.LOG",
  "rh_lateralorbitofrontal_thickness"="R.LOFG",
  "rh_lingual_thickness"="R.LG",
  "rh_medialorbitofrontal_thickness"="R.MOFG",
  "rh_middletemporal_thickness"="R.MTG",
  "rh_parahippocampal_thickness"="R.PHG",
  "rh_paracentral_thickness"="R.PaCG",
  "rh_parsopercularis_thickness"="R.POP",
  "rh_parsorbitalis_thickness"="R.PORB",
  "rh_parstriangularis_thickness"="R.PTRI",
  "rh_pericalcarine_thickness"="R.PCAL",
  "rh_postcentral_thickness"="R.PoCG",
  "rh_posteriorcingulate_thickness"="R.PCG",
  "rh_precentral_thickness"="R.PrCG",
  "rh_precuneus_thickness"="R.PCU",
  "rh_rostralanteriorcingulate_thickness"="R.RACG",
  "rh_rostralmiddlefrontal_thickness"="R.RMFG",
  "rh_superiorfrontal_thickness"="R.SFG",
  "rh_superiorparietal_thickness"="R.SPG",
  "rh_superiortemporal_thickness"="R.STG",
  "rh_supramarginal_thickness"="R.SMG",
  "rh_frontalpole_thickness"="R.FP",
  "rh_temporalpole_thickness"="R.TP",
  "rh_transversetemporal_thickness"="R.TTG",
  "rh_insula_thickness"="R.IN")

# Rename columns
colnames(aparc_df) <- ifelse(
  colnames(aparc_df) %in% names(name_map),
  name_map[colnames(aparc_df)],
  colnames(aparc_df))

# Save
write.csv(
  aparc_df,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aparc_thickness_fs_default.csv",
  row.names = FALSE)


# Subcorticcal
# Read
aseg_df <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aseg_volumes_age_sex.csv",
  check.names = FALSE)


name_map_sub <- c(
  "Left-Thalamus" = "L.TH",
  "Left-Caudate" = "L.CA",
  "Left-Putamen" = "L.PU",
  "Left-Pallidum" = "L.PA",
  "Left-Hippocampus" = "L.HI",
  "Left-Amygdala" = "L.AM",
  "Left-Accumbens-area" = "L.ACC",
  
  "Right-Thalamus" = "R.TH",
  "Right-Caudate" = "R.CA",
  "Right-Putamen" = "R.PU",
  "Right-Pallidum" = "R.PA",
  "Right-Hippocampus" = "R.HI",
  "Right-Amygdala" = "R.AM",
  "Right-Accumbens-area" = "R.ACC")

# Metadata columns
meta_cols <- c("Subjects", "Timepoint")

# Keep only metadata + regions in connectome
aseg_df <- aseg_df %>%
  select(all_of(meta_cols), any_of(names(name_map_sub)))

# Rename
colnames(aseg_df) <- ifelse(
  colnames(aseg_df) %in% names(name_map_sub),
  name_map_sub[colnames(aseg_df)],
  colnames(aseg_df))

# Save
write.csv(
  aseg_df,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aseg_statistics_fsdefault.csv",
  row.names = FALSE)

# Now do the scaling
# Read
aparc <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aparc_thickness_fs_default.csv",
  check.names = FALSE)

# Min-max normalization
norm01 <- function(x){
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Columns to normalize
region_cols <- setdiff(
  names(aparc),
  c("Subjects", "Timepoint")
)

# Normalize
aparc[region_cols] <- lapply(
  aparc[region_cols],
  norm01
)

# Save
write.csv(
  aparc,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aparc_thickness_fs_default_scaled.csv",
  row.names = FALSE)

# Subcortical
# Read
aseg <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aseg_volumes_fs_default.csv",
  check.names = FALSE)

# Min-max normalization
norm01 <- function(x){
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

# Columns to normalize
region_cols <- setdiff(
  names(aseg),
  c("Subjects", "Timepoint", "Age", "Sex", "ICV")
)

# Make sure they are numeric
aseg[region_cols] <- lapply(aseg[region_cols], as.numeric)

# Min-max normalization
norm01 <- function(x){
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

aseg[region_cols] <- lapply(aseg[region_cols], norm01)

# Save
write.csv(
  aseg,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aseg_volumes_fs_default_scaled.csv",
  row.names = FALSE)

cat("Done! Saved as aseg_statistics_fsdefault_scaled.csv\n")



# Combine all in one csv file
# Read the normalized tables
aparc <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aparc_thickness_fs_default_scaled.csv",
  check.names = FALSE)

aseg <- read.csv(
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/aparc_aseg_volumes/DBS_longitudinal_aseg_volumes_fs_default_scaled.csv",
  check.names = FALSE)

# Keep only the subcortical regions
aseg_scaled <- aseg %>%
  select(
    Subjects,
    Timepoint,
    L.TH, L.CA, L.PU, L.PA, L.HI, L.AM, L.ACC,
    R.TH, R.CA, R.PU, R.PA, R.HI, R.AM, R.ACC, L.CER, R.CER)

# Merge
aparc_scaled <- left_join(
  aparc,
  aseg,
  by = c("Subjects", "Timepoint"))

# Save
write.csv(
  morphology,
  "/Users/rafaelp/Desktop/localR/Network_analyses/Volumetry/DBS_longitudinal_morphology_84regions_scaled.csv",
  row.names = FALSE)
