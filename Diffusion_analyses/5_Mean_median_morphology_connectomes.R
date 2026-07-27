##############################################################

# Generate mean and median values for each region per timepoint

##############################################################

library(dplyr)
mean_multimodal <- multimodal_long %>%
  group_by(Timepoint, Region) %>%
  summarise(
    Morphology = mean(Morphology, na.rm = T),
    Strength = mean(Strength, na.rm = T),
    I = mean(I, na.rm = T),
    .groups = "drop")

median_multimodal <- multimodal_long %>%
  group_by(Timepoint, Region) %>%
  summarise(
    Morphology = median(Morphology, na.rm = T),
    Strength = median(Strength, na.rm = T),
    I = median(I, na.rm = T),
    .groups = "drop")

# save both
write.csv(mean_multimodal, file = file.path(shapiro_dir, "mean_multimodal.csv"),
          row.names = T)

write.csv(median_multimodal, file = file.path(shapiro_dir, "median_multimodal.csv"),
          row.names = T)



######################################################

# Mean - median connectome for each region-region per timepoint

######################################################
base_dir <- "/Users/rafaelp/Desktop/localR/Network_analyses/Connectivity_networks/connectomes_to_analyse"

# mean
mean_connectome <- function(session){
  
  files <- list.files(
    path = base_dir,
    pattern = paste0("^labeled_.*_", session, "_connectome\\.csv$"),
    recursive = TRUE,
    full.names = TRUE)
  
  cat(length(files), "files found\n")
  
  mats <- lapply(files, function(f){
    
    as.matrix(
      read.csv(
        f,
        row.names = 1,
        check.names = FALSE))
  })
  
  mean_mat <- Reduce("+", mats) / length(mats)
  
  rownames(mean_mat) <- rownames(mats[[1]])
  colnames(mean_mat) <- colnames(mats[[1]])
  
  return(mean_mat)
}

mean_preop    <- mean_connectome("ses-preop")
mean_post01m  <- mean_connectome("ses-postop01m")
mean_post03m  <- mean_connectome("ses-postop03m")
mean_post06m  <- mean_connectome("ses-postop06m")
mean_post12m  <- mean_connectome("ses-postop12m")

write.csv(mean_preop, file = file.path(base_dir,"mean_connectome_preop.csv"))
write.csv(mean_post01m, file = file.path(base_dir, "mean_connectome_postop01m.csv"))
write.csv(mean_post03m, file = file.path(base_dir, "mean_connectome_postop03m.csv"))
write.csv(mean_post06m, file = file.path(base_dir, "mean_connectome_postop06m.csv"))
write.csv(mean_post12m, file = file.path(base_dir, "mean_connectome_postop12m.csv"))

# median
median_connectome <- function(session){
  
  files <- list.files(
    path = base_dir,
    pattern = paste0("^labeled_.*_", session, "_connectome\\.csv$"),
    recursive = TRUE,
    full.names = TRUE)
  
  cat(length(files), "files found\n")
  
  mats <- lapply(files, function(f){
    
    as.matrix(
      read.csv(
        f,
        row.names = 1,
        check.names = FALSE
      )
    )
    
  })
  
  median_mat <- apply(
    simplify2array(mats),
    c(1, 2),
    median,
    na.rm = TRUE)
  
  rownames(median_mat) <- rownames(mats[[1]])
  colnames(median_mat) <- colnames(mats[[1]])
  
  return(median_mat)}
  
median_preop    <- median_connectome("ses-preop")
median_post01m  <- median_connectome("ses-postop01m")
median_post03m  <- median_connectome("ses-postop03m")
median_post06m  <- median_connectome("ses-postop06m")
median_post12m  <- median_connectome("ses-postop12m")

write.csv(median_preop, file = file.path(base_dir,"median_connectome_preop.csv"))
write.csv(median_post01m, file = file.path(base_dir, "median_connectome_postop01m.csv"))
write.csv(median_post03m, file = file.path(base_dir, "median_connectome_postop03m.csv"))
write.csv(median_post06m, file = file.path(base_dir, "median_connectome_postop06m.csv"))
write.csv(median_post12m, file = file.path(base_dir, "median_connectome_postop12m.csv"))