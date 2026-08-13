library(parallel)

##############################################
# Freesurfer's longitudinal analysis (3 steps)
##############################################

subjects_dir <- "/home/rafaelp/META-BRAIN/PPMI/freesurfer"
base_dir <- "/home/rafaelp/META-BRAIN/PPMI/nifti/T1WI"

# Log directories
log_dir_1 <- file.path(subjects_dir, "logs")
log_dir_2 <- file.path(subjects_dir, "logs_base")
log_dir_3 <- file.path(subjects_dir, "logs_long")

dir.create(subjects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir_1, showWarnings = FALSE)
dir.create(log_dir_2, showWarnings = FALSE)
dir.create(log_dir_3, showWarnings = FALSE)

Sys.setenv(SUBJECTS_DIR = subjects_dir)

# Find every T1 image
files <- list.files(
  base_dir,
  pattern = "_T1w\\.nii(\\.gz)?$",
  recursive = TRUE,
  full.names = TRUE)

###########################
# First recon-all-long step
###########################
mclapply(files, function(image){

  filename <- basename(image)

  # Remove extension
  subject_name <- sub("\\.nii(\\.gz)?$", "", filename)

  output_dir <- file.path(subjects_dir, subject_name)

  logfile <- file.path(
    log_dir,
    paste0(subject_name, ".log"))

  if(dir.exists(output_dir)){
    message("Skipping ", subject_name)
    return(NULL)}

  command <- paste(
    "recon-all",
    "-i", shQuote(image),
    "-s", shQuote(subject_name),
    "-all",
    "-openmp 4",
    ">", shQuote(logfile),
    "2>&1")

  message("Running ", subject_name)

  system(command)

}, mc.cores = 10)

############################
# Second recon-all-long step
############################

# Create FreeSurfer longitudinal base templates
# PPMI controls
# Directories

# Log directory
dir.create(log_dir, showWarnings = FALSE)

# Subjects
subjects <- sprintf("sub-PD%02d", 1:14)

# Create base templates

mclapply(subjects, function(sub){

  baseline <- paste0(sub, "_Baseline_T1w")
  month12  <- paste0(sub, "_12m_T1w")
  base     <- paste0(sub, "_base")

  logfile <- file.path(
    log_dir,
    paste0(base, ".log"))

  # Skip if already exists
  if(dir.exists(file.path(subjects_dir, base))){
    message("Skipping ", base)
    return(NULL)}

  command <- paste(

    "recon-all",
    "-base", base,
    "-tp", baseline,
    "-tp", month12,
    "-all",
    "-openmp 4",
    ">", shQuote(logfile),
    "2>&1")

  message("Running ", base)

  system(command)

}, mc.cores = 10)

cat("\nFinished creating all base templates.\n")

############################
# Third recon-all-long step
############################
sessions <- c("Baseline", "12m")

jobs <- expand.grid(
  Subject = subjects,
  Session = sessions,
  stringsAsFactors = FALSE)

mclapply(seq_len(nrow(jobs)), function(i){
  
  sub <- jobs$Subject[i]
  ses <- jobs$Session[i]
  
  long_name <- paste0(sub, "_", ses, "_T1w.long.", sub, "_base")
  
  output_dir <- file.path(
    subjects_dir,
    long_name)
  
  logfile <- file.path(
    log_dir,
    paste0(long_name, ".log"))
  
  # Skip completed subjects
  if(dir.exists(output_dir)){
    message("Skipping ", long_name)
    return(NULL)}
  
  command <- paste(
    
    "recon-all",
    "-long",
    paste0(sub, "_", ses, "_T1w"),
    paste0(sub, "_base"),
    "-all",
    "-openmp 4",
    ">", shQuote(logfile),
    "2>&1")
  
  message("Running ", long_name)
  
  system(command)
  }, mc.cores = 10)

cat("\nFinished longitudinal reconstructions.\n")
