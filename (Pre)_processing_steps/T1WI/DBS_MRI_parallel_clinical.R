library(tidyverse)
library(parallel)

# base directory
library(tidyverse) library(parallel) 
base_dir <- "/home/rafaelp/META-BRAIN/open-DBS/rawdata" 
files <- list.files( base_dir, pattern = "_T1w\\.nii(\\.gz)?$", recursive = TRUE, full.names = TRUE ) 

subjects <- c(
  "sub-DBS05",
  "sub-DBS06",
  "sub-DBS07"
)

sessions <- c(
  "ses-preop",
  "ses-postop01m",
  "ses-postop03m",
  "ses-postop06m",
  "ses-postop12m"
)


# Step 1: Create longitudinal base templates
mclapply(subjects, function(sub){
  
  tp_string <- paste(
    paste0(
      "-tp ",
      sub,
      "_",
      sessions
    ),
    collapse = " "
  )
  
  command <- paste(
    "recon-all",
    "-base",
    paste0(sub, "_base"),
    tp_string,
    "-all"
  )
  
  print(command)
  system(command)
  
}, mc.cores = 3)



# Step 2: Run longitudinal processing for each timepoint
jobs <- expand.grid(
  Subject = subjects,
  Session = sessions,
  stringsAsFactors = FALSE
)

mclapply(seq_len(nrow(jobs)), function(i){
  
  sub <- jobs$Subject[i]
  ses <- jobs$Session[i]
  
  command <- paste(
    "recon-all",
    "-long",
    paste0(sub, "_", ses),
    paste0(sub, "_base"),
    "-all"
  )
  
  print(command)
  system(command)
  
}, mc.cores = 10)


# Step 3: Longitudinal processing
jobs <- expand.grid(
  Subject = subjects,
  Session = sessions,
  stringsAsFactors = FALSE
)

mclapply(seq_len(nrow(jobs)), function(i){
  
  sub <- jobs$Subject[i]
  ses <- jobs$Session[i]
  
  command <- paste(
    "recon-all",
    "-long",
    paste0(sub, "_", ses),
    paste0(sub, "_base"),
    "-all"
  )
  
  print(command)
  system(command)
  
}, mc.cores = 10)
