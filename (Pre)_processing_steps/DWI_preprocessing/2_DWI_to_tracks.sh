#!/bin/bash

# SUBJECT

SUB=sub-DBS14

# Sessions
sessions=("ses-preop" "ses-postop01m" "ses-postop03m" "ses-postop06m" "ses-postop12m")
BASE=/home/rafaelp/META-BRAIN/open-DBS/diffusion_analyses

# Create a LOOP to get all scans from a single subject

for SES in "${sessions[@]}"; do

  echo "========================="
  echo "Processing $SUB $SES"
  echo "========================="

  OUT=$BASE/$SUB/$SES
  SES_CLEAN=${SES#ses-}
  FS_SUB=${SUB}_${SES_CLEAN}.long.${SUB//-}_base

  # ========================
  # CHECK INPUT EXISTS
  # ========================

  if [ ! -f "$OUT/preproc/dwi_preproc.mif" ]; then
    echo "Skipping $SES (no preprocessed DWI)"
    continue
  fi

  # CREATE TRACT FOLDER
  mkdir -p $OUT/tracts
  mkdir -p $OUT/5tt

  # ========================
  # 12. RESPONSE + FOD (continue from where left in file1)
  # ========================

  echo "Running response + FOD..."

  dwi2response dhollander \
  $OUT/preproc/dwi_preproc.mif \
  $OUT/tracts/response_wm.txt \
  $OUT/tracts/response_gm.txt \
  $OUT/tracts/response_csf.txt

  echo "Running MSMT-CSD..."

  dwi2fod msmt_csd \
  $OUT/preproc/dwi_preproc.mif \
  $OUT/tracts/response_wm.txt $OUT/tracts/wm_fod.mif \
  $OUT/tracts/response_gm.txt $OUT/tracts/gm.mif \
  $OUT/tracts/response_csf.txt $OUT/tracts/csf.mif

  # ========================
  # 13. GENERATE 5TT
  # ========================

  echo "Generating 5TT image..."

  5ttgen freesurfer \
  $SUBJECTS_DIR/$FS_SUB/mri/aparc+aseg.mgz \
  $OUT/5tt/5tt_T1.mif \
  -force

  # ========================
  # 14. CONVERT BRAIN.MGZ → NII
  # ========================
  
  echo "Converting brain.mgz to NIfTI..."
  
  mri_convert \
  $SUBJECTS_DIR/$FS_SUB/mri/brain.mgz \
  $OUT/registration/brain.nii.gz
  
  # ========================
  # 15. T1 → DWI TRANSFORM
  # ========================
  
  echo "Registering T1 to DWI..."
  
  flirt \
  -in $OUT/registration/brain.nii.gz \
  -ref $OUT/b0/mean_b0.nii.gz \
  -omat $OUT/registration/T12DWI.mat
  
  # ========================
  # 16. FSL → MRTRIX MATRIX
  # ========================
  
  echo "Converting transform..."
  
  transformconvert \
  $OUT/registration/T12DWI.mat \
  $OUT/registration/brain.nii.gz \
  $OUT/b0/mean_b0.nii.gz \
  flirt_import \
  $OUT/registration/T12DWI.txt

  # ========================
  # 17. TRANSFORM 5TT → DWI
  # ========================

  echo "Transforming 5TT to DWI space..."

  mrtransform \
  $OUT/5tt/5tt_T1.mif \
  -linear $OUT/registration/T12DWI.txt \
  -template $OUT/preproc/dwi_preproc.mif \
  $OUT/5tt/5tt_DWI.mif \
  -force

 # ========================
  # 18. ACT TRACTOGRAPHY
  # ========================

  echo "Running ACT tractography..."

  tckgen \
  $OUT/tracts/wm_fod.mif \
  $OUT/tracts/tracks_ACT.tck \
  -algorithm iFOD2 \
  -act $OUT/5tt/5tt_DWI.mif \
  -backtrack \
  -crop_at_gmwmi \
  -seed_dynamic $OUT/tracts/wm_fod.mif \
  -select 10M \
  -cutoff 0.06
  - nthreads 12

  # ========================
  # 19. SIFT2
  # ========================

  echo "Running SIFT2..."

  tcksift2 \
  $OUT/tracts/tracks_ACT.tck \
  $OUT/tracts/wm_fod.mif \
  $OUT/tracts/sift2_weights.txt

  echo "Completed $SUB $SES"

done

echo "ALL SESSIONS COMPLETED"