#!/bin/bash

# ========================
# SUBJECT
# ========================

SUB=sub-DBS14

# ========================
# ALL SESSIONS
# ========================

sessions=(
  "ses-preop"
  "ses-postop01m"
  "ses-postop03m"
  "ses-postop06m"
  "ses-postop12m"
)


FS_DIR=/home/rafaelp/META-BRAIN/open-DBS/freesurfer_longitudinal_analyses
export SUBJECTS_DIR=$FS_DIR

# ========================
# LOOP THROUGH SESSIONS
# ========================

for SES in "${sessions[@]}"; do

  echo "========================="
  echo "Processing $SUB $SES"
  echo "========================="

  RAW=/home/rafaelp/META-BRAIN/open-DBS/ds005849_DBS/$SUB/$SES/dwi
  OUT=/home/rafaelp/META-BRAIN/open-DBS/diffusion_analyses/$SUB/$SES

  SES_CLEAN=${SES#ses-}
  FS_SUB=${SUB}_${SES_CLEAN}.long.${SUB//-}_base

  # ========================
  # FIND DWI FILE (ROBUST)
  # ========================

  DWI_FILE=$(ls $RAW/${SUB}_${SES}_run-01_dwi.nii.gz 2>/dev/null)

  if [ -z "$DWI_FILE" ]; then
    DWI_FILE=$(ls $RAW/${SUB}_${SES}*dwi.nii.gz 2>/dev/null | head -n 1)
  fi

  if [ -z "$DWI_FILE" ]; then
    echo "Skipping $SES (no DWI found)"
    continue
  fi

  echo "Using DWI: $DWI_FILE"

  PREFIX=${DWI_FILE%.nii.gz}
  BVEC=${PREFIX}.bvec
  BVAL=${PREFIX}.bval

  # ========================
  # CREATE FOLDERS
  # ========================

  mkdir -p $OUT/{mif,denoise,degibbs,preproc,mask,dti,b0,registration}

  # ========================
  # 1. CONVERT TO MIF
  # ========================

  mrconvert \
  $DWI_FILE \
  $OUT/mif/dwi.mif \
  -fslgrad $BVEC \
  $BVAL

  # ========================
  # 2. DENOISE
  # ========================

  dwidenoise \
  $OUT/mif/dwi.mif \
  $OUT/denoise/dwi_denoised.mif

  # ========================
  # 3. DEGIBBS
  # ========================

  mrdegibbs \
  $OUT/denoise/dwi_denoised.mif \
  $OUT/degibbs/dwi_degibbs.mif

  # ========================
  # 4. PREPROCESS
  # ========================

  dwifslpreproc \
  $OUT/degibbs/dwi_degibbs.mif \
  $OUT/preproc/dwi_preproc.mif \
  -rpe_none \
  -pe_dir AP

  # ========================
  # 5. B0 EXTRACTION
  # ========================

  dwiextract $OUT/preproc/dwi_preproc.mif -bzero - | \
  mrmath - mean $OUT/b0/mean_b0.mif -axis 3

  mrconvert $OUT/b0/mean_b0.mif $OUT/b0/mean_b0.nii.gz

  # ========================
  # 6. MASK
  # ========================

  dwi2mask \
  $OUT/preproc/dwi_preproc.mif \
  $OUT/mask/mask.mif

  # ========================
  # 7. TENSOR
  # ========================

  dwi2tensor \
  $OUT/preproc/dwi_preproc.mif \
  $OUT/dti/dti.mif \
  -mask $OUT/mask/mask.mif

  # ========================
  # 8. METRICS
  # ========================

  tensor2metric \
  $OUT/dti/dti.mif \
  -fa $OUT/dti/fa.mif \
  -ad $OUT/dti/ad.mif \
  -rd $OUT/dti/rd.mif \
  -adc $OUT/dti/md.mif

  mrconvert $OUT/dti/fa.mif $OUT/dti/fa.nii.gz

  # ========================
  # 9. REGISTRATION
  # ========================

   #bbregister \
   #--s $FS_SUB \
   #--mov $OUT/b0/mean_b0.nii.gz \
   #--reg $OUT/registration/dwi2t1.dat \
   #--dti \
   #--init-fsl

  # ========================
  # 10. B0 → T1 (QC)
  # ========================

   #mri_vol2vol \
   #--mov $OUT/b0/mean_b0.nii.gz \
   #--targ $SUBJECTS_DIR/$FS_SUB/mri/brain.mgz \
   #--reg $OUT/registration/dwi2t1.dat \
   #--o $OUT/registration/b0_in_T1.nii.gz \
   #--interp trilinear

  # ========================
  # 11. FA → T1
  # ========================

   #mri_vol2vol \
   #--mov $OUT/dti/fa.nii.gz \
   #--targ $SUBJECTS_DIR/$FS_SUB/mri/brain.mgz \
   #--reg $OUT/registration/dwi2t1.dat \
   #--o $OUT/registration/fa_in_T1.nii.gz \
   #--interp trilinear

   #mrconvert \
   #$OUT/registration/fa_in_T1.nii.gz \
   #$OUT/registration/fa_in_T1.mif

  echo "Completed $SUB $SES"

done

echo "ALL SESSIONS COMPLETED"