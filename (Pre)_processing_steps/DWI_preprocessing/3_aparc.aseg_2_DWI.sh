#!/bin/bash

# ========================
# SUBJECT
# ========================

SUB=sub-DBS14

sessions=("ses-preop" "ses-postop01m" "ses-postop03m" "ses-postop06m" "ses-postop12m")

FS_DIR=/home/rafaelp/META-BRAIN/open-DBS/freesurfer_longitudinal_analyses
export SUBJECTS_DIR=$FS_DIR

BASE=/home/rafaelp/META-BRAIN/open-DBS/diffusion_analyses

# ========================
# LOOP
# ========================

for SES in "${sessions[@]}"; do

  echo "========================="
  echo "Processing $SUB $SES"
  echo "========================="

  OUT=$BASE/$SUB/$SES

  SES_CLEAN=${SES#ses-}
  FS_SUB=${SUB}_${SES_CLEAN}.long.${SUB//-}_base

  # ========================
  # CHECK FILES
  # ========================

  if [ ! -f "$OUT/b0/mean_b0.nii.gz" ]; then
    echo "Skipping $SES (missing b0)"
    continue
  fi


  if [ ! -f "$SUBJECTS_DIR/$FS_SUB/mri/aparc+aseg.mgz" ]; then
    echo "Skipping $SES (missing DSK atlas)"
    continue
  fi
  
  # ========================
  # 20. PARCELLATION → DWI
  # ========================
  
  echo "Applying atlas to DWI space..."
  
  mrtransform \
  $SUBJECTS_DIR/$FS_SUB/mri/aparc+aseg.mgz \
  -linear $OUT/registration/T12DWI.txt \
  -template $OUT/preproc/dwi_preproc.mif \
  -interp nearest \
  $OUT/registration/aparc+aseg_in_DWI.mif \
  -force

  # ========================
  # 21. LABEL FIX
  # ========================

  echo "Running labelconvert..."

  labelconvert \
  $OUT/registration/aparc+aseg_in_DWI.mif \
  $FREESURFER_HOME/FreeSurferColorLUT.txt \
  /home/rafaelp/miniconda3/bin/fs_default.txt \
  $OUT/registration/nodes.mif

  # ========================
  # 22. CONNECTOME
  # ========================

  mkdir -p $OUT/connectome

  echo "Generating structural connectome..."

  tck2connectome \
  $OUT/tracts/tracks_ACT.tck \
  $OUT/registration/nodes.mif \
  $OUT/connectome/${SUB}_${SES}_connectome.csv \
  -tck_weights_in $OUT/tracts/sift2_weights.txt \
  -assignment_radial_search 6 \
  -symmetric \
  -zero_diagonal \
  -scale_invnodevol \
  -force
  
  # ========================
  # 23. FA-WEIGHTED CONNECTOME
  # ========================

  #echo "Sampling FA along streamlines..."

  #tcksample \
  #$OUT/tracts/tracks_ACT.tck \
  #$OUT/dti/fa.mif \
  #$OUT/tracts/fa_per_streamline.csv \
  #-stat_tck mean \
  #-force

  #echo "Generating FA-weighted connectome..."

  #tck2connectome \
  #$OUT/tracts/tracks_ACT.tck \
  #$OUT/registration/nodes.mif \
  #$OUT/connectome/connectome_FA.csv \
  #-scale_file $OUT/tracts/fa_per_streamline.csv \
  #-stat_edge mean \
  #-tck_weights_in $OUT/tracts/sift2_weights.txt \
  #-assignment_radial_search 2 \
  #-force

  echo "Completed $SUB $SES"

  done

  echo "ALL SESSIONS COMPLETED"