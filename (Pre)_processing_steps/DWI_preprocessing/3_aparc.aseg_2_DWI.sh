#!/bin/bash

# ========================
# SUBJECTS / SESSIONS
# ========================

sessions=("12m" "Baseline")

BASE=/mnt/shared_data/rafaelp/META-BRAIN/PPMI/diffusion_analyses

FS_DIR=/mnt/shared_data/rafaelp/META-BRAIN/PPMI/freesurfer_longitudinal_analysis
export SUBJECTS_DIR="$FS_DIR"


# ========================
# LOOP
# ========================

for SUB_PATH in "$BASE"/sub-PD*; do

  SUB=$(basename "$SUB_PATH")

  # Only PD04-PD14
  if [[ "$SUB" == "sub-PD01" || "$SUB" == "sub-PD02" || "$SUB" == "sub-PD03" ]]; then
    echo "Skipping $SUB"
    continue
  fi

  for SES in "${sessions[@]}"; do

    echo "========================="
    echo "Processing $SUB $SES"
    echo "========================="

    OUT="$BASE/$SUB/$SES"

    # ========================
    # FreeSurfer longitudinal subject
    # ========================

    FS_SUB="${SUB}_${SES}_T1w.long.${SUB}_base"

    echo "FreeSurfer subject: $FS_SUB"

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

  echo "Completed all sessions for $SUB"

done

echo "=========================================="
echo "ALL SUBJECTS AND SESSIONS COMPLETED"
echo "=========================================="