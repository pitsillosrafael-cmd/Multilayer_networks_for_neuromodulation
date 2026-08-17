#!/bin/bash
export OMP_NUM_THREADS=4
# ============================================================
# SUBJECTS
# ============================================================

SUBJECTS=(sub-PD04 sub-PD05 sub-PD06 sub-PD07 sub-PD08)

# ============================================================
# SESSIONS
# ============================================================

SESSIONS=(Baseline 12m)

# ============================================================
# ROOT DIRECTORIES
# ============================================================

DWI_ROOT=/home/rafaelp/META-BRAIN/PPMI/nifti/DWI
OUT_ROOT=/home/rafaelp/META-BRAIN/PPMI/diffusion_analyses

# ============================================================
# LOOP SUBJECTS
# ============================================================

for SUB in "${SUBJECTS[@]}"; do

    # --------------------------------------------------------
    # READOUT TIME
    # --------------------------------------------------------

    case "$SUB" in

        sub-PD04)
            READOUT=0.0431799
            ;;

        sub-PD05)
            READOUT=0.0495301
            ;;

        sub-PD06)
            READOUT=0.0412758
            ;;

        sub-PD07)
            READOUT=0.0450851
            ;;

        sub-PD08)
            READOUT=0.0444509
            ;;

        *)
            echo "ERROR: No readout time defined for $SUB"
            continue
            ;;

    esac


    # ========================================================
    # LOOP SESSIONS
    # ========================================================

    for SES in "${SESSIONS[@]}"; do

        echo
        echo "================================================"
        echo "Processing $SUB | $SES"
        echo "Readout time: $READOUT"
        echo "L-R: i-"
        echo "R-L: i"
        echo "================================================"


        # ----------------------------------------------------
        # DIRECTORIES
        # ----------------------------------------------------

        LR_DIR="$DWI_ROOT/$SUB/$SES/L-R"
        RL_DIR="$DWI_ROOT/$SUB/$SES/R-L"

        OUT="$OUT_ROOT/$SUB/$SES"


        # ----------------------------------------------------
        # FIND L-R DWI
        # ----------------------------------------------------

        LR_DWI=$(find "$LR_DIR" \
            -maxdepth 1 \
            -name "*.nii.gz" \
            | head -n 1)

        if [ -z "$LR_DWI" ]; then
            echo "ERROR: No L-R DWI found for $SUB $SES"
            continue
        fi

        # ----------------------------------------------------
        # FIND R-L DWI
        # ----------------------------------------------------

        RL_DWI=$(find "$RL_DIR" \
            -maxdepth 1 \
            -name "*.nii.gz" \
            | head -n 1)

        if [ -z "$RL_DWI" ]; then
            echo "ERROR: No R-L DWI found for $SUB $SES"
            continue
        fi
        echo "L-R DWI: $LR_DWI"
        echo "R-L DWI: $RL_DWI"

        # ----------------------------------------------------
        # GRADIENT FILES
        # ----------------------------------------------------

        LR_PREFIX="${LR_DWI%.nii.gz}"
        RL_PREFIX="${RL_DWI%.nii.gz}"

        LR_BVEC="${LR_PREFIX}.bvec"
        LR_BVAL="${LR_PREFIX}.bval"

        RL_BVEC="${RL_PREFIX}.bvec"
        RL_BVAL="${RL_PREFIX}.bval"


        # ====================================================
        # CREATE FOLDERS
        # ====================================================

        mkdir -p \
    "$OUT/mif" \
    "$OUT/denoise" \
    "$OUT/degibbs" \
    "$OUT/preproc" \
    "$OUT/mask" \
    "$OUT/dti" \
    "$OUT/b0" \
    "$OUT/registration"

        # ====================================================
        # 1. CONVERT L-R
        # ====================================================

        echo "Converting L-R..."

        mrconvert \
            "$LR_DWI" \
            "$OUT/mif/dwi_LR.mif" \
            -fslgrad "$LR_BVEC" "$LR_BVAL"

        # ====================================================
        # 2. CONVERT R-L
        # ====================================================

        echo "Converting R-L..."

        mrconvert \
            "$RL_DWI" \
            "$OUT/mif/dwi_RL.mif" \
            -fslgrad "$RL_BVEC" "$RL_BVAL"

        # ====================================================
        # 3. DENOISE
        # ====================================================

        echo "Denoising L-R..."

        dwidenoise \
            "$OUT/mif/dwi_LR.mif" \
            "$OUT/denoise/dwi_LR_denoised.mif"

        echo "Denoising R-L..."

        dwidenoise \
            "$OUT/mif/dwi_RL.mif" \
            "$OUT/denoise/dwi_RL_denoised.mif"

        # ====================================================
        # 4. DEGIBBS
        # ====================================================

        echo "Gibbs correction L-R..."

        mrdegibbs \
            "$OUT/denoise/dwi_LR_denoised.mif" \
            "$OUT/degibbs/dwi_LR_degibbs.mif"

        echo "Gibbs correction R-L..."

        mrdegibbs \
            "$OUT/denoise/dwi_RL_denoised.mif" \
            "$OUT/degibbs/dwi_RL_degibbs.mif"


        # ====================================================
        # 5. CONCATENATE LR + RL
        # ====================================================

        echo "Concatenating L-R + R-L..."

        mrcat \
            "$OUT/degibbs/dwi_LR_degibbs.mif" \
            "$OUT/degibbs/dwi_RL_degibbs.mif" \
            "$OUT/preproc/dwi_all.mif" \
            -axis 3

        # ====================================================
        # CHECK CONCATENATED DATA
        # ====================================================

        echo "Checking concatenated DWI..."
        mrinfo \
            "$OUT/preproc/dwi_all.mif" \
            -size

        # ====================================================
        # 6. TOPUP + EDDY
        # ====================================================

        echo "Running TOPUP + EDDY..."

        dwifslpreproc \
            "$OUT/preproc/dwi_all.mif" \
            "$OUT/preproc/dwi_preproc.mif" \
            -rpe_all \
            -pe_dir i- \
            -readout_time "$READOUT"

        # ====================================================
        # 7. B0 EXTRACTION
        # ====================================================

        echo "Extracting mean b0..."

        dwiextract \
            "$OUT/preproc/dwi_preproc.mif" \
            -bzero - | \
        mrmath \
            - mean \
            "$OUT/b0/mean_b0.mif" \
            -axis 3

        mrconvert \
            "$OUT/b0/mean_b0.mif" \
            "$OUT/b0/mean_b0.nii.gz"


        # ====================================================
        # 8. MASK
        # ====================================================

        echo "Generating mask..."

        dwi2mask \
            "$OUT/preproc/dwi_preproc.mif" \
            "$OUT/mask/mask.mif"


        # ====================================================
        # 9. TENSOR
        # ====================================================

        echo "Fitting diffusion tensor..."

        dwi2tensor \
            "$OUT/preproc/dwi_preproc.mif" \
            "$OUT/dti/dti.mif" \
            -mask "$OUT/mask/mask.mif"


        # ====================================================
        # 10. METRICS
        # ====================================================

        echo "Generating diffusion metrics..."

        tensor2metric \
            "$OUT/dti/dti.mif" \
            -fa "$OUT/dti/fa.mif" \
            -ad "$OUT/dti/ad.mif" \
            -rd "$OUT/dti/rd.mif" \
            -adc "$OUT/dti/md.mif"

        mrconvert \
            "$OUT/dti/fa.mif" \
            "$OUT/dti/fa.nii.gz"

        # ====================================================
        # FINISHED
        # ====================================================

        echo
        echo "Completed $SUB | $SES"
        echo

    done

done

echo "================================================"
echo "ALL SUBJECTS AND SESSIONS COMPLETED"
echo "================================================"