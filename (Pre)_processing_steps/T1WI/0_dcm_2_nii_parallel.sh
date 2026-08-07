#!/bin/bash

###############################################################################
# Convert PPMI T1 DICOMs to NIfTI
#
# Input:
# ~/PPMI/T1WI/dcm/sub-PDXX/{Baseline,12m}/I*******/
#
# Output:
# ~/PPMI/T1WI/nifti/sub-PDXX/{Baseline,12m}/
###############################################################################

# Input and output directories
INPUT_ROOT="$HOME/PPMI/T1WI/dcm"
OUTPUT_ROOT="$HOME/PPMI/T1WI/nifti"

# Loop through subjects
for i in $(seq -w 1 14); do

    SUBJECT="sub-PD${i}"

    echo "========================================"
    echo "Processing ${SUBJECT}"
    echo "========================================"

    for SESSION in Baseline 12m; do

        INPUT_DIR="${INPUT_ROOT}/${SUBJECT}/${SESSION}"

        # Find the DICOM folder (Ixxxxxxx)
        DICOM_DIR=$(find "$INPUT_DIR" -maxdepth 1 -type d -name "I*" | head -n 1)

        if [ -z "$DICOM_DIR" ]; then
            echo "No DICOM folder found for ${SUBJECT} (${SESSION})"
            continue
        fi

        OUTPUT_DIR="${OUTPUT_ROOT}/${SUBJECT}/${SESSION}"
        mkdir -p "$OUTPUT_DIR"

        echo "Converting:"
        echo "  ${DICOM_DIR}"
        echo "  --> ${OUTPUT_DIR}"

        dcm2niix \
            -z y \
            -b y \
            -m y \
            -f "${SUBJECT}_${SESSION}_T1w" \
            -o "$OUTPUT_DIR" \
            "$DICOM_DIR"

        echo ""
    done

done

echo "========================================"
echo "All conversions completed successfully."
echo "========================================"
