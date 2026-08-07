###############################################################################
# Convert PPMI DWI DICOMs to NIfTI
###############################################################################

INPUT_ROOT="/mnt/c/Users/rafaelp/Desktop/PPMI_dcms"
OUTPUT_ROOT="$HOME/PPMI/DWI/nifti"

for i in $(seq -w 1 14)
do

    SUBJECT="sub-PD${i}"

    echo "======================================"
    echo "Processing ${SUBJECT}"
    echo "======================================"

    for SESSION in Baseline 12m
    do

        for PE in L-R R-L
        do

            INPUT_DIR="${INPUT_ROOT}/${SUBJECT}/DWI/${SESSION}/${PE}"

            # Find the Ixxxxxx DICOM folder
            DICOM_DIR=$(find "$INPUT_DIR" -maxdepth 1 -type d -name "I*" | head -n 1)

            if [ -z "$DICOM_DIR" ]; then
                echo "No DICOM folder found:"
                echo "$INPUT_DIR"
                continue
            fi

            OUTPUT_DIR="${OUTPUT_ROOT}/${SUBJECT}/${SESSION}/${PE}"

            mkdir -p "$OUTPUT_DIR"

            # Remove hyphen for cleaner filenames
            PE_NAME=$(echo "$PE" | tr -d '-')

            echo "Converting:"
            echo "$DICOM_DIR"

            dcm2niix \
                -z y \
                -b y \
                -m y \
                -f "${SUBJECT}_${SESSION}_${PE_NAME}_dwi" \
                -o "$OUTPUT_DIR" \
                "$DICOM_DIR"

            echo ""

        done

    done

done

echo "======================================"
echo "Conversion finished."
echo "======================================"
