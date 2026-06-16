BASE="/home/rafaelp/META-BRAIN/open-DBS"
OUT="$BASE/freesurfer_longitudinal_analyses"

mkdir -p "$OUT"

for i in $(seq -w 1 14); do
    SUB="sub-DBS${i}"

    SRC="$BASE/$SUB/anat_output/long"

    if [ -d "$SRC" ]; then
        echo "Processing $SUB ..."

        for folder in "$SRC"/*; do
            if [ -d "$folder" ]; then
                name=$(basename "$folder")

                DEST="$OUT/${name}"

                echo "Moving $folder → $DEST"
                mv "$folder" "$DEST"
            fi
        done
    else
        echo "Skipping $SUB (no long folder found)"
    fi
done

echo "Done."

echo "hello"