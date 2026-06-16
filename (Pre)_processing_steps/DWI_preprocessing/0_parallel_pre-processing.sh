#!/bin/bash

# ========================
# SUBJECTS
# ========================

subjects=("sub-DBS11" "sub-DBS12" "sub-DBS13")

MAX_JOBS=3

SCRIPT_DIR=/home/rafaelp/META-BRAIN/bash_scripts
LOG_DIR=$SCRIPT_DIR/logs

mkdir -p $LOG_DIR

# ========================
# LAUNCH SUBJECTS
# ========================

for SUB in "${subjects[@]}"; do

  while (( $(jobs -r | wc -l) >= MAX_JOBS )); do
    sleep 5
  done

  echo "Launching $SUB..."

  bash $SCRIPT_DIR/1_DWI_pre-processing_loop.sh $SUB \
    > $LOG_DIR/${SUB}_preproc.log 2>&1 &

done

wait

echo "ALL SUBJECTS COMPLETED"