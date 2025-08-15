#!/bin/bash

# Point to root
cd "$(dirname "$0")/../.."

# List of models to process
models=("GAMM-I" "GAMM-II" "GAMM-III" "GAMM-IV" "GAMM-V" "GAMM-VI")

for mod in "${models[@]}"
do
  echo "Running LOO for model: $mod"
  Rscript code/code_model-validation/loo-cv_GAMM.R "$mod"
done
