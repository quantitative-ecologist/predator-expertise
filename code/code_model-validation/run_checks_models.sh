#!/bin/bash

# Point to root
cd "$(dirname "$0")/../.."

# List of models to process
models=("LM_PreySpeed" "LM_PreySpace" "GAMM_I" "GAMM_II" "GAMM_III" "GAMM_IV" "GAMM_V")

for mod in "${models[@]}"
do
  echo "Processing model: $mod"
  Rscript code/code_model-validation/checks_models.R "$mod"
done