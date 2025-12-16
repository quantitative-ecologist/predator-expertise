#!/bin/bash

# Point to root
cd "$(dirname "$0")/../.."

# List of models to process
models=("LM_PreySpeed" "LM_PreySpace" "asym_I" "asym_II" "asym_III" "asym_IV")

for mod in "${models[@]}"
do
  echo "Processing model: $mod"
  Rscript code/code_model-validation/checks_models.R "$mod"
done