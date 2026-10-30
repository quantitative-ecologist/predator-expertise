# =======================================================================
#
#          Compute WAIC for an asym model
#
# =======================================================================

# Usage, from the project root:
#   Rscript code/code_model-validation/waic-compute.R asym-baseline

# Minimal setup
library(brms)

# Model name from the command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript waic-compute.R <model>, e.g. asym-baseline")
}
mod <- args[1]

# Paths
model_path <- file.path(
  getwd(), "outputs", "outputs_models", paste0(mod, ".rds")
)
output_path <- file.path(
  getwd(), "outputs", "outputs_model-validation", paste0("waic_", mod, ".rds")
)

# Check if WAIC already exists
if (file.exists(output_path)) {
  message("WAIC result already exists at: ", output_path)
  quit(save = "no")
}

# Check if model exists
if (!file.exists(model_path)) {
  stop("Model file not found: ", model_path)
}

# Load model
message("Loading model from: ", model_path)
model <- readRDS(model_path)

# Compute WAIC
message("Computing WAIC...")
waic_result <- waic(model, pointwise=TRUE)

# Save immediately
message("Saving WAIC result to: ", output_path)
saveRDS(waic_result, file = output_path)

# Print result
print(waic_result)

# Clean up
rm(model, waic_result)
gc()

message("WAIC computation complete")
