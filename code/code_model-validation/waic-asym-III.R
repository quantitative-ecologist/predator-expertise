# =======================================================================
#
#          Compute WAIC for asym-III model
#
# =======================================================================

# Minimal setup
library(brms)

# Paths
model_path <- file.path(getwd(), "asym-III.rds")
output_path <- file.path(
  getwd(),
  "waic_asym-III.rds"
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
