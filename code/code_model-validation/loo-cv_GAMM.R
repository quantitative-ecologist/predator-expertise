# ==========================================================================

#          Perform approximate leave-one-out cross-validation

# ==========================================================================

# Load libraries
options(mc.cores = parallel::detectCores())
library(data.table)
library(brms)
library(parallel)

# Define paths
model_path <- file.path(getwd(), "outputs", "outputs_models")
output_path <- file.path(getwd(), "outputs", "outputs_model-validation")
if (!dir.exists(model_path)) dir.create(model_path, recursive = TRUE)
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Define model files and OSF base URL
model_files <- list(
  "GAMM-I"   = "GAMM-I.rds",
  "GAMM-II"  = "GAMM-II.rds",
  "GAMM-III" = "GAMM-III.rds",
  "GAMM-IV"  = "GAMM-IV.rds",
  "GAMM-V"   = "GAMM-V.rds"
)

osf_base <- "https://osf.io/hdv38/download"

# Download any missing model files
for (file_name in model_files) {
  local_path <- file.path(model_path, file_name)
  if (!file.exists(local_path)) {
    message(sprintf("Downloading %s from OSF...", file_name))
    download.file(url = osf_base, destfile = local_path, mode = "wb")
  }
}

# Load models
models <- lapply(model_files, function(f) readRDS(file.path(model_path, f)))

# Run LOO in parallel
loo_results <- mclapply(models, function(mod) loo(mod), mc.cores = length(models))

# Save LOO objects
mapply(function(name, loo_obj) {
  saveRDS(loo_obj, file = file.path(output_path, paste0(name, "_loo.rds")))
}, names(model_files), loo_results)

# Compare models
loo_tab <- do.call(loo_compare, loo_results)

# Format results
loo_table <- data.table(print(loo_tab, simplify = FALSE), keep.rownames = TRUE)
setnames(loo_table, "rn", "model")

# Save LOO comparison table
saveRDS(loo_table, file = file.path(output_path, "GAMM_loo-cv-table.rds"))