# =======================================================================

#                 Approximate LOO cross-validation

# =======================================================================


# =======================================================================
# 1. Prepare script
# =======================================================================

options(mc.cores = parallel::detectCores())
library(data.table)
library(brms)
library(parallel)
library(renv)

# Load renv project environment
renv::load()

# Get model name from command line
args <- commandArgs(trailingOnly = TRUE)
mod <- args[1]

# Define paths
model_path <- file.path(getwd(), "outputs", "outputs_models")
output_path <- file.path(getwd(), "outputs", "outputs_model-validation")
if (!dir.exists(model_path)) dir.create(model_path, recursive = TRUE)
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Define expected model files
model_files <- list(
  "GAMM-I"   = "GAMM-I.rds",
  "GAMM-II"  = "GAMM-II.rds",
  "GAMM-III" = "GAMM-III.rds",
  "GAMM-IV"  = "GAMM-IV.rds",
  "GAMM-V"  = "GAMM-V.rds"
)

# Check model name
if (!mod %in% names(model_files)) {
  stop("Unknown model: ", mod)
}

file_name <- model_files[[mod]]
file_path <- file.path(model_path, file_name)
loo_path <- file.path(output_path, paste0("loo_", mod, ".rds"))

# =======================================================================
# =======================================================================




# =======================================================================
# 2. Run checks
# =======================================================================

# Check if file exists
if (!file.exists(file_path)) {
  message(sprintf(
    "Model file not found: %s\nPlease download it from OSF and place it in: %s",
    file_name, model_path
  ))
  quit(save = "no", status = 1)
}

# Skip if already done
if (file.exists(loo_path)) {
  message("Skipping ", mod, " — LOO result file already exists in ", loo_path)
  quit(save = "no")
}

# Run LOO
model <- readRDS(file_path)
loo_result <- loo(model)

# Save result
saveRDS(loo_result, file = loo_path)
message("Saved LOO result for ", mod, " to: ", loo_path)

rm(model, loo_result)
gc()

# =======================================================================
# =======================================================================