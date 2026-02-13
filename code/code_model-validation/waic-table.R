# =======================================================================
#
#                   Compute WAIC table for asym models
#
# =======================================================================

# Import libraries
library(data.table)
library(brms)

# Define path and file names
output_path <- file.path(getwd(), "outputs", "outputs_model-validation")
model_names <- c("asym-I", "asym-II", "asym-III")

# Import WAIC model output files
waic_results <- lapply(
  model_names,
  function(m) readRDS(file.path(output_path, paste0("waic_", m, ".rds")))
)
names(waic_results) <- model_names

# Compute WAIC comparison table
waic_tab <- loo_compare(waic_results)

waic_table <- data.table(
  print(waic_tab, simplify = FALSE),
  keep.rownames = TRUE
)

setnames(waic_table, "rn", "model")

# Save output
saveRDS(waic_table, file = file.path(output_path, "waic-table.rds"))
message("WAIC comparison table saved to: ", file.path(output_path, "waic-table.rds"))
fwrite(waic_table, file = file.path(output_path, "waic-table.csv"))
message("WAIC comparison table (CSV) saved to: ", file.path(output_path, "waic-table.csv"))