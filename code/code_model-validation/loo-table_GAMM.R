# =======================================================================

#                   Compute loo table for GAMM models

# =======================================================================

# Import libraries 
library(data.table)
library(brms)

# Define path and file names
output_path <- file.path(getwd(), "outputs", "outputs_model-validation")
model_names <- c("GAMM-I", "GAMM-II", "GAMM-III", "GAMM-IV", "GAMM-V", "GAMM-VI")

# Import loo-cv model output files
loo_results <- lapply(
    model_names,
    function(m) readRDS(file.path(output_path, paste0("loo_", m, ".rds")))
)
names(loo_results) <- model_names

# Compute loo table
loo_tab <- loo_compare(loo_results)

loo_table <- data.table(
  print(loo_tab, simplify = FALSE),
  keep.rownames = TRUE
)

setnames(loo_table, "rn", "model")

# Save output
saveRDS(loo_table, file = file.path(output_path, "loo-cv-table_GAMM.rds"))
