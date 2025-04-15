# =======================================================================

#                 GAMM leave-one-out cross-validation                   #

# =======================================================================





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================



# Librairies ------------------------------------------------------------

# Detect cores
options(mc.cores = parallel::detectCores())

library(data.table)
library(brms)
library(parallel)



# Import the model ------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_models")

# Load models
mod1 <- readRDS(file.path(path, "GAMM-I.rds"))
mod2 <- readRDS(file.path(path, "GAMM-II.rds"))
mod3 <- readRDS(file.path(path, "GAMM-III.rds"))
mod4 <- readRDS(file.path(path, "GAMM-IV.rds"))
mod5 <- readRDS(file.path(path, "GAMM-V.rds"))

# Check object size
#print(object.size(mod1), units = "MB")
#print(object.size(mod2), units = "MB")
#print(object.size(mod3), units = "MB")
#print(object.size(mod4), units = "MB")
#print(object.size(mod5), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Model comparison - LOO-PSIS
# =======================================================================


# Compute LOO-PSIS ------------------------------------------------------

# LOO-PSIS
loo1 <- loo(mod1)
rm(mod1)
loo2 <- loo(mod2)
rm(mod2)
loo3 <- loo(mod3)
rm(mod3)
loo4 <- loo(mod4)
rm(mod4)
loo5 <- loo(mod5)
rm(mod5)



# Compare models --------------------------------------------------------

# Compare models
loo_tab <- loo_compare(loo1, loo2, loo3, loo4, loo5)

# Compute table with complete information
loo_table <- data.table(
 print(loo_tab, simplify = FALSE),
 keep.rownames = TRUE
)
setnames(loo_table, "rn", "model")


# Save outputs ----------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_model-validation")

# Save loo
saveRDS(loo1, file = file.path(path, "GAMM-I_loo.rds"))
saveRDS(loo2, file = file.path(path, "GAMM-II_loo.rds"))
saveRDS(loo3, file = file.path(path, "GAMM-III_loo.rds"))
saveRDS(loo4, file = file.path(path, "GAMM-IV_loo.rds"))
saveRDS(loo5, file = file.path(path, "GAMM-V_loo.rds"))

# Save table
saveRDS(loo_table, file = file.path(path, "GAMM_loo-cv-table.rds"))

# =======================================================================
# =======================================================================