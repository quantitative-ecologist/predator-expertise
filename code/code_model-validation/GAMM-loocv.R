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

 path <- file.path(getwd(), "outputs", "01_outputs_models")

 # Load models
 mod1 <- readRDS(file.path(path, "A1_GAMM-rank.rds"))
 mod2 <- readRDS(file.path(path, "A2_GAMM-rank.rds"))
 mod3 <- readRDS(file.path(path, "A3_GAMM-rank.rds"))
 mod4 <- readRDS(file.path(path, "A2_GAMM-speed-rank.rds"))
 mod5 <- readRDS(file.path(path, "A3_GAMM-speed-rank.rds"))

 # Check object size
 #print(object.size(mod1), units = "MB")
 #print(object.size(mod2), units = "MB")
 #print(object.size(mod3), units = "MB")
 #print(object.size(mod4), units = "MB")
 #print(object.size(mod5), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare functions for post-processing (beta-binom custom family)
# =======================================================================

 # Expose functions
 expose_functions(mod1, vectorize = TRUE)

 # Define the log likelihood function
 log_lik_beta_binomial2 <- function(i, prep) {
   mu <- brms::get_dpar(prep, "mu", i = i)
   phi <- brms::get_dpar(prep, "phi", i = i)
   trials <- prep$data$vint1[i]
   y <- prep$data$Y[i]
   beta_binomial2_lpmf(y, mu, phi, trials)
 }

 # Define function for posterior_predict
 posterior_predict_beta_binomial2 <- function(i, prep, ...) {
   mu <- brms::get_dpar(prep, "mu", i = i)
   phi <- brms::get_dpar(prep, "phi", i = i)
   trials <- prep$data$vint1[i]
   beta_binomial2_rng(mu, phi, trials)
 }

 # Define function for posterior_epred
 posterior_epred_beta_binomial2 <- function(prep) {
   mu <- brms::get_dpar(prep, "mu")
   trials <- prep$data$vint1
   trials <- matrix(trials,
                    nrow = nrow(mu),
                    ncol = ncol(mu),
                    byrow = TRUE)
   mu * trials
 }

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Model comparison - LOO-PSIS
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

 path <- file.path(getwd(), "outputs", "02_outputs_model-validation")

 # Save loo
 saveRDS(loo1, file = file.path(path, "A1_rank-loo.rds"))
 saveRDS(loo2, file = file.path(path, "A2_rank-loo.rds"))
 saveRDS(loo3, file = file.path(path, "A3_rank-loo.rds"))
 saveRDS(loo4, file = file.path(path, "A2_speed-rank-loo.rds"))
 saveRDS(loo5, file = file.path(path, "A3_speed-rank-loo.rds"))

 # Save table
 saveRDS(loo_table, file = file.path(path, "GAMM_loo-cv-table.rds"))

# =======================================================================
# =======================================================================