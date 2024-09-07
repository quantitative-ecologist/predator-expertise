# =======================================================================

#                       GAMM model diagnostics                          #

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
library(ggpubr)



# Import the model ------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_models")

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
# 3. Basic model diagnostics
# =======================================================================


# Trace plots and parameter distributions -------------------------------

plot(mod1)
plot(mod2)
plot(mod3)
plot(mod4)
plot(mod5)



# Posterior predictive checks -------------------------------------------

# Check distributions
pp1 <- pp_check(mod1)
pp1 <- pp1 + ggtitle("GAMM - A1 rank") +
 theme(legend.position = "bottom")

pp2 <- pp_check(mod2)
pp2 <- pp2 + ggtitle("GAMM - A2 rank") +
 theme(legend.position = "bottom")

pp3 <- pp_check(mod3)
pp3 <- pp3 + ggtitle("GAMM - A3 rank") +
 theme(legend.position = "bottom")

pp4 <- pp_check(mod4)
pp4 <- pp4 + ggtitle("GAMM - A2 rank + speed") +
 theme(legend.position = "bottom")

pp5 <- pp_check(mod5)
pp5 <- pp5 + ggtitle("GAMM - A3 rank + speed") +
 theme(legend.position = "bottom")


# Effects plot (mean variance plot)
stat1 <- pp_check(mod1, type = "stat_2d")
stat1 <- stat1 + ggtitle("GAMM - A1 rank") +
 theme(legend.position = "bottom")

stat2 <- pp_check(mod2, type = "stat_2d")
stat2 <- stat2 + ggtitle("GAMM - A2 rank") +
 theme(legend.position = "bottom")

stat3 <- pp_check(mod3, type = "stat_2d")
stat3 <- stat3 + ggtitle("GAMM - A3 rank") +
 theme(legend.position = "bottom")

stat4 <- pp_check(mod4, type = "stat_2d")
stat4 <- stat4 + ggtitle("GAMM - A2 rank + speed") +
 theme(legend.position = "bottom")

stat5 <- pp_check(mod5, type = "stat_2d")
stat5 <- stat5 + ggtitle("GAMM - A3 rank + speed") +
 theme(legend.position = "bottom")


# Predicted means
mean1 <- pp_check(mod1, type = "stat", stat = "mean")
mean1 <- mean1 + ggtitle("GAMM - A1") +
 theme(legend.position = "bottom")

mean2 <- pp_check(mod2, type = "stat", stat = "mean")
mean2 <- mean2 + ggtitle("GAMM - A2") +
 theme(legend.position = "bottom")

mean3 <- pp_check(mod3, type = "stat", stat = "mean")
mean3 <- mean3 + ggtitle("GAMM - A3") +
 theme(legend.position = "bottom")

mean4 <- pp_check(mod4, type = "stat", stat = "mean")
mean4 <- mean4 + ggtitle("GAMM - A2 prey") +
 theme(legend.position = "bottom")

mean5 <- pp_check(mod5, type = "stat", stat = "mean")
mean5 <- mean5 + ggtitle("GAMM - A3 prey") +
 theme(legend.position = "bottom")


# Error scatter
e_scat1 <- pp_check(mod1, type = "error_scatter_avg")
e_scat1 <- e_scat1 + ggtitle("GAMM - A1 rank") +
 theme(legend.position = "bottom")

e_scat2 <- pp_check(mod2, type = "error_scatter_avg")
e_scat2 <- e_scat2 + ggtitle("GAMM - A2 rank") +
 theme(legend.position = "bottom")

e_scat3 <- pp_check(mod3, type = "error_scatter_avg")
e_scat3 <- e_scat3 + ggtitle("GAMM - A3 rank") +
 theme(legend.position = "bottom")

e_scat4 <- pp_check(mod4, type = "error_scatter_avg")
e_scat4 <- e_scat4 + ggtitle("GAMM - A2 rank + speed") +
 theme(legend.position = "bottom")

e_scat5 <- pp_check(mod5, type = "error_scatter_avg")
e_scat5 <- e_scat5 + ggtitle("GAMM - A3 rank + speed") +
 theme(legend.position = "bottom")



# Save plots as figure --------------------------------------------------

plots <- list(
 # Arrange a figure
 stat_fig1 <- ggarrange(
  pp1, stat1, mean1, e_scat1,
  ncol = 2, nrow = 2
 ),

 stat_fig2 <- ggarrange(
  pp2, stat2, mean2, e_scat2,
  ncol = 2, nrow = 2
 ),

 stat_fig3 <- ggarrange(
  pp3, stat3, mean3, e_scat3,
  ncol = 2, nrow = 2
 ),

 stat_fig4 <- ggarrange(
  pp4, stat4, mean4, e_scat4,
  ncol = 2, nrow = 2
 ),

 stat_fig5 <- ggarrange(
  pp5, stat5, mean5, e_scat5,
  ncol = 2, nrow = 2
 )
)

# Export the figures into a .pdf file
#ggexport(
#  plots,
#  filename = file.path(path, "GAMM-diagnostics.pdf")
#)
# =======================================================================
# =======================================================================