# =============================================================================

#                      Inspect prior and posterior draws
#                               from GAMM models

# =============================================================================





# =============================================================================
# Prepare session
# =============================================================================


# Load libraries --------------------------------------------------------------

 library(brms)
 library(ggplot2)



# Prepare model draws ---------------------------------------------------------

 path <- file.path(getwd(), "outputs", "outputs_models")

 # Import model in R session
 mod1 <- readRDS(file.path(path, "GAMM-I.rds"))
 mod2 <- readRDS(file.path(path, "GAMM-II.rds"))
 mod3 <- readRDS(file.path(path, "GAMM-III.rds"))
 mod4 <- readRDS(file.path(path, "GAMM-IV.rds"))
 mod5 <- readRDS(file.path(path, "GAMM-V.rds"))


 # Extract posterior draws
 posterior_fit1 <- as_draws_df(mod1)
 posterior_fit2 <- as_draws_df(mod2)
 posterior_fit3 <- as_draws_df(mod3)
 posterior_fit4 <- as_draws_df(mod4)
 posterior_fit5 <- as_draws_df(mod5)

# =============================================================================
# =============================================================================





# =============================================================================
# 1. Model 1
# =============================================================================



# Plot prior and posterior draws ----------------------------------------------

 # Intercept
 ggplot(posterior_fit1) +
   geom_density(aes(prior_Intercept),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(Intercept),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()


 # Game duration
 ggplot(posterior_fit1) +
   geom_density(aes(prior_b_Zgame_duration),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(b_Zgame_duration),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()

 # slope term of cumul xp
 ggplot(posterior_fit1) +
   geom_density(aes(prior_bs_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(bs_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()


 # Standard deviation of cumul XP smooth term
 ggplot(posterior_fit1) +
   geom_density(aes(prior_sds_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()

 # Standard deviation of predator ID
 ggplot(posterior_fit1) +
   geom_density(aes(prior_sds_spredator_id_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_spredator_id_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()

 # Phi parameter
 ggplot(posterior_fit1) +
   geom_density(aes(prior_phi),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(phi),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) + 
   theme_classic()

# =============================================================================
# =============================================================================





# =============================================================================
# 2. Model 2
# =============================================================================



# Plot prior and posterior draws ----------------------------------------------

 # Intercept
 ggplot(posterior_fit2) +
   geom_density(aes(prior_Intercept),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(Intercept),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()


 # Game duration
 ggplot(posterior_fit2) +
   geom_density(aes(prior_b_Zgame_duration),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(b_Zgame_duration),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()


 # slope term of cumul xp
 ggplot(posterior_fit2) +
   geom_density(aes(prior_bs_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(bs_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()


 # Standard deviation of cumul XP smooth term
 ggplot(posterior_fit2) +
   geom_density(aes(prior_sds_sZcumul_xp_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_sZcumul_xp_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()

# =============================================================================
# =============================================================================





# =============================================================================
# 3. Model 3
# =============================================================================



# Plot prior and posterior draws ----------------------------------------------

 # Intercept
 ggplot(posterior_fit3) +
   geom_density(aes(prior_Intercept),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(Intercept),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()


 # Game duration
 ggplot(posterior_fit3) +
   geom_density(aes(prior_b_Zgame_duration),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(b_Zgame_duration),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()

 # Standard deviation of predator ID
 ggplot(posterior_fit3) +
   geom_density(aes(prior_sds_sZcumul_xppredator_id_1),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(sds_sZcumul_xppredator_id_1),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()

 # Phi parameter
 ggplot(posterior_fit3) +
   geom_density(aes(prior_phi),
                fill = "steelblue",
                color = "black",
                alpha = 0.6) +
   geom_density(aes(phi),
                fill = "#FC4E07",
                color = "black",
                alpha = 0.6) +
   theme_classic()

# =============================================================================
# =============================================================================