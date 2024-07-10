# ==========================================================================

#                               Figure S1

# ==========================================================================

# Figure of the relationship between prey speed and predator XP



# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries -----------------------------------------------------------

library(data.table)
library(brms)
library(ggplot2)
library(viridis)



# Import model -------------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_models")
fit <- readRDS(file.path(path, "A2_GAMM-speed-rank.rds"))



# Load the data ------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2023-data.csv",
  select = c("predator_id",
             "game_duration",
             "pred_speed",
             "prey_avg_speed",
             "prey_avg_rank",
             "cumul_xp_pred",
             "total_xp_pred",
             "hunting_success")
)

data[, predator_id := as.factor(predator_id)]
# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Compute the predictions
# ==========================================================================

# Post processing preparations for custom family ---------------------------

expose_functions(fit, vectorize = TRUE)

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
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}



# Predictions --------------------------------------------------------------

preds <- conditional_effects(
  x = fit,
  method = "fitted",
  effects = "Zprey_speed",
  prob = 0.89,
  robust = TRUE,
  re_formula = NULL,
  conditions = data.frame(predator_id = NA)
)
preds_t <- data.table(preds[[1]])



# Transform values --------------------------------------------------------

# Back transform x-axis values
range_speed <- seq(
  min(data$prey_avg_speed, na.rm = TRUE),
  max(data$prey_avg_speed, na.rm = TRUE),
  length.out = 5
)
sequence <- range_speed - mean(data$prey_avg_speed, na.rm = TRUE)
standev <- sd(data$prey_avg_speed, na.rm = TRUE)
scaled_breaks <- sequence / standev

# Function to apply transformation
# Computes non standardized cumulative XP
func <- function(x) {
  x[, prey_avg_speed := (Zprey_speed * standev) + mean(data$prey_avg_speed, na.rm = TRUE)]
}

# Apply function
func(preds_t)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Plot the posterior distributions
# ==========================================================================

# Prepare figure -----------------------------------------------------------

custom_theme <- theme(
  # axis values size
  axis.text = element_text(face = "plain",
                           size = 14,
                           color = "black"),
  # axis ticks lenght
  axis.ticks.length = unit(.15, "cm"),
  # axis ticks width
  axis.ticks = element_line(linewidth = 0.90,
                            color = "black"),
  # axis titles size
  axis.title = element_text(size = 16,
                            face = "plain",
                            color = "black"),
  axis.line = element_line(linewidth = 0.95,
                           color = "black"),
  legend.position = "none",
  panel.grid = element_blank(),
  panel.background = element_blank()
)

p <- ggplot(
  preds_t,
  aes(x = Zprey_speed,
      y = estimate__ / 4)
) +
  geom_ribbon(
    aes(
      ymin = lower__ / 4,
      ymax = upper__ / 4
    ),
    fill = "gray"
  ) +
  geom_line(linewidth = 1) +
  ylab("Hunting success\n") +
  ggtitle("Prey rank + prey speed") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = scaled_breaks,
    labels = round(range_speed, digits = 2)
  ) +
  xlab("\nPrey speed (m/s)") +
  custom_theme



# Export the figure --------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  p,
  filename = file.path(path, "figureS1.png"),
  units = "px"
)

# ==========================================================================
# ==========================================================================