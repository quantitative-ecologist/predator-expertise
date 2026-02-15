# ==========================================================================

#               Figure of the relationship between success
#                     and prey speed, space, and rank
#                               Figure S2

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries -----------------------------------------------------------

library(data.table)
library(brms)
library(ggplot2)
library(ggpubr)
library(viridis)



# Import model -------------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_models")
fit <- readRDS(file.path(path, "asym-III.rds"))



# Load the data ------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2025-data.csv",
  select = c("predator_id",
             "game_duration",
             "pred_speed",
             "prey_avg_speed",
             "prey_avg_space_rate",
             "prey_avg_rank",
             "cumul_xp_pred",
             "total_xp_pred",
             "hunting_success")
)

data[, predator_id := as.factor(predator_id)]

# Remove any NAs
data <- data[complete.cases(data)]

# Space rate within 0.2
data <- data[prey_avg_space_rate <= 0.2]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Compute the predictions
# ==========================================================================


# Extract posterior draws ---------------------------------------------------

draws <- as_draws_df(fit)

a_int     <- draws$b_a_Intercept
a_speed   <- draws$b_a_Zprey_speed
a_space   <- draws$b_a_Zprey_space
b_int     <- draws$b_b_Intercept
c_int     <- draws$b_c_Intercept
c_speed   <- draws$b_c_Zprey_speed
c_space   <- draws$b_c_Zprey_space
beta_rank <- draws$b_betarank_Intercept

# Reference cumulative experience (median)
cumul_xp_ref <- median(data$cumul_xp_pred, na.rm = TRUE)



# Standardization constants ------------------------------------------------

speed_mean <- mean(data$prey_avg_speed, na.rm = TRUE)
speed_sd   <- sd(data$prey_avg_speed, na.rm = TRUE)
space_mean <- mean(data$prey_avg_space_rate, na.rm = TRUE)
space_sd   <- sd(data$prey_avg_space_rate, na.rm = TRUE)
rank_mean  <- mean(data$prey_avg_rank, na.rm = TRUE)
rank_sd    <- sd(data$prey_avg_rank, na.rm = TRUE)



# Predictions for speed ----------------------------------------------------

n_points <- 100
z_speed_seq <- seq(
  (min(data$prey_avg_speed, na.rm = TRUE) - speed_mean) / speed_sd,
  (max(data$prey_avg_speed, na.rm = TRUE) - speed_mean) / speed_sd,
  length.out = n_points
)

preds_speed <- rbindlist(lapply(z_speed_seq, function(zs) {
  a_val <- a_int + a_speed * zs
  b_val <- b_int
  c_val <- c_int + c_speed * zs
  logit_mu <- a_val - (a_val - b_val) * exp(-exp(c_val) * cumul_xp_ref)
  mu <- plogis(logit_mu)
  data.table(
    prey_avg_speed = zs * speed_sd + speed_mean,
    estimate = median(mu),
    lower = quantile(mu, probs = 0.055),
    upper = quantile(mu, probs = 0.945)
  )
}))



# Predictions for space ----------------------------------------------------

z_space_seq <- seq(
  (min(data$prey_avg_space_rate, na.rm = TRUE) - space_mean) / space_sd,
  (max(data$prey_avg_space_rate, na.rm = TRUE) - space_mean) / space_sd,
  length.out = n_points
)

preds_space <- rbindlist(lapply(z_space_seq, function(zsp) {
  a_val <- a_int + a_space * zsp
  b_val <- b_int
  c_val <- c_int + c_space * zsp
  logit_mu <- a_val - (a_val - b_val) * exp(-exp(c_val) * cumul_xp_ref)
  mu <- plogis(logit_mu)
  data.table(
    prey_avg_space_rate = zsp * space_sd + space_mean,
    estimate = median(mu),
    lower = quantile(mu, probs = 0.055),
    upper = quantile(mu, probs = 0.945)
  )
}))



# Predictions for rank -----------------------------------------------------

z_rank_seq <- seq(
  (min(data$prey_avg_rank, na.rm = TRUE) - rank_mean) / rank_sd,
  (max(data$prey_avg_rank, na.rm = TRUE) - rank_mean) / rank_sd,
  length.out = n_points
)

preds_rank <- rbindlist(lapply(z_rank_seq, function(zr) {
  a_val <- a_int
  b_val <- b_int
  c_val <- c_int
  logit_mu <- a_val - (a_val - b_val) * exp(-exp(c_val) * cumul_xp_ref) + beta_rank * zr
  mu <- plogis(logit_mu)
  data.table(
    prey_avg_rank = zr * rank_sd + rank_mean,
    estimate = median(mu),
    lower = quantile(mu, probs = 0.055),
    upper = quantile(mu, probs = 0.945)
  )
}))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Plot the posterior distributions
# ==========================================================================


# Figure theme -------------------------------------------------------------

custom_theme <- theme(
  # Title
  plot.title = element_text(size = 12),
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



# Compute plots ------------------------------------------------------------

p1 <- ggplot(
  preds_speed,
  aes(x = prey_avg_speed, y = estimate)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(linewidth = 1) +
  ylab("Hunting success\n") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 4, 1),
    limits = c(0, 4)
  ) +
  xlab("\nPrey speed (m/s)") +
  custom_theme


p2 <- ggplot(
  preds_space,
  aes(x = prey_avg_space_rate, y = estimate)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(linewidth = 1) +
  ylab("Hunting success\n") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.16, 0.04),
    limits = c(0, 0.17)
  ) +
  xlab("\nPrey space covered (tile/s)") +
  custom_theme

p3 <- ggplot(
  preds_rank,
  aes(x = prey_avg_rank, y = estimate)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(linewidth = 1) +
  ylab("Hunting success\n") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 20, 5),
    limits = c(0, 21)
  ) +
  xlab("\nPrey rank") +
  custom_theme


# Prepare figure ------------------------------------------------------------

figure <- ggarrange(
  NULL, p1, NULL, p2, NULL, p3,
  ncol = 6, nrow = 1,
  labels = c("(A)", "", "(B)", "", "(C)", ""),
  widths = c(0.15, 1.5, 0.15, 1.5, 0.15, 1.5)
)



# Export the figure -----------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  figure,
  filename = file.path(path, "figureS2.png"),
  units = "in",
  dpi = 300,
  width = 15,
  height = 4
)

# ==========================================================================
# ==========================================================================