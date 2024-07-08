# ==========================================================================

#                Code to produce Appendix 1 : Figure S1                    #

# ==========================================================================





# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries and models ------------------------------------------------

library(brms)
library(data.table)
library(ggplot2)
library(ggpubr)

path <- file.path(getwd(), "outputs", "01_outputs_models")

fit1 <- readRDS(file.path(path, "A2_GAMM.rds"))
#fit1 <- readRDS(file.path(path, "A2_GAMM-speed-rank.rds"))



# Load the data ------------------------------------------------------------

data <- fread("./data/FraserFrancoetal2023-data.csv",
              select = c("predator_id",
                         "game_duration",
                         "pred_speed",
                         "prey_avg_speed",
                         "cumul_xp_pred",
                         "total_xp_pred",
                         "hunting_success"))

data[, predator_id := as.factor(predator_id)]



# Post processing preparations for custom family ---------------------------

expose_functions(fit1, vectorize = TRUE)

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



# Setup a custom theme for the plot ----------------------------------------

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

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Prepare data for the figure
# ==========================================================================

fig1 <- conditional_effects(
  fit1, method = "fitted", effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
)

# Extract values in a table
tab1 <- data.table(fig1[[1]])


# Back transform x-axis values
sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_pred))
standev <- sd(data$cumul_xp_pred)
scaled_breaks <- sequence / standev



# Cut fitted values based on player XP ------------------------------------

# Extract player IDs with their total XP from the original data
xp <- unique(data[, .(predator_id, total_xp_pred)])

# Compute non standardized cumulative XP
tab1[, cumul_xp := (Zcumul_xp * standev) + mean(data$cumul_xp_pred)]

# Now merge the two tables adding the total XP
tab1 <- merge(tab1, xp, by = "predator_id")

# Cut all matches where fitted values are above total XP
tab1 <- tab1[cumul_xp <= total_xp_pred, ]



# Extract players with greatest increase and decrease ---------------------

# Calculate 1st and final value
tab1[
  , ":=" (
    first = min(Zcumul_xp),
    last = max(Zcumul_xp)
  )
  , by = predator_id
]

tab1[, equal1 := ifelse(Zcumul_xp == first, 1, 0), by = predator_id]
tab1[, equal2 := ifelse(Zcumul_xp == last, 1, 0), by = predator_id]


# First and last value into a second table
tab2 <- tab1[equal1 == 1 | equal2 == 1, .(predator_id, Zcumul_xp, estimate__)]
tab2[, range := rep(c("min", "max"), 253)]

tab2 <- dcast(
  tab2,
  predator_id ~ range,
  value.var = "estimate__"
)


# Calculate the difference in predicted success between minimum and maximum xp
# - value means decrease in success
# + value means increase in success
tab2[, difference := plogis(tab2$max) - plogis(tab2$min)]


# Remerge the table with the original
table <- merge(
  tab1[, .(predator_id, Zcumul_xp, estimate__)],
  tab2[, .(predator_id, difference)],
  by = "predator_id"
)


# Extract players with the greatest increase and decrease in success
quantile(table$difference)

# increase, decrease, and stable
table[, diff_dir := ifelse(difference > 0.10, "increase", NA)]
table[difference < -0.10, diff_dir := "decrease"]
table[difference %between% c(-0.10, 0.10), diff_dir := "stable"]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Produce the GAMM plots
# ==========================================================================

# Plot for players that had an increase in success -------------------------

length(unique(table[diff_dir == "increase", predator_id]))
# 23 players had an increase in hunting success

gamm_plot1 <- ggplot(table[difference > 0.05, ],
                     aes(x = Zcumul_xp,
                         y = estimate__ / 4,
                         color = predator_id)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme



# Plot for players that had an decrease in success -------------------------

length(unique(table[diff_dir == "decrease", predator_id]))
# 3 players had a decrease in hunting success

gamm_plot2 <- ggplot(table[difference < -0.05, ],
                     aes(x = Zcumul_xp,
                         y = estimate__ / 4,
                         color = predator_id)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme



# Plot for players that kept a stable success ------------------------------

gamm_plot3 <- ggplot(table[difference %between% c(-0.05, 0.05), ],
                     aes(x = Zcumul_xp,
                         y = estimate__ / 4,
                         color = predator_id)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme


# ==========================================================================
# ==========================================================================





# =======================================================================
# 4. Combine plots into a figure
# =======================================================================


# Prepare figure --------------------------------------------------------

# Arrange paneled figure
figure <- ggarrange(
   NULL, gamm_plot1, NULL, gamm_plot2, NULL, gamm_plot3,
   ncol = 6, nrow = 1,
   labels = c("(A)", "", "(B)", "", "(C)"),
   widths = c(0.15, 1.5, 0.15, 1.5, 0.15, 1.5)
)

# Export the figure -----------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

ggexport(
  figure,
  filename = file.path(path, "appendix1_figureS1.png"),
  width = 4500,
  height = 1200,
  res = 300
)

# =======================================================================
# =======================================================================
