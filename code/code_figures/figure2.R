# ==========================================================================

#                                 Figure 2
#             Plot fixed effects on asymptote and rate parameters

# ==========================================================================




# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries and model -------------------------------------------------

library(brms)
library(data.table)
library(ggplot2)
library(ggridges)

path <- file.path(getwd(), "outputs", "outputs_models")
fit5 <- readRDS(file.path(path, "asym-VII.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws of predictions into a tidy table
# ==========================================================================

# Function
make_ce_df <- function(
  fit, nlpar, effects, labels,
  prob = 0.89,
  method = "fitted",
  robust = TRUE,
  re_formula = NULL
) {
  if (length(effects) != length(labels)) {
    stop("effects and labels must have the same length.")
  }

  out_list <- vector("list", length(effects))

  for (i in seq_along(effects)) {
    ce <- conditional_effects(
      x = fit,
      effects = effects[i],
      nlpar = nlpar,
      prob = prob,
      robust = robust,
      re_formula = re_formula,
      method = method
    )[[1]]

    # variable label
    ce$variable <- labels[i]

    # Columns for plotting
    ce <- ce[, c("variable", "effect1__", "estimate__", "lower__", "upper__")]

    out_list[[i]] <- ce
  }

  # bind into one data frame
  do.call(rbind, out_list)
}

# Select traits ------------------------------------------------------------

effects_prey  <- c("Zprey_speed", "Zprey_space")
labels_prey <- c("prey_speed", "prey_space")



# Prepare tables -----------------------------------------------------------

df_a <- make_ce_df(
  fit = fit5,
  nlpar = "a",
  effects = effects_prey,
  labels  = labels_prey
)

df_c <- make_ce_df(
  fit = fit5,
  nlpar = "c",
  effects = effects_prey,
  labels = labels_prey
)

df_a$parameter <- "Maximum hunting success (a)"
df_c$parameter <- "Rate of gain in expertise (c)"
df <- rbind(df_a, df_c)

# ==========================================================================
# ==========================================================================




# ==========================================================================
# 3. Extract posterior draws of individual means
# ==========================================================================

mf <- model.frame(fit5)
dt <- as.data.table(mf)

pred_covars <- dt[, .(
  #Zprey_avg_rank = mean(Zprey_avg_rank),
  Zprey_speed = mean(Zprey_speed),
  Zprey_space = mean(Zprey_space)
), by = predator_id]

draws <- as_draws_df(fit5)

make_pred_values <- function(fit, draws, param, prob = 0.89) {
  alpha <- (1 - prob) / 2
  lower_p <- alpha
  upper_p <- 1 - alpha

  # Names of fixed-effect columns for this parameter
  b_int   <- paste0("b_", param, "_Intercept")
  #b_rank  <- paste0("b_", param, "_Zprey_avg_rank")
  b_speed <- paste0("b_", param, "_Zprey_speed")
  b_space <- paste0("b_", param, "_Zprey_space")

  n_pred <- nrow(pred_covars)
  est <- numeric(n_pred)
  lower <- numeric(n_pred)
  upper <- numeric(n_pred)

  for (i in seq_len(n_pred)) {
    pid <- pred_covars$predator_id[i]

    # Random intercept column for this predator and param
    re_col <- grep(
      pattern = paste0("^r_predator_id__", param, "\\[", pid, ",Intercept\\]$"),
      x = names(draws),
      value = TRUE
    )

    lin <- draws[[b_int]] +
      #draws[[b_rank]] * pred_covars$Zprey_avg_rank[i] +
      draws[[b_speed]] * pred_covars$Zprey_speed[i] +
      draws[[b_space]] * pred_covars$Zprey_space[i] +
      draws[[re_col]]

    est[i] <- stats::quantile(lin, 0.5)
    lower[i] <- stats::quantile(lin, lower_p)
    upper[i] <- stats::quantile(lin, upper_p)
  }

  data.frame(
    predator_id = pred_covars$predator_id,
    estimate__ = est,
    lower__ = lower,
    upper__ = upper,
    #Zprey_avg_rank = pred_covars$Zprey_avg_rank,
    Zprey_speed = pred_covars$Zprey_speed,
    Zprey_space = pred_covars$Zprey_space,
    stringsAsFactors = FALSE
  )
}

# Predator-specific a_j and c_j
pred_a_vals <- make_pred_values(fit5, draws, "a")
pred_c_vals <- make_pred_values(fit5, draws, "c")

df_a_speed <- data.frame(
  predator_id = pred_a_vals$predator_id,
  variable = "prey_speed",
  effect1__ = pred_a_vals$Zprey_speed,
  estimate__ = pred_a_vals$estimate__,
  lower__ = pred_a_vals$lower__,
  upper__ = pred_a_vals$upper__,
  parameter = "Maximum hunting success (a)",
  stringsAsFactors = FALSE
)

df_a_space <- data.frame(
  predator_id = pred_a_vals$predator_id,
  variable = "prey_space",
  effect1__ = pred_a_vals$Zprey_space,
  estimate__ = pred_a_vals$estimate__,
  lower__ = pred_a_vals$lower__,
  upper__ = pred_a_vals$upper__,
  parameter = "Maximum hunting success (a)",
  stringsAsFactors = FALSE
)

df_c_speed <- data.frame(
  predator_id = pred_c_vals$predator_id,
  variable = "prey_speed",
  effect1__ = pred_c_vals$Zprey_speed,
  estimate__ = pred_c_vals$estimate__,
  lower__ = pred_c_vals$lower__,
  upper__ = pred_c_vals$upper__,
  parameter = "Rate of gain in expertise (c)",
  stringsAsFactors = FALSE
)

df_c_space <- data.frame(
  predator_id = pred_c_vals$predator_id,
  variable = "prey_space",
  effect1__ = pred_c_vals$Zprey_space,
  estimate__ = pred_c_vals$estimate__,
  lower__ = pred_c_vals$lower__,
  upper__ = pred_c_vals$upper__,
  parameter = "Rate of gain in expertise (c)",
  stringsAsFactors = FALSE
)

df_pred <- rbind(df_a_speed, df_a_space, df_c_speed, df_c_space)

# ==========================================================================
# ==========================================================================




# ==========================================================================
# 4. Build the figure of regression on individual means
# ==========================================================================

# Prepare figure -----------------------------------------------------------

panel_names <- c(
#  prey_rank = "Average prey rank",
  prey_speed = "Average prey speed",
  prey_space = "Average prey space coverage"
)

fixef <- ggplot() +
  geom_blank(
    data = subset(df, parameter == "Maximum hunting success (a)"),
    aes(x = 0, y = -4.5)
  ) +
  geom_blank(
    data = subset(df, parameter == "Maximum hunting success (a)"),
    aes(x = 0, y = 3)
  ) +
  geom_blank(
    data = subset(df, parameter == "Rate of gain in expertise (c)"),
    aes(x = 0, y = -1.5)
  ) +
  geom_blank(
    data = subset(df, parameter == "Rate of gain in expertise (c)"),
    aes(x = 0, y = 1.5)
  ) +
  geom_line(
    data = df,
    aes(x = effect1__, y = estimate__),
    linewidth = 1.1
  ) +
  geom_ribbon(
    data = df,
    aes(x = effect1__, ymin = lower__, ymax = upper__),
    alpha = 0.2
  ) +
  geom_pointrange(
    data = df_pred,
    aes(x = effect1__, y = estimate__, ymin = lower__, ymax = upper__),
    size  = 0.2,
    alpha = 0.2,
    position = position_jitter(width = 0.03, height = 0)
  ) +
  scale_x_continuous(
    breaks = seq(-2, 2, 0.5),
    limits = c(-1.5, 1.5)
  ) +
  facet_grid(
    parameter ~ variable,
    scales = "free_y",
    labeller = labeller(variable = panel_names)
  ) +
  labs(,
    x = "Predictor (scaled)",
    y = "Parameter value"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  )
fixef


path_to_save <- file.path(getwd(), "outputs", "outputs_figures")
ggsave(
  plot = fixef,
  filename = file.path(path_to_save, "figure2.png"),
  dpi = 300,
  height = 6,
  width = 8
)

# ==========================================================================
# ==========================================================================