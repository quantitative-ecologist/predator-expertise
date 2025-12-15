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
library(ggpubr)

path <- file.path(getwd(), "outputs", "outputs_models")
fit4 <- readRDS(file.path(path, "asym-IV.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws of predictions into a tidy table
# ==========================================================================

mf <- model.frame(fit4)
dt <- as.data.table(mf)

pred_covars <- dt[, .(
  Zprey_speed = mean(Zprey_speed),
  Zprey_space = mean(Zprey_space)
), by = predator_id]

# Function
make_ce_df <- function(
  fit, nlpar, effects, labels,
  prob = 0.89,
  method = "fitted",
  robust = TRUE,
  re_formula = NULL
) {

  out_list <- vector("list", length(effects))

  for (i in seq_along(effects)) {

    ce <- conditional_effects(
      x = fit,
      effects = effects[i],
      nlpar = nlpar,
      prob = prob,
      robust = robust,
      re_formula = re_formula,
      method = method,
      resolution = 100
    )[[1]]

    # variable label
    ce$variable <- labels[i]
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
  fit = fit4,
  nlpar = "a",
  effects = effects_prey,
  labels  = labels_prey
)

df_c <- make_ce_df(
  fit = fit4,
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

draws <- as_draws_df(fit4)

make_pred_values <- function(fit, draws, param, prob = 0.89) {
  alpha <- (1 - prob) / 2
  lower_p <- alpha
  upper_p <- 1 - alpha

  # Names of fixed-effect columns for this parameter
  b_int   <- paste0("b_", param, "_Intercept")
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
    Zprey_speed = pred_covars$Zprey_speed,
    Zprey_space = pred_covars$Zprey_space,
    stringsAsFactors = FALSE
  )
}

# Predator-specific a_j and c_j
pred_a_vals <- make_pred_values(fit4, draws, "a")
pred_c_vals <- make_pred_values(fit4, draws, "c")

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


# Custom theme -------------------------------------------------------------

custom_theme <- theme(
  plot.title = element_text(size = 14),
  axis.text = element_text(face = "plain", size = 14, color = "black"),
  axis.ticks.length = unit(.15, "cm"),
  axis.ticks = element_line(linewidth = 0.90, color = "black"),
  axis.title = element_text(size = 16, face = "plain", color = "black"),
  axis.line = element_line(linewidth = 0.95, color = "black"),
  legend.position = "none",
  panel.grid = element_blank(),
  panel.background = element_blank()
)



# Prepare figure -----------------------------------------------------------

x_axis_labels <- c(
  prey_speed = "Average prey speed",
  prey_space = "Average prey space coverage"
)

y_axis_labels <- c(
  "Maximum hunting success (a)" = "Maximum hunting success (a)",
  "Rate of gain in expertise (c)" = "Rate of gain in expertise (c)"
)

make_panel <- function(
  df_fix,
  df_ind,
  parameter,
  variable,
  xpad_lower = 0,
  xpad_upper = 0
) {

  xlim_data <- range(df_ind$effect1__, na.rm = TRUE)
  xlim_data <- c(xlim_data[1] - xpad_lower, xlim_data[2] + xpad_upper)

  p <- ggplot() +

    geom_line(
      data = df_fix,
      aes(x = effect1__, y = estimate__),
      linewidth = 1.1
    ) +
    geom_ribbon(
      data = df_fix,
      aes(x = effect1__, ymin = lower__, ymax = upper__),
      alpha = 0.2
    ) +
    geom_pointrange(
      data = df_ind,
      aes(x = effect1__, y = estimate__, ymin = lower__, ymax = upper__),
      size  = 0.4,
      alpha = 0.2,
      position = position_jitter(width = 0.03, height = 0)
    ) +
    labs(
      x = paste0("\n", x_axis_labels[[variable]]),
      y = paste0(y_axis_labels[[parameter]], "\n")
    ) +
    coord_cartesian(xlim = xlim_data) +
    custom_theme

  p
}



# Compute each panel -------------------------------------------------------

df_fix <- subset(
  x = df,
  parameter == "Maximum hunting success (a)" & variable == "prey_speed"
)
df_ind <- subset(
  x = df_pred,
  parameter == "Maximum hunting success (a)" & variable == "prey_speed"
)

p_a_speed <- make_panel(
  df_fix, df_ind,
  parameter = "Maximum hunting success (a)",
  variable = "prey_speed",
  xpad_lower = 0.01,
  xpad_upper = 0.2
) + scale_x_continuous(breaks = seq(-1, 1, 0.5))

df_fix <- subset(
  x = df,
  parameter == "Maximum hunting success (a)" & variable == "prey_space"
)

df_ind <- subset(
  x = df_pred,
  parameter == "Maximum hunting success (a)" & variable == "prey_space"

)
p_a_space <- make_panel(
  df_fix, df_ind,
  parameter = "Maximum hunting success (a)",
  variable = "prey_space",
  xpad_lower = 0.05,
  xpad_upper = 0.05
) + scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4))

df_fix <- subset(
  x = df,
  parameter == "Rate of gain in expertise (c)" & variable == "prey_speed"
)
df_ind <- subset(
  x = df_pred,
  parameter == "Rate of gain in expertise (c)" & variable == "prey_speed"
)
p_c_speed <- make_panel(
  df_fix, df_ind,
  parameter = "Rate of gain in expertise (c)",
  variable = "prey_speed",
  xpad_lower = 0.01,
  xpad_upper = 0.2
) + scale_x_continuous(breaks = seq(-1, 1, 0.5))

df_fix <- subset(
  x = df,
  parameter == "Rate of gain in expertise (c)" & variable == "prey_space"
)
df_ind <- subset(
  x = df_pred,
  parameter == "Rate of gain in expertise (c)" & variable == "prey_space"
)

p_c_space <- make_panel(
  df_fix, df_ind,
  parameter = "Rate of gain in expertise (c)",
  variable  = "prey_space",
  xpad_lower = 0.05,
  xpad_upper = 0.05
)

figure <- ggarrange(
  p_a_speed, p_a_space,
  p_c_speed, p_c_space,
  ncol = 2, nrow = 2
)


path_to_save <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  plot = figure,
  filename = file.path(path_to_save, "figure2.png"),
  dpi = 500,
  height = 8,
  width = 10
)

# ==========================================================================
# ==========================================================================