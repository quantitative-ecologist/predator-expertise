# ==========================================================================

#                                 Figure 3
#                   Plot random effect mean correlations

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

fit1 <- readRDS(file.path(path, "asym-I.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws into a tidy table
# ==========================================================================


# Function to process draws ------------------------------------------------

extract_pred_id <- function(x) sub("^r_predator_id__[abc]\\[([^,]+),Intercept\\]$", "\\1", x)

pred_total_from_draws <- function(fit, nlpar = c("a", "b", "c"), prob = 0.89) {
  draws <- as_draws_df(fit)
  nlpar <- match.arg(nlpar)

  # fixed intercept draws
  bname <- paste0("b_", nlpar, "_Intercept")
  b_int <- draws[[bname]]

  # random intercept draws columns
  patt <- paste0("^r_predator_id__", nlpar, "\\[[^,]+,Intercept\\]$")
  rcols <- grep(patt, names(draws), value = TRUE)

  # compute total draws per predator: fixed + random
  draws_mat <- as.matrix(draws)
  rmat <- draws_mat[, rcols, drop = FALSE]
  total_mat <- sweep(x = rmat, MARGIN = 1, STATS = b_int, FUN = "+")

  # summarize each predator column
  alpha <- (1 - prob) / 2
  qs <- apply(
    X = total_mat,
    MARGIN = 2,
    FUN = quantile,
    probs = c(alpha, 0.5, 1 - alpha),
    na.rm = TRUE
  )

  out <- data.table(
    predator_id = extract_pred_id(colnames(total_mat)),
    lower = as.numeric(qs[1, ]),
    est   = as.numeric(qs[2, ]),
    upper = as.numeric(qs[3, ])
  )

  setnames(
    out,
    c("lower", "est", "upper"),
    paste0(nlpar, c("_lower", "_est", "_upper"))
  )
  out[]
}


# Process draws ------------------------------------------------------------

prob_ci <- 0.89
dt_a <- pred_total_from_draws(
  fit = fit1,
  nlpar = "a",
  prob = prob_ci
)
dt_b <- pred_total_from_draws(
  fit = fit1,
  nlpar = "b",
  prob = prob_ci
)
dt_c <- pred_total_from_draws(
  fit = fit1,
  nlpar = "c",
  prob = prob_ci
)

# Combine the tables together
dt <- Reduce(
  function(x, y) merge(x, y, by = "predator_id"),
  list(dt_a, dt_b, dt_c)
)



# Get correlations ---------------------------------------------------------

# Function to summarize vector of correlations
summ_vec <- function(x, prob = 0.89) {
  alpha <- (1 - prob) / 2
  qs <- quantile(
    x,
    probs = c(alpha, 0.5, 1 - alpha),
    na.rm = TRUE
  )
  list(
    lower = unname(qs[1]),
    median = unname(qs[2]),
    upper = unname(qs[3])
  )
}
# Function that applies summ_vec
summ_draws <- function(x, prob = 0.89) summ_vec(x, prob)

# Get draws
draws <- as_draws_df(fit1)

rho_ab <- summ_draws(
  draws[["cor_predator_id__a_Intercept__b_Intercept"]],
  prob_ci
)
rho_ac <- summ_draws(
  draws[["cor_predator_id__a_Intercept__c_Intercept"]],
  prob_ci
)
rho_bc <- summ_draws(
  draws[["cor_predator_id__b_Intercept__c_Intercept"]],
  prob_ci
)

# ==========================================================================
# ==========================================================================




# ==========================================================================
# 3. Plot the posterior distribution of predator means
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

param_labels <- c(
  a = "Maximum hunting success (a)",
  b = "Initial hunting success (b)",
  c = "Rate of gain (c)"
)

make_pair_panel <- function(dt, x, y, rho, labels) {
  x_est <- paste0(x, "_est")
  x_lo <- paste0(x, "_lower")
  x_hi <- paste0(x, "_upper")
  y_est <- paste0(y, "_est")
  y_lo <- paste0(y, "_lower")
  y_hi <- paste0(y, "_upper")

  # Correlations
  lab <- sprintf(
    "cor. = %.2f [%.2f, %.2f]",
    rho$median,
    rho$lower,
    rho$upper
  )

  # Plot
  ggplot(dt, aes(x = .data[[x_est]], y = .data[[y_est]])) +
    geom_errorbar(
      aes(ymin = .data[[y_lo]], ymax = .data[[y_hi]]),
      width = 0,
      alpha = 0.25
    ) +
    geom_errorbarh(
      aes(xmin = .data[[x_lo]], xmax = .data[[x_hi]]),
      height = 0,
      alpha = 0.25
    ) +
    geom_point(alpha = 0.5, size = 1.8) +
    geom_smooth(
      method = "lm",
      linetype = 1,
      color = "#e55c30",
      se = FALSE
    ) +
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = lab,
      hjust = -0.05,
      vjust = 1.1,
      size = 5
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = paste0("\n", labels[[x]]),
      y = paste0(labels[[y]], "\n")
    ) +
    custom_theme
}

p_ab <- make_pair_panel(
  dt = dt,
  x = "a",
  y = "b",
  rho = rho_ab,
  labels = param_labels
)
p_ab <- p_ab +
  scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4.3, 4))

p_ac <- make_pair_panel(
  dt = dt,
  x = "a",
  y = "c",
  rho = rho_ac,
  labels = param_labels
)
p_ac <- p_ac +
  scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4.3, 4)) +
  scale_y_continuous(breaks = seq(-8, 0, 2), limits = c(-8, 0))

p_bc <- make_pair_panel(
  dt = dt,
  x = "b",
  y = "c",
  rho = rho_bc,
  labels = param_labels
)
p_bc <- p_bc +
  scale_y_continuous(breaks = seq(-8, 0, 2), limits = c(-8, 0))

figure <- ggarrange(
  p_ab, p_ac, p_bc,
  ncol = 3,
  nrow = 1
)



# Save figure --------------------------------------------------------------

path_to_save <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  plot = figure,
  filename = file.path(path_to_save, "figure3.png"),
  dpi = 500,
  height = 4.5,
  width = 14
)

# ==========================================================================
# ==========================================================================