# ==========================================================================

#                              Figure 1                                    #
#        Asymptotic models: gain curves (global + random effects)          #

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================

options(mc.cores = parallel::detectCores())

library(parallel)
library(brms)
library(data.table)
library(ggplot2)
library(ggpubr)
library(viridis)



# Load models --------------------------------------------------------------

path_models <- file.path(getwd(), "outputs", "outputs_models")

mod2 <- readRDS(file.path(path_models, "asym-II.rds")) # control for rank
mod3 <- readRDS(file.path(path_models, "asym-III.rds")) # mod2 + Prey speed
mod4 <- readRDS(file.path(path_models, "asym-IV.rds")) # mod2 + Prey speed + space



# Load data ---------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2025-data.csv",
  select = c(
    "predator_id",
    "game_duration",
    "pred_speed",
    "prey_avg_speed",
    "prey_avg_amount_tiles_visited",
    "prey_avg_space_rate",
    "prey_avg_rank",
    "cumul_xp_pred",
    "total_xp_pred",
    "hunting_success"
  )
)

data[, predator_id := as.factor(predator_id)]

# Remove any NAs
data <- data[complete.cases(data)]

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[
  ,
  c("Zprey_speed", "Zgame_duration", "Zprey_avg_rank", "Zprey_space") := lapply(
    .SD, standardize
  ),
  .SDcols = c("prey_avg_speed", "game_duration", "prey_avg_rank", "prey_avg_amount_tiles_visited")
]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Helper functions
# ==========================================================================

# Conditional effects for one model ----------------------------------------

make_ce_tables <- function(fit, data, nsamples = NULL) {

  pred_ids <- data[, unique(predator_id)]
  conds <- data.frame(predator_id = pred_ids)

  if ("Zgame_duration" %in% names(data)) {
    conds$Zgame_duration <- 0
  }
  if ("Zprey_avg_rank" %in% names(data)) {
    conds$Zprey_avg_rank <- 0
  }
  if ("Zprey_speed" %in% names(data)) {
    conds$Zprey_speed <- 0
  }
  if ("Zprey_space" %in% names(data)) {
    conds$Zprey_space <- 0
  }

  # Individual predator curves
  tab_re <- conditional_effects(
    fit,
    method = "fitted",
    effects = "cumul_xp_pred",
    conditions = conds,
    robust = TRUE,
    re_formula = NULL,
    ndraws = nsamples
  )[[1L]]

  # Global trend
  tab_global <- conditional_effects(
    fit,
    method = "fitted",
    robust = TRUE,
    re_formula = NA,
    effects = "cumul_xp_pred"
  )[[1L]]

  list(
    tab_re = as.data.table(tab_re),
    tab_global = data.table(tab_global)
  )
}



# Cut fitted values above each predator's total XP ------------------------

cut_by_total_xp <- function(tab_re, xp_dt) {
  tab <- merge(tab_re, xp_dt, by = "predator_id")
  tab[cumul_xp_pred <= total_xp_pred]
}



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



# Plot individual predator curves -----------------------------------------

plot_individual_curves <- function(tab_re, title) {
  ggplot(
    tab_re,
    aes(x = cumul_xp_pred, y = estimate__ / 4, color = predator_id)
  ) +
    geom_line(linewidth = 0.5, alpha = 0.20) +
    scale_color_viridis(discrete = TRUE, option = "D") +
    ylab("Hunting success\n") +
    ggtitle(title) +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 500, 100), limits = c(0, 500)) +
    xlab("\nCumulative experience") +
    custom_theme
}



# Compute baseline, asymptote, and rate ----------------------------------

compute_param_summaries <- function(fit) {
  sum_pars <- posterior_summary(
    fit,
    variable = c("b_a_Intercept", "b_b_Intercept", "b_c_Intercept")
  )

  a_hat <- sum_pars["b_a_Intercept", "Estimate"]
  b_hat <- sum_pars["b_b_Intercept", "Estimate"]
  c_hat <- sum_pars["b_c_Intercept", "Estimate"]

  baseline <- plogis(b_hat)
  asymptote <- plogis(a_hat)
  rate <- exp(c_hat)

  list(
    baseline = baseline,
    asymptote = asymptote,
    rate = rate
  )
}



# Plot global trend with annotations --------------------------------------

plot_global_curve <- function(tab_global, title, param_summ) {
  label_text <- sprintf(
    "Baseline: %.2f\nMaximum: %.2f\nRate of gain: %.2f",
    param_summ$baseline,
    param_summ$asymptote,
    param_summ$rate
  )

  x_min <- min(tab_global$cumul_xp_pred)

  ggplot(
    tab_global,
    aes(x = cumul_xp_pred, y = estimate__ / 4)
  ) +
    geom_ribbon(
      aes(ymin = lower__ / 4, ymax = upper__ / 4),
      fill = "gray"
    ) +
    geom_line(linewidth = 1) +
    annotate(
      "text",
      x = x_min,
      y = 0.85,
      hjust = 0,
      label = label_text,
      size = 3.5,
      color = "black"
    ) +
    ylab("Hunting success\n") +
    ggtitle(title) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.25),
      limits = c(0, 1)
    ) +
    scale_x_continuous(
      labels = seq(0, 500, 100),
      limits = c(0, 500),
    ) +
    xlab("\nCumulative experience") +
    custom_theme
}

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Prepare conditional effects for models 3, 4, 5
# ==========================================================================

# Extract total XP per predator
xp <- unique(data[, .(predator_id, total_xp_pred)])


# ---------------------------- Model 3 -------------------------------------

ce3 <- make_ce_tables(
  fit = mod3,
  data = data,
  nsamples = 100
)

tab3_re <- cut_by_total_xp(ce3$tab_re, xp)
tab3_global <- ce3$tab_global

params3 <- compute_param_summaries(mod3)

plot3_ind <- plot_individual_curves(
  tab_re = tab3_re,
  title = " "
)

plot3_glob <- plot_global_curve(
  tab_global = tab3_global,
  title = "Model III: no prey behaviour",
  param_summ = params3
)



# ---------------------------- Model 4 -------------------------------------

ce4 <- make_ce_tables(
  fit = mod4,
  data = data,
  nsamples = 100
)

tab4_re <- cut_by_total_xp(ce4$tab_re, xp)
tab4_global <- ce4$tab_global

params4 <- compute_param_summaries(mod4)

plot4_ind <- plot_individual_curves(
  tab_re = tab4_re,
  title = " "
)

plot4_glob <- plot_global_curve(
  tab_global = tab4_global,
  title = "Model IV: prey speed",
  param_summ = params4
)



# ---------------------------- Model 5 -------------------------------------

ce5 <- make_ce_tables(
  fit = mod5,
  data = data,
  nsamples = 100
)

tab5_re <- cut_by_total_xp(ce5$tab_re, xp)
tab5_global <- ce5$tab_global

params5 <- compute_param_summaries(mod5)

plot5_ind <- plot_individual_curves(
  tab_re = tab5_re,
  title = " "
)

plot5_glob <- plot_global_curve(
  tab_global = tab5_global,
  title = "Model V: prey speed + prey space",
  param_summ = params5
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Combine plots into 1 figure
# ==========================================================================

figure <- ggarrange(
  NULL, plot3_glob, NULL, plot4_glob, NULL, plot5_glob,
  NULL, plot3_ind,  NULL, plot4_ind,  NULL, plot5_ind,
  ncol = 6, nrow = 2,
  labels = c(
    "(A)", "", "(B)", "", "(C)", "",
    " ", "", " ", "", " ", ""
  ),
  widths = c(
    0.15, 1.5, 0.15, 1.5, 0.15, 1.5,
    0.15, 1.5, 0.15, 1.5, 0.15, 1.5
  )
)

# ==========================================================================
# 5. Export the figure
# ==========================================================================

path_fig <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  filename = file.path(path_fig, "figure1.png"),
  plot     = figure,
  width    = 4000 / 300,
  height   = 2200 / 300,
  dpi      = 300,
  device   = grDevices::png,
  type     = "cairo-png"
)
