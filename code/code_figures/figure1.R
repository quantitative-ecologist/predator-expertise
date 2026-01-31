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

mod1 <- readRDS(file.path(path_models, "asym-I.rds"))
mod2 <- readRDS(file.path(path_models, "asym-II.rds"))
mod3 <- readRDS(file.path(path_models, "asym-III.rds"))



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

# Space rate within 0.2
data <- data[prey_avg_space_rate <= 0.2]

# Standardize traits
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[
  ,
  c("Zprey_speed", "Zgame_duration", "Zprey_avg_rank", "Zprey_space") := lapply(
    .SD, standardize
  ),
  .SDcols = c(
    "prey_avg_speed",
    "game_duration",
    "prey_avg_rank",
    "prey_avg_space_rate"
  )
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



# Compute group-level SD summaries for a, b, c ----------------------------

compute_re_sd_summaries <- function(fit, prob = 0.89) {
  alpha <- (1 - prob) / 2
  q_lo  <- alpha
  q_hi  <- 1 - alpha

  vars <- c(
    "sd_predator_id__a_Intercept",
    "sd_predator_id__b_Intercept",
    "sd_predator_id__c_Intercept"
  )

  sum_sd <- posterior_summary(fit, variable = vars, probs = c(q_lo, q_hi))

  list(
    sd_a = c(
      est = sum_sd["sd_predator_id__a_Intercept", "Estimate"],
      lo = sum_sd["sd_predator_id__a_Intercept", 3],
      hi = sum_sd["sd_predator_id__a_Intercept", 4]
    ),
    sd_b = c(
      est = sum_sd["sd_predator_id__b_Intercept", "Estimate"],
      lo = sum_sd["sd_predator_id__b_Intercept", 3],
      hi = sum_sd["sd_predator_id__b_Intercept", 4]
    ),
    sd_c = c(
      est = sum_sd["sd_predator_id__c_Intercept", "Estimate"],
      lo = sum_sd["sd_predator_id__c_Intercept", 3],
      hi = sum_sd["sd_predator_id__c_Intercept", 4]
    ),
    prob = prob
  )
}



# Plot individual predator curves -----------------------------------------

plot_individual_curves <- function(tab_re, title, re_sd = NULL) {
  p <- ggplot(
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

  if (!is.null(re_sd)) {
    label_sd <- sprintf(
      "RE SDs (predator):\nSD(b): %.2f [%.2f, %.2f]\nSD(a): %.2f [%.2f, %.2f]\nSD(c): %.2f [%.2f, %.2f]",
      re_sd$sd_b["est"],
      re_sd$sd_b["lo"],
      re_sd$sd_b["hi"],
      re_sd$sd_a["est"],
      re_sd$sd_a["lo"],
      re_sd$sd_a["hi"],
      re_sd$sd_c["est"],
      re_sd$sd_c["lo"],
      re_sd$sd_c["hi"]
    )
    # Put the text in the bottom-right to avoid covering curves
    p <- p +
      annotate(
        "text",
        x = 250,
        y = 0.05,
        hjust = 0,
        vjust = 0,
        label = label_sd,
        size = 3.2,
        color = "black"
      )
  }
}



# Compute baseline, asymptote, and rate ----------------------------------

compute_param_summaries <- function(fit, prob = 0.89) {
  alpha <- (1 - prob) / 2
  q_lo  <- alpha
  q_hi  <- 1 - alpha

  dr <- posterior::as_draws_df(fit)

  a_draw <- dr$b_a_Intercept
  b_draw <- dr$b_b_Intercept
  c_draw <- dr$b_c_Intercept

  baseline_draw  <- plogis(b_draw)
  asymptote_draw <- plogis(a_draw)
  rate_draw      <- exp(c_draw)

  summ <- function(x) {
    c(
      est = median(x),
      lo  = unname(stats::quantile(x, probs = q_lo)),
      hi  = unname(stats::quantile(x, probs = q_hi))
    )
  }

  list(
    baseline = summ(baseline_draw),
    asymptote = summ(asymptote_draw),
    rate = summ(rate_draw),
    prob = prob
  )
}



# Plot global trend with annotations --------------------------------------

plot_global_curve <- function(tab_global, title, param_summ) {
  label_text <- sprintf(
    "Population param. values:\nMedian(b): %.2f [%.2f, %.2f]\nMedian(a): %.2f [%.2f, %.2f]\nMedian(c): %.2f [%.2f, %.2f]",
    param_summ$baseline["est"],
    param_summ$baseline["lo"],
    param_summ$baseline["hi"],
    param_summ$asymptote["est"],
    param_summ$asymptote["lo"],
    param_summ$asymptote["hi"],
    param_summ$rate["est"],
    param_summ$rate["lo"],
    param_summ$rate["hi"]
  )

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
      x = 225,
      y = 0.05,
      hjust = 0,
      vjust = 0,
      label = label_text,
      size = 3.2,
      color = "black"
    ) +
    ylab("Hunting success\n") +
    ggtitle(title) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.25),
      limits = c(0, 1)
    ) +
    scale_x_continuous(
      breaks = seq(0, 500, 100),
      limits = c(0, 500)
    ) +
    xlab("\nCumulative experience") +
    custom_theme
}

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Prepare conditional effects for models 1, 2, 3
# ==========================================================================

# Extract total XP per predator
xp <- unique(data[, .(predator_id, total_xp_pred)])

re_sd1 <- compute_re_sd_summaries(mod1, prob = 0.89)
re_sd2 <- compute_re_sd_summaries(mod2, prob = 0.89)
re_sd3 <- compute_re_sd_summaries(mod3, prob = 0.89)



# ---------------------------- Model 1 -------------------------------------

ce1 <- make_ce_tables(
  fit = mod1,
  data = data,
  nsamples = 100
)

tab1_re <- cut_by_total_xp(ce1$tab_re, xp)
tab1_global <- ce1$tab_global

params1 <- compute_param_summaries(mod1)

plot1_ind <- plot_individual_curves(
  tab_re = tab1_re,
  title = " ",
  re_sd = re_sd1
)

plot1_glob <- plot_global_curve(
  tab_global = tab1_global,
  title = "Model I: no prey behaviour",
  param_summ = params1
)



# ---------------------------- Model 2 -------------------------------------

ce2 <- make_ce_tables(
  fit = mod2,
  data = data,
  nsamples = 100
)

tab2_re <- cut_by_total_xp(ce2$tab_re, xp)
tab2_global <- ce2$tab_global

params2 <- compute_param_summaries(mod2)

plot2_ind <- plot_individual_curves(
  tab_re = tab2_re,
  title = " ",
  re_sd = re_sd2
)

plot2_glob <- plot_global_curve(
  tab_global = tab2_global,
  title = "Model II: prey speed",
  param_summ = params2
)



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
  title = " ",
  re_sd = re_sd3
)

plot3_glob <- plot_global_curve(
  tab_global = tab3_global,
  title = "Model III: prey speed + prey space", 
  param_summ = params3
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Combine plots into 1 figure
# ==========================================================================

figure <- ggarrange(
  NULL, plot1_glob, NULL, plot2_glob, NULL, plot3_glob,
  NULL, plot1_ind,  NULL, plot2_ind,  NULL, plot3_ind,
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
# ==========================================================================





# ==========================================================================
# 5. Export the figure
# ==========================================================================

path_fig <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  filename = file.path(path_fig, "figure1.png"),
  plot = figure,
  width = 4000 / 300,
  height = 2200 / 300,
  dpi = 300,
  device = grDevices::png,
  type = "cairo-png"
)
figure

# ==========================================================================
# ==========================================================================