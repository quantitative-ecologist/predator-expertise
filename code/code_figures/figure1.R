# ==========================================================================

#                              Figure 1                                    #
#     Asymptotic model I: gain curves (average individual + predators)     #

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



# Load model ---------------------------------------------------------------

path_models <- file.path(getwd(), "outputs", "outputs_models")

mod1 <- readRDS(file.path(path_models, "asym-I.rds"))



# Load data ---------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2025-data.csv",
  select = c(
    "predator_id",
    "game_duration",
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
  c("Zgame_duration", "Zprey_avg_rank") := lapply(.SD, standardize),
  .SDcols = c("game_duration", "prey_avg_rank")
]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Helper functions
# ==========================================================================

# Conditional effects for one model ----------------------------------------

make_ce_tables <- function(fit, data, nsamples = NULL, prob = 0.89) {

  pred_ids <- data[, unique(predator_id)]
  conds <- data.frame(
    predator_id = pred_ids,
    Zgame_duration = 0,
    Zprey_avg_rank = 0
  )

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

  # Average individual (random effects set to zero)
  tab_global <- conditional_effects(
    fit,
    method = "fitted",
    robust = TRUE,
    re_formula = NA,
    effects = "cumul_xp_pred",
    prob = prob
  )[[1L]]

  list(
    tab_re = as.data.table(tab_re),
    tab_global = as.data.table(tab_global)
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



# Plot average individual curve -------------------------------------------

plot_global_curve <- function(tab_global, title) {
  ggplot(
    tab_global,
    aes(x = cumul_xp_pred, y = estimate__ / 4)
  ) +
    geom_ribbon(
      aes(ymin = lower__ / 4, ymax = upper__ / 4),
      fill = "gray"
    ) +
    geom_line(linewidth = 1) +
    ylab("Hunting success\n") +
    ggtitle(title) +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 500, 100), limits = c(0, 500)) +
    xlab("\nCumulative experience") +
    custom_theme
}

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Prepare conditional effects for model I
# ==========================================================================

# Extract total XP per predator
xp <- unique(data[, .(predator_id, total_xp_pred)])

ce1 <- make_ce_tables(
  fit = mod1,
  data = data,
  nsamples = 100
)

tab1_re <- cut_by_total_xp(ce1$tab_re, xp)
tab1_global <- ce1$tab_global

plot1_glob <- plot_global_curve(
  tab_global = tab1_global,
  title = "Average individual"
)

plot1_ind <- plot_individual_curves(
  tab_re = tab1_re,
  title = "Individual predators"
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Combine plots into 1 figure
# ==========================================================================

figure <- ggarrange(
  NULL, plot1_glob, NULL, plot1_ind,
  ncol = 4, nrow = 1,
  labels = c("(A)", "", "(B)", ""),
  widths = c(0.15, 1.5, 0.15, 1.5)
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
  width = 10,
  height = 4,
  units = "in",
  dpi = 300,
  device = grDevices::png,
  type = "cairo-png"
)
figure

# ==========================================================================
# ==========================================================================
