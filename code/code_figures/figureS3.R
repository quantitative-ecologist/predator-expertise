# ==========================================================================
#         Figure: Model III population trend by prey rank level
# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================

options(mc.cores = parallel::detectCores())

library(brms)
library(data.table)
library(ggplot2)

# Load model
path_models <- file.path(getwd(), "outputs", "outputs_models")
mod3 <- readRDS(file.path(path_models, "asym-III.rds"))

# Load data
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
data <- data[complete.cases(data)]
data <- data[prey_avg_space_rate <= 0.2]

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
# 2. Build conditions for three prey rank levels
# ==========================================================================

# Three conditions: mean (0), +2 SD (+2), -2 SD (-2) on the standardized scale
rank_levels <- c(-2, 0, 2)
rank_labels <- c("Prey rank: -2 SD", "Prey rank: mean", "Prey rank: +2 SD")

conds <- data.frame(
  Zprey_avg_rank = rank_levels,
  Zgame_duration = 0,
  Zprey_speed = 0,
  Zprey_space = 0
)
rownames(conds) <- rank_labels

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Compute conditional effects
# ==========================================================================

ce <- conditional_effects(
  mod3,
  method = "fitted",
  effects = "cumul_xp_pred",
  conditions = conds,
  robust = TRUE,
  re_formula = NA
)[[1L]]

ce <- as.data.table(ce)

# Rename the condition column for legibility
ce[, rank_group := factor(cond__, levels = rank_labels)]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Custom theme
# ==========================================================================

custom_theme <- theme(
  plot.title = element_text(size = 14),
  axis.text = element_text(face = "plain", size = 14, color = "black"),
  axis.ticks.length = unit(.15, "cm"),
  axis.ticks = element_line(linewidth = 0.90, color = "black"),
  axis.title = element_text(size = 16, face = "plain", color = "black"),
  axis.line = element_line(linewidth = 0.95, color = "black"),
  legend.position = "right",
  legend.title = element_text(size = 13),
  legend.text = element_text(size = 12),
  panel.grid = element_blank(),
  panel.background = element_blank()
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 5. Plot
# ==========================================================================

# Colour palette
rank_colours <- c(
  "Prey rank: -2 SD" = "#f98e09",
  "Prey rank: mean"  = "#bc3754",
  "Prey rank: +2 SD" = "#57106e"
)

figure <- ggplot(
  data = ce,
  aes(
    x = cumul_xp_pred,
    y = estimate__ / 4,
    ymin = lower__ / 4,
    ymax = upper__ / 4,
    color = rank_group,
    fill = rank_group
  )
) +
  geom_ribbon(alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = rank_colours, name = "Prey rank") +
  scale_fill_manual(values  = rank_colours, name = "Prey rank") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 500, 100), limits = c(0, 500)) +
  ylab("Hunting success\n") +
  xlab("\nCumulative experience") +
  custom_theme

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 6. Export
# ==========================================================================

path_fig <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  filename = file.path(path_fig, "figureS3.png"),
  plot = figure,
  width = 2500 / 300,
  height = 1800 / 300,
  dpi = 300,
  device = grDevices::png,
  type = "cairo-png"
)

# ==========================================================================
# ==========================================================================