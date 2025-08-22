# ==========================================================================

#               Figure of the relationship between success 
#                     and prey speed, space, and rank
#                               Figure 2

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
fit <- readRDS(file.path(path, "GAMM-VI.rds"))



# Load the data ------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2025-data.csv",
  select = c("predator_id",
             "game_duration",
             "pred_speed",
             "prey_avg_speed",
             "prey_avg_amount_tiles_visited",
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


# Predictions --------------------------------------------------------------

preds <- conditional_effects(
  x = fit,
  method = "fitted",
  effects = c("Zprey_speed", "Zprey_space", "Zprey_avg_rank"),
  prob = 0.89,
  robust = TRUE,
  re_formula = NULL,
  conditions = data.frame(predator_id = NA)
)
preds_speed <- data.table(preds[[1]])
preds_space <- data.table(preds[[2]])
preds_rank <- data.table(preds[[3]])



# Transform values --------------------------------------------------------

# Back transform x-axis values
range_speed <- seq(
  min(data$prey_avg_speed, na.rm = TRUE),
  max(data$prey_avg_speed, na.rm = TRUE),
  length.out = 5
)
range_space <- seq(
  min(data$prey_avg_amount_tiles_visited, na.rm = TRUE),
  max(data$prey_avg_amount_tiles_visited, na.rm = TRUE),
  length.out = 5
)
range_rank <- seq(
  min(data$prey_avg_rank, na.rm = TRUE),
  max(data$prey_avg_rank, na.rm = TRUE),
  length.out = 5
)

speed_sequence <- range_speed - mean(data$prey_avg_speed, na.rm = TRUE)
space_sequence <- range_space - mean(data$prey_avg_amount_tiles_visited
, na.rm = TRUE)
rank_sequence <- range_rank - mean(data$prey_avg_rank, na.rm = TRUE)

speed_standev <- sd(data$prey_avg_speed, na.rm = TRUE)
space_standev <- sd(data$prey_avg_amount_tiles_visited, na.rm = TRUE)
rank_standev <- sd(data$prey_avg_rank, na.rm = TRUE)

speed_scaled_breaks <- speed_sequence / speed_standev
space_scaled_breaks <- space_sequence / space_standev
rank_scaled_breaks <- rank_sequence / rank_standev

# Function to apply transformation
# Computes non standardized cumulative XP
speed_func <- function(x) {
  x[, prey_avg_speed := (Zprey_speed * speed_standev) + mean(data$prey_avg_speed, na.rm = TRUE)]
}

space_func <- function(x) {
  x[, prey_avg_amount_tiles_visited := (Zprey_space * space_standev) + mean(data$prey_avg_amount_tiles_visited, na.rm = TRUE)]
}

rank_func <- function(x) {
  x[, prey_avg_rank := (Zprey_avg_rank * rank_standev) + mean(data$prey_avg_rank, na.rm = TRUE)]
}

# Apply function
speed_func(preds_speed)
space_func(preds_space)
rank_func(preds_rank)

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
  aes(x = prey_avg_speed,
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
  ggtitle("Model VI: rank + speed + space") +
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
  aes(x = prey_avg_amount_tiles_visited,
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
  ggtitle("Model VI: rank + speed + space") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  # scale_x_continuous(
  #   breaks = seq(0, 4, 1),
  #   limits = c(0.5, 4)
  # ) +
  xlab("\nPrey space covered") +
  custom_theme

p3 <- ggplot(
  preds_rank,
  aes(x = prey_avg_rank,
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
  ggtitle("Model VI: rank + speed + space") +
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
  filename = file.path(path, "figure2.png"),
  units = "in",
  dpi = 300,
  width = 15,
  height = 4
)

# ==========================================================================
# ==========================================================================