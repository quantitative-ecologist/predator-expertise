# ==========================================================================

#                          Plot regression models
#                         of prey speed and space
#                                Figure 5

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================



# Load libraries and model -------------------------------------------------

options(mc.cores = parallel::detectCores())

library(parallel)
library(brms)
library(rstan)
library(data.table)
library(ggplot2)
library(viridis)
library(ggpubr)

path <- file.path(getwd(), "outputs", "outputs_models")

mod_speed <- readRDS(file.path(path, "LM-PreySpeed.rds"))
mod_space <- readRDS(file.path(path, "LM-PreySpace.rds"))



# Load the data ------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2025-data.csv",
  select = c(
    "predator_id",
    "game_duration",
    "prey_avg_speed",
    "prey_avg_amount_tiles_visited",
    "prey_avg_rank"
  )
)

# Predator ID as factor
data[, predator_id := as.factor(predator_id)]

# Remove any NAs
data <- data[complete.cases(data)]


# Standardise the variables (Z-scores) -------------------------------------

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[,
  c("Zgame_duration", "Zprey_avg_rank") := lapply(.SD, standardize),
  .SDcols = c(2, 5)
]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Prepare the data for the plots
# ==========================================================================



# Prepare the plots --------------------------------------------------------

# To unscale prey rank
mean_rank <- mean(data$prey_avg_rank)
sd_rank <- sd(data$prey_avg_rank)

# Prepare data for predictions
new_data <- data.frame(
  prey_avg_rank = seq(
    min(data$prey_avg_rank),
    max(data$prey_avg_rank),
    length.out = 100
  ),
  Zgame_duration = 0,
  predator_id = NA
)

# Standardize rank
new_data$Zprey_avg_rank <- (new_data$prey_avg_rank - mean_rank) / sd_rank

# Get posterior expected predictions
epred_speed <- posterior_epred(mod_speed, newdata = new_data, re_formula = NA)
epred_space <- posterior_epred(mod_space, newdata = new_data, re_formula = NA)


# Convert to long format
draws_long_speed <- as.data.table(t(epred_speed))
draws_long_speed[, prey_avg_rank := new_data$prey_avg_rank]
draws_long_speed <- melt(draws_long_speed, id.vars = "prey_avg_rank", variable.name = "draw")

draws_long_space <- as.data.table(t(epred_space))
draws_long_space[, prey_avg_rank := new_data$prey_avg_rank]
draws_long_space <- melt(draws_long_space, id.vars = "prey_avg_rank", variable.name = "draw")

# Compute the median and randomly sample 50 draws
median_preds_speed <- draws_long_speed[, .(value = median(value)), by = prey_avg_rank]
sampled_draws_speed <- draws_long_speed[draw %in% sample(unique(draw), 50)]

median_preds_space <- draws_long_space[, .(value = median(value)), by = prey_avg_rank]
sampled_draws_space <- draws_long_space[draw %in% sample(unique(draw), 50)]



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
# 3. Compute plot
# ==========================================================================


# Speed -----------------------------------------------------------------

p1 <- ggplot() +
  geom_hex(
    data = data,
    aes(x = prey_avg_rank, y = prey_avg_speed),
    bins = 60
  ) +
  geom_line(
    data = sampled_draws_speed,
    aes(x = prey_avg_rank, y = value, group = draw),
    alpha = 0.2,
    color = "gray"
  ) +
  geom_line(
    data = median_preds_speed,
    aes(x = prey_avg_rank, y = value),
    linewidth = 0.2,
    color = "black"
  ) +
  ylab("Prey average speed\n") +
  xlab("\nPrey rank") +
  scale_y_continuous(
    breaks = seq(0, 4, 1), 
    limits = c(0, 4)
  ) +
  scale_x_continuous(
    breaks = seq(0, max(data$prey_avg_rank), 5), 
    limits = c(0, max(data$prey_avg_rank)+1)
  ) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  theme_classic() +
  custom_theme #+theme(plot.margin = margin(5, 20, 5, 5))



# Space covered ---------------------------------------------------------

p2 <- ggplot() +
  geom_hex(
    data = data,
    aes(x = prey_avg_rank, y = prey_avg_amount_tiles_visited),
    bins = 60
  ) +
  geom_line(
    data = sampled_draws_space,
    aes(x = prey_avg_rank, y = value, group = draw),
    alpha = 0.2,
    color = "gray"
  ) +
  geom_line(
    data = median_preds_space,
    aes(x = prey_avg_rank, y = value),
    linewidth = 0.2,
    color = "black"
  ) +
  ylab("Prey average space covered\n") +
  xlab("\nPrey rank") +
#   scale_y_continuous(
#     breaks = seq(0, 4, 1), 
#     limits = c(0, 4)
#   ) +
  scale_x_continuous(
    breaks = seq(0, max(data$prey_avg_rank), 5), 
    limits = c(0, max(data$prey_avg_rank)+1)
  ) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  theme_classic() +
  custom_theme# +theme(plot.margin = margin(5, 5, 5, 20))



# Prepare figure ------------------------------------------------------------

# Arrange paneled figure
figure <- ggarrange(
  NULL, p1, NULL, p2,
  ncol = 4, nrow = 1,
  labels = c("(A)", "", "(B)", ""),
  widths = c(0.15, 1.5, 0.15, 1.5)
)



# Export the figure -----------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  figure,
  filename = file.path(path, "figure5.png"),
  units = "in",
  dpi = 300,
  width = 10,
  height = 4
)

# ==========================================================================
# ==========================================================================