# ==========================================================================

#                 Plot GAMM model without rank - Figure S3

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================



# Load libraries and model -------------------------------------------------

options(mc.cores = parallel::detectCores())

library(parallel)
library(brms)
library(data.table)
library(ggplot2)
library(ggpubr)
library(viridis)

path <- file.path(getwd(), "outputs", "outputs_models")

mod <- readRDS(file.path(path, "GAMM-I.rds"))



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
# 2. Prepare the data for the plots
# ==========================================================================



# Prepare the plots --------------------------------------------------------


# Group-level smooths model 2
tab_a <- conditional_effects(
  mod, method = "fitted",
  effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
)
tab_a <- data.table(tab_a[[1]])

# Cumulative XP global trend model 2
tab_b <- conditional_effects(
  mod, method = "fitted",
  robust = TRUE, re_formula = NULL,
  effects = "Zcumul_xp",
  conditions = data.frame(predator_id = NA)
)
tab_b <- data.table(tab_b[[1]])


# Transform values --------------------------------------------------------

# Back transform x-axis values
mean_xp <- mean(data$cumul_xp_pred)
standev <- sd(data$cumul_xp_pred)
sequence <- (seq(0, 500, 100) - mean_xp)
scaled_breaks <- sequence / standev

# List the tables
tables <- list(
  tab_a, tab_b
)
names(tables) <- c(
  "tab_a", "tab_b"
)

# Function to apply transformation
# Computes non standardized cumulative XP
func <- function(x) {
  x[, cumul_xp := (Zcumul_xp * standev) + mean(data$cumul_xp_pred)]
}

# Apply function
lapply(tables, func)



# Cut fitted values based on player XP ------------------------------------

# Extract player IDs with their total XP from the original data
xp <- unique(data[, .(predator_id, total_xp_pred)])

# Merge the two tables adding the total XP
tab_a <- merge(tab_a, xp, by = "predator_id")

# Cut all matches where fitted values are above total XP
tab_a <- tab_a[cumul_xp <= total_xp_pred, ]


# Setup a custom theme for the plot ----------------------------------------

custom_theme <- theme(
  # Plot title
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

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Individual variance plot
# ==========================================================================


# Model 2 no rank ----------------------------------------------------------

p1 <- ggplot(
  tab_a,
  aes(x = Zcumul_xp,
      y = estimate__ / 4,
      color = predator_id)
) +
  geom_line(linewidth = 0.5, alpha = 0.25) +
  scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  ggtitle("Model I") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Global trend plot
# ==========================================================================

# Model 2 no rank ----------------------------------------------------------

p2 <- ggplot(
  tab_b,
  aes(x = Zcumul_xp,
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
  ggtitle("Model I") +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = scaled_breaks,
    labels = seq(0, 500, 100)
  ) +
  xlab("\nCumulative experience") +
  custom_theme

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Combine plots into 1 figure
# ==========================================================================

# Prepare figure ------------------------------------------------------------

# Arrange paneled figure
figure <- ggarrange(
  NULL, p1, NULL, p2,
  ncol = 4, nrow = 1,
  labels = c(
    "(A)", "", "(B)", ""
  ),
  widths = c(
    0.15, 1.5, 0.15, 1.5
  )
)



# Export the figure -----------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

# If using Windows
# ggsave(
#   figure,
#   filename = file.path(path, "figureS3.png"),
#   units = "in",
#   dpi = 300,
#   width = 10,
#   height = 4
# )

# If using linux
ggsave(
  filename = file.path(path, "figureS3.png"),
  plot = figure,
  units = "in",
  width = 10,
  height = 4,
  dpi = 300,
  device = grDevices::png,
  type = "cairo-png"
)

# ==========================================================================
# ==========================================================================