# ==========================================================================

#                           Plot GAMM models

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

mod2 <- readRDS(file.path(path, "GAMM-II.rds"))
mod5 <- readRDS(file.path(path, "GAMM-V.rds"))
mod6 <- readRDS(file.path(path, "GAMM-VI.rds"))



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
tab2_a <- conditional_effects(
  mod2, method = "fitted",
  effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
)
tab2_a <- data.table(tab2_a[[1]])

# Cumulative XP global trend model 2
tab2_b <- conditional_effects(
  mod2, method = "fitted",
  robust = TRUE, re_formula = NULL,
  effects = "Zcumul_xp",
  conditions = data.frame(predator_id = NA)
)
tab2_b <- data.table(tab2_b[[1]])

# Group-level smooths model 5
tab5_a <- conditional_effects(
  mod5, method = "fitted",
  effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
)
tab5_a <- data.table(tab5_a[[1]])

# Cumulative XP global trend model 5
tab5_b <- conditional_effects(
  mod5, method = "fitted",
  robust = TRUE, re_formula = NULL,
  effects = "Zcumul_xp",
  conditions = data.frame(predator_id = NA)
)
tab5_b <- data.table(tab5_b[[1]])

# Group-level smooths model 6
tab6_a <- conditional_effects(
  mod6, method = "fitted",
  effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
)
tab6_a <- data.table(tab6_a[[1]])

# Group-level smooths model 6
tab6_b <- conditional_effects(
  mod6, method = "fitted",
  robust = TRUE, re_formula = NULL,
  effects = "Zcumul_xp",
  conditions = data.frame(predator_id = NA)
)
tab6_b <- data.table(tab6_b[[1]])


# Transform values --------------------------------------------------------

# Back transform x-axis values
mean_xp <- mean(data$cumul_xp_pred)
standev <- sd(data$cumul_xp_pred)
sequence <- (seq(0, 500, 100) - mean_xp)
scaled_breaks <- sequence / standev

# List the tables
tables <- list(
  tab2_a, tab2_b,
  tab5_a, tab5_b,
  tab6_a, tab6_b
)
names(tables) <- c(
  "tab2_a", "tab2_b",
  "tab5_a", "tab5_b",
  "tab6_a", "tab6_b"
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
tab2_a <- merge(tab2_a, xp, by = "predator_id")
tab5_a <- merge(tab5_a, xp, by = "predator_id")
tab6_a <- merge(tab6_a, xp, by = "predator_id")

# Cut all matches where fitted values are above total XP
tab2_a <- tab2_a[cumul_xp <= total_xp_pred, ]
tab5_a <- tab5_a[cumul_xp <= total_xp_pred, ]
tab6_a <- tab6_a[cumul_xp <= total_xp_pred, ]


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
# 3. Individual variance plots
# ==========================================================================


# Model 2 rank only --------------------------------------------------------

plot2_a <- ggplot(
  tab2_a,
  aes(x = Zcumul_xp,
      y = estimate__ / 4,
      color = predator_id)
) +
  geom_line(linewidth = 0.5, alpha = 0.25) +
  scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  ggtitle("Model II: rank") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme



# Model 5 rank + speed -----------------------------------------------------

plot5_a <- ggplot(
  tab5_a,
  aes(x = Zcumul_xp,
      y = estimate__ / 4,
      color = predator_id)
) +
  geom_line(linewidth = 0.5, alpha = 0.25) +
  scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  ggtitle("Model V: rank + speed") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme



# Model 6 rank + speed + space ---------------------------------------------

plot6_a <- ggplot(
  tab6_a,
  aes(x = Zcumul_xp,
      y = estimate__ / 4,
      color = predator_id)
) +
  geom_line(linewidth = 0.5, alpha = 0.25) +
  scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  ggtitle("Model VI: rank + speed + space") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Global trend plots
# ==========================================================================


# Model 2 rank only --------------------------------------------------------

# Predicted values from the GAMM
predicted_values2 <- tab2_b$estimate__

# Define the x-axis range
x2 <- tab2_b$Zcumul_xp

# Calculate the first derivative using finite differences
dx2 <- mean(diff(x2))
derivatives2 <- diff(predicted_values2) / dx2

# Establish the treshold at a value very close to 0 since its optimization
threshold <- 0.05
# Find the point where the slope is close to zero
stabilized_point2 <- x2[which(abs(derivatives2) <= threshold)][1]

(stabilized_point2 * standev) + mean_xp

plot2_b <- ggplot(
  tab2_b,
  aes(x = Zcumul_xp,
      y = estimate__ / 4)
) +
  geom_vline(
    xintercept = min(tab2_b$Zcumul_xp),
    lty = "dashed",
    color = "#440154"
  ) +
  geom_text(
    aes(label = paste("y =", round(min(tab2_b$estimate__ / 4), digits = 2)),
        y = min(tab2_b$estimate__ / 4),
        x = min(tab2_b$Zcumul_xp) + 0.6),
    color = "#440154",
    size = 5
  ) +
  geom_vline(
    xintercept = tab2_b[Zcumul_xp == stabilized_point2]$Zcumul_xp,
    lty = "dashed",
    color = "#440154"
  ) +
  geom_text(
    aes(label = paste(
      "y =",
      format(
        round(tab2_b[Zcumul_xp == stabilized_point2]$estimate__ / 4, digits = 2),
        nsmall = 2
      )
    ),
    y = (tab2_b[Zcumul_xp == stabilized_point2]$estimate__ / 4) + 0.10,
    x = tab2_b[Zcumul_xp == stabilized_point2]$Zcumul_xp - 0.55),
    color = "#440154",
    size = 5
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
  ggtitle("Model II: rank") +
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



# Model 5 rank + speed -----------------------------------------------------

# Predicted values from the GAMM
predicted_values5 <- tab5_b$estimate__

# Define the x-axis range
x5 <- tab5_b$Zcumul_xp

# Calculate the first derivative using finite differences
dx5 <- mean(diff(x5))
derivatives5 <- diff(predicted_values5) / dx5

# Establish the treshold at a value very close to 0 since its optimization
threshold <- 0.05
# Find the point where the slope is close to zero
stabilized_point5 <- x5[which(abs(derivatives5) <= threshold)][1]

(stabilized_point5 * standev) + mean_xp

plot5_b <- ggplot(
  tab5_b,
  aes(x = Zcumul_xp,
      y = estimate__ / 4)
) +
  geom_vline(
    xintercept = min(tab5_b$Zcumul_xp),
    lty = "dashed",
    color = "#440154"
  ) +
  geom_text(
    aes(label = paste("y =", round(min(tab5_b$estimate__ / 4), digits = 2)),
        y = min(tab5_b$estimate__ / 4),
        x = min(tab5_b$Zcumul_xp) + 0.6),
    color = "#440154",
    size = 5
  ) +
  geom_vline(
    xintercept = tab5_b[Zcumul_xp == stabilized_point5]$Zcumul_xp,
    lty = "dashed",
    color = "#440154"
  ) +
  geom_text(
    aes(label = paste(
      "y =",
      format(
        round(tab5_b[Zcumul_xp == stabilized_point5]$estimate__ / 4, digits = 2),
        nsmall = 2
      )
    ),
    y = (tab5_b[Zcumul_xp == stabilized_point5]$estimate__ / 4) + 0.10,
    x = tab5_b[Zcumul_xp == stabilized_point5]$Zcumul_xp - 0.55),
    color = "#440154",
    size = 5
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
  ggtitle("Model V: rank + speed") +
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



# Model 6 rank + speed + space ---------------------------------------------

# Predicted values from the GAMM
predicted_values6 <- tab6_b$estimate__

# Define the x-axis range
x6 <- tab6_b$Zcumul_xp

# Calculate the first derivative using finite differences
dx6 <- mean(diff(x6))
derivatives6 <- diff(predicted_values6) / dx6

# Establish the treshold at a value very close to 0 since its optimization
threshold <- 0.05
# Find the point where the slope is close to zero
stabilized_point6 <- x6[which(abs(derivatives6) <= threshold)][1]

(stabilized_point6 * standev) + mean_xp

plot6_b <- ggplot(
  tab6_b,
  aes(x = Zcumul_xp,
      y = estimate__ / 4)
) +
  geom_vline(
    xintercept = min(tab6_b$Zcumul_xp),
    lty = "dashed",
    color = "#440154"
  ) +
  geom_text(
    aes(label = paste("y =", round(min(tab6_b$estimate__ / 4), digits = 2)),
        y = min(tab6_b$estimate__ / 4),
        x = min(tab6_b$Zcumul_xp) + 0.6),
    color = "#440154",
    size = 5
  ) +
  geom_vline(
    xintercept = tab6_b[Zcumul_xp == stabilized_point6]$Zcumul_xp,
    lty = "dashed",
    color = "#440154"
  ) +
  geom_text(
    aes(label = paste(
      "y =",
      format(
        round(tab6_b[Zcumul_xp == stabilized_point6]$estimate__ / 4, digits = 2),
        nsmall = 2
      )
    ),
    y = (tab6_b[Zcumul_xp == stabilized_point6]$estimate__ / 4) + 0.10,
    x = tab6_b[Zcumul_xp == stabilized_point6]$Zcumul_xp - 0.55),
    color = "#440154",
    size = 5
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
  NULL, plot2_b, NULL, plot5_b, NULL, plot6_b, 
  NULL, plot2_a, NULL, plot5_a, NULL, plot6_a,
  ncol = 6, nrow = 2,
  labels = c(
    "(A)", "", "(B)", "", "(C)", "",
    "(D)", "", "(E)", "", "(F)", ""
  ),
  widths = c(
    0.15, 1.5, 0.15, 1.5, 0.15, 1.5,
    0.15, 1.5, 0.15, 1.5, 0.15, 1.5
  )
)



# Export the figure -----------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

# If using Windows
#ggexport(
#  figure,
#  filename = file.path(path, "figure1.png"),
#  width = 4000,
#  height = 2200,
#  res = 300
#)

# If using linux
ggsave(
  filename = file.path(path, "figure1.png"),
  plot = figure,
  width = 4000 / 300,
  height = 2200 / 300,
  dpi = 300,
  device = grDevices::png,
  type = "cairo-png"
)

# ==========================================================================
# ==========================================================================