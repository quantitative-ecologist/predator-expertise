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

 path <- file.path(getwd(), "outputs", "01_outputs_models")

 modA2 <- readRDS(file.path(path, "A2_GAMM-rank.rds"))
 modA2_prey <- readRDS(file.path(path, "A2_GAMM-speed-rank.rds"))



# Load the data ------------------------------------------------------------

 data <- fread("./data/FraserFrancoetal2023-data.csv",
               select = c("predator_id",
                          "game_duration",
                          "pred_speed",
                          "prey_avg_speed",
                          "prey_avg_rank",
                          "cumul_xp_pred",
                          "total_xp_pred",
                          "hunting_success"))

 data[, predator_id := as.factor(predator_id)]



# Post processing preparations for custom family ---------------------------

 expose_functions(modA2, vectorize = TRUE)

 # Define the log likelihood function
 log_lik_beta_binomial2 <- function(i, prep) {
   mu <- brms::get_dpar(prep, "mu", i = i)
   phi <- brms::get_dpar(prep, "phi", i = i)
   trials <- prep$data$vint1[i]
   y <- prep$data$Y[i]
   beta_binomial2_lpmf(y, mu, phi, trials)
 }

 # Define function for posterior_predict
 posterior_predict_beta_binomial2 <- function(i, prep, ...) {
   mu <- brms::get_dpar(prep, "mu", i = i)
   phi <- brms::get_dpar(prep, "phi", i = i)
   trials <- prep$data$vint1[i]
   beta_binomial2_rng(mu, phi, trials)
 }

 # Define function for posterior_epred
 posterior_epred_beta_binomial2 <- function(prep) {
   mu <- brms::get_dpar(prep, "mu")
   trials <- prep$data$vint1
   trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
   mu * trials
 }

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Prepare the data for the plots
# ==========================================================================



# Prepare the plots --------------------------------------------------------

 # Group-level smooths model A2
 tabA2_a <- conditional_effects(
  modA2, method = "fitted",
  effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
 )
 tabA2_a <- data.table(tabA2_a[[1]])

 # Cumulative XP global trend model A2
 tabA2_b <- conditional_effects(
  modA2, method = "fitted",
  robust = TRUE, re_formula = NULL,
  effects = "Zcumul_xp",
  conditions = data.frame(predator_id = NA)
 )
 tabA2_b <- data.table(tabA2_b[[1]])

 # Group-level smooths model A2prey
 tabA2_prey_a <- conditional_effects(
  modA2_prey, method = "fitted",
  effects = "Zcumul_xp:predator_id",
  robust = TRUE, re_formula = NULL
 )
 tabA2_prey_a <- data.table(tabA2_prey_a[[1]])

 # Cumulative XP global trend model A2prey
 tabA2_prey_b <- conditional_effects(
  modA2_prey, method = "fitted",
  robust = TRUE, re_formula = NULL,
  effects = "Zcumul_xp",
  conditions = data.frame(predator_id = NA)
 )
 tabA2_prey_b <- data.table(tabA2_prey_b[[1]])



# Transform values --------------------------------------------------------

 # Back transform x-axis values
 sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_pred))
 standev <- sd(data$cumul_xp_pred)
 scaled_breaks <- sequence / standev

 # List the tables
 tables <- list(
  tabA2_a, tabA2_b,
  tabA2_prey_a, tabA2_prey_b
 )
 names(tables) <- c(
  "tabA2_a", "tabA2_b",
  "tabA2_prey_a", "tabA2_prey_b"
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
 tabA2_a <- merge(tabA2_a, xp, by = "predator_id")
 tabA2_prey_a <- merge(tabA2_prey_a, xp, by = "predator_id")

 # Cut all matches where fitted values are above total XP
 tabA2_a <- tabA2_a[cumul_xp <= total_xp_pred]
 tabA2_prey_a <- tabA2_prey_a[cumul_xp <= total_xp_pred, ]



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
# 3. Individual variance plots
# ==========================================================================


# Model A2 rank only -------------------------------------------------------

 plotA2_a <- ggplot(tabA2_a,
                      aes(x = Zcumul_xp,
                          y = estimate__ / 4,
                          color = predator_id)) +
    geom_line(linewidth = 1, alpha = 0.5) +
    scale_color_viridis(discrete = TRUE, option = "D") + #B
    ylab("Hunting success\n") +
    ggtitle("Prey rank") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = scaled_breaks,
                       labels = seq(0, 500, 100)) +
    xlab("\nCumulative experience") +
    custom_theme



# Model A2 rank + speed ----------------------------------------------------

 plotA2p_a <- ggplot(tabA2_prey_a,
                      aes(x = Zcumul_xp,
                          y = estimate__ / 4,
                          color = predator_id)) +
    geom_line(linewidth = 1, alpha = 0.5) +
    scale_color_viridis(discrete = TRUE, option = "D") + #B
    ylab("Hunting success\n") +
    ggtitle("Prey rank + prey speed") +
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


# Model A2 rank only -------------------------------------------------------

 plotA2_b <- ggplot(
    tabA2_b,
    aes(x = Zcumul_xp,
        y = estimate__ / 4)
    ) +
     geom_vline(
     xintercept = min(tabA2_b$Zcumul_xp),
     lty = "dashed",
     color = "#5ec962") +
   geom_text(
     aes(label = paste("y =", round(min(tabA2_b$estimate__ / 4), digits = 2)),
         y = min(tabA2_b$estimate__ / 4),
         x = min(tabA2_b$Zcumul_xp) + 0.6),
     color = "#5ec962",
     size = 5
     ) +
   geom_vline(
     xintercept = tabA2_b[which.max(estimate__), Zcumul_xp],
     lty = "dashed",
     color = "#440154") +
   geom_text(
     aes(label = paste("y =", format(round(max(tabA2_b$estimate__ / 4), digits = 2), nsmall = 2)),
         y = max(tabA2_b$estimate__ / 4) + 0.10,
         x = tabA2_b[which.max(estimate__), Zcumul_xp] + 0.55),
     color = "#440154",
     size = 5
   ) +
    geom_ribbon(
      aes(
        ymin = lower__ / 4,
        ymax = upper__ / 4),
      fill = "gray") +
    geom_line(linewidth = 1) +
    ylab("Hunting success\n") +
    ggtitle("Prey rank") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = scaled_breaks,
                       labels = seq(0, 500, 100)) +
    xlab("\nCumulative experience") +
    custom_theme



# Model A2 rank + speed ----------------------------------------------------

 # Predicted values from the GAMM
 predicted_values <- tabA2_prey_b$estimate__

 # Define the x-axis range
 x <- tabA2_prey_b$Zcumul_xp

# Fit a spline to the predicted values
spline_fit <- splinefun(x, predicted_values)

 # Calculate the first derivative using finite differences
 dx <- mean(diff(x))
 derivatives <- diff(predicted_values) / dx

 # Establish the treshold at a value very close to 0 since its optimization
 threshold <- 0.0088
 # Find the point where the slope is close to zero
 stabilized_point <- x[which(abs(derivatives) <= threshold)][1]

 plotA2p_b <- ggplot(
   tabA2_prey_b,
   aes(x = Zcumul_xp,
       y = estimate__ / 4)
   ) +
   geom_vline(
     xintercept = min(tabA2_prey_b$Zcumul_xp),
     lty = "dashed",
     color = "#5ec962") +
   geom_text(
     aes(label = paste("y =", round(min(tabA2_prey_b$estimate__ / 4), digits = 2)),
         y = min(tabA2_prey_b$estimate__ / 4),
         x = min(tabA2_prey_b$Zcumul_xp) + 0.6),
     color = "#5ec962",
     size = 5
     ) +
   geom_vline(
     xintercept = tabA2_prey_b[Zcumul_xp == stabilized_point]$Zcumul_xp,
     lty = "dashed",
     color = "#440154") +
   geom_text(
     aes(label = paste(
      "y =",
      format(
        round(tabA2_prey_b[Zcumul_xp == stabilized_point]$estimate__ / 4, digits = 2),
        nsmall = 2
        )
      ),
         y = (tabA2_prey_b[Zcumul_xp == stabilized_point]$estimate__ / 4) + 0.10,
         x = tabA2_prey_b[Zcumul_xp == stabilized_point]$Zcumul_xp + 0.3),
     color = "#440154",
     size = 5
   ) +
   geom_ribbon(
     aes(
       ymin = lower__ / 4,
       ymax = upper__ / 4),
     fill = "gray") +
   geom_line(linewidth = 1) +
   ylab("Hunting success\n") +
   ggtitle("Prey rank + prey speed") +
   scale_y_continuous(breaks = seq(0, 1, 0.25),
                      limits = c(0, 1)) +
   scale_x_continuous(breaks = scaled_breaks,
                      labels = seq(0, 500, 100)) +
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
    NULL, plotA2_b, NULL, plotA2_a,
    NULL, plotA2p_b, NULL, plotA2p_a,
    ncol = 4, nrow = 2,
    labels = c(
      "(A)", "", "(B)", "",
      "(C)", "", "(D)", ""
    ),
    widths = c(
      0.15, 1.5, 0.15, 1.5,
      0.15, 1.5, 0.15, 1.5
    )
 )

# Export the figure -----------------------------------------------------

 path <- file.path(getwd(), "outputs", "04_outputs_figures")

 ggexport(
  figure,
  filename = file.path(path, "figure1.png"),
  width = 2700,
  height = 2200,
  res = 300
)

# ==========================================================================
# ==========================================================================