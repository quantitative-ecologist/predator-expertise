# ==========================================================================

#                         Plot GAMM sds parameters

# ==========================================================================

# Figure of the posterior parameters for:
#- Intercept standard deviation
#- Slope standard deviation
#- Wiggliness standard deviation



# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries and model -------------------------------------------------

library(brms)
library(data.table)
library(ggplot2)
library(ggridges)

path <- file.path(getwd(), "outputs", "outputs_models")

fit1 <- readRDS(file.path(path, "GAMM-I.rds"))
fit2 <- readRDS(file.path(path, "GAMM-II.rds"))
fit5 <- readRDS(file.path(path, "GAMM-V.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws into a tidy table
# ==========================================================================

# Extract posterior draws fit
post1 <- data.table(
  as_draws_df(
    fit1,
    variable = c(
      "sds_sZcumul_xppredator_id_1",
      "sds_sZcumul_xppredator_id_2",
      "sds_sZcumul_xppredator_id_3"
    )
  )
)


# Extract posterior draws fit1
post2 <- data.table(
  as_draws_df(
    fit2,
    variable = c(
      "sds_sZcumul_xppredator_id_1",
      "sds_sZcumul_xppredator_id_2",
      "sds_sZcumul_xppredator_id_3"
    )
  )
)

# Extract posterior draws fit2
post5 <- data.table(
  as_draws_df(
    fit5,
    variable = c(
      "sds_sZcumul_xppredator_id_1",
      "sds_sZcumul_xppredator_id_2",
      "sds_sZcumul_xppredator_id_3"
    )
  )
)

# Clear memory
rm(fit1)
rm(fit2)
rm(fit5)

# Combine posterior draws
figdat <- rbind(
  post1,
  post2,
  post5
)

# Add model variable
figdat[, model := c(rep("fit1", 4000), rep("fit2", 4000), rep("fit5", 4000))]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Summarize results from the posterior
# ==========================================================================

# Get intervals function
get_HPD_interval <- function(x, prob = 0.95) {
  interval <- coda::HPDinterval(as.mcmc(x), prob)
  list(lower = interval[1], upper = interval[2])
}

# Declare the results function
compute_results <- function(dt, col_name) {
  x <- dt[[col_name]]
  data.table(
    column = col_name,
    median = median(x),
    lower_50 = get_HPD_interval(x, 0.50)$lower,
    upper_50 = get_HPD_interval(x, 0.50)$upper,
    lower_80 = get_HPD_interval(x, 0.80)$lower,
    upper_80 = get_HPD_interval(x, 0.80)$upper,
    lower_95 = get_HPD_interval(x, 0.95)$lower,
    upper_95 = get_HPD_interval(x, 0.95)$upper
  )
}

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Prepare table for ploting
# ==========================================================================

# Convert to long format
figdat_long <- melt(
  figdat,
  id.vars = "model",
  measure.vars = patterns("^sds_sZcumul_xppredator_id_"),
  variable.name = "parameter",
  value.name = "value"
)

figdat_long[
  , parameter := factor(
    parameter,
    levels = c("sds_sZcumul_xppredator_id_1",
               "sds_sZcumul_xppredator_id_2",
               "sds_sZcumul_xppredator_id_3"),
    labels = c("Individual\nintercepts",
               "Individual\nslopes",
               "Individual\nwiggliness")
  )
]
# ==========================================================================
# ==========================================================================





# ==========================================================================
# 5. Plot the posterior distribution of sds parameters
# ==========================================================================

# Prepare figure -----------------------------------------------------------

# Custom theme
custom_theme <- theme(
  # axis values size
  axis.text = element_text(size = 15, color = "black"),
  # axis titles size
  axis.title = element_text(size = 17),
  strip.text.x = element_text(size = 15),
  panel.grid = element_blank(),
  panel.background = element_blank(),
  legend.title = element_text(size = 13),
  legend.text = element_text(size = 13),
  #legend.position = c(.73, .9),
  legend.position = "top"
)

p <- ggplot(
  data = figdat_long,
  aes(x = value, y = parameter, fill = model)
)

fig2 <- p + geom_density_ridges(
  scale=0.75, alpha = 0.5,
  quantile_lines = TRUE,
  quantiles = 2,
  rel_min_height = 0.0009
  ) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 11)) +
  scale_fill_manual(
    values = c("#fde725", "#482173FF", "#51C56AFF"),
    labels = c("Model I", "Model II", "Model V")
  ) +
  labs(fill = " ") +
  ylab("") +
  xlab("\nStandard deviation") +
  theme_classic() +
  custom_theme



# Save plot ----------------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  fig2,
  filename = file.path(path, "figure2.png"),
  width = 8.5,
  height = 7.5
)

# ==========================================================================
# ==========================================================================