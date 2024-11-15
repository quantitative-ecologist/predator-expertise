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

path <- file.path(getwd(), "outputs", "outputs_models")

path <- file.path(getwd(), "outputs", "outputs_models2024")

fit1 <- readRDS(file.path(path, "GAMM-II.rds"))
fit2 <- readRDS(file.path(path, "GAMM-V.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws into a tidy table
# ==========================================================================

# Extract posterior draws fit1
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

# Extract posterior draws fit2
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

# Clear memory
rm(fit1)
rm(fit2)

# Combine posterior draws
figdat <- rbind(
  post1,
  post2
)

# Add model variable
figdat[, model := c(rep("fit1", 1000), rep("fit2", 1000))]

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

# Compute the results function for each model and each column
# Assign to results data.table
results <- figdat[
  , {
    rbind(
      compute_results(.SD, "sds_sZcumul_xppredator_id_1"),
      compute_results(.SD, "sds_sZcumul_xppredator_id_2"),
      compute_results(.SD, "sds_sZcumul_xppredator_id_3")
    )
  }
  , by = model
]
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

# Relabel
results[
  , column := factor(
    column,
    levels = c("sds_sZcumul_xppredator_id_1",
               "sds_sZcumul_xppredator_id_2",
               "sds_sZcumul_xppredator_id_3"),
    labels = c("Individual intercepts",
               "Individual slopes",
               "Individual wiggliness")
  )
]
setnames(results, "column", "parameter")

figdat_long[
  , parameter := factor(
    parameter,
    levels = c("sds_sZcumul_xppredator_id_1",
               "sds_sZcumul_xppredator_id_2",
               "sds_sZcumul_xppredator_id_3"),
    labels = c("Individual intercepts",
               "Individual slopes",
               "Individual wiggliness")
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
  legend.position = "top"
)

# Basic plot variables
p <- ggplot(
  data = figdat_long,
  aes(x = value, fill = model, color = model)
)

# Customize
p <- p + geom_density(
  alpha = 0.4,
  position = "identity",
  trim = TRUE
) +
  geom_vline(
    data = results,
    aes(xintercept = median, color = model),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("#482173FF", "#51C56AFF"),
    labels = c("Prey rank", "Prey rank + prey speed")
  ) +
  scale_fill_manual(
    values = c("#482173FF", "#51C56AFF"),
    labels = c("Prey rank", "Prey rank + prey speed")
  ) +
  facet_wrap(~parameter, scales = "fixed") +
  labs(fill = "Model:", color = "Model:") +
  ylab("Density\n") +
  xlab("\nStandard deviation") +
  theme_bw() +
  custom_theme

# Export the figure --------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  p,
  filename = file.path(path, "figure2.png"),
  units = "px",
  width = 3800,
  height = 1700,
  dpi = 400
)

# ==========================================================================
# ==========================================================================










# Test ----------------------------------------------------------------------

# Testing plot 2 with less information

library(ggridges)

p <- ggplot(data = figdat_long, aes(x = value, y = parameter, fill = model))

#p + geom_violinhalf() #+ geom_jitter(width = 0.2, shape=20)

fig2_test <- p + geom_density_ridges(
  scale=0.75, alpha = 0.5,
  quantile_lines = TRUE,
  quantiles = 2,
  rel_min_height = 0.0009
  ) +
  #geom_vline(
  #  data = results,
  #  aes(xintercept = median, color = model, linetype = parameter),
  #  #linetype = model,
  #  show.legend = FALSE
  #) +
  scale_fill_manual(
    values = c("#482173FF", "#51C56AFF"),
    labels = c("II (Prey rank)", "V (Prey rank + prey speed)")
  ) +
  labs(fill = "Model:") +
  ylab("") +
  xlab("\nStandard deviation") +
  theme_classic() +
  custom_theme

fig2_test

path <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  fig2_test,
  filename = file.path(path, "figure2.png"),
  width = 8,
  height = 6
)
