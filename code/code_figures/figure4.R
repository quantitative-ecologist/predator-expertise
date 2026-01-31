# ==========================================================================

#                                 Figure 4
#                 Plot differences in parameters among models

# ==========================================================================

# Figure of the posterior parameters for:
#- Baseline
#- Rate
#- Asymptote



# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries and model -------------------------------------------------

library(brms)
library(data.table)
library(ggplot2)
library(ggridges)

path <- file.path(getwd(), "outputs", "outputs_models")

fit1 <- readRDS(file.path(path, "asym-I.rds"))
fit2 <- readRDS(file.path(path, "asym-II.rds"))
fit3 <- readRDS(file.path(path, "asym-III.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws into a tidy table
# ==========================================================================

# Function to extract posterior draws
extract_posterior_draws <- function(model, variables) {
  post <- data.table(
    as_draws_df(
      model,
      variable = variables
    )
  )
  post
}

# Variables
variables <- c(
  "sd_predator_id__a_Intercept",
  #"sd_predator_id__b_Intercept",
  "sd_predator_id__c_Intercept"
)

models <- list(fit1, fit2, fit3)

posterior_results <- list()

for (i in seq_along(models)) {
  posterior_results[[paste0("post", i)]] <- extract_posterior_draws(
    models[[i]],
    variables
  )
}

# Clear memory
rm(fit1)
rm(fit2)
rm(fit3)

# Combine posterior draws
figdat <- rbind(
  posterior_results$post1,
  posterior_results$post2,
  posterior_results$post3
)

# Add model name
figdat[, model := c(rep("fit1", 12000), rep("fit2", 16000), rep("fit3", 16000))]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Summarize results from the posterior
# ==========================================================================

# Get intervals function
get_hpd_interval <- function(x, prob = 0.95) {
  interval <- coda::HPDinterval(as.mcmc(x), prob)
  list(lower = interval[1], upper = interval[2])
}

# Declare the results function
compute_results <- function(dt, col_name) {
  x <- dt[[col_name]]
  data.table(
    column = col_name,
    median = median(x),
    lower_50 = get_hpd_interval(x, 0.50)$lower,
    upper_50 = get_hpd_interval(x, 0.50)$upper,
    lower_80 = get_hpd_interval(x, 0.80)$lower,
    upper_80 = get_hpd_interval(x, 0.80)$upper,
    lower_95 = get_hpd_interval(x, 0.95)$lower,
    upper_95 = get_hpd_interval(x, 0.95)$upper
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
  measure.vars = patterns("^sd_predator_id__"),
  variable.name = "parameter",
  value.name = "value"
)

panel_names <- c(
  a = "a: Asymptote (maximum hunting success)",
  #b = "b: Baseline (initial hunting success)",
  c = "c: Rate (expertise acquisition)"
)

figdat_long[
  , parameter := factor(
    parameter,
    levels = c("sd_predator_id__a_Intercept",
               #"sd_predator_id__b_Intercept",
               "sd_predator_id__c_Intercept"),
    labels = c("Asymptote (a):\nmaximum hunting success",
               #"Baseline (b):\ninitial hunting success",
               "Rate (c):\nexpertise acquisition")
  )
]
# ==========================================================================
# ==========================================================================





# ==========================================================================
# 5. Plot the posterior distribution of parameters
# ==========================================================================

# Prepare figure -----------------------------------------------------------

# Custom theme
custom_theme <- theme(
  axis.text = element_text(size = 14, color = "black"),
  axis.title = element_text(size = 16),
  strip.text.x = element_text(size = 15),
  axis.ticks.length = unit(.15, "cm"),
  axis.ticks = element_line(linewidth = 0.90, color = "black"),
  axis.line = element_line(linewidth = 0.95, color = "black"),
  panel.grid = element_blank(),
  panel.background = element_blank(),
  legend.title = element_text(size = 13),
  legend.text = element_text(size = 13),
  legend.position = "top"
)

p <- ggplot(
  data = figdat_long,
  aes(x = value, y = parameter, fill = model)
)

fig <- p + geom_density_ridges(
  scale = 0.75,
  alpha = 0.5,
  quantile_lines = TRUE,
  quantiles = 2,
  rel_min_height = 0.0009
) +
  scale_x_continuous(breaks = seq(0, 2, 0.5), limits = c(0, 2)) +
  scale_fill_manual(
    values = c("#e55c30", "#781c6d", "#140b34"),
    labels = c("Model I", "Model II", "Model III")
  ) +
  labs(fill = " ") +
  ylab("") +
  xlab("\nStandard deviation") +
  theme_classic() +
  custom_theme



# Save figure --------------------------------------------------------------

path_to_save <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  plot = fig,
  filename = file.path(path_to_save, "figure4.png"),
  dpi = 500,
  height = 6,
  width = 8
)

# ==========================================================================
# ==========================================================================