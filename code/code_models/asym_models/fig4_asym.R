# ==========================================================================

#                         Plot GAMM sds parameters
#                                 Figure 4

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

fit3 <- readRDS(file.path(path, "asym-III.rds"))
fit4 <- readRDS(file.path(path, "asym-IV.rds"))
fit5 <- readRDS(file.path(path, "asym-V.rds"))

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
  "sd_predator_id__b_Intercept",
  "sd_predator_id__c_Intercept"
)

models <- list(fit3, fit4, fit5)

posterior_results <- list()

for (i in seq_along(models)) {
  posterior_results[[paste0("post", i)]] <- extract_posterior_draws(
    models[[i]],
    variables
  )
}

# Clear memory
#rm(fit1)
#rm(fit2)
#rm(fit5)
#rm(fit6)

# Combine posterior draws
figdat <- rbind(
  #post2,
  posterior_results$post1,
  posterior_results$post2,
  posterior_results$post3
)

# Add model variable
#figdat[, model := c(rep("fit1", 4000), rep("fit2", 4000), rep("fit5", 4000), rep("fit6", 4000))]

figdat[, model := c(rep("fit3", 12000), rep("fit4", 12000), rep("fit5", 12000))]

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
  measure.vars = patterns("^sd_predator_id__"),
  variable.name = "parameter",
  value.name = "value"
)

panel_names <- c(
  a = "a: Asymptote (long-term hunting success)",
  b = "b: Baseline (initial hunting success)",
  c = "c: Rate (expertise acquisition)"
)

figdat_long[
  , parameter := factor(
    parameter,
    levels = c("sd_predator_id__a_Intercept",
               "sd_predator_id__b_Intercept",
               "sd_predator_id__c_Intercept"),
    labels = c("Asymptote:\nlong-term hunting success",
               "Baseline:\ninitial hunting success",
               "Rate:\nexpertise acquisition")
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

fig4 <- p + geom_density_ridges(
  scale=0.75, alpha = 0.5,
  quantile_lines = TRUE,
  quantiles = 2,
  rel_min_height = 0.0009
  ) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.5), limits = c(0, 2)) +
  scale_fill_manual(
    #values = c("#f6d746", "#e55c30", "#781c6d", "#140b34"),
    #labels = c("Model I", "Model II", "Model V", "Model VI")
    values = c("#e55c30", "#781c6d", "#140b34"),
    labels = c("Model III", "Model IV", "Model V")
  ) +
  labs(fill = " ") +
  ylab("") +
  xlab("\nStandard deviation") +
  theme_classic() +
  custom_theme

fig4

path_to_save = file.path(getwd(), "test")
ggsave(plot=fig4, filename=file.path(path_to_save, "fig4_asym.png"), dpi=300, width=10)


# ==========================================================================
# ==========================================================================