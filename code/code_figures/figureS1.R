# ==========================================================================

#                               Figure S1

# ==========================================================================

# Figure of the posterior parameters for:
# - Groups of players that are randomly sampled
# - The group studied in the paper



# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries -----------------------------------------------------------

library(data.table)
library(brms)
library(ggplot2)
library(viridis)



# Import model -------------------------------------------------------------

path <- file.path(getwd(), "ouputs", "outputs_models")
fit <- readRDS(
  file.path(path, "DHMLM-RandomSample.rds")
)
# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws
# ==========================================================================

# Extraction with specified parameters
draws <- data.table(
  brms::as_draws_df(
    x = fit,
    variable = c(
      "b_huntingsuccess_total_xp_predearly_quitters",
      "b_huntingsuccess_total_xp_predengaged",
      "b_huntingsuccess_total_xp_predmid_quitters",
      "b_huntingsuccess_total_xp_predslightly_engaged"
    )
  )
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Reshape posterior draws table for plotting
# ==========================================================================

# Convert to long format
drawsl <- melt(
  draws,
  measure.vars = patterns("^b_huntingsuccess"),
  variable.name = "group",
  value.name = "value"
)
drawsl <- drawsl[, c(4,5)] # delete the chain info

# Relabel
drawsl[
  , group := factor(
    group,
    levels = c("b_huntingsuccess_total_xp_predearly_quitters",
               "b_huntingsuccess_total_xp_predengaged",
               "b_huntingsuccess_total_xp_predmid_quitters",
               "b_huntingsuccess_total_xp_predslightly_engaged"),
    labels = c("Group 1",
               "Group 4",
               "Group 2",
               "Group 3")
  )
]

# Back-transform to probabiliy
drawsl[, value_plogis := plogis(value)]

# Calculate median values and back-transform
medians <- drawsl[, .(median = median(value)), by = group]
medians[, median_plogis := plogis(median)]
# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Plot the posterior distributions
# ==========================================================================

# Prepare figure -----------------------------------------------------------

# Custom theme
custom_theme <- theme(
  # axis values size
  axis.text = element_text(size = 14, color = "black"),
  # axis titles size
  axis.title = element_text(size = 16),
  strip.text.x = element_text(size = 14),
  panel.grid = element_blank(),
  panel.background = element_blank(),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.position = "top"
)

# Basic plot variables
p <- ggplot(
  data = drawsl,
  aes(x = value_plogis, color = group, fill = group)
)

# Customize
p <- p + geom_density(
  alpha = 0.4,
  position = "identity",
  trim = TRUE
) +
  geom_vline(
    data = medians,
    aes(xintercept = median_plogis, color = group),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  scale_color_viridis(
    discrete = TRUE,
    breaks = c("Group 1", "Group 2", "Group 3", "Group 4")
  ) +
  scale_fill_viridis(
    discrete = TRUE,
    breaks = c("Group 1", "Group 2", "Group 3", "Group 4")
  ) +
  scale_x_continuous(
    breaks = seq(0.40, 0.60, 0.05),
    limits = c(0.40, 0.60)
  ) +
  labs(fill = "Group:", color = "Group:") +
  ylab("Density\n") +
  xlab("\nHunting success") +
  theme_bw() +
  custom_theme



# Export the figure --------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_figures")

ggsave(
  p,
  filename = file.path(path, "figureS1.png"),
  units = "px",
  #width = 3800,
  #height = 1700,
  #dpi = 400
)

# ==========================================================================
# ==========================================================================