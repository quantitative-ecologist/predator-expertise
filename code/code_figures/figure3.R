# ==========================================================================

#                                 Figure 3
#                   Plot random effect means distributions

# ==========================================================================




# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries and model -------------------------------------------------

library(brms)
library(data.table)
library(ggplot2)
library(ggridges)

path <- file.path(getwd(), "outputs", "outputs_models")

fit3 <- readRDS(file.path(path, "asym-II.rds"))
fit4 <- readRDS(file.path(path, "asym-III.rds"))
fit5 <- readRDS(file.path(path, "asym-IV.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws into a tidy table
# ==========================================================================


# Function to process draws ------------------------------------------------

summarise_param_dt <- function(draws_dt, param) {

  # Global intercept column
  global_col <- paste0("b_", param, "_Intercept")

  # Random-effect columns for this parameter
  re_cols <- grep(
    paste0("^r_predator_id__", param, "\\["),
    names(draws_dt),
    value = TRUE
  )

  # Keep only needed columns + a draw index
  sub_dt <- draws_dt[, c(global_col, re_cols), with = FALSE]
  sub_dt[, .draw := .I]

  # Melt to long format
  long_dt <- melt(
    sub_dt,
    id.vars = c(".draw", global_col),
    measure.vars = re_cols,
    variable.name = "re_name",
    value.name = "re"
  )

  # Extract predator_id from column names:
  # e.g. "r_predator_id__a[pred143150,Intercept]" -> "pred143150"
  long_dt[
    , predator_id := sub(
        paste0("r_predator_id__", param, "\\[([^,]+),Intercept\\]"),
        "\\1",
        re_name
    )
  ]

  # Predator-specific parameter value = global intercept + random effect
  long_dt[, value := get(global_col) + re]

  # Summaries by predator
  pred_summ <- long_dt[,
    .(
      med = median(value),
      q5.5 = quantile(value, 0.055),
      q94.5 = quantile(value, 0.945)
    ),
    by = predator_id
  ]

  # Global median (population-level intercept only)
  global_med <- median(draws_dt[[global_col]])

  list(
    pred = pred_summ,
    global_med = global_med
  )
}



# Process draws ------------------------------------------------------------

draws <- as.data.table(as_draws_df(fit5))

a_res <- summarise_param_dt(draws, "a")
b_res <- summarise_param_dt(draws, "b")
c_res <- summarise_param_dt(draws, "c")

a_dt <- a_res$pred[, `:=`(param = "a", global_med = a_res$global_med)]
b_dt <- b_res$pred[, `:=`(param = "b", global_med = b_res$global_med)]
c_dt <- c_res$pred[, `:=`(param = "c", global_med = c_res$global_med)]

all_dt <- rbindlist(list(a_dt, b_dt, c_dt))

# ==========================================================================
# ==========================================================================




# ==========================================================================
# 3. Plot the posterior distribution of predator means
# ==========================================================================

# Prepare figure -----------------------------------------------------------

panel_names <- c(
  a = "a: Asymptote (long-term hunting success)",
  b = "b: Baseline (initial hunting success)",
  c = "c: Rate (expertise acquisition)"
)

ranef <- ggplot(all_dt, aes(x = reorder(predator_id, med), y = med)) +
  geom_hline(
    aes(yintercept = global_med),
    linetype = "dashed",
    color = "purple"
  ) +
  geom_point(fill = "#bebebead", shape = 16) +
  geom_errorbar(aes(ymin = q5.5, ymax = q94.5), width = 0, alpha = 0.5) +
  coord_flip() +
  facet_wrap(
    ~ param,
    scales = "free_x",
    labeller = labeller(param = panel_names)
  ) +
  labs(
    x = "Predator",
    y = "Parameter value"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  )



# Save figure --------------------------------------------------------------

path_to_save <- file.path(getwd(), "outputs", "outputs_models")
ggsave(
  plot = ranef,
  filename = file.path(path_to_save, "figure3.png"),
  dpi = 500,
  height = 6,
  width = 12
)


# ==========================================================================
# ==========================================================================