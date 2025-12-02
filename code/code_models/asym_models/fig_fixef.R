# ==========================================================================

#                         Plot fixed effects parameters
#                                 Figure X

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

fit3 <- readRDS(file.path(path, "asym-III.rds"))
fit4 <- readRDS(file.path(path, "asym-IV.rds"))
fit5 <- readRDS(file.path(path, "asym-V.rds"))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Extract posterior draws into a tidy table
# ==========================================================================

# Function
make_ce_df <- function(fit, nlpar, effects, labels,
                       prob = 0.89,
                       method = "fitted",
                       robust = TRUE,
                       re_formula = NULL) {
  if (length(effects) != length(labels)) {
    stop("effects and labels must have the same length.")
  }

  out_list <- vector("list", length(effects))

  for (i in seq_along(effects)) {
    ce <- conditional_effects(
      x = fit,
      effects = effects[i],
      nlpar = nlpar,
      prob = prob,
      robust = robust,
      re_formula = re_formula,
      method = method
    )[[1]]

    # variable label
    ce$variable <- labels[i]

    # Columns for plotting
    ce <- ce[, c("variable", "effect1__", "estimate__", "lower__", "upper__")]

    out_list[[i]] <- ce
  }

  # bind into one data frame
  do.call(rbind, out_list)
}

# Select traits ------------------------------------------------------------

effects_prey  <- c("Zprey_avg_rank", "Zprey_speed", "Zprey_space")
labels_prey <- c("prey_rank", "prey_speed", "prey_space")



# Prepare tables -----------------------------------------------------------

df_a <- make_ce_df(
  fit = fit5,
  nlpar = "a",
  effects = effects_prey,
  labels  = labels_prey
)

df_c <- make_ce_df(
  fit = fit5,
  nlpar = "c",
  effects = effects_prey,
  labels = labels_prey
)



# ==========================================================================
# ==========================================================================




# ==========================================================================
# 3. Plot the posterior distribution of predator means
# ==========================================================================

# Prepare figure -----------------------------------------------------------

panel_names <- c(
  prey_rank = "Average prey rank",
  prey_speed = "Average prey speed",
  prey_space = "Average prey space coverage"
)

fixef_a <- ggplot(df_a, aes(x = effect1__, y = estimate__)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  scale_y_continuous(
    breaks = seq(-2, 2, 0.5),
    limits = c(-1.5, 2.5)
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_x",
    labeller = labeller(variable = panel_names)
  ) +
  labs(
    title = "Effect of prey traits on asymptotic parameter (a)",
    x = "Predictor (scaled)",
    y = "Asymptote: maximum hunting success (linear scale)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 12, face="bold")
  )
fixef_a


fixef_c <- ggplot(df_c, aes(x = effect1__, y = estimate__)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  scale_y_continuous(
    breaks = seq(-2, 2, 0.5),
    limits = c(-1, 1)
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_x",
    labeller = labeller(variable = panel_names)
  ) +
  labs(
    title = "Effect of prey traits on rate of gain parameter (c)",
    x = "Predictor (scaled)",
    y = "Rate: gain in expertise acquisition (linear scale)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 12, face="bold")
  )
fixef_c


path_to_save <- file.path(getwd(), "test")
ggsave(
    plot=fixef_a,
    filename=file.path(path_to_save, "fig_fixef_a_asym.png"),
    dpi=300, height=6, width=12
)

ggsave(
    plot=fixef_c,
    filename=file.path(path_to_save, "fig_fixef_c_asym.png"),
    dpi=300, height=6, width=12
)

# ==========================================================================
# ==========================================================================