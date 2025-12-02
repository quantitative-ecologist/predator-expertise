getwd()
library(brms)
library(data.table)
library(ggplot2)
library(viridis)
library(ggpubr)

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

data <- fread(
  file.path(getwd(), "data","FraserFrancoetal2025-data.csv"),
  select = c("predator_id",
             "hunting_success",
             "prey_avg_speed",
             "game_duration",
             "cumul_xp_pred",
             "prey_avg_rank",
             "prey_avg_amount_tiles_visited")
)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]

# Remove any NAs
data <- data[complete.cases(data)]

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[
  ,
  c("Zprey_speed", "Zgame_duration", "Zcumul_xp", "Zprey_avg_rank", "Zprey_space") := lapply(
    .SD, standardize
  ),
  .SDcols = c("prey_avg_speed", "game_duration", "cumul_xp_pred", "prey_avg_rank", "prey_avg_amount_tiles_visited")
]


path <- file.path(getwd(), "outputs", "outputs_models")
fit1 <- readRDS(file.path(path, "asym-I.rds"))
fit2 <- readRDS(file.path(path, "asym-II.rds"))
fit3 <- readRDS(file.path(path, "asym-III.rds"))
fit4 <- readRDS(file.path(path, "asym-IV.rds"))
fit5 <- readRDS(file.path(path, "asym-V.rds"))

plot(fit1)
plot(fit2)
plot(fit3)
plot(fit4)
plot(fit5)


summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)


conditional_effects(fit4)

plot_dat1 <-data.table(conditional_effects(fit1)[[1]])
plot_dat2 <-data.table(conditional_effects(fit2)[[1]])

p1 <- ggplot(data=plot_dat1, aes(y = estimate__/4, x = Zcumul_xp)) +
  geom_ribbon(
    aes(
      ymin = lower__ / 4,
      ymax = upper__ / 4
    ),
    fill = "gray"
  ) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  )

p2 <- ggplot(data=plot_dat2, aes(y = estimate__/4, x = Zcumul_xp)) +
  geom_ribbon(
    aes(
      ymin = lower__ / 4,
      ymax = upper__ / 4
    ),
    fill = "gray"
  ) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  )
p1
p2

pred_ids <- data[, unique(predator_id)]
conds <- data.frame(
  predator_id = pred_ids,
  Zgame_duration = 0,  # or some reference value
  Zprey_avg_rank = 0   # (if z-scored, 0 = mean)
)

ce_pred2 <- conditional_effects(
  fit2,
  effects = "Zcumul_xp",
  conditions = conds,
  re_formula = NULL # keep group-level effects
)

ce_pred3 <- conditional_effects(
  fit3,
  effects = "Zcumul_xp",
  conditions = conds,
  re_formula = NULL # keep group-level effects
)

df_ce2 <- as.data.table(ce_pred2$Zcumul_xp)
df_ce3 <- as.data.table(ce_pred3$Zcumul_xp)

curves2 <-ggplot(df_ce2, aes(x = Zcumul_xp, y = estimate__/4, colour = predator_id)) +
  geom_line(linewidth = 0.5, alpha = 0.25) +
  scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  labs(
    x = "Cumulative experience (Zcumul_xp)",
    y = "Expected number of prey captured"
  ) +
  custom_theme

curves3<- ggplot(df_ce3, aes(x = Zcumul_xp, y = estimate__/4, colour = predator_id)) +
  geom_line(linewidth = 0.5, alpha = 0.25) +
  scale_color_viridis(discrete = TRUE, option = "D") + #B
  ylab("Hunting success\n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     limits = c(0, 1)) +
  labs(
    x = "Cumulative experience (Zcumul_xp)",
    y = "Expected number of prey captured"
  ) +
  custom_theme

curves2
curves3

ggarrange(curves2, curves3)


# random effects -------------------------

draws5 <- as_draws_df(fit5)
draws_dt5 <- as.data.table(draws5)
draws4 <- as_draws_df(fit4)
draws_dt4 <- as.data.table(draws4)
draws3 <- as_draws_df(fit3)
draws_dt3 <- as.data.table(draws3)

summarise_param_dt <- function(draws_dt, par_short) {
  # par_short: "a", "b", or "c"
  
  # Global (population-level) intercept column, e.g. "b_a_Intercept"
  global_col <- paste0("b_", par_short, "_Intercept")
  
  # Random-effect columns for this parameter, e.g. "r_predator_id__a[ ... ]"
  re_cols <- grep(
    paste0("^r_predator_id__", par_short, "\\["),
    names(draws_dt),
    value = TRUE
  )
  
  # Keep only needed columns + a draw index
  sub_dt <- draws_dt[, c(global_col, re_cols), with = FALSE]
  sub_dt[, .draw := .I]
  
  # Melt to long format: one row per (draw, predator)
  long_dt <- melt(
    sub_dt,
    id.vars = c(".draw", global_col),
    measure.vars = re_cols,
    variable.name = "re_name",
    value.name = "re"
  )
  
  # Extract predator_id from column names:
  # e.g. "r_predator_id__a[pred143150,Intercept]" -> "pred143150"
  long_dt[, predator_id := sub(
    paste0("r_predator_id__", par_short, "\\[([^,]+),Intercept\\]"),
    "\\1",
    re_name
  )]
  
  # Predator-specific parameter value = global intercept + random effect
  long_dt[, value := get(global_col) + re]
  
  # Summaries by predator
  pred_summ <- long_dt[
    ,
    .(
      med = median(value),
      q5.5  = quantile(value, 0.055),
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

a_res5 <- summarise_param_dt(draws_dt5, "a")
b_res5 <- summarise_param_dt(draws_dt5, "b")
c_res5 <- summarise_param_dt(draws_dt5, "c")
a_res4 <- summarise_param_dt(draws_dt4, "a")
b_res4 <- summarise_param_dt(draws_dt4, "b")
c_res4 <- summarise_param_dt(draws_dt4, "c")
a_res3 <- summarise_param_dt(draws_dt3, "a")
b_res3 <- summarise_param_dt(draws_dt3, "b")
c_res3 <- summarise_param_dt(draws_dt3, "c")

a_dt5 <- a_res5$pred[, `:=`(param = "a", global_med = a_res5$global_med)]
b_dt5 <- b_res5$pred[, `:=`(param = "b", global_med = b_res5$global_med)]
c_dt5 <- c_res5$pred[, `:=`(param = "c", global_med = c_res5$global_med)]
a_dt4 <- a_res4$pred[, `:=`(param = "a", global_med = a_res4$global_med)]
b_dt4 <- b_res4$pred[, `:=`(param = "b", global_med = b_res4$global_med)]
c_dt4 <- c_res4$pred[, `:=`(param = "c", global_med = c_res4$global_med)]
a_dt3 <- a_res3$pred[, `:=`(param = "a", global_med = a_res3$global_med)]
b_dt3 <- b_res3$pred[, `:=`(param = "b", global_med = b_res3$global_med)]
c_dt3 <- c_res3$pred[, `:=`(param = "c", global_med = c_res3$global_med)]

all_dt5 <- rbindlist(list(a_dt5, b_dt5, c_dt5))
all_dt4 <- rbindlist(list(a_dt4, b_dt4, c_dt4))
all_dt3 <- rbindlist(list(a_dt3, b_dt3, c_dt3))


panel_names <- c(
  a = "a: Asymptote (long-term hunting success)",
  b = "b: Baseline (initial hunting success)",
  c = "c: Rate (expertise acquisition)"
)

ranef3 <- ggplot(all_dt3, aes(x = reorder(predator_id, med), y = med)) +
  geom_hline(aes(yintercept = global_med),
             linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = q5.5, ymax = q94.5), width = 0) +
  coord_flip() +
  facet_wrap(~ param, scales = "free_x", labeller = labeller(param = panel_names)) +
  labs(
    x = "Predator",
    y = "Parameter value (logit scale)",
    title = "Predator-specific a, b, c vs global intercept"
  ) +
  theme_bw() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

ranef4 <- ggplot(all_dt4, aes(x = reorder(predator_id, med), y = med)) +
  geom_hline(aes(yintercept = global_med),
             linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = q5.5, ymax = q94.5), width = 0) +
  coord_flip() +
  facet_wrap(~ param, scales = "free_x", labeller = labeller(param = panel_names)) +
  labs(
    x = "Predator",
    y = "Parameter value (logit scale)",
    title = "Predator-specific a, b, c vs global intercept"
  ) +
  theme_bw() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

ranef5 <- ggplot(all_dt5, aes(x = reorder(predator_id, med), y = med)) +
  geom_hline(aes(yintercept = global_med),
             linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = q5.5, ymax = q94.5), width = 0) +
  coord_flip() +
  facet_wrap(~ param, scales = "free_x", labeller = labeller(param = panel_names)) +
  labs(
    x = "Predator",
    y = "Parameter value (logit scale)",
    title = "Predator-specific a, b, c vs global intercept"
  ) +
  theme_bw() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

ranef3
ranef4

ggarrange(ranef3, ranef5)

summary(fit3)
summary(fit4)


