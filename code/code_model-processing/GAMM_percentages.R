# ==========================================================================

#                       Code to calculate percentages                      #

# ==========================================================================





# ==========================================================================
# 1. Prepare script
# ==========================================================================


# Load libraries and models ------------------------------------------------

library(brms)
library(data.table)

path <- file.path(getwd(), "outputs", "outputs_models")

fit1 <- readRDS(file.path(path, "A2_GAMM-rank.rds"))
fit2 <- readRDS(file.path(path, "A2_GAMM-speed-rank.rds"))



# Load the data ------------------------------------------------------------

data <- fread("./data/FraserFrancoetal2023-data.csv",
              select = c("predator_id",
                         "game_duration",
                         "pred_speed",
                         "prey_avg_speed",
                         "cumul_xp_pred",
                         "total_xp_pred",
                         "hunting_success"))

data[, predator_id := as.factor(predator_id)]



# Post processing preparations for custom family ---------------------------

expose_functions(fit2, vectorize = TRUE)

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
# 2. Prepare data
# ==========================================================================

dat1 <- conditional_effects(
  fit1, method = "fitted", effects = "Zcumul_xp:predator_id",
  robust = FALSE, re_formula = NULL
)

dat2 <- conditional_effects(
  fit2, method = "fitted", effects = "Zcumul_xp:predator_id",
  robust = FALSE, re_formula = NULL
)

# Extract values in a table
tab1 <- data.table(dat1[[1]])
tab2 <- data.table(dat2[[1]])


# Back transform x-axis values
sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_pred))
standev <- sd(data$cumul_xp_pred)
scaled_breaks <- sequence / standev



# Cut fitted values based on player XP ------------------------------------

# Extract player IDs with their total XP from the original data
xp <- unique(data[, .(predator_id, total_xp_pred)])

# Compute non standardized cumulative XP
tab1[, cumul_xp := (Zcumul_xp * standev) + mean(data$cumul_xp_pred)]
tab2[, cumul_xp := (Zcumul_xp * standev) + mean(data$cumul_xp_pred)]

# Now merge the two tables adding the total XP
tab1 <- merge(tab1, xp, by = "predator_id")
tab2 <- merge(tab2, xp, by = "predator_id")

# Cut all matches where fitted values are above total XP
tab1 <- tab1[cumul_xp <= total_xp_pred, ]
tab2 <- tab2[cumul_xp <= total_xp_pred, ]



# Extract players with greatest increase and decrease ---------------------

# Calculate 1st and final value
tab1[, ":=" (
  first = min(Zcumul_xp),
  last = max(Zcumul_xp)),
  by = predator_id]

tab1[, equal1 := ifelse(Zcumul_xp == first, 1, 0), by = predator_id]
tab1[, equal2 := ifelse(Zcumul_xp == last, 1, 0), by = predator_id]

tab2[, ":=" (
  first = min(Zcumul_xp),
  last = max(Zcumul_xp)),
  by = predator_id]

tab2[, equal1 := ifelse(Zcumul_xp == first, 1, 0), by = predator_id]
tab2[, equal2 := ifelse(Zcumul_xp == last, 1, 0), by = predator_id]


# First and last value into a second table
tab1a <- tab1[
    equal1 == 1 | equal2 == 1,
    .(predator_id, Zcumul_xp, estimate__)
]
tab1a[, range := rep(c("min", "max"), 253)]

tab1a <- dcast(
  tab1a,
  predator_id ~ range,
  value.var = "estimate__"
  )

tab2a <- tab2[
    equal1 == 1 | equal2 == 1,
    .(predator_id, Zcumul_xp, estimate__)
]
tab2a[, range := rep(c("min", "max"), 253)]

tab2a <- dcast(
  tab2a,
  predator_id ~ range,
  value.var = "estimate__"
  )


# Calculate the difference in predicted success between minimum and maximum xp
# - value means decrease in success
# + value means increase in success
tab1a[, difference := plogis(tab1a$max) - plogis(tab1a$min)]
tab2a[, difference := plogis(tab2a$max) - plogis(tab2a$min)]

tab1a[
    , c("max", "min")
    := lapply(.SD, function(x) {plogis(x)}),
    .SDcols = c("max", "min")
]

tab2a[
    , c("max", "min")
    := lapply(.SD, function(x) {plogis(x)}),
    .SDcols = c("max", "min")
]

# Extract players with the greatest increase and decrease in success
quantile(tab1a$difference)
quantile(tab2a$difference)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Create a table and save
# ==========================================================================

# Create the table ---------------------------------------------------------

# percentages table
val1 <- c(
length(unique(tab1a[difference < -0.05, predator_id])) / 253,
length(unique(tab1a[difference > 0.05, predator_id])) / 253,
length(
    unique(tab1a[
        difference %between% c(-0.05, 0.05), predator_id]
    )
) / 253
)

val2 <- c(
length(unique(tab2a[difference < -0.05, predator_id])) / 253,
length(unique(tab2a[difference > 0.05, predator_id])) / 253,
length(
    unique(
        tab2a[difference %between% c(-0.05, 0.05), predator_id]
    )
) / 253
)

dat <- data.frame(
    model = c(rep("model1", 3), rep("model2", 3)),
    trend = rep(c(">5% decrease", ">5% increase", "between -5% and 5%"), 2),
    percentage = c(val1, val2)
)



# Save the data.frame ------------------------------------------------------

path <- file.path(getwd(), "outputs", "outputs_model-processing")
saveRDS(dat, file = file.path(path, "GAMM_percentages.rds"))

# ==========================================================================
# ==========================================================================
