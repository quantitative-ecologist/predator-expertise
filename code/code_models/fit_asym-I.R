# ==========================================================================

#                   Asymptotic model - Hunting success
#                               Model I
#               - Game duration as fixed effect of hunting success
#               - rank as fixed effect of hunting success

# ==========================================================================





# ==========================================================================
# 1. Prepare the data
# ==========================================================================



# Load packages ------------------------------------------------------------

# Detect cores
options(mc.cores = parallel::detectCores())

# Load libraries
library(data.table)
library(brms)
library(parallel)



# Load data ----------------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "maxime11", "projects", "def-monti", 
                    "maxime11", "phd_project", "data")

# Load data on compute canada
data <- fread(
  file.path(folder, "FraserFrancoetal2025-data.csv"),
  select = c("predator_id",
             "hunting_success",
             "prey_avg_speed",
             "prey_avg_space_rate",
             "game_duration",
             "cumul_xp_pred",
             "prey_avg_rank")
)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]

# Remove any NAs
data <- data[complete.cases(data)]

# Space rate within 0.2
data <- data[prey_avg_space_rate <= 0.2]



# Standardise the variables (Z-scores) -------------------------------------

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[
  ,
  c("Zprey_speed", "Zgame_duration", "Zprey_avg_rank") := lapply(
    .SD, standardize
  ),
  .SDcols = c("prey_avg_speed", "game_duration", "prey_avg_rank")
]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Parametrize the model
# ==========================================================================



# Model formula ------------------------------------------------------------

model_formula <- brmsformula(
  hunting_success | trials(4) ~
    a - (a - b) * exp(-exp(c) * cumul_xp_pred) +
      betaduration * Zgame_duration +
      betarank * Zprey_avg_rank,

  a ~ 1 + (1 | p | predator_id),
  b ~ 1 + (1 | p | predator_id),
  c ~ 1 + (1 | p | predator_id),

  betaduration ~ 1,
  betarank ~ 1,

  nl = TRUE
)



# Inspect priors -----------------------------------------------------------

get_prior(
  formula = model_formula,
  family = beta_binomial(),
  data = data
)



# Define priors ------------------------------------------------------------

# Values for a and b equivalent to this
qlogis80 <- qlogis(0.80)
qlogis25 <- qlogis(0.25)

priors <- c(
  # Intercept on a, b, c
  prior(normal(1.386294, 0.7), nlpar = "a", class = "b", coef = "Intercept"),
  prior(normal(-1.098612, 0.7), nlpar = "b", class = "b", coef = "Intercept"),
  prior(normal(-5.5, 1), nlpar = "c", class = "b", coef = "Intercept"),

  # Random effects on a, b, c
  prior(normal(0, 1), nlpar = "a", class = "sd"),
  prior(normal(0, 1), nlpar = "b", class = "sd"),
  prior(normal(0, 1), nlpar = "c", class = "sd"),

  # Covariates on hunting success
  prior(normal(0, 0.5), nlpar = "betaduration", class = "b"),
  prior(normal(0, 0.5), nlpar = "betarank", class = "b"),

  # Prior on correlation matrix of random effects
  prior(lkj(2), class = "cor"),

  # Shape parameter
  prior(normal(log(2), 0.5), class = "phi")
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Run the model
# ==========================================================================

fit <- brm(
  formula = model_formula,
  family = beta_binomial(),
  warmup = 1500,
  iter = 7500,
  thin = 2,
  chains = 4,
  threads = threading(8),
  backend = "cmdstanr",
  seed = 123,
  prior = priors,
  sample_prior = FALSE,
  init = "0",
  control = list(adapt_delta = 0.99),
  data = data
)

saveRDS(fit, file = "asym-I.rds")

# ==========================================================================
# ==========================================================================
