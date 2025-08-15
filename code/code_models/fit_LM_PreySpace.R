# ==========================================================================

#                Model how prey speed changes with prey rank

# ==========================================================================



# ==========================================================================
# 1. Prepare script
# ==========================================================================



# Load libraries and model -------------------------------------------------

options(mc.cores = parallel::detectCores())

library(parallel)
library(brms)
library(data.table)



# Load the data ------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2025-data.csv",
  select = c(
    "predator_id",
    "game_duration",
    "prey_avg_amount_tiles_visited",
    "prey_avg_rank"
  )
)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]

# Remove any NAs
data <- data[complete.cases(data)]



# Standardise the variables (Z-scores) -------------------------------------

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[, c("Zgame_duration", "Zprey_avg_rank") := lapply(.SD, standardize),
  .SDcols = c(2, 4)
]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Parametrize the model
# ==========================================================================



# Model formula ------------------------------------------------------------

model_formula <- brmsformula(
  prey_avg_amount_tiles_visited ~ Zprey_avg_rank + Zgame_duration + (1 | predator_id)
)



# Inspect priors -----------------------------------------------------------

get_prior(
  formula = model_formula,
  family = gaussian(),
  data = data
)



# Define priors ------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior(
    "normal(0, 0.5)",
    class = "b",
    coef = "Zgame_duration"
  ),
  set_prior(
    "normal(-0.5, 1)",
    class = "b",
    coef = "Zprey_avg_rank"
  ),
  # prior on the intercept
  set_prior(
    "normal(15, 0.5)",
    class = "Intercept"
  ),
  # prior on sd parameters
  set_prior(
    "normal(0, 0.5)",
    class = "sd"
  ),
  # prior on sigma
  set_prior(
    "normal(0, 0.5)",
    class = "sigma"
  )
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Run the model
# ==========================================================================

fit <- brm(
  formula = model_formula,
  family = gaussian(),
  warmup = 500,
  iter = 1500,
  thin = 1,
  chains = 4,
  threads = threading(4),
  backend = "cmdstanr",
  init = "0",
  seed = 123,
  prior = priors,
  sample_prior = TRUE,
  control = list(adapt_delta = 0.99),
  data = data
)

path <- file.path(getwd(), "outputs", "outputs_models")
saveRDS(fit, file = file.path(path, "LM-PreySpace.rds"))

# ==========================================================================
# ==========================================================================