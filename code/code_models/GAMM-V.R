# ==========================================================================

#           GAMM model GS - group-level smoother - hunting success
#                           Model V in article

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

# Load data
data <- fread(
  file.path(folder, "FraserFrancoetalXXXX-data.csv"),
  select = c("predator_id",
             "hunting_success",
             "prey_avg_speed",
             "game_duration",
             "cumul_xp_pred",
             "prey_avg_rank")
)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]

# Remove any NAs
data <- data[complete.cases(data)]



# Standardise the variables (Z-scores) -------------------------------------

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[
  ,
  c("Zprey_speed", "Zgame_duration", "Zcumul_xp", "Zprey_avg_rank") := lapply(
    .SD, standardize
  ),
  .SDcols = c("Zprey_speed", "Zgame_duration", "Zcumul_xp", "Zprey_avg_rank")
]


# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Parametrize the model
# ==========================================================================


# Model formula ------------------------------------------------------------

model_formula <- brmsformula(
  hunting_success | trials(4) ~
    s(Zcumul_xp) +
    s(Zcumul_xp, predator_id, bs = "fs") +
    Zgame_duration +
    Zprey_speed +
    Zprey_avg_rank
)



# Inspect priors -----------------------------------------------------------

get_prior(
  formula = model_formula,
  family = beta_binomial(),
  data = data
)



# Define priors ------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior(
    "normal(1, 0.5)",
    class = "b",
    coef = "Zgame_duration"),
  set_prior(
    "normal(-1, 0.5)",
    class = "b",
    coef = "Zprey_speed"
  ),
  set_prior(
    "normal(0, 1)",
    class = "b",
    coef = "Zprey_avg_rank"
  ),
  set_prior(
    "normal(0, 2)",
    class = "b",
    coef = "sZcumul_xp_1"
  ),
  # prior on the intercept
  set_prior(
    "normal(0, 0.5)",
    class = "Intercept"
  ),
  # prior on sds parameters
  set_prior(
    "normal(0, 0.5)",
    class = "sds"
  ),
  # priors on phi
  set_prior(
    "normal(2, 0.5)",
    class = "phi"
  )
)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Run the model
# ==========================================================================

fit <- brm(
  formula = model_formula,
  family = beta_binomial(),
  warmup = 500,
  iter = 1500,
  thin = 1,
  chains = 4,
  threads = threading(8),
  backend = "cmdstanr",
  init = "0",
  seed = 123,
  prior = priors,
  sample_prior = FALSE,
  control = list(adapt_delta = 0.99),
  data = data
)

saveRDS(fit, file = "GAMM-V.rds")

# ==========================================================================
# ==========================================================================