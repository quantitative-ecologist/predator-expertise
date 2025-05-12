# =======================================================================

#            Compare results of our data to random samples              #

# =======================================================================





# =======================================================================
# 1. Load libraries, and import dataset
# =======================================================================



# Detect number of cores ------------------------------------------------

options(mc.cores = parallel::detectCores())



# Load libraries --------------------------------------------------------

library(data.table)
library(brms)
library(parallel)



# Load the data ------------------------------------------------------------

data <- fread(
  "./data/FraserFrancoetal2023-data-random.csv",
  select = c("predator_id",
             "game_duration",
             "pred_speed",
             "prey_avg_speed",
             "prey_avg_rank",
             "cumul_xp_pred",
             "total_xp_pred",
             "hunting_success")
)

# Predator id as factor
data[, predator_id := as.factor(predator_id)]

# Group players by experience
data[, player_type := "empty"]
data[total_xp_pred < 50, player_type := "early_quitters"]
data[total_xp_pred %between% c(50, 99), player_type := "mid_quitters"]
data[total_xp_pred %between% c(100, 299), player_type := "slightly_engaged"]
data[total_xp_pred >= 300, player_type := "engaged"]

data[, player_type := as.factor(player_type)]
data[, total_xp_pred := player_type]

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the data for the model
# =======================================================================


# Standardise the variables (Z-scores) ----------------------------------

# Apply the function and create new columns
# The function standardizes the variables by group :
# in this case, by level of experience

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data[
  , c("Zcumul_xp_pred", "Zprey_avg_rank", "Zgame_duration") :=
  lapply(.SD, standardize),
  .SDcols = c("cumul_xp_pred", "prey_avg_rank", "game_duration"),
]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the hunting success model for random sample of players
# =======================================================================


# Success model ---------------------------------------------------------

success <- brmsformula(
  hunting_success | trials(4) ~
    0 +
    Zgame_duration +
    Zcumul_xp_pred +
    Zprey_avg_rank +
    total_xp_pred +
    (1 | predator_id)
)



# Prior summary ---------------------------------------------------------

get_prior(
  success,
  data = data,
  family = beta_binomial()
)



# Priors ----------------------------------------------------------------

priors <- c(
  set_prior(
    "normal(1, 0.5)",
    class = "b",
    coef = "Zgame_duration",
  ),
  set_prior(
    "normal(-0.001, 0.5)",
    class = "b",
    coef = c(
      "total_xp_predearly_quitters",
      "total_xp_predengaged",
      "total_xp_predmid_quitters",
      "total_xp_predslightly_engaged"
    )
  ),
  set_prior(
    "normal(-0.5, 1)",
    class = "b",
    coef = "Zprey_avg_rank"
  ),
  set_prior(
    "normal(0.5, 1)",
    class = "b",
    coef = "Zcumul_xp_pred"
  ),
  set_prior(
    "normal(0, 1)",
    class = "sd"
  ),
  set_prior(
    "normal(log(2), 0.5)",
    class = "phi"
  )
)

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Run the model
# =======================================================================

model <- brm(
  formula = success,
  family = beta_binomial(),
  warmup = 500,
  iter = 1500,
  thin = 1,
  chains = 4,
  init = "0",
  threads = threading(3), # for a multicore machine, comment out otherwise
  backend = "cmdstanr",
  seed = 123,
  prior = priors,
  sample_prior = FALSE,
  control = list(adapt_delta = 0.99),
  data = data
)

file_path <- file.path(getwd(), "outouts", "outputs_models")
saveRDS(model, file = "GLM-RandomSample.rds")

# =======================================================================
# =======================================================================