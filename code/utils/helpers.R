# 1. Function to extract transformed parameters with medians
extract_model_params <- function(fit, prob = 0.89) {
  # Calculate quantiles for credible intervals
  alpha <- (1 - prob) / 2
  q_lo  <- alpha
  q_hi  <- 1 - alpha

  # Get posterior draws
  dr <- posterior::as_draws_df(fit)

  # Extract raw parameters (on model scale)
  a_draw <- dr$b_a_Intercept
  b_draw <- dr$b_b_Intercept
  c_draw <- dr$b_c_Intercept

  # Transform
  baseline_draw  <- plogis(b_draw)
  asymptote_draw <- plogis(a_draw)
  rate_draw <- exp(c_draw)

  # Helper to compute median and CIs
  summ <- function(x) {
    c(
      median = median(x),
      lo = unname(stats::quantile(x, probs = q_lo)),
      hi = unname(stats::quantile(x, probs = q_hi))
    )
  }

  # Create a list with raw and transformed parameters
  list(
    # Transformed parameters
    baseline = summ(baseline_draw),
    asymptote = summ(asymptote_draw),
    rate = summ(rate_draw),

    # Raw parameters
    raw_a = summ(a_draw),
    raw_b = summ(b_draw),
    raw_c = summ(c_draw),

    # Store probability level
    prob = prob
  )
}


# 2. Function to extract slopes on parameters 
# (for models with covariates on asym parameters)
extract_param_slopes <- function(fit, prob = 0.89) {
  alpha <- (1 - prob) / 2
  q_lo  <- alpha
  q_hi  <- 1 - alpha

  dr <- posterior::as_draws_df(fit)

  summ <- function(x) {
    c(
      median = median(x),
      lo     = unname(stats::quantile(x, probs = q_lo)),
      hi     = unname(stats::quantile(x, probs = q_hi))
    )
  }

  # Initialize empty list
  slopes <- list()

  if ("b_a_Zprey_speed" %in% names(dr)) {
    slopes$a_prey_speed <- summ(dr$b_a_Zprey_speed)
  }
  if ("b_c_Zprey_speed" %in% names(dr)) {
    slopes$c_prey_speed <- summ(dr$b_c_Zprey_speed)
  }

  if ("b_a_Zprey_space" %in% names(dr)) {
    slopes$a_prey_space <- summ(dr$b_a_Zprey_space)
  }
  if ("b_c_Zprey_space" %in% names(dr)) {
    slopes$c_prey_space <- summ(dr$b_c_Zprey_space)
  }

  if ("b_betaduration_Intercept" %in% names(dr)) {
    slopes$game_duration <- summ(dr$b_betaduration_Intercept)
  }
  if ("b_betarank_Intercept" %in% names(dr)) {
    slopes$prey_rank <- summ(dr$b_betarank_Intercept)
  }

  slopes
}


# 3. Function to extract random effect standard deviations
extract_random_effects <- function(fit, prob = 0.89) {
  alpha <- (1 - prob) / 2
  q_lo  <- alpha
  q_hi  <- 1 - alpha

  vars <- c(
    "sd_predator_id__a_Intercept",
    "sd_predator_id__b_Intercept",
    "sd_predator_id__c_Intercept"
  )

  sum_sd <- posterior_summary(
    fit,
    variable = vars,
    probs = c(q_lo, q_hi),
    robust = TRUE
  )

  list(
    sd_a = c(
      median = sum_sd["sd_predator_id__a_Intercept", "Estimate"],
      lo = sum_sd["sd_predator_id__a_Intercept", 3],
      hi = sum_sd["sd_predator_id__a_Intercept", 4]
    ),
    sd_b = c(
      median = sum_sd["sd_predator_id__b_Intercept", "Estimate"],
      lo = sum_sd["sd_predator_id__b_Intercept", 3],
      hi = sum_sd["sd_predator_id__b_Intercept", 4]
    ),
    sd_c = c(
      median = sum_sd["sd_predator_id__c_Intercept", "Estimate"],
      lo = sum_sd["sd_predator_id__c_Intercept", 3],
      hi = sum_sd["sd_predator_id__c_Intercept", 4]
    ),
    prob = prob
  )
}