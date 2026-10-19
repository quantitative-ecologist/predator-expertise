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


# 4. Function to build a parameter comparison table across models
extract_param_table <- function(fits, prob = 0.89, digits = 2) {
  alpha <- (1 - prob) / 2
  q_lo  <- alpha
  q_hi  <- 1 - alpha

  rows <- data.frame(
    section = c(
      rep("Asymptotic parameters", 5),
      rep("Covariates on hunting success", 2),
      rep("Prey behaviour on a", 2),
      rep("Prey behaviour on c", 2),
      rep("Predator random effects (SD)", 3),
      rep("Predator random effects (correlation)", 3),
      "Overdispersion"
    ),
    parameter = c(
      "a (maximum success)", "a (maximum success)",
      "b (initial success)", "b (initial success)",
      "c (rate of gain)",
      "game duration", "prey rank",
      "prey speed", "prey space",
      "prey speed", "prey space",
      "SD(a)", "SD(b)", "SD(c)",
      "cor(a, b)", "cor(a, c)", "cor(b, c)",
      "phi"
    ),
    scale = c(
      "logit", "probability",
      "logit", "probability",
      "log",
      "logit", "logit",
      "logit", "logit",
      "log", "log",
      "logit", "logit", "log",
      "—", "—", "—",
      "—"
    ),
    variable = c(
      "b_a_Intercept", "b_a_Intercept",
      "b_b_Intercept", "b_b_Intercept",
      "b_c_Intercept",
      "b_betaduration_Intercept", "b_betarank_Intercept",
      "b_a_Zprey_speed", "b_a_Zprey_space",
      "b_c_Zprey_speed", "b_c_Zprey_space",
      "sd_predator_id__a_Intercept",
      "sd_predator_id__b_Intercept",
      "sd_predator_id__c_Intercept",
      "cor_predator_id__a_Intercept__b_Intercept",
      "cor_predator_id__a_Intercept__c_Intercept",
      "cor_predator_id__b_Intercept__c_Intercept",
      "phi"
    ),
    transform = c(
      "identity", "plogis",
      "identity", "plogis",
      rep("identity", 14)
    ),
    stringsAsFactors = FALSE
  )

  fmt <- function(x) {
    sprintf(
      paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
      median(x),
      unname(stats::quantile(x, probs = q_lo)),
      unname(stats::quantile(x, probs = q_hi))
    )
  }

  out <- rows[, c("section", "parameter", "scale")]

  for (m in names(fits)) {
    # Only extract the variables present in this model
    vars <- intersect(unique(rows$variable), brms::variables(fits[[m]]))
    dr <- posterior::as_draws_df(fits[[m]], variable = vars)

    out[[m]] <- vapply(seq_len(nrow(rows)), function(i) {
      v <- rows$variable[i]
      if (!v %in% vars) return("—")
      x <- dr[[v]]
      if (rows$transform[i] == "plogis") x <- plogis(x)
      fmt(x)
    }, character(1))
  }

  out
}
