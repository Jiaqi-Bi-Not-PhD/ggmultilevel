extract_model <- function(model, data, predictor, outcome, grouping_var) {
  fixed_effects <- fixef(model)

  if ("(Intercept)" %in% names(fixed_effects)) {  ## `if` when no fixed intercept is present started ##
    fixed_intercept <- fixed_effects["(Intercept)"]
  } else {
    fixed_intercept <- 0  # fixed_intercept = 0 when no fixed intercept
  } ## `if` when no fixed intercept is present ended ##

  if (predictor %in% names(fixed_effects)) {
    fixed_slope <- fixed_effects[predictor]
  } else {
    stop(paste("Covariates/Predictors ", predictor, " not found in fixed effects of the model."))
  }

  random_effects_list <- ranef(model)
  if (grouping_var %in% names(random_effects_list)) {
    random_effects <- random_effects_list[[grouping_var]]
  } else {
    stop(paste("Clustered/Grouping variable ", grouping_var, " not found in random effects of the model."))
  }

  random_effects_df <- data.frame(
    Group = rownames(random_effects),
    random_effects,
    stringsAsFactors = FALSE,
    check.names = FALSE  # Here, this prevents R from altering column names
  )

  if ("(Intercept)" %in% colnames(random_effects_df)) {
    colnames(random_effects_df)[colnames(random_effects_df) == "(Intercept)"] <- "RandomIntercept"
  }
  else {
    random_effects_df$RandomIntercept <- 0  # no random intercept, random intercept = 0
  }

  if (predictor %in% colnames(random_effects_df)) {
    colnames(random_effects_df)[colnames(random_effects_df) == predictor] <- "RandomSlope"
  } else {
    random_effects_df$RandomSlope <- 0  # no random slope, random slope = 0
  }

  random_lines <- random_effects_df |>
    mutate(
      Intercept = fixed_intercept + RandomIntercept,
      Slope = fixed_slope + RandomSlope  # mutate all intercepts and slopes to incorporate random effects
    )
  return(random_lines)
}
