#' Title
#'
#' @param model Your multilevel model, should be `lmer`, `glmer` or `glmmTMB`
#' @param data Your data
#' @param predictor The predictor you would like to plot, in string form
#' @param outcome The outcome of the model
#' @param grouping_var The grouping/cluster variable, in string form
#' @param family Declare your family distribution, default is `"gaussian"`
#' @param y_scale Declare your y-axis scale, default is `"response"` which is identical to the model response, e.g., `"odds ratio"` for binomial link
#' @param x_limits Set the x-axis minimum and maximum value
#' @param y_limits Set the y-axis minimum and maximum value
#' @param x_label Set the x-axis name
#' @param y_label Set the y-axis name
#' @param plot_title Set the plot title
#' @param x_breaks Set the breaks of x-axis
#' @param y_breaks Set the breaks of y-axis
#' @param x_num_size Set the x-axis value size
#' @param y_num_size Set the y-axis value size
#'
#' @return a plot
#' @export
#'
#' @examples
plot_glmm <- function(model, data, predictor, outcome, grouping_var,
                      family = "gaussian", y_scale = NULL,
                      x_limits = NULL, y_limits = NULL,
                      x_label = NULL, y_label = NULL,
                      plot_title = NULL,
                      x_breaks = NULL, y_breaks = NULL,
                      x_num_size = 10, y_num_size = 10) {

  # 1) Check if it's glmmTMB (vs. lme4)
  is_glmmTMB <- inherits(model, "glmmTMB")

  # 2) Identify the levels of the chosen grouping variable
  if (is_glmmTMB) {
    group_levels <- levels(model$frame[[grouping_var]])
  } else {
    group_levels <- levels(lme4::getME(model, "flist")[[grouping_var]])
  }
  # Ensure 'grouping_var' in 'data' is a factor with the same levels
  data[[grouping_var]] <- factor(data[[grouping_var]], levels = group_levels)

  # 3) Determine range for the chosen predictor (x-axis)
  predictor_vals <- data[[predictor]]
  if (is.null(x_limits)) {
    x_min <- floor(min(predictor_vals, na.rm = TRUE))
    x_max <- ceiling(max(predictor_vals, na.rm = TRUE))
    x_limits <- c(x_min, x_max)
  } else {
    x_min <- x_limits[1]
    x_max <- x_limits[2]
  }
  if (is.null(x_breaks)) {
    x_breaks <- pretty(x_limits)
  }
  predictor_seq <- seq(x_min, x_max, length.out = 100)

  # 4) Identify all other predictors in the model & assign “typical” values
  #    (we skip the chosen predictor + grouping_var)
  all_predictors <- attr(terms(model), "term.labels")  # all RHS terms
  other_vars <- setdiff(all_predictors, c(predictor, grouping_var))

  # Build a named list of typical values for these other variables
  typical_values <- lapply(other_vars, function(v) {
    col_data <- data[[v]]
    if (is.numeric(col_data)) {
      mean(col_data, na.rm = TRUE)  # or median, if you prefer
    } else if (is.factor(col_data)) {
      levels(col_data)[1]          # pick first factor level
    } else {
      unique(col_data)[1]          # fallback for characters, etc.
    }
  })
  names(typical_values) <- other_vars

  # 5) Build a data frame for random (full) predictions
  #    Expand each predictor value across all grouping-levels
  df_random <- expand.grid(
    seq_idx = seq_along(predictor_seq),
    group_idx = seq_along(group_levels)
  )
  df_random[[predictor]] <- predictor_seq[df_random$seq_idx]
  df_random[[grouping_var]] <- group_levels[df_random$group_idx]
  df_random$seq_idx <- NULL
  df_random$group_idx <- NULL

  # Add the typical values for all other variables
  for (v in other_vars) {
    df_random[[v]] <- typical_values[[v]]
  }

  # Predict the linear predictor for the random-effects-included model
  df_random$Eta <- predict(
    model, newdata = df_random, type = "link", re.form = NULL
  )

  # 6) Build a data frame for fixed-effects-only predictions
  #    Use just one arbitrary grouping level (the first) for re.form=NA
  df_fixed <- data.frame(
    seq_idx = seq_along(predictor_seq)
  )
  df_fixed[[predictor]] <- predictor_seq
  df_fixed[[grouping_var]] <- group_levels[1]
  df_fixed$seq_idx <- NULL

  # Add typical values
  for (v in other_vars) {
    df_fixed[[v]] <- typical_values[[v]]
  }
  df_fixed$Eta <- predict(
    model, newdata = df_fixed, type = "link", re.form = NA
  )

  # 7) Apply the link-function transformations based on family & y_scale
  transform_eta <- function(eta, fam, scale_choice) {
    if (fam == "binomial") {
      if (is.null(scale_choice)) scale_choice <- "probability"
      switch(scale_choice,
             "probability" = stats::plogis(eta),
             "odds"        = exp(eta),
             "log odds"    = eta,
             stop("For binomial: y_scale must be 'probability', 'odds', or 'log odds'.")
      )
    } else if (fam == "poisson") {
      if (is.null(scale_choice)) scale_choice <- "count"
      switch(scale_choice,
             "count"     = exp(eta),
             "log count" = eta,
             stop("For Poisson: y_scale must be 'count' or 'log count'.")
      )
    } else if (fam == "gaussian") {
      eta  # identity link
    } else if (fam == "Gamma") {
      if (is.null(scale_choice)) scale_choice <- "response"
      switch(scale_choice,
             "response"     = exp(eta),
             "log response" = eta,
             stop("For Gamma: y_scale must be 'response' or 'log response'.")
      )
    } else if (fam == "beta") {
      if (is.null(scale_choice)) scale_choice <- "probability"
      switch(scale_choice,
             "probability" = stats::plogis(eta),
             "logit"       = eta,
             stop("For beta: y_scale must be 'probability' or 'logit'.")
      )
    } else if (fam == "negative binomial") {
      if (is.null(scale_choice)) scale_choice <- "count"
      switch(scale_choice,
             "count"     = exp(eta),
             "log count" = eta,
             stop("For Negative Binomial: y_scale must be 'count' or 'log count'.")
      )
    } else {
      stop("Unsupported family. Supported families: ",
           "'gaussian', 'binomial', 'poisson', 'Gamma', 'beta', 'negative binomial'.")
    }
  }

  df_random$Outcome <- transform_eta(df_random$Eta, family, y_scale)
  df_fixed$Outcome  <- transform_eta(df_fixed$Eta, family, y_scale)

  # 8) Determine y-limits if not provided
  if (is.null(y_limits)) {
    # e.g., binomial "probability" or beta "probability"
    if ((family %in% c("binomial", "beta")) &&
        (is.null(y_scale) || y_scale %in% c("probability"))) {
      y_limits <- c(0, 1)
    } else if (family == "gaussian") {
      # use range of the actual outcome in the data
      y_limits <- range(data[[outcome]], na.rm = TRUE)
    } else {
      # fallback: range of the predicted outcome
      y_min <- min(df_random$Outcome, na.rm = TRUE)
      y_max <- max(df_random$Outcome, na.rm = TRUE)
      # don't go below zero for count families
      y_limits <- c(max(0, y_min), y_max)
    }
  }

  # 9) Y-axis breaks if not given
  if (is.null(y_breaks)) {
    y_breaks <- pretty(y_limits)
  }

  # 10) Axis labels, title defaults
  if (is.null(x_label)) x_label <- predictor
  if (is.null(y_label)) {
    # Provide a family-specific default
    y_label <- switch(
      family,
      "binomial" = {
        if (is.null(y_scale) || y_scale == "probability") "Probability"
        else if (y_scale == "odds") "Odds"
        else if (y_scale == "log odds") "Log Odds"
        else "Binomial Outcome"
      },
      "poisson" = {
        if (is.null(y_scale) || y_scale == "count") "Count"
        else if (y_scale == "log count") "Log Count"
        else "Poisson Outcome"
      },
      "gaussian" = outcome,
      "Gamma" = {
        if (is.null(y_scale) || y_scale == "response") outcome
        else paste("Log of", outcome)
      },
      "beta" = {
        if (is.null(y_scale) || y_scale == "probability") "Probability"
        else "Logit"
      },
      "negative binomial" = {
        if (is.null(y_scale) || y_scale == "count") "Count"
        else "Log Count"
      },
      outcome
    )
  }
  if (is.null(plot_title)) {
    plot_title <- paste("Random Slope Plot:", predictor, "vs.", outcome)
  }

  # 11) Build final ggplot
  library(ggplot2)
  p <- ggplot() +
    geom_line(
      data = df_random,
      aes(x = .data[[predictor]], y = .data[["Outcome"]],
          group = .data[[grouping_var]]),
      color = "grey", linetype = "dashed", alpha = 0.5
    ) +
    geom_line(
      data = df_fixed,
      aes(x = .data[[predictor]], y = .data[["Outcome"]]),
      color = "black", size = 1
    ) +
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      oob = scales::censor
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      oob = scales::censor
    ) +
    theme_minimal() +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme(
      text            = element_text(size = 12),
      plot.title      = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
      axis.title.x    = element_text(margin = margin(t = 15)),
      axis.title.y    = element_text(margin = margin(r = 15)),
      axis.text.x     = element_text(size = x_num_size),
      axis.text.y     = element_text(size = y_num_size),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line       = element_line(color = "black", linewidth = 0.5)
    )

  return(p)
}


