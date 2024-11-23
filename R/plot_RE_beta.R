##########################################################################################
################################ Beta multilevel plot ####################################
##########################################################################################
plot_glmer_binomial <- function(model, data, predictor, outcome, grouping_var,
                                y_scale = "probability",
                                x_limits = NULL, y_limits = NULL,
                                x_label = NULL, y_label = NULL,
                                plot_title = NULL,
                                x_breaks = NULL, y_breaks = NULL,
                                x_num_size = 10, y_num_size = 10,
                                title_size = 12, text_size = 12)
  +{
  ext_mod <- extract_model(model = model, data = data,
                           predictor = predictor, outcome = outcome,
                           grouping_var = grouping_var)
  fixed_intercept <- ext_mod$fixed_intercept
  fixed_slope <- ext_mod$fixed_slope
  random_lines <- ext_mod$random_lines
  ## X LIMITS ##
  predictor_values_data <- data[[predictor]]
  if (is.null(x_limits)) {
    x_min <- floor(min(predictor_values_data, na.rm = TRUE))
    x_max <- ceiling(max(predictor_values_data, na.rm = TRUE))
    x_limits <- c(x_min, x_max)
  } else {
    x_min <- x_limits[1]
    x_max <- x_limits[2]
  }
  if (is.null(x_breaks)) {
    x_breaks <- pretty(c(x_min, x_max))
  }
  predictor_range <- seq(x_min, x_max, length.out = 100)
  predictor_df <- data.frame(predictor_range)
  colnames(predictor_df) <- predictor

  prediction_df <- random_lines |>
    crossing(predictor_df) |>
    mutate(
      Eta = Intercept + Slope * .data[[predictor]] # here the beta has incorporated the random effects
    )

  ## Binomial Link - Y scales ##
  ## probability, odds, or log odds ##

  if (y_scale == "probability") { # default is prob
    prediction_df <- prediction_df |>
      mutate(
        Outcome = plogis(Eta)
      )
    fixed_line_df <- data.frame(
      predictor = predictor_range,
      Eta = fixed_intercept + fixed_slope * predictor_range
    ) |>
      mutate(
        Outcome = plogis(Eta)
      )
    ## Y LIMITS - Prob ##
    if (is.null(y_limits)) {
      y_limits <- c(0, 1)
    }
    if (is.null(y_label)) {
      y_label <- "Probability"
    }
  } else if (y_scale == "odds") { # Odds `if` start
    prediction_df <- prediction_df |>
      mutate(
        Outcome = exp(Eta)
      )
    fixed_line_df <- data.frame(
      predictor = predictor_range,
      Eta = fixed_intercept + fixed_slope * predictor_range
    ) |>
      mutate(
        Outcome = exp(Eta)
      )
    ## Y LIMITS - Odds ##
    if (is.null(y_limits)) {
      y_limits <- c(0, max(prediction_df$Outcome, na.rm = TRUE))
    }
    if (is.null(y_label)) {
      y_label <- "Odds"
    } # Odds `if` ended
  } else if (y_scale == "log odds") { # log odds `if` ended
    prediction_df <- prediction_df |>
      mutate(
        Outcome = Eta
      )
    fixed_line_df <- data.frame(
      predictor = predictor_range,
      Eta = fixed_intercept + fixed_slope * predictor_range
    ) |>
      mutate(
        Outcome = Eta
      )
    ## Y LIMITS - Log Odds ##
    if (is.null(y_limits)) {
      y_limits <- c(min(prediction_df$Outcome, na.rm = TRUE), max(prediction_df$Outcome, na.rm = TRUE))
    } # log odds can be negative!
    if (is.null(y_label)) {
      y_label <- "Log Odds"
    }
  } # log odds `if` ended
  else {
    stop("For binomial family, y_scale must be 'probability', 'odds', or 'log odds'.")
  }

  ## Y BREAKS ##
  if (is.null(y_breaks)) {
    y_min <- y_limits[1]
    y_max <- y_limits[2]
    y_breaks <- pretty(c(y_min, y_max))
  }

  ## X & Y LABELS ##
  if (is.null(x_label)) {
    x_label <- predictor
  }

  ## TITLE ##
  if (is.null(plot_title)) {
    plot_title <- paste("Random Slope Plot:", predictor, "vs.", y_label)
  }

  ## PLOT ##
  plot <- ggplot() +
    geom_line(
      data = prediction_df,
      aes(x = .data[[predictor]], y = Outcome, group = Group),
      color = "grey",
      linetype = "dashed",
      alpha = 0.5
    ) +
    geom_line(
      data = fixed_line_df,
      aes(x = predictor, y = Outcome),
      color = "black",
      size = 1
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
    theme( ## this can be expanded to many options, as long as we can wrap them up ##
      text = element_text(size = text_size),
      plot.title = element_text(
        size = title_size,
        hjust = 0.5,
        margin = margin(b = 20)
      ),
      axis.title.x = element_text(
        margin = margin(t = 15)
      ),
      axis.title.y = element_text(
        margin = margin(r = 15)
      ),
      axis.text.x = element_text(
        size = x_num_size
      ),
      axis.text.y = element_text(
        size = y_num_size
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5)
    )
  return(plot)
}
