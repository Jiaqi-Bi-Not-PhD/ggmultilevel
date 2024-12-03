##########################################################################################
############################### Poisson multilevel plot ##################################
##########################################################################################
plot_glmer_poisson <- function(model, data, predictor, outcome, grouping_var,
                               y_scale = "count",  # Added y_scale parameter
                               x_limits = NULL, y_limits = NULL,
                               x_label = NULL, y_label = NULL,
                               plot_title = NULL,
                               x_breaks = NULL, y_breaks = NULL,
                               x_num_size = 10, y_num_size = 10) {

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

  ## Prepare prediction_df by crossing random_lines with predictor_df ##
  prediction_df <- random_lines |>
    crossing(predictor_df)

  ## Add the grouping variable ##
  prediction_df[[grouping_var]] <- prediction_df$Group

  ## Ensure grouping variable is a factor with the same levels as in the model ##
  group_levels <- levels(getME(model, "flist")[[grouping_var]])
  prediction_df[[grouping_var]] <- factor(prediction_df[[grouping_var]], levels = group_levels)

  ## Use predict() to calculate Eta ##
  # Include all necessary variables in newdata
  newdata_random <- prediction_df %>%
    select(all_of(c(predictor, grouping_var)))

  prediction_df$Eta <- predict(model, newdata = newdata_random, re.form = NULL, type = "link")

  ## For fixed effects only ##
  fixed_line_df <- data.frame(
    predictor = predictor_range
  )
  # Ensure fixed_line_df has the predictor variable with correct name
  colnames(fixed_line_df) <- predictor

  # Use predict() for fixed effects only
  fixed_line_df$Eta <- predict(model, newdata = fixed_line_df, re.form = NA, type = "link")

  ## Poisson Link - Y scales ##
  ## count, log count ##
  if (y_scale == "count") { # default is count
    prediction_df <- prediction_df |>
      mutate(
        Outcome = exp(Eta)
      )
    fixed_line_df <- fixed_line_df |>
      mutate(
        Outcome = exp(Eta)
      )
    ## Y LIMITS ##
    if (is.null(y_limits)) {
      y_limits <- c(0, max(prediction_df$Outcome, na.rm = TRUE))
    }
    if (is.null(y_label)) {
      y_label <- "Count"
    }
  } else if (y_scale == "log count") {
    prediction_df <- prediction_df |>
      mutate(
        Outcome = Eta
      )
    fixed_line_df <- fixed_line_df |>
      mutate(
        Outcome = Eta
      )
    ## Y LIMITS ##
    if (is.null(y_limits)) {
      y_limits <- c(min(prediction_df$Outcome, na.rm = TRUE), max(prediction_df$Outcome, na.rm = TRUE))
    }
    if (is.null(y_label)) {
      y_label <- "Log Count"
    }
  } else {
    stop("For Poisson family, y_scale must be 'count' or 'log count'.")
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
      aes(x = .data[[predictor]], y = Outcome),
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
    theme(
      text = element_text(size = 12),
      plot.title = element_text(
        size = 12,
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

