##########################################################################################
######################## Truncated at minimum y and maximum y ############################
##########################################################################################
plot_lmer <- function(model, data, predictor, outcome, grouping_var,
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

  ## Y LIMITS ##
  outcome_values_data <- data[[outcome]]
  if (is.null(y_limits)) {
    y_min <- floor(min(outcome_values_data, na.rm = TRUE))
    y_max <- ceiling(max(outcome_values_data, na.rm = TRUE))
    y_limits <- c(y_min, y_max)
  } else {
    y_min <- y_limits[1]
    y_max <- y_limits[2]
  }

  if (is.null(x_breaks)) {
    x_breaks <- pretty(c(x_min, x_max))
  }
  if (is.null(y_breaks)) {
    y_breaks <- pretty(c(y_min, y_max))
  }

  ## Create prediction data frame ##
  prediction_df <- random_lines |>
    crossing(predictor_range = seq(x_min, x_max, length.out = 100)) |>
    mutate(
      predictor = predictor_range,
      outcome = Intercept + Slope * predictor_range
    ) |>
    filter(outcome >= y_min & outcome <= y_max)  #% Filter outcome values to be within y limits

  ## Fixed Line Data Frame ##
  fixed_line_df <- data.frame(
    predictor = seq(x_min, x_max, length.out = 100),
    outcome = fixed_intercept + fixed_slope * seq(x_min, x_max, length.out = 100)
  ) |>
    filter(outcome >= y_min & outcome <= y_max)  #% Filter outcome values to be within y limits

  ## X & Y LABELS ##
  if (is.null(x_label)) {
    x_label <- predictor
  }
  if (is.null(y_label)) {
    y_label <- outcome
  }

  ## TITLE ##
  if (is.null(plot_title)) {
    plot_title <- paste("Random Slope Plot:", predictor, "vs.", y_label)
  }

  ## PLOT ##
  plot <- ggplot() +
    geom_line(
      data = prediction_df,
      aes(x = predictor, y = outcome, group = Group),
      color = "grey",
      linetype = "dashed",
      alpha = 0.5
    ) +
    geom_line(
      data = fixed_line_df,
      aes(x = predictor, y = outcome),
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






