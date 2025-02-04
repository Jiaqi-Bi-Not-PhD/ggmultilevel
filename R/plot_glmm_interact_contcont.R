plot_glmm_interact_contcont <- function(model, data, predictor_cont1, predictor_cont2, outcome, grouping_var,
                                        family = "gaussian", y_scale = "response",
                                        include_random = TRUE,
                                        quantiles = c(0.25, 0.5, 0.75),
                                        continuous_quantile = FALSE,  ## If TRUE, treat predictor_cont2 continuously
                                        x_limits = NULL, y_limits = NULL,
                                        x_label = NULL, y_label = NULL,
                                        plot_title = NULL,
                                        plot_subtitle = NULL,
                                        x_breaks = NULL, y_breaks = NULL,
                                        x_num_size = 12, y_num_size = 12,
                                        colors = NULL,
                                        legend_name = NULL,
                                        line_size = 0.7,
                                        line_alpha = 0.1,
                                        base_size = 15,
                                        legend_position = "right",
                                        grid_color = "lightgrey",
                                        grid_size = 0.25,
                                        background_color = "white",
                                        text_color = "black") {
  ## Very important! Convert grouping_var to factor and remove NAs! ##
  data[[grouping_var]] <- as.factor(data[[grouping_var]])

  data <- data |>
    filter(!is.na(.data[[grouping_var]])) |>
    filter(!is.na(.data[[predictor_cont1]])) |>
    filter(!is.na(.data[[predictor_cont2]]))

  data[[grouping_var]] <- droplevels(data[[grouping_var]])

  ## Calculate specified quantiles of the second continuous predictor ##
  quantile_values <- quantile(data[[predictor_cont2]], probs = quantiles, na.rm = TRUE)

  ## Create a sequence of values for the first continuous predictor for plotting ##
  predictor_cont1_values <- seq(
    min(data[[predictor_cont1]], na.rm = TRUE),
    max(data[[predictor_cont1]], na.rm = TRUE),
    length.out = 100
  )

  ## x_limits and x_breaks ##
  if (is.null(x_limits)) {
    x_min <- floor(min(predictor_cont1_values, na.rm = TRUE))
    x_max <- ceiling(max(predictor_cont1_values, na.rm = TRUE))
    x_limits <- c(x_min, x_max)
  } else {
    x_min <- x_limits[1]
    x_max <- x_limits[2]
  }
  if (is.null(x_breaks)) {
    x_breaks <- pretty(c(x_min, x_max))
  }

  ## Construct new data for fixed effects predictions ##
  newdata_fixed <- expand.grid(
    predictor_cont1 = predictor_cont1_values,
    predictor_cont2 = quantile_values
  )
  colnames(newdata_fixed)[colnames(newdata_fixed) == "predictor_cont1"] <- predictor_cont1
  colnames(newdata_fixed)[colnames(newdata_fixed) == "predictor_cont2"] <- predictor_cont2

  ## Predict fixed effects (no random effects) ##
  fixed_preds <- predict(
    model,
    newdata = newdata_fixed,
    re.form = NA, # no random effects
    type = "link",
    allow.new.levels = FALSE
  )

  ## Add predictions to newdata_fixed ##
  newdata_fixed$emmean <- fixed_preds

  ## Transform predictions based on family and y_scale ##
  if (family == "gaussian") {
    if (y_scale == "response" || y_scale == "link") {
      newdata_fixed <- newdata_fixed |> mutate(response = emmean)
      if (is.null(y_label)) y_label <- outcome
    } else {
      stop("For gaussian family, y_scale must be 'response' or 'link'.")
    }
  } else if (family == "binomial") {
    if (y_scale == "probability") {
      newdata_fixed <- newdata_fixed |> mutate(response = plogis(emmean))
      if (is.null(y_label)) y_label <- "Probability"
    } else if (y_scale == "log odds") {
      newdata_fixed <- newdata_fixed |> mutate(response = emmean)
      if (is.null(y_label)) y_label <- "Log Odds"
    } else if (y_scale == "odds") {
      newdata_fixed <- newdata_fixed |> mutate(response = exp(emmean))
      if (is.null(y_label)) y_label <- "Odds"
    } else {
      stop("For binomial family, y_scale must be 'probability', 'log odds', or 'odds'.")
    }
  } else if (family == "poisson") {
    if (y_scale == "count") {
      newdata_fixed <- newdata_fixed |> mutate(response = exp(emmean))
      if (is.null(y_label)) y_label <- "Count"
    } else if (y_scale == "log count") {
      newdata_fixed <- newdata_fixed |> mutate(response = emmean)
      if (is.null(y_label)) y_label <- "Log Count"
    } else {
      stop("For poisson family, y_scale must be 'count' or 'log count'.")
    }
  } else if (family == "Gamma") {
    if (y_scale == "response") {
      newdata_fixed <- newdata_fixed |> mutate(response = exp(emmean))
      if (is.null(y_label)) y_label <- outcome
    } else if (y_scale == "log response") {
      newdata_fixed <- newdata_fixed |> mutate(response = emmean)
      if (is.null(y_label)) y_label <- "Log of Response"
    } else {
      stop("For Gamma family, y_scale must be 'response' or 'log response'.")
    }
  } else if (family == "inverse.gaussian") {
    if (y_scale == "response") {
      newdata_fixed <- newdata_fixed |> mutate(response = 1 / emmean)
      if (is.null(y_label)) y_label <- outcome
    } else if (y_scale == "inverse response") {
      newdata_fixed <- newdata_fixed |> mutate(response = emmean)
      if (is.null(y_label)) y_label <- "Inverse of Response"
    } else {
      stop("For inverse.gaussian family, y_scale must be 'response' or 'inverse response'.")
    }
  } else if (family == "negative binomial") {
    if (y_scale == "count") {
      newdata_fixed <- newdata_fixed |> mutate(response = exp(emmean))
      if (is.null(y_label)) y_label <- "Count"
    } else if (y_scale == "log count") {
      newdata_fixed <- newdata_fixed |> mutate(response = emmean)
      if (is.null(y_label)) y_label <- "Log Count"
    } else {
      stop("For negative binomial family, y_scale must be 'count' or 'log count'.")
    }
  } else if (family == "beta") {
    if (y_scale == "probability") {
      newdata_fixed <- newdata_fixed |> mutate(response = plogis(emmean))
      if (is.null(y_label)) y_label <- "Probability"
    } else if (y_scale == "logit") {
      newdata_fixed <- newdata_fixed |> mutate(response = emmean)
      if (is.null(y_label)) y_label <- "Logit"
    } else {
      stop("For beta family, y_scale must be 'probability' or 'logit'.")
    }
  } else {
    stop("Unsupported family.")
  }

  ## Handle continuous vs discrete quantiles ##
  if (continuous_quantile) {
    ## Continuous: keep numeric values
    newdata_fixed$QuantileValue <- newdata_fixed[[predictor_cont2]]
  } else {
    ## Discrete: factor the quantiles
    newdata_fixed$QuantileGroup <- factor(
      newdata_fixed[[predictor_cont2]],
      levels = quantile_values,
      labels = paste0("Q", seq_along(quantile_values))
    )
  }

  ## Random effects predictions if requested ##
  if (include_random) {
    grouping_var_levels <- rownames(ranef(model)[[grouping_var]])
    grouping_var_levels <- as.character(grouping_var_levels)

    newdata_random <- expand.grid(
      grouping_var = grouping_var_levels,
      predictor_cont1 = predictor_cont1_values,
      predictor_cont2 = quantile_values
    )
    colnames(newdata_random)[colnames(newdata_random) == "predictor_cont1"] <- predictor_cont1
    colnames(newdata_random)[colnames(newdata_random) == "predictor_cont2"] <- predictor_cont2
    colnames(newdata_random)[colnames(newdata_random) == "grouping_var"] <- grouping_var

    newdata_random[[grouping_var]] <- factor(
      newdata_random[[grouping_var]],
      levels = grouping_var_levels
    )

    newdata_random <- newdata_random |>
      filter(!is.na(.data[[grouping_var]])) |>
      filter(!is.na(.data[[predictor_cont1]])) |>
      filter(!is.na(.data[[predictor_cont2]]))

    newdata_random$Predicted <- predict(
      model,
      newdata = newdata_random,
      re.form = NULL,
      type = "link",
      allow.new.levels = FALSE
    )

    ## Transform predictions (same logic as above) ##
    if (family == "gaussian") {
      # no transform needed
    } else if (family == "binomial") {
      if (y_scale == "probability") {
        newdata_random$Predicted <- plogis(newdata_random$Predicted)
      } else if (y_scale == "odds") {
        newdata_random$Predicted <- exp(newdata_random$Predicted)
      }
    } else if (family %in% c("poisson", "negative binomial")) {
      if (y_scale == "count") {
        newdata_random$Predicted <- exp(newdata_random$Predicted)
      }
    } else if (family == "Gamma") {
      if (y_scale == "response") {
        newdata_random$Predicted <- exp(newdata_random$Predicted)
      }
    } else if (family == "inverse.gaussian") {
      if (y_scale == "response") {
        newdata_random$Predicted <- 1 / newdata_random$Predicted
      }
    } else if (family == "beta") {
      if (y_scale == "probability") {
        newdata_random$Predicted <- plogis(newdata_random$Predicted)
      }
    }

    if (continuous_quantile) {
      newdata_random$QuantileValue <- newdata_random[[predictor_cont2]]
    } else {
      newdata_random$QuantileGroup <- factor(
        newdata_random[[predictor_cont2]],
        levels = quantile_values,
        labels = paste0("Q", seq_along(quantile_values))
      )
    }
  }

  ## Create the plot ##
  plot <- ggplot()

  ## If including random effects: plot all random lines in background ##
  if (include_random) {
    if (continuous_quantile) {
      plot <- plot +
        geom_line(
          data = newdata_random,
          aes(
            x = .data[[predictor_cont1]],
            y = Predicted,
            group = interaction(.data[[grouping_var]], .data[[predictor_cont2]]),
            color = .data[[predictor_cont2]]
          ),
          size = line_size,
          alpha = line_alpha
        )
    } else {
      plot <- plot +
        geom_line(
          data = newdata_random,
          aes(
            x = .data[[predictor_cont1]],
            y = Predicted,
            group = interaction(.data[[grouping_var]], QuantileGroup),
            color = QuantileGroup
          ),
          size = line_size,
          alpha = line_alpha
        )
    }
  }

  ## Add fixed effects lines (no confidence intervals) ##
  ## IMPORTANT: If continuous_quantile = TRUE, we do NOT plot these fixed effect lines
  ## to avoid showing a straight line for continuous mediator.
  if (!continuous_quantile) {
    plot <- plot +
      geom_line(
        data = newdata_fixed,
        aes(
          x = .data[[predictor_cont1]],
          y = response,
          color = QuantileGroup
        ),
        size = line_size,
        alpha = 1
      )
  }

  ## Set color scale depending on continuous_quantile ##
  if (continuous_quantile) {
    if (!is.null(colors)) {
      plot <- plot +
        scale_color_gradientn(
          name = ifelse(is.null(legend_name), paste0(predictor_cont2, " (Continuous)"), legend_name),
          colors = colors
        )
    } else {
      plot <- plot +
        scale_color_viridis_c(
          name = ifelse(is.null(legend_name), paste0(predictor_cont2, " (Continuous)"), legend_name),
          option = "plasma"
        )
    }
  } else {
    if (!is.null(colors)) {
      plot <- plot +
        scale_color_manual(
          name = ifelse(is.null(legend_name), paste0(predictor_cont2, " Quantiles"), legend_name),
          values = colors
        )
    } else {
      plot <- plot +
        scale_color_discrete(
          name = ifelse(is.null(legend_name), paste0(predictor_cont2, " Quantiles"), legend_name)
        )
    }
  }

  ## Define labels ##
  if (is.null(x_label)) {
    x_label <- predictor_cont1
  }
  if (is.null(plot_title)) {
    plot_title <- paste("Continuous-Continuous Interaction:", predictor_cont1, "and", predictor_cont2, "vs.", y_label)
  }

  ## x and y limits ##
  if (!is.null(x_limits)) {
    plot <- plot +
      scale_x_continuous(
        limits = x_limits,
        breaks = x_breaks
      )
  }
  if (!is.null(y_limits)) {
    plot <- plot +
      scale_y_continuous(
        limits = y_limits,
        breaks = y_breaks
      )
  }

  ## Apply theme and styling ##
  plot <- plot +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_label,
      y = y_label
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 12,
        margin = margin(b = 20)
      ),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = legend_position,
      panel.background = element_rect(fill = background_color, color = NA),
      plot.background = element_rect(fill = background_color, color = NA),
      text = element_text(color = text_color),
      axis.text = element_text(color = text_color),
      axis.title = element_text(color = text_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      panel.grid.major = element_line(color = grid_color, size = grid_size),
      panel.grid.minor = element_line(color = grid_color, size = grid_size),
      axis.line = element_line(color = "black", linewidth = 0.5)
    )

  return(plot)
}

