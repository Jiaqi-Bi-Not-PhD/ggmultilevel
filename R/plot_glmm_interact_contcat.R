plot_glmm_interact_contcat <- function(model, data, predictor_cont, predictor_cat, outcome, grouping_var,
                                       family = "gaussian", y_scale = "response", include_random = TRUE,
                                       x_limits = NULL, y_limits = NULL,
                                       x_label = NULL, y_label = NULL,
                                       plot_title = NULL,
                                       x_breaks = NULL, y_breaks = NULL,
                                       x_num_size = 10, y_num_size = 10,
                                       colors = NULL, legend_name = NULL) {

  data[[predictor_cat]] <- as.factor(data[[predictor_cat]])
  data[[grouping_var]] <- as.factor(data[[grouping_var]])

  ## Very important! Remove NA! ##
  data <- data |>
    filter(!is.na(.data[[grouping_var]])) |>
    filter(!is.na(.data[[predictor_cont]])) |>
    filter(!is.na(.data[[predictor_cat]]))

  data[[grouping_var]] <- droplevels(data[[grouping_var]])

  ## sequence of continuous predictors ##
  predictor_cont_values <- seq(
    min(data[[predictor_cont]], na.rm = TRUE),
    max(data[[predictor_cont]], na.rm = TRUE),
    length.out = 100
  )
  predictor_cat_levels <- levels(data[[predictor_cat]])

  ## x_limits and x_breaks ##
  if (is.null(x_limits)) {
    x_min <- floor(min(predictor_cont_values, na.rm = TRUE))
    x_max <- ceiling(max(predictor_cont_values, na.rm = TRUE))
    x_limits <- c(x_min, x_max)
  } else {
    x_min <- x_limits[1]
    x_max <- x_limits[2]
  }
  if (is.null(x_breaks)) {
    x_breaks <- pretty(c(x_min, x_max))
  }

  ## specs formula for emmeans ##
  specs_formula <- as.formula(paste("pairwise ~", predictor_cont, "|", predictor_cat))

  emm <- emmeans(
    model,
    specs = specs_formula,
    at = setNames(list(predictor_cont_values), predictor_cont),
    type = "link",  # link first, transform to others based on family
    lmer.df = "asymptotic"  # asymptotic degrees of freedom to suppress warnings in emmeans
  )
  ### For plotting and visualization purposes, it's generally acceptable to proceed without enabling exact D.f. calculations, especially with large datasets. Therefore, I recommend modifying the emmeans call within the plot_glmm_interact_contcat function to use asymptotic degrees of freedom, which will suppress the warnings and maintain reasonable computation times. ###

  fixed_preds <- as.data.frame(emm$emmeans)

  fixed_preds[[predictor_cat]] <- factor(
    fixed_preds[[predictor_cat]],
    levels = predictor_cat_levels
  )

  ## emmeans gives very weird name of CI, so changed it ##
  if ("lower.CL" %in% colnames(fixed_preds) && "upper.CL" %in% colnames(fixed_preds)) {
    ci_lower <- "lower.CL"
    ci_upper <- "upper.CL"
  } else if ("asymp.LCL" %in% colnames(fixed_preds) && "asymp.UCL" %in% colnames(fixed_preds)) {
    ci_lower <- "asymp.LCL"
    ci_upper <- "asymp.UCL"
  } else {
    stop("Could not find confidence interval columns in emmeans output.")
  }

  ## Transform predictions based on the y_scale argument and family ##
  if (family == "gaussian") {
    if (y_scale == "response" || y_scale == "link") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = emmean,
          lower.CL = .data[[ci_lower]],
          upper.CL = .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- outcome
    } else {
      stop("For gaussian family, y_scale must be 'response' or 'link'.")
    }
  } else if (family == "binomial") {
    if (y_scale == "probability") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = plogis(emmean),
          lower.CL = plogis(.data[[ci_lower]]),
          upper.CL = plogis(.data[[ci_upper]])
        )
      if (is.null(y_label)) y_label <- "Probability"
    } else if (y_scale == "log odds") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = emmean,
          lower.CL = .data[[ci_lower]],
          upper.CL = .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- "Log Odds"
    } else if (y_scale == "odds") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = exp(emmean),
          lower.CL = exp(.data[[ci_lower]]),
          upper.CL = exp(.data[[ci_upper]])
        )
      if (is.null(y_label)) y_label <- "Odds"
    } else {
      stop("For binomial family, y_scale must be 'probability', 'log odds', or 'odds'.")
    }
  } else if (family == "poisson") {
    if (y_scale == "count") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = exp(emmean),
          lower.CL = exp(.data[[ci_lower]]),
          upper.CL = exp(.data[[ci_upper]])
        )
      if (is.null(y_label)) y_label <- "Count"
    } else if (y_scale == "log count") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = emmean,
          lower.CL = .data[[ci_lower]],
          upper.CL = .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- "Log Count"
    } else {
      stop("For poisson family, y_scale must be 'count' or 'log count'.")
    }
  } else if (family == "Gamma") {
    if (y_scale == "response") {  # Commonly uses log link
      fixed_preds <- fixed_preds |>
        mutate(
          response = exp(emmean),
          lower.CL = exp(.data[[ci_lower]]),
          upper.CL = exp(.data[[ci_upper]])
        )
      if (is.null(y_label)) y_label <- outcome
    } else if (y_scale == "log response") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = emmean,
          lower.CL = .data[[ci_lower]],
          upper.CL = .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- "Log of Response"
    } else {
      stop("For Gamma family, y_scale must be 'response' or 'log response'.")
    }
  } else if (family == "inverse.gaussian") {
    if (y_scale == "response") {  # Commonly uses inverse link
      fixed_preds <- fixed_preds |>
        mutate(
          response = 1 / emmean,
          lower.CL = 1 / .data[[ci_lower]],
          upper.CL = 1 / .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- outcome
    } else if (y_scale == "inverse response") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = emmean,
          lower.CL = .data[[ci_lower]],
          upper.CL = .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- "Inverse of Response"
    } else {
      stop("For inverse.gaussian family, y_scale must be 'response' or 'inverse response'.")
    }
  } else if (family == "negative binomial") {
    if (y_scale == "count") {  # Commonly uses log link
      fixed_preds <- fixed_preds |>
        mutate(
          response = exp(emmean),
          lower.CL = exp(.data[[ci_lower]]),
          upper.CL = exp(.data[[ci_upper]])
        )
      if (is.null(y_label)) y_label <- "Count"
    } else if (y_scale == "log count") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = emmean,
          lower.CL = .data[[ci_lower]],
          upper.CL = .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- "Log Count"
    } else {
      stop("For negative binomial family, y_scale must be 'count' or 'log count'.")
    }
  } else if (family == "beta") {
    if (y_scale == "probability") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = plogis(emmean),
          lower.CL = plogis(.data[[ci_lower]]),
          upper.CL = plogis(.data[[ci_upper]])
        )
      if (is.null(y_label)) y_label <- "Probability"
    } else if (y_scale == "logit") {
      fixed_preds <- fixed_preds |>
        mutate(
          response = emmean,
          lower.CL = .data[[ci_lower]],
          upper.CL = .data[[ci_upper]]
        )
      if (is.null(y_label)) y_label <- "Logit"
    } else {
      stop("For beta family, y_scale must be 'probability' or 'logit'.")
    }
  } else {
    stop("Unsupported family. Supported families are: 'gaussian', 'binomial', 'poisson', 'Gamma', 'inverse.gaussian', 'negative binomial', 'beta'.")
  }

  ## random effects predictions ##
  if (include_random) {
    grouping_var_levels <- rownames(ranef(model)[[grouping_var]])
    grouping_var_levels <- as.character(grouping_var_levels)

    newdata_random <- expand.grid(
      grouping_var = grouping_var_levels,
      predictor_cont = predictor_cont_values,
      predictor_cat = predictor_cat_levels
    )
    colnames(newdata_random)[colnames(newdata_random) == "predictor_cont"] <- predictor_cont
    colnames(newdata_random)[colnames(newdata_random) == "predictor_cat"] <- predictor_cat
    colnames(newdata_random)[colnames(newdata_random) == "grouping_var"] <- grouping_var

    newdata_random[[grouping_var]] <- factor(
      newdata_random[[grouping_var]],
      levels = grouping_var_levels
    )
    newdata_random[[predictor_cat]] <- factor(
      newdata_random[[predictor_cat]],
      levels = predictor_cat_levels
    )

    ## Remove rows with NA in grouping_var or predictors ##
    newdata_random <- newdata_random |>
      filter(!is.na(.data[[grouping_var]])) |>
      filter(!is.na(.data[[predictor_cont]])) |>
      filter(!is.na(.data[[predictor_cat]]))

    ## prediction with Random ##
    newdata_random$Predicted <- predict(
      model,
      newdata = newdata_random,
      re.form = NULL,
      type = "link",
      allow.new.levels = FALSE  # To avoid new IDs/Grouping variables...
    )

    ## Transform predictions based on the y_scale argument and family ##
    if (family == "gaussian") {
      newdata_random$Predicted <- newdata_random$Predicted  # No transformation
    } else if (family == "binomial") {
      if (y_scale == "probability") {
        newdata_random$Predicted <- plogis(newdata_random$Predicted)
      } else if (y_scale == "odds") {
        newdata_random$Predicted <- exp(newdata_random$Predicted)
      }
    } else if (family == "poisson" || family == "negative binomial") {
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
  }

  ################################################################
  ######################## PLOT PLOT PLOT ########################
  ################################################################
  plot <- ggplot()

  if (include_random) {
    plot <- plot +
      geom_line(
        data = newdata_random,
        aes(
          x = .data[[predictor_cont]],
          y = Predicted,
          group = interaction(.data[[grouping_var]], .data[[predictor_cat]]),
          color = .data[[predictor_cat]]
        ),
        alpha = 0.1,
        show.legend = FALSE,
        size = 0.5
      )
  }

  ## Add fixed effects predictions ##
  plot <- plot +
    geom_ribbon(
      data = fixed_preds,
      aes(
        x = .data[[predictor_cont]],
        ymin = lower.CL,
        ymax = upper.CL,
        fill = .data[[predictor_cat]]
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = fixed_preds,
      aes(
        x = .data[[predictor_cont]],
        y = response,
        color = .data[[predictor_cat]]
      ),
      size = 1
    )

  ## colors ##
  if (!is.null(colors)) {
    if (!is.null(legend_name)) {
      plot <- plot +
        scale_color_manual(
          name = legend_name,
          values = colors
        ) +
        scale_fill_manual(
          name = legend_name,
          values = colors
        )
    } else {
      plot <- plot +
        scale_color_manual(
          name = predictor_cat,
          values = colors
        ) +
        scale_fill_manual(
          name = predictor_cat,
          values = colors
        )
    }
  } else {
    plot <- plot +
      scale_color_discrete(
        name = predictor_cat
      ) +
      scale_fill_discrete(
        name = predictor_cat
      )
  }

  ## labels ##
  if (is.null(x_label)) {
    x_label <- predictor_cont
  }
  if (is.null(plot_title)) {
    plot_title <- paste("Interaction Plot -", predictor_cont, "vs.", y_label, "by", predictor_cat)
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

  ## labels and theme ##
  plot <- plot +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
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


