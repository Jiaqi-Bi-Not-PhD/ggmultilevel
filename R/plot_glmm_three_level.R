#' Visualize predictions from a three-level multilevel model
#'
#' @description
#' `plot_glmm_three_level()` extends the existing plotting utilities to models
#' with two nested grouping variables (e.g., students within classrooms within
#' schools). It produces an interactive three-dimensional visualization that
#' displays the predicted outcome across the focal predictor, second-level
#' clusters, and the third-level clusters that contain them.
#'
#' @param model A fitted multilevel model of class `lmer`, `glmer`, or
#'   `glmmTMB`.
#' @param data A data frame that contains the variables used to fit `model`.
#' @param predictor The focal predictor to display on the x-axis, supplied as a
#'   string.
#' @param outcome The outcome variable from the fitted model, supplied as a
#'   string. Used for axis labelling when `z_label` is not provided.
#' @param level2_var The second-level grouping variable (e.g., classroom),
#'   supplied as a string.
#' @param level3_var The third-level grouping variable (e.g., school), supplied
#'   as a string.
#' @param family A character string identifying the distribution used when
#'   fitting `model`. Supported families are `"gaussian"`, `"binomial"`,
#'   `"poisson"`, `"Gamma"`, `"beta"`, and `"negative binomial"`.
#' @param y_scale Optional specification of the transformation for the
#'   predictions, matched to the supplied `family`. Defaults mirror the response
#'   scale for each family.
#' @param include_random Logical; if `TRUE` (default) predictions incorporate the
#'   random effects associated with `level2_var` and `level3_var`. Set to `FALSE`
#'   to show only the fixed-effects surface.
#' @param x_limits Optional numeric vector of length two that constrains the
#'   predictor range.
#' @param x_label Optional label for the x-axis. Defaults to `predictor`.
#' @param y_label Optional label for the y-axis. Defaults to `level2_var`.
#' @param z_label Optional label for the z-axis. Defaults to `outcome`.
#' @param plot_title Optional plot title.
#' @param colors Optional vector of colours used to distinguish the
#'   third-level clusters.
#' @param legend_name Optional legend title. Defaults to `level3_var`.
#' @param line_width Numeric width of the trajectories drawn for each
#'   second-level cluster. Defaults to `2`.
#'
#' @return A `plotly` object that renders a three-dimensional visualization of
#'   the model-implied trajectories across the two grouping levels.
#' @export
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   n_school <- 8
#'   n_class <- 5
#'   n_student <- 20
#'
#'   school <- factor(rep(seq_len(n_school), each = n_class * n_student))
#'   classroom_within_school <- rep(rep(seq_len(n_class), each = n_student),
#'                                  times = n_school)
#'   classroom <- factor(paste0("S", school, "_C", classroom_within_school))
#'   time <- rep(seq(0, 5, length.out = n_student), times = n_school * n_class)
#'
#'   school_intercept <- rnorm(n_school, sd = 1)[school]
#'   class_intercept <- rnorm(n_school * n_class, sd = 0.6)[classroom]
#'   school_slope <- rnorm(n_school, sd = 0.2)[school]
#'   class_slope <- rnorm(n_school * n_class, sd = 0.1)[classroom]
#'
#'   linear_predictor <- 2 + school_intercept + class_intercept +
#'     (0.3 + school_slope + class_slope) * time
#'   score <- linear_predictor + rnorm(length(linear_predictor), sd = 1.2)
#'
#'   sim_data <- data.frame(
#'     school = school,
#'     classroom = classroom,
#'     time = time,
#'     score = score
#'   )
#'
#'   three_level_model <- lme4::lmer(
#'     score ~ time + (time | school/classroom),
#'     data = sim_data
#'   )
#'
#'   plot_glmm_three_level(model = three_level_model,
#'                         data = sim_data,
#'                         predictor = "time",
#'                         outcome = "score",
#'                         level2_var = "classroom",
#'                         level3_var = "school")
#' }
plot_glmm_three_level <- function(model, data, predictor, outcome,
                                  level2_var, level3_var,
                                  family = "gaussian", y_scale = NULL,
                                  include_random = TRUE,
                                  x_limits = NULL,
                                  x_label = NULL, y_label = NULL,
                                  z_label = NULL,
                                  plot_title = NULL,
                                  colors = NULL, legend_name = NULL,
                                  line_width = 2) {

  predictor_sym <- rlang::sym(predictor)
  level2_sym <- rlang::sym(level2_var)
  level3_sym <- rlang::sym(level3_var)

  data[[level2_var]] <- as.factor(data[[level2_var]])
  data[[level3_var]] <- as.factor(data[[level3_var]])

  data <- data |>
    dplyr::filter(!is.na(.data[[predictor]]),
                  !is.na(.data[[level2_var]]),
                  !is.na(.data[[level3_var]]))

  data[[level2_var]] <- droplevels(data[[level2_var]])
  data[[level3_var]] <- droplevels(data[[level3_var]])

  level2_levels <- levels(data[[level2_var]])

  predictor_vals <- data[[predictor]]
  if (is.null(x_limits)) {
    x_min <- floor(min(predictor_vals, na.rm = TRUE))
    x_max <- ceiling(max(predictor_vals, na.rm = TRUE))
    x_limits <- c(x_min, x_max)
  } else {
    x_min <- x_limits[1]
    x_max <- x_limits[2]
  }
  predictor_seq <- seq(x_min, x_max, length.out = 100)

  all_predictors <- attr(terms(model), "term.labels")
  grouping_vars <- unique(c(level2_var, level3_var))
  other_vars <- setdiff(all_predictors, c(predictor, grouping_vars))

  typical_values <- lapply(other_vars, function(v) {
    col_data <- data[[v]]
    if (is.numeric(col_data)) {
      mean(col_data, na.rm = TRUE)
    } else if (is.factor(col_data)) {
      levels(col_data)[1]
    } else {
      unique(col_data)[1]
    }
  })
  names(typical_values) <- other_vars

  if (include_random) {
    nesting <- data |>
      dplyr::distinct(!!level3_sym, !!level2_sym) |>
      dplyr::arrange(!!level3_sym, !!level2_sym)
  } else {
    nesting <- data.frame(
      setNames(
        list(
          factor(
            levels(data[[level3_var]])[1],
            levels = levels(data[[level3_var]])
          ),
          factor(
            levels(data[[level2_var]])[1],
            levels = levels(data[[level2_var]])
          )
        ),
        c(level3_var, level2_var)
      ),
      check.names = FALSE
    )
  }

  nesting <- nesting |>
    dplyr::group_by(!!level3_sym) |>
    dplyr::mutate(show_level3_legend = dplyr::row_number() == 1) |>
    dplyr::ungroup()

  n_predictor <- length(predictor_seq)
  n_groups <- nrow(nesting)

  pred_grid <- nesting[rep(seq_len(n_groups), each = n_predictor), , drop = FALSE]
  pred_grid[[predictor]] <- rep(predictor_seq, times = n_groups)

  for (v in other_vars) {
    pred_grid[[v]] <- typical_values[[v]]
  }

  re_form <- if (include_random) NULL else NA
  pred_grid$Eta <- stats::predict(
    model,
    newdata = pred_grid,
    type = "link",
    re.form = re_form,
    allow.new.levels = FALSE
  )

  transform_eta <- function(eta, fam, scale_choice) {
    if (fam == "binomial") {
      if (is.null(scale_choice)) scale_choice <- "probability"
      switch(scale_choice,
             "probability" = stats::plogis(eta),
             "odds" = exp(eta),
             "log odds" = eta,
             stop("For binomial: y_scale must be 'probability', 'odds', or 'log odds'.")
      )
    } else if (fam == "poisson") {
      if (is.null(scale_choice)) scale_choice <- "count"
      switch(scale_choice,
             "count" = exp(eta),
             "log count" = eta,
             stop("For Poisson: y_scale must be 'count' or 'log count'.")
      )
    } else if (fam == "gaussian") {
      eta
    } else if (fam == "Gamma") {
      if (is.null(scale_choice)) scale_choice <- "response"
      switch(scale_choice,
             "response" = exp(eta),
             "log response" = eta,
             stop("For Gamma: y_scale must be 'response' or 'log response'.")
      )
    } else if (fam == "beta") {
      if (is.null(scale_choice)) scale_choice <- "probability"
      switch(scale_choice,
             "probability" = stats::plogis(eta),
             "logit" = eta,
             stop("For beta: y_scale must be 'probability' or 'logit'.")
      )
    } else if (fam == "negative binomial") {
      if (is.null(scale_choice)) scale_choice <- "count"
      switch(scale_choice,
             "count" = exp(eta),
             "log count" = eta,
             stop("For Negative Binomial: y_scale must be 'count' or 'log count'.")
      )
    } else {
      stop("Unsupported family. Supported families: 'gaussian', 'binomial', 'poisson', 'Gamma', 'beta', 'negative binomial'.")
    }
  }

  if (is.null(x_label)) x_label <- predictor
  if (is.null(y_label)) y_label <- level2_var
  if (is.null(z_label)) z_label <- outcome

  pred_grid <- pred_grid |>
    dplyr::mutate(
      Prediction = transform_eta(Eta, family, y_scale),
      level2_label = .data[[level2_var]],
      level3_label = .data[[level3_var]],
      level2_index = as.numeric(level2_label),
      predictor_value = .data[[predictor]],
      custom_level2 = as.character(level2_label),
      custom_level3 = as.character(level3_label),
      trace_id = as.character(interaction(level3_label, level2_label, drop = TRUE)),
      hover_template = paste0(
        level3_var, ": ", custom_level3, "<br>",
        level2_var, ": ", custom_level2, "<br>",
        predictor, ": ", predictor_value, "<br>",
        z_label, ": ", Prediction, "<extra></extra>"
      )
    )

  used_level2 <- level2_levels[level2_levels %in% pred_grid$custom_level2]
  max_level2_index <- length(level2_levels)
  mean_index <- max_level2_index + 1
  mean_label <- "Mean prediction"

  mean_line <- pred_grid |>
    dplyr::group_by(predictor_value) |>
    dplyr::summarise(
      Prediction = mean(Prediction, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      level2_index = mean_index,
      hover_template = paste0(
        mean_label, "<br>",
        predictor, ": ", predictor_value, "<br>",
        z_label, ": ", Prediction,
        "<extra></extra>"
      )
    )

  if (is.null(plot_title)) {
    plot_title <- paste0("Three-level predictions for ", outcome)
  }
  if (is.null(legend_name)) legend_name <- level3_var

  axis_y <- list(
    title = y_label,
    tickmode = "array",
    tickvals = c(match(used_level2, level2_levels), mean_index),
    ticktext = c(used_level2, mean_label)
  )

  axis_x <- list(title = x_label, range = x_limits)
  axis_z <- list(title = z_label)

  plot_obj <- plotly::plot_ly(
    data = pred_grid,
    x = ~predictor_value,
    y = ~level2_index,
    z = ~Prediction,
    split = ~trace_id,
    color = ~level3_label,
    colors = colors,
    type = "scatter3d",
    mode = "lines",
    line = list(width = line_width),
    text = ~hover_template,
    hoverinfo = "text",
    hovertemplate = ~hover_template,
    legendgroup = ~level3_label,
    name = ~level3_label
  ) |>
    plotly::add_trace(
      data = mean_line,
      x = ~predictor_value,
      y = ~level2_index,
      z = ~Prediction,
      type = "scatter3d",
      mode = "lines",
      name = mean_label,
      line = list(color = "#000000", width = line_width + 1),
      hoverinfo = "text",
      hovertemplate = ~hover_template,
      showlegend = TRUE,
      inherit = FALSE
    ) |>
    plotly::layout(
      title = plot_title,
      legend = list(title = list(text = legend_name)),
      scene = list(
        xaxis = axis_x,
        yaxis = axis_y,
        zaxis = axis_z
      )
    )

  built_plot <- plotly::plotly_build(plot_obj)

  if (!is.null(built_plot$x$data)) {
    seen_groups <- character()
    for (i in seq_along(built_plot$x$data)) {
      trace <- built_plot$x$data[[i]]
      if (!is.null(trace$legendgroup)) {
        group <- trace$legendgroup
        if (length(group) == 0) {
          next
        }

        group <- group[1]
        trace$legendgroup <- group

        if (!is.null(trace$name)) {
          trace$name <- trace$name[1]
        }

        if (group %in% seen_groups) {
          trace$showlegend <- FALSE
        } else {
          trace$showlegend <- TRUE
          seen_groups <- c(seen_groups, group)
        }

        built_plot$x$data[[i]] <- trace
      }
    }
  }

  built_plot
}
