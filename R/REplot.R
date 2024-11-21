##########################################################################################
######################################## Wrapping up #####################################
##########################################################################################
REplot <- function(model, data, predictor, outcome, grouping_var,
                                    family = NULL,
                                    ...) {
  if (!is.null(family)) {
    if (family == "binomial") {
      plot <- plot_glmer_binomial(
        model = model,
        data = data,
        predictor = predictor,
        outcome = outcome,
        grouping_var = grouping_var,
        ...
      )
    } else if (family == "poisson") {
      plot <- plot_glmer_poisson(
        model = model,
        data = data,
        predictor = predictor,
        outcome = outcome,
        grouping_var = grouping_var,
        ...
      )
    }
  }
   else if (is.null(family)) {
    plot <- plot_lmer(
      model = model,
      data = data,
      predictor = predictor,
      outcome = outcome,
      grouping_var = grouping_var,
      ...
    )
  }
  else {
    stop("Unsupported family. Please choose 'binomial' or 'poisson', or leave it blank if it is a lmer model.")
  }
  return(plot)
}
