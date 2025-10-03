# ggmultilevel

`ggmultilevel` is an R package that helps you explore generalized linear mixed-effects models (GLMMs) with publication-ready graphics powered by **ggplot2**. The package provides a small set of plotting helpers that transform model predictions onto interpretable scales and display both fixed and random effects in a consistent, minimalist style.

## Key features

- Visualize single-predictor effects from `lmer`, `glmer`, or `glmmTMB` models with `plot_glmm()`.
- Plot continuous-by-categorical interactions (including confidence intervals) with `plot_glmm_interact_contcat()`.
- Support for a wide range of response families (gaussian, binomial, poisson, Gamma, beta, negative binomial, and more), with automatic transformation to common scales such as probabilities, odds, or counts.
- Fine-grained control over labels, axis limits, tick marks, legend titles, and colour palettes so that plots can match your project's style guide.

## Installation

The package is not yet on CRAN. You can install the development version from GitHub using `remotes` (or `devtools`):

```r
# install.packages("remotes")
remotes::install_github("mattrohr/ggmultilevel")
```

After installation, load the package alongside whichever modelling package you used (`lme4`, `glmmTMB`, etc.).

```r
library(ggmultilevel)
library(lme4)
```

## Simulated example

Below is a fully reproducible workflow that simulates clustered data, fits two multilevel models with `lme4`, and then visualises the fitted relationships with both plotting helpers. The example uses a continuous predictor (`study_hours`) and a categorical moderator (`condition`) observed across schools.

```r
set.seed(123)
library(dplyr)
library(lme4)
library(ggmultilevel)

# --- Simulate multilevel data ---------------------------------------------
n_schools <- 40
students_per_school <- 25

school_ids <- factor(rep(seq_len(n_schools), each = students_per_school))
random_intercepts <- rnorm(n_schools, sd = 5)
random_slopes <- rnorm(n_schools, sd = 0.7)

study_hours <- rnorm(n_schools * students_per_school, mean = 10, sd = 2)
condition <- factor(sample(c("Control", "Treatment"),
                           size = n_schools * students_per_school,
                           replace = TRUE))
condition_effect <- ifelse(condition == "Treatment", 2, 0)

# Generate an outcome with random intercepts/slopes and an interaction
math_score <- 60 +
  random_intercepts[school_ids] +
  (3 + random_slopes[school_ids]) * study_hours +
  condition_effect +
  1.2 * study_hours * (condition == "Treatment") +
  rnorm(n_schools * students_per_school, sd = 6)

simulated_data <- tibble(
  school = school_ids,
  study_hours = study_hours,
  condition = condition,
  math_score = math_score
)

# --- Fit multilevel models -------------------------------------------------
model_main <- lmer(math_score ~ study_hours + (study_hours | school), data = simulated_data)
model_interaction <- lmer(math_score ~ study_hours * condition + (study_hours | school),
                          data = simulated_data)

# --- Plot a single predictor -----------------------------------------------
plot_glmm(
  model = model_main,
  data = simulated_data,
  predictor = "study_hours",
  outcome = "math_score",
  grouping_var = "school"
)

# --- Plot a continuous x categorical interaction --------------------------
plot_glmm_interact_contcat(
  model = model_interaction,
  data = simulated_data,
  predictor_cont = "study_hours",
  predictor_cat = "condition",
  outcome = "math_score",
  grouping_var = "school",
  include_random = TRUE,
  legend_name = "Condition"
)
```

Both plotting functions return `ggplot` objects, so you can continue customising them with any `ggplot2` layer or theme.

## Getting help

- Run `?plot_glmm` or `?plot_glmm_interact_contcat` inside R for the full argument reference.
- Open an issue on [GitHub](https://github.com/mattrohr/ggmultilevel/issues) if you encounter bugs or have feature requests.

## License

`ggmultilevel` is released under the MIT License. See [`LICENSE.md`](LICENSE.md) for details.
