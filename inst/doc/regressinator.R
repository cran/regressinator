## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(regressinator)

linear_pop <- population(
  x1 = predictor("rnorm", mean = 4, sd = 10),
  x2 = predictor("runif", min = 0, max = 10),
  y = response(
    0.7 + 2.2 * x1 - 0.2 * x2, # relationship between X and Y
    family = gaussian(),       # link function and response distribution
    error_scale = 1.5          # errors are scaled by this amount
  )
)

## -----------------------------------------------------------------------------
logistic_pop <- population(
  x1 = predictor("rnorm", mean = 0, sd = 10),
  x2 = predictor("runif", min = 0, max = 10),
  y = response(0.7 + 2.2 * x1 - 0.2 * x2,
               family = binomial(link = "logit"))
)

## -----------------------------------------------------------------------------
heteroskedastic_pop <- population(
  x1 = predictor("rnorm", mean = 5, sd = 4),
  x2 = predictor("runif", min = 0, max = 10),
  y = response(
    4 + 2 * x1 - 3 * x2, # relationship between X and Y
    family = ols_with_error(rnorm), # distribution of the errors
    error_scale = 0.5 + x2 / 10 # errors depend on x2
  )
)

## -----------------------------------------------------------------------------
heavy_tail_pop <- population(
  x1 = predictor("rnorm", mean = 5, sd = 4),
  x2 = predictor("runif", min = 0, max = 10),
  y = response(
    4 + 2 * x1 - 3 * x2, # relationship between X and Y
    family = ols_with_error(rt, df = 3), # distribution of the errors
    error_scale = 1.0 # errors are multiplied by this scale factor
  )
)

## -----------------------------------------------------------------------------
# 40% of draws have lambda = 0, the rest have lambda given by the inverse link
zeroinfpois <- function(ys) {
  n <- length(ys)
  rpois(n, lambda = ys * rbinom(n, 1, prob = 0.4))
}

pop <- population(
  x1 = predictor("rnorm", mean = 2, sd = 2),
  y = response(
    0.7 + 0.8 * x1,
    family = custom_family(zeroinfpois, exp)
  )
)

## -----------------------------------------------------------------------------
# default is equally likely levels
rfactor(5, c("foo", "bar", "baz"))

# but probabilities per level can be specified
rfactor(5, c("foo", "bar", "baz"), c(0.4, 0.3, 0.3))

## -----------------------------------------------------------------------------
intercepts <- c("foo" = 2, "bar" = 30, "baz" = 7)

factor_intercept_pop <- population(
  group = predictor("rfactor",
                    levels = c("foo", "bar", "baz"),
                    prob = c(0.1, 0.6, 0.3)),
  x = predictor("runif", min = 0, max = 10),
  y = response(by_level(group, intercepts) + 0.3 * x,
               error_scale = 1.5)
)

## -----------------------------------------------------------------------------
slopes <- c("foo" = 2, "bar" = 30, "baz" = 7)

factor_slope_pop <- population(
  group = predictor("rfactor",
                    levels = c("foo", "bar", "baz"),
                    prob = c(0.1, 0.6, 0.3)),
  x = predictor("runif", min = 0, max = 10),
  y = response(7 + by_level(group, slopes) * x,
               error_scale = 1.5)
)

## -----------------------------------------------------------------------------
sample_x(linear_pop, n = 10)

## -----------------------------------------------------------------------------
logistic_pop |>
  sample_x(n = 10) |>
  sample_y()

## ---- fig.width=4, fig.height=4-----------------------------------------------
library(broom)

nonlinear_pop <- population(
  x1 = predictor("runif", min = 1, max = 8),
  x2 = predictor("runif", min = 0, max = 10),
  y = response(0.7 + x1**2 - x2, family = gaussian(),
               error_scale = 4.0)
)

nonlinear_data <- nonlinear_pop |>
  sample_x(n = 100) |>
  sample_y()

nonlinear_fit <- lm(y ~ x1 + x2, data = nonlinear_data)

# to see the kind of information we get from an augmented fit
augment(nonlinear_fit)

library(ggplot2)

ggplot(augment(nonlinear_fit),
       aes(x = .fitted, y = .resid)) +
  geom_point() +
  labs(x = "Fitted value", y = "Residual")

## ---- fig.height=4------------------------------------------------------------
ggplot(augment_longer(nonlinear_fit),
       aes(x = .predictor_value, y = .resid)) +
  geom_point() +
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor value", y = "Residual")

## ---- fig.height=4------------------------------------------------------------
ggplot(partial_residuals(nonlinear_fit),
       aes(x = .predictor_value, y = .partial_resid)) +
  geom_point() + # partial residuals
  geom_smooth(se = FALSE) + # smoothed residuals
  geom_line(aes(x = .predictor_value, y = .predictor_effect)) + # effects
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor value", y = "Partial residual")

## ----fig.height=4-------------------------------------------------------------
quadratic_fit <- lm(y ~ poly(x1, 2) + x2, data = nonlinear_data)

ggplot(partial_residuals(quadratic_fit),
       aes(x = .predictor_value, y = .partial_resid)) +
  geom_point() + # partial residuals
  geom_smooth(se = FALSE) + # smoothed residuals
  geom_line(aes(x = .predictor_value, y = .predictor_effect)) + # effects
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor value", y = "Partial residual")

## ---- fig.width=6, fig.height=6-----------------------------------------------
model_lineup(nonlinear_fit) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  facet_wrap(vars(.sample)) +
  labs(x = "Fitted value", y = "Residual")

## ---- fig.width=6, fig.height=6-----------------------------------------------
heavy_tail_pop <- population(
  x1 = predictor("rnorm", mean = 5, sd = 4),
  x2 = predictor("runif", min = 0, max = 10),
  y = response(
    4 + 2 * x1 - 3 * x2,
    family = ols_with_error(rt, df = 3),
    error_scale = 1.0
  )
)

heavy_tail_sample <- heavy_tail_pop |>
  sample_x(n = 100) |>
  sample_y()

fit <- lm(y ~ x1 + x2, data = heavy_tail_sample)

model_lineup(fit) |>
  ggplot(aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(vars(.sample)) +
  labs(x = "Theoretical quantiles", y = "Observed quantiles")

## -----------------------------------------------------------------------------
d <- linear_pop |>
  sample_x(n = 30) |>
  sample_y()

fit <- lm(y ~ x1 + x2, data = d)
tidy(fit)

sampling_distribution(fit, d, nsim = 4)

## ---- fig.height=4------------------------------------------------------------
samples <- sampling_distribution(fit, d, nsim = 1000)

samples |>
  ggplot(aes(x = estimate)) +
  geom_histogram() +
  facet_wrap(vars(term), scales = "free") +
  labs(x = "Estimate", y = "Frequency")

## ---- message=FALSE-----------------------------------------------------------
library(dplyr)

samples |>
  group_by(term) |>
  summarize(mean = mean(estimate),
            sd = sd(estimate))

## -----------------------------------------------------------------------------
quadratic_fit <- lm(y ~ x1 + I(x1^2) + x2, data = nonlinear_data)

anova(nonlinear_fit, quadratic_fit)

## ---- fig.height=4------------------------------------------------------------
quadratic_coefs <- parametric_boot_distribution(nonlinear_fit, quadratic_fit) |>
  filter(term == "I(x1^2)")

ggplot(quadratic_coefs, aes(x = estimate)) +
  geom_histogram() +
  geom_vline(xintercept = coef(quadratic_fit)["I(x1^2)"],
             color = "red") +
  labs(x = "Quadratic term coefficient",
       y = "Count",
       title = "Null distribution of quadratic term")

