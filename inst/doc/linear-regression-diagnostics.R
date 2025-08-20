## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(regressinator)
library(ggplot2)
library(broom)

## -----------------------------------------------------------------------------
nonlinear_pop <- population(
  x1 = predictor(runif, min = 1, max = 8),
  x2 = predictor(runif, min = 4, max = 12),
  y = response(0.7 + 0.8 * x1**2 + 1.2 * x2,
               family = gaussian(), error_scale = 4.0)
)

nonlinear_data <- sample_x(nonlinear_pop, n = 100) |>
  sample_y()

fit <- lm(y ~ x1 + x2, data = nonlinear_data)

## ----resids-v-fitted----------------------------------------------------------
augment(fit) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Residual")

## ----resids-augmented---------------------------------------------------------
augment_longer(fit) |>
  ggplot(aes(x = .predictor_value, y = .resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Residual")

## ----resid-lineup-------------------------------------------------------------
model_lineup(fit, fn = augment_longer, n = 5) |>
  ggplot(aes(x = .predictor_value, y = .resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(rows = vars(.sample), cols = vars(.predictor_name),
             scales = "free_x") +
  labs(x = "Predictor", y = "Residual")

## ----partial-resids-----------------------------------------------------------
partial_residuals(fit) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid)) +
  geom_point() + # partial residuals
  geom_smooth(se = FALSE) + # smoothed residuals
  geom_line(aes(x = .predictor_value, y = .predictor_effect)) + # effects
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor", y = "Partial residual")

## ----partial-resid-lineup-----------------------------------------------------
model_lineup(fit, partial_residuals, n = 5) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_line(aes(x = .predictor_value, y = .predictor_effect)) +
  facet_grid(rows = vars(.sample), cols = vars(.predictor_name),
             scales = "free") +
  labs(x = "Predictor", y = "Partial residual")

## -----------------------------------------------------------------------------
intercepts <- c(
  "foo" = -0.3,
  "bar" = 1.7
)

slopes <- c(
  "foo" = 1.8,
  "bar" = -0.4
)

interact_pop <- population(
  x1 = predictor(runif, min = 1, max = 8),
  x2 = predictor(rfactor, levels = c("foo", "bar")),
  y = response(by_level(x2, intercepts) + by_level(x2, slopes) * x1,
               family = gaussian(), error_scale = 4.0)
)

interact_data <- sample_x(interact_pop, n = 100) |>
  sample_y()

no_interact_fit <- lm(y ~ x1 + x2, data = interact_data)

## ----partial-resid-no-interact------------------------------------------------
partial_residuals(no_interact_fit) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid, color = x2)) +
  geom_point() + # partial residuals
  geom_smooth(se = FALSE) + # smoothed residuals
  geom_line(aes(x = .predictor_value, y = .predictor_effect, color = NULL)) + # effects
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor", y = "Partial residual")

## ----partial-resid-interact---------------------------------------------------
interact_fit <- lm(y ~ x1 * x2, data = interact_data)

partial_residuals(interact_fit) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid, color = x2)) +
  geom_point() + # partial residuals
  geom_smooth(se = FALSE) + # smoothed residuals
  geom_line(aes(x = .predictor_value, y = .predictor_effect, color = NULL)) + # effects
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor", y = "Partial residual")

## ----cooks-dist---------------------------------------------------------------
augment(fit) |>
  ggplot(aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_col() +
  labs(x = "Row index", y = "Cook's distance")

## ----qq-----------------------------------------------------------------------
augment(fit) |>
  ggplot(aes(sample = .std.resid)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q plot of standardized residuals",
       x = "Theoretical quantiles", y = "Observed quantiles")

## ----qq-lineup----------------------------------------------------------------
model_lineup(fit) |>
  ggplot(aes(sample = .std.resid)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(vars(.sample)) +
  labs(title = "Normal Q-Q plot of standardized residuals",
       x = "Theoretical quantiles", y = "Observed quantiles")

## -----------------------------------------------------------------------------
library(palmerpenguins)

## -----------------------------------------------------------------------------
penguin_1 <- lm(bill_length_mm ~ flipper_length_mm + species,
                data = penguins)

## ----penguin-partial-resids---------------------------------------------------
partial_residuals(penguin_1, flipper_length_mm) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid)) +
  geom_point(aes(color = species)) +
  geom_smooth(aes(color = species), se = FALSE) +
  geom_line(aes(y = .predictor_effect)) +
  labs(x = "Flipper length (mm)", y = "Partial residual",
       color = "Species")

## -----------------------------------------------------------------------------
penguin_2 <- lm(bill_depth_mm ~ flipper_length_mm * species,
                data = penguins)

## ----penguin-interact-resids--------------------------------------------------
partial_residuals(penguin_2, flipper_length_mm) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid)) +
  geom_point(aes(color = species)) +
  geom_smooth(aes(color = species), se = FALSE) +
  geom_line(aes(y = .predictor_effect)) +
  labs(x = "Flipper length (mm)", y = "Partial residual",
       color = "Species")

## ----penguin-qq---------------------------------------------------------------
augment(penguin_2) |>
  ggplot(aes(sample = .std.resid)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(vars(species)) +
  labs(title = "Normal Q-Q plot of standardized residuals",
       x = "Theoretical quantiles", y = "Observed quantiles")

## ----penguin-cooks------------------------------------------------------------
augment(penguin_2) |>
  ggplot(aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_col() +
  labs(x = "Row index", y = "Cook's distance")

