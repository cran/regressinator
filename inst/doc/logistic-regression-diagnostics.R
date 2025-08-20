## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(regressinator)
library(dplyr)
library(ggplot2)
library(broom)

## -----------------------------------------------------------------------------
logistic_pop <- population(
  x1 = predictor(rnorm, mean = 0, sd = 10),
  x2 = predictor(runif, min = 0, max = 10),
  y = response(0.7 + 0.2 * x1 + x1^2 / 100 - 0.2 * x2,
               family = binomial(link = "logit"))
)

logistic_data <- sample_x(logistic_pop, n = 100) |>
  sample_y()

fit <- glm(y ~ x1 + x2, data = logistic_data, family = binomial)

## ----x1-empirical-logit-------------------------------------------------------
logistic_data |>
  bin_by_quantile(x1, breaks = 6) |>
  summarize(x = mean(x1),
            response = empirical_link(y, binomial)) |>
  ggplot(aes(x = x, y = response)) +
  geom_point() +
  labs(x = "X1", y = "logit(Y)")

## ----x2-empirical-logit-------------------------------------------------------
logistic_data |>
  bin_by_quantile(x2, breaks = 6) |>
  summarize(x = mean(x2),
            response = empirical_link(y, binomial)) |>
  ggplot(aes(x = x, y = response)) +
  geom_point() +
  labs(x = "X2", y = "logit(Y)")

## ----logit-resids-------------------------------------------------------------
augment(fit) |>
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Residual")

## ----logit-resids-predictors--------------------------------------------------
augment_longer(fit) |>
  ggplot(aes(x = .predictor_value, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Residual")

## ----marginal-model-----------------------------------------------------------
augment_longer(fit, type.predict = "response") |>
  ggplot(aes(x = .predictor_value)) +
  geom_point(aes(y = y)) +
  geom_smooth(aes(y = .fitted), color = "red") +
  geom_smooth(aes(y = y)) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Y")

## ----logit-partial-resids-----------------------------------------------------
partial_residuals(fit) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid)) +
  geom_point() +
  geom_smooth() +
  geom_line(aes(x = .predictor_value, y = .predictor_effect)) +
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor", y = "Partial residual")

## ----binned-resids------------------------------------------------------------
binned_residuals(fit) |>
  ggplot(aes(x = predictor_mean, y = resid_mean)) +
  facet_wrap(vars(predictor_name), scales = "free") +
  geom_point() +
  labs(x = "Predictor", y = "Residual mean")

## ----binned-resids-fitted-----------------------------------------------------
binned_residuals(fit, predictor = .fitted) |>
  ggplot(aes(x = predictor_mean, y = resid_mean)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual mean")

## ----rqr-fitted---------------------------------------------------------------
augment_quantile(fit) |>
  ggplot(aes(x = .fitted, y = .quantile.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Randomized quantile residual")

## ----rqr-predictors-----------------------------------------------------------
augment_quantile_longer(fit) |>
  ggplot(aes(x = .predictor_value, y = .quantile.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Randomized quantile residual")

