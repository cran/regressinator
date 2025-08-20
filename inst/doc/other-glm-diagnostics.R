## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(regressinator)
library(dplyr)
library(ggplot2)
library(patchwork)
library(broom)

## -----------------------------------------------------------------------------
pois_pop <- population(
  x1 = predictor(runif, min = -5, max = 15),
  x2 = predictor(runif, min = 0, max = 10),
  y = response(0.7 + 0.2 * x1 + x1^2 / 100 - 0.2 * x2,
               family = poisson(link = "log"))
)

pois_data <- sample_x(pois_pop, n = 100) |>
  sample_y()

fit <- glm(y ~ x1 + x2, data = pois_data, family = poisson)

## ----empirical-link-----------------------------------------------------------
p1 <- ggplot(pois_data, aes(x = x1, y = y)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  labs(x = "X1", y = "Y")

p2 <- ggplot(pois_data, aes(x = x2, y = y)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  labs(x = "X2", y = "Y")

p1 + p2

## ----binned-empirical-link----------------------------------------------------
p1 <- pois_data |>
  bin_by_quantile(x1, breaks = 8) |>
  summarize(x = mean(x1),
            response = empirical_link(y, poisson)) |>
  ggplot(aes(x = x, y = response)) +
  geom_point() +
  labs(x = "X1", y = "log(Y)")

p2 <- pois_data |>
  bin_by_quantile(x2, breaks = 8) |>
  summarize(x = mean(x2),
            response = empirical_link(y, poisson)) |>
  ggplot(aes(x = x, y = response)) +
  geom_point() +
  labs(x = "X2", y = "log(Y)")

p1 + p2

## ----resids-v-fitted----------------------------------------------------------
# .fitted is the linear predictor, unless we set `type.predict = "response"` as
# an argument to augment()
augment(fit) |>
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Residual")

## ----resids-v-predictors------------------------------------------------------
augment_longer(fit) |>
  ggplot(aes(x = .predictor_value, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Residual")

## ----resid-lineup-------------------------------------------------------------
model_lineup(fit, fn = augment_longer, nsim = 5) |>
  ggplot(aes(x = .predictor_value, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(rows = vars(.sample), cols = vars(.predictor_name),
             scales = "free_x") +
  labs(x = "Predictor", y = "Residual")

## ----marginal-model-----------------------------------------------------------
augment_longer(fit, type.predict = "response") |>
  ggplot(aes(x = .predictor_value)) +
  geom_point(aes(y = y)) +
  geom_smooth(aes(y = .fitted), color = "red") +
  geom_smooth(aes(y = y)) +
  scale_y_log10() +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Y")

## ----partial-resids-----------------------------------------------------------
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

## ----rqr-glm-fitted-----------------------------------------------------------
augment_quantile(fit) |>
  ggplot(aes(x = .fitted, y = .quantile.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Randomized quantile residual")

## ----rqr-glm-predictors-------------------------------------------------------
augment_quantile_longer(fit) |>
  ggplot(aes(x = .predictor_value, y = .quantile.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Randomized quantile residual")

## ----rqr-qq-------------------------------------------------------------------
augment_quantile(fit) |>
  ggplot(aes(sample = .quantile.resid)) +
  geom_qq(distribution = qunif) +
  geom_qq_line(distribution = qunif) +
  labs(x = "Theoretical quantiles", y = "Observed quantiles")

## ----mean-fn------------------------------------------------------------------
ggplot() +
  geom_function(fun = function(x1) {
    exp(0.7 + 0.2 * x1 + x1^2 / 100 - 0.2 * 5)
  }) +
  xlim(-5, 10) +
  labs(x = "X1", y = "μ(x1, 5)")

