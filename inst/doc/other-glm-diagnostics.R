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

## ----fig.width=6, fig.height=4------------------------------------------------
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

## -----------------------------------------------------------------------------
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

## ----fig.width=5, fig.height=4------------------------------------------------
# .fitted is the linear predictor, unless we set `type.predict = "response"` as
# an argument to augment()
augment(fit) |>
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Residual")

## ----fig.width=6, fig.height=4------------------------------------------------
augment_longer(fit) |>
  ggplot(aes(x = .predictor_value, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Residual")

## ----fig.height=8-------------------------------------------------------------
model_lineup(fit, fn = augment_longer, nsim = 5) |>
  ggplot(aes(x = .predictor_value, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(rows = vars(.sample), cols = vars(.predictor_name),
             scales = "free_x") +
  labs(x = "Predictor", y = "Residual")

## ----fig.width=6, fig.height=4------------------------------------------------
augment_longer(fit, type.predict = "response") |>
  ggplot(aes(x = .predictor_value)) +
  geom_point(aes(y = y)) +
  geom_smooth(aes(y = .fitted), color = "red") +
  geom_smooth(aes(y = y)) +
  scale_y_log10() +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Y")

## ----fig.width=6, fig.height=4------------------------------------------------
partial_residuals(fit) |>
  ggplot(aes(x = .predictor_value, y = .partial_resid)) +
  geom_point() +
  geom_smooth() +
  geom_line(aes(x = .predictor_value, y = .predictor_effect)) +
  facet_wrap(vars(.predictor_name), scales = "free") +
  labs(x = "Predictor", y = "Partial residual")

## ----fig.width=5, fig.height=3------------------------------------------------
binned_residuals(fit) |>
  ggplot(aes(x = predictor_mean, y = resid_mean)) +
  facet_wrap(vars(predictor_name), scales = "free") +
  geom_point() +
  labs(x = "Predictor", y = "Residual mean")

## ----fig.width=5, fig.height=3------------------------------------------------
binned_residuals(fit, predictor = .fitted) |>
  ggplot(aes(x = predictor_mean, y = resid_mean)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual mean")

## ----rqr-glm-fitted, fig.width=5, fig.height=4--------------------------------
augment_quantile(fit) |>
  ggplot(aes(x = .fitted, y = .quantile.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Randomized quantile residual")

## ----rqr-glm-predictors, fig.width=6, fig.height=4----------------------------
augment_quantile_longer(fit) |>
  ggplot(aes(x = .predictor_value, y = .quantile.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(.predictor_name), scales = "free_x") +
  labs(x = "Predictor", y = "Randomized quantile residual")

## ----rqr-qq, fig.width=5, fig.height=4----------------------------------------
augment_quantile(fit) |>
  ggplot(aes(sample = .quantile.resid)) +
  geom_qq(distribution = qunif) +
  geom_qq_line(distribution = qunif) +
  labs(x = "Theoretical quantiles", y = "Observed quantiles")

## ----fig.width=5, fig.height=4------------------------------------------------
ggplot() +
  geom_function(fun = function(x1) {
    exp(0.7 + 0.2 * x1 + x1^2 / 100 - 0.2 * 5)
  }) +
  xlim(-5, 10) +
  labs(x = "X1", y = "μ(x1, 5)")

