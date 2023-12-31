% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residuals.R
\name{augment_longer}
\alias{augment_longer}
\title{Augment a model fit with residuals, in "long" format}
\usage{
augment_longer(x, ...)
}
\arguments{
\item{x}{A model fit object, such as those returned by \code{lm()} or \code{glm()}. See
the broom documentation for the full list of model types supported.}

\item{...}{Additional arguments passed to \code{broom::augment()}.}
}
\value{
A data frame (tibble) in similar form to those produced by
\code{broom::augment()}, but expanded to have one row per predictor per
observation. Columns \code{.predictor_name} and \code{.predictor_value} identify the
predictor and its value. An additional column \code{.obs} records the original
observation numbers so results can be matched to observations in the
original model data.
}
\description{
Use \code{broom::augment()} to augment a model fit with residual and fit
information, then reformat the resulting data frame into a "long" format with
one row per predictor per observation, to facilitate plotting of the result.
}
\details{
When there are factor predictors, this function is less useful. Because a
data frame column can contain values of only one type, factor or character
values will force all values of the \code{.predictor_value} column, including for
other predictors, to be converted.

The name comes by analogy to \code{tidyr::pivot_longer()}, and the concept of long
versus wide data formats.
}
\examples{
fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)

# each observation appears 3 times, once per predictor:
augment_longer(fit)
}
