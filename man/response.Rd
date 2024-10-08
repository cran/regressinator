% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population.R
\name{response}
\alias{response}
\title{Specify a response variable in terms of predictors}
\usage{
response(expr, family = gaussian(), error_scale = NULL, size = 1L)
}
\arguments{
\item{expr}{An expression, in terms of other predictor or response variables,
giving this predictor's value on the link scale.}

\item{family}{The family of this response variable, e.g. \code{gaussian()} for an
ordinary Gaussian linear relationship.}

\item{error_scale}{Scale factor for errors. Used only for linear families,
such as \code{gaussian()} and \code{ols_with_error()}. Errors drawn while simulating
the response variable will be multiplied by this scale factor. The scale
factor can be a scalar value (such as a fixed standard deviation), or an
expression in terms of the predictors, which will be evaluated when
simulating response data. For generalized linear models, leave as \code{NULL}.}

\item{size}{When the \code{family} is \code{binomial()}, this is the number of trials
for each observation. Defaults to 1, as in logistic regression. May be
specified either as a vector of the same length as the number of
observations or as a scalar. May be written terms of other predictor or
response variables. For other families, \code{size} is ignored.}
}
\value{
A \code{response_dist} object, to be used in \code{population()} to specify a
population distribution
}
\description{
Response variables are related to predictors (and other response variables)
through a link function and response distribution. First the expression
provided is evaluated using the predictors, to give this response variable's
value on the link scale; then the inverse link function and response
distribution are used to get the response value. See Details for more
information.
}
\details{
Response variables are drawn based on a typical generalized linear model
setup. Let \eqn{Y} represent the response variable and \eqn{X} represent the
predictor variables. We specify that

\deqn{Y \mid X \sim \text{SomeDistribution},}{%
      Y | X ~ SomeDistribution,}

where

\deqn{\mathbb{E}[Y \mid X = x] = g^{-1}(\mu(x)).}{%
      E[Y | X = x] = g^{-1}(\mu(x)).}

Here \eqn{\mu(X)} is the expression \code{expr}, and both the distribution and
link function \eqn{g} are specified by the \code{family} provided. For instance,
if the \code{family} is \code{gaussian()}, the distribution is Normal and the link is
the identity function; if the \code{family} is \code{binomial()}, the distribution is
binomial and the link is (by default) the logistic link.
\subsection{Response families}{

The following response families are supported.

\describe{
\item{\code{gaussian()}}{
The default family is \code{gaussian()} with the identity link function,
specifying the relationship

\deqn{Y \mid X \sim \text{Normal}(\mu(X), \sigma^2),}{%
      Y | X ~ Normal(mu(X), \sigma^2),}

where \eqn{\sigma^2} is given by \code{error_scale}.
}

\item{\code{ols_with_error()}}{Allows specification of custom non-Normal error
distributions, specifying the relationship

\deqn{Y = \mu(X) + e,}

where \eqn{e} is drawn from an arbitrary distribution, specified by the
\code{error} argument to \code{ols_with_error()}.
}

\item{\code{binomial()}}{Binomial responses include binary responses (as in logistic
regression) and responses giving a total number of successes out of a number
of trials. The response has distribution

\deqn{Y \mid X \sim \text{Binomial}(N, g^{-1}(\mu(X))),}{%
      Y | X ~ Binomial(N, g^{-1}(\mu(X))),
}

where \eqn{N} is set by the \code{size} argument and \eqn{g} is the link function.
The default link is the logistic link, and others can be chosen with the
\code{link} argument to \code{binomial()}. The default \eqn{N} is 1, representing a
binary outcome.
}

\item{\code{poisson()}}{Poisson-distributed responses with distribution

\deqn{Y \mid X \sim \text{Poisson}(g^{-1}(\mu(X))),}{%
      Y | X ~ Poisson(g^{-1}(\mu(X))),
}

where \eqn{g} is the link function. The default link is the log link, and
others can be chosen with the \code{link} argument to \code{poisson()}.
}

\item{\code{custom_family()}}{Responses drawn from an arbitrary distribution with
arbitrary link function, i.e.

\deqn{Y \mid X \sim \text{SomeDistribution}(g^{-1}(\mu(X))),}{%
       Y | X ~ SomeDistribution(g^{-1}(\mu(X))),}

where both \eqn{g} and SomeDistribution are specified by arguments to
\code{custom_family()}.
}
}
}

\subsection{Evaluation and scoping}{

The \code{expr}, \code{error_scale}, and \code{size} arguments are evaluated only when
simulating data for this response variable. They are evaluated in an
environment with access to the predictor variables and the preceding response
variables, which they can refer to by name. Additionally, these arguments can
refer to variables in scope when the enclosing \code{population()} was defined.
See the Examples below.
}
}
\examples{
# Defining a binomial response. The expressions can refer to other predictors
# and to the environment where the `population()` is defined:
slope1 <- 2.5
slope2 <- -3
intercept <- -4.6
size <- 10
population(
  x1 = predictor(rnorm),
  x2 = predictor(rnorm),
  y = response(intercept + slope1 * x1 + slope2 * x2,
               family = binomial(), size = size)
)
}
\seealso{
\code{\link[=predictor]{predictor()}} and \code{\link[=population]{population()}} to define populations;
\code{\link[=ols_with_error]{ols_with_error()}} and \code{\link[=custom_family]{custom_family()}} for custom response distributions
}
