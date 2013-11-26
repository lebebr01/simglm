\name{sim.reg.nested}
\alias{sim.reg.nested}
\title{Function to simulate nested data}
\usage{
  sim.reg.nested(fixed, random, fixed.param, random.param,
    w.var, cov.param, n, p, errorVar, randCor, rand.dist,
    err.dist, serCor, serCorVal, data.str, num.dist)
}
\arguments{
  \item{fixed}{One sided formula for fixed effects in the
  simulation.  To suppress intercept add -1 to formula.}

  \item{random}{One sided formula for random effects in the
  simulation. Must be a subset of fixed.}

  \item{fixed.param}{Fixed effect parameter values (i.e.
  beta weights).  Must be same length as fixed.}

  \item{random.param}{Variance of random effects. Must be
  same length as random.}

  \item{w.var}{Number of within cluster variables,
  including intercept if applicable. Also could be number
  of level one covariates for cross-sectional clustering.}

  \item{cov.param}{List of mean and variance for fixed
  effects. Does not include intercept, time, or
  interactions. Must be same order as fixed formula above.}

  \item{n}{Cluster sample size.}

  \item{p}{Within cluster sample size.}

  \item{errorVar}{Scalar of error variance.}

  \item{randCor}{Correlation between random effects.}

  \item{rand.dist}{Simulated random effect distribution.
  Must be "lap", "chi", "norm", "bimod", "norm" is
  default.}

  \item{err.dist}{Simulated within cluster error
  distribution. Must be "lap", "chi", "norm", "bimod",
  "norm" is default.}

  \item{serCor}{Simulation of serial correlation. Must be
  "AR", "MA", "ARMA", or "ID", "ID" is default.}

  \item{serCorVal}{Serial correlation parameters. A list of
  values to pass on to arima.sim.}

  \item{data.str}{Type of data. Must be "cross", "long", or
  "single".}

  \item{num.dist}{Number of distributions for bimod random
  distribution}
}
\description{
  Takes simulation parameters as inputs and returns
  simulated data.
}
\details{
  Simulates data for the linear mixed model, both cross
  sectional and longitudinal data.  Returns a data frame
  with ID variables, fixed effects, and many other
  variables useful to help when running simulation studies.
}
\seealso{
  \code{\link{sim.reg}} for a convenient wrapper for all
  data conditions.
}

