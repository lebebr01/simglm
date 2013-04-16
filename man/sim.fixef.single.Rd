\name{sim.fixef.single}
\alias{sim.fixef.single}
\title{Simulates design matrix for single level model.}
\usage{
  sim.fixef.single(fixed, fixed.vars, n, cov.param)
}
\arguments{
  \item{fixed}{One sided formula for fixed effects in the
  simulation, currently assumes intercept.}

  \item{fixed.vars}{Character vector of covariates for
  design matrix.}

  \item{n}{Number of clusters.}

  \item{cov.param}{List of mean and variance for fixed
  effects. Does not include intercept, time, or
  interactions. Must be same order as fixed formula above.}
}
\description{
  Input fixed variables, sample size, and number of within
  variables, returns design matrix.
}
\details{
  Simulates the fixed effects for the \code{\link{sim.reg}}
  function when simulating a simple regression model.
}

