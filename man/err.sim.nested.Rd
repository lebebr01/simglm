\name{err.sim.nested}
\alias{err.sim.nested}
\title{Function that simulates errors.}
\usage{
  err.sim.nested(errorVar, n, p, serCor, serCorVal,
    err.dist, num.dist)
}
\arguments{
  \item{errorVar}{Scalar of error variance}

  \item{n}{Cluster sample size.}

  \item{p}{Within cluster sample size.}

  \item{serCor}{Simulation of serial correlation. Must be
  "AR", "MA", "ARMA", or "ID", "ID" is default.}

  \item{serCorVal}{Serial correlation parameters. A list of
  values to pass on to arima.sim.}

  \item{err.dist}{Simulated within cluster error
  distribution. Must be "lap", "chi", "norm", "bimod",
  "norm" is default.}

  \item{num.dist}{Number of distributions for bimod random
  variables.}
}
\description{
  Input error simulation parameters and outputs simulated
  errors.
}

