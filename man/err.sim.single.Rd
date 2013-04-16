\name{err.sim.single}
\alias{err.sim.single}
\title{Function that simulates errors.}
\usage{
  err.sim.single(errorVar, n, err.dist)
}
\arguments{
  \item{errorVar}{Scalar of error variance}

  \item{n}{Cluster sample size.}

  \item{err.dist}{Simulated within cluster error
  distribution. Must be "lap", "chi", "norm", "bimod",
  "norm" is default.}
}
\description{
  Input error simulation parameters and outputs simulated
  errors.
}
\details{
  Simulates error term for single level regression models.
}

