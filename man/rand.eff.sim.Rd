\name{rand.eff.sim}
\alias{rand.eff.sim}
\title{Function to simulate random effects.}
\usage{
  rand.eff.sim(random.param, cor, n,
    dist = c("lap", "chi", "norm", "bimod"), num.dist)
}
\arguments{
  \item{random.param}{Variance of random effects. Must be
  same length as random.}

  \item{Cor}{Correlation between random effects.}

  \item{n}{Cluster sample size.}

  \item{dist}{Simulated random effect distribution.  Must
  be "lap", "chi", "norm", "bimod", "norm" is default.}
}
\description{
  Input simulation parameters and returns random effects.
}
\details{
  Simulates random effects for the master function
  \code{\link{sim.reg}} when simulating a linear mixed
  model, both cross sectional and longitudinal.  Allows the
  ability to simulate random effects from a Laplace,
  chi-square (1), mixture normal, or normal distribution.
}

