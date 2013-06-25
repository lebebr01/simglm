\name{data.reg.nested}
\alias{data.reg.nested}
\title{Simulates nested data}
\usage{
  data.reg.nested(Xmat, Zmat, beta, rand.eff, n, p, err)
}
\arguments{
  \item{Xmat}{A matrix of covariates.}

  \item{Zmat}{Design matrix for random effects.}

  \item{beta}{A vector of regression parameters.}

  \item{rand.eff}{A vector of random effects, must be
  stacked.}

  \item{n}{Number of clusters.}

  \item{p}{Number of units within each cluster.}

  \item{err}{A vector of within cluster errors.}
}
\description{
  Takes simulation paramter arguments and returns simulated
  data.
}
\details{
  This is a helper function to the master function
  \code{\link{sim.reg}}, this function does the actual
  simulation to return the data for linear mixed models.
}

