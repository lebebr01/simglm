\name{data.reg.single}
\alias{data.reg.single}
\title{Simulates single level data}
\usage{
  data.reg.single(Xmat, beta, n, err)
}
\arguments{
  \item{Xmat}{A matrix of covariates.}

  \item{beta}{A vector of regression parameters.}

  \item{n}{Number of clusters.}

  \item{err}{A vector of within cluster errors.}
}
\description{
  Takes simulation paramter arguments and returns simulated
  data.
}
\details{
  This is a helper function to the master function
  \code{\link{sim.reg}}, this function does the actual
  simulation to return the data for single level models.
}

