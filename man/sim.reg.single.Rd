\name{sim.reg.single}
\alias{sim.reg.single}
\title{Master function to simulate single level data.}
\usage{
  sim.reg.single(fixed, fixed.param, cov.param, n,
    errorVar, err.dist, data.str, num.dist)
}
\arguments{
  \item{fixed}{One sided formula for fixed effects in the
  simulation.  To suppress intercept add -1 to formula.}

  \item{fixed.param}{Fixed effect parameter values (i.e.
  beta weights).  Must be same length as fixed.}

  \item{cov.param}{List of mean and standard deviation for
  fixed effects. Does not include intercept, time, or
  interactions. Must be same order as fixed formula above.}

  \item{n}{Cluster sample size.}

  \item{errorVar}{Scalar of error variance.}

  \item{err.dist}{Simulated within cluster error
  distribution. Must be "lap", "chi", "norm", "bimod",
  "norm" is default.}

  \item{data.str}{Type of data. Must be "cross", "long", or
  "single".}

  \item{num.dist}{Number of distributions for bimodal
  random variables}
}
\description{
  Takes simulation parameters as inputs and returns
  simulated data.
}
\details{
  Simulates data for the simple regression models.  Returns
  a data frame with ID variables, fixed effects, and many
  other variables useful to help when running simulation
  studies.
}
\seealso{
  \code{\link{sim.reg}} for a convenient wrapper for all
  data conditions.
}

