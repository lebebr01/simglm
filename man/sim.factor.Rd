\name{sim.factor}
\alias{sim.factor}
\title{Simulate categorical, factor, or discrete variables}
\usage{
  sim.factor(n, p, numlevels, replace = TRUE, prob = NULL,
    data.str = c("long", "cross", "single"))
}
\arguments{
  \item{n}{Number of clusters or number of observations for
  single level}

  \item{p}{Number of within cluster observations for
  multilevel}

  \item{numlevels}{Number of levels for categorical,
  factor, or discrete variable}

  \item{replace}{Whether to replace levels of categorical
  variable, TRUE/FALSE}

  \item{prob}{Probability of levels for variable, must be
  same length as numlevels}

  \item{data.str}{Data structure for the data}
}
\description{
  Function that simulates discrete, factor, or categorical
  variables.  Is essentially a wrapper around the sample
  function from base R.
}

