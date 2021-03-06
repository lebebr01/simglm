% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_formula.r
\name{parse_formula}
\alias{parse_formula}
\title{Parses tidy formula simulation syntax}
\usage{
parse_formula(sim_args)
}
\arguments{
\item{sim_args}{A named list with special model formula syntax. See details and examples
for more information. The named list may contain the following:
\itemize{
  \item fixed: This is the fixed portion of the model (i.e. covariates)
  \item random: This is the random portion of the model (i.e. random effects)
  \item error: This is the error (i.e. residual term).
}}
}
\description{
A function that parses the formula simulation syntax in order to simulate data.
}
