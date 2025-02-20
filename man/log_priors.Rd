% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_helpers.R
\name{log_priors}
\alias{log_priors}
\title{Calculate log prior}
\usage{
log_priors(.samples, .l_params, .transform)
}
\arguments{
\item{.samples}{A vector/dataset containing sampled/proposed values.}

\item{.l_params}{A list that contains a vector of parameter names,
distributions and distributions' arguments.}

\item{.transform}{Logical for whether to back-transform parameters to
their original scale.}
}
\description{
Calculate log prior
}
\examples{
\dontrun{
}
}
