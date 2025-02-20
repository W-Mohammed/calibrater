% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SickSicker_MarkovModel.R
\name{SS_MicroSim}
\alias{SS_MicroSim}
\title{Sick-Sicker (SS) Micro-simulation model}
\usage{
SS_MicroSim(
  .v_params_ = NULL,
  calibrate_ = TRUE,
  n_i = 10000,
  p_S1S2 = 0.105,
  hr_S1 = 3,
  hr_S2 = 10
)
}
\arguments{
\item{.v_params_}{A vector of named vector with values to replace
default model parameter values (usually those that require calibration).}

\item{calibrate_}{If \code{TRUE} (default), the model outputs natural
history data; otherwise, discounted outcomes \code{(costs and QALYs)}
are returned.}

\item{n_i}{Number of individuals to simulate}

\item{p_S1S2}{Probability to become sicker when sick}

\item{hr_S1}{Hazard ratio of death in sick vs healthy}

\item{hr_S2}{Hazard ratio of death in sicker vs healthy}
}
\description{
Sick-Sicker (SS) Micro-simulation model
}
\examples{
\dontrun{
}
}
