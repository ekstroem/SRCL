% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SRCL_functions.R
\name{SRCL_6_sum_of_individual_effects}
\alias{SRCL_6_sum_of_individual_effects}
\title{Predict the risk based on the sum of individual effects}
\usage{
SRCL_6_sum_of_individual_effects(X, model)
}
\arguments{
\item{X}{The exposure data}

\item{model}{The fitted the monotonistic neural network}
}
\description{
By summing the through the risk as if each individual had been exposed to only one exposure, with the value the individual actually had.
}
\examples{
#See the example under SRCL_0_synthetic_data
}
