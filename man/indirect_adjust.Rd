% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sir.R
\name{indirect_adjust}
\alias{indirect_adjust}
\title{indirect_adjust: Indirect adjustment}
\usage{
indirect_adjust(df, study_count, study_pop, ref_count, ref_pop,
  places = 2, level = 95)
}
\arguments{
\item{df}{A data frame.}

\item{study_count}{Number of events in study group.}

\item{study_pop}{Number of person-years at risk in study group.}

\item{ref_count}{Number of events in referent group.}

\item{ref_pop}{Number of person-years at risk in referent group.}

\item{places}{Number of decimal places in SIR's and confidence intervals.}

\item{level}{Confidence level as percentage.}
}
\value{
A data table with the following fields:
\describe{
  \item{\code{observed}}{Number of events in study group}
  \item{\code{expected}}{Number of events expected in study group based on
  rate in referent group}
  \item{\code{sir}}{SIR: Standardized incidence (or mortality) ratio}
  \item{\code{sir_lci}}{Lower confidence limit of SIR using method of
  Garwood (1936)}
  \item{\code{sir_uci}}{Upper confidence limit of SIR by Garwood}}
}
\description{
Find standardized incidence ratios and standardized mortality ratios with
indirect adjustment.
}
\examples{
# standardized incidence ratio of US cancer incidence for males
#   compared to females
library(tidyepi)
library(dplyr)
sir_by_year <- cancer \%>\%
  group_by(Year) \%>\%
  do(reshape_for_SIR(., agegroup, Sex, "Male", n, pop)) \%>\%
  do(indirect_adjust(., study_count, study_pop, ref_count, ref_pop))
}
\references{
Garwood F (1936) Fiducial limits for the Poisson distribution,
 Biometrika 28:437-442.
}