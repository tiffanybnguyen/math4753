% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{Project 1: Flight Overbooking Problem}
\usage{
ntickets(N = 400, gamma = 0.02, p = 0.95)
}
\arguments{
\item{N}{number of seats on the flight}

\item{gamma}{probability a plane will be truly overbooked}

\item{p}{probability of a "show"}
}
\value{
a list with N, gamma, p, the discrete ticket count, and the continuous ticket count
}
\description{
Project 1: Flight Overbooking Problem
}
\examples{
\dontrun{ntickets(N=400, gamma=0.02, p=0.95)}
}
