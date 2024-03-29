% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotRelative.R
\name{plotYieldRelative}
\alias{plotYieldRelative}
\alias{plotlyYieldRelative}
\title{Plot change in yield over time}
\usage{
plotYieldRelative(sim, object_original, species = NULL, ...)

plotlyYieldRelative(sim, object_original, species = NULL, ...)
}
\arguments{
\item{sim}{A MizerSim object}

\item{object_original}{A MizerParams or MizerSim object to calculate
differences from.}

\item{species}{The species to be selected. Optional. By default all target
species are selected. A vector of species names, or a numeric vector with
the species indices, or a logical vector indicating for each species
whether it is to be selected (TRUE) or not.}

\item{...}{Parameters passed to \code{getYield()}}
}
\description{
Plot change in yield over time
}
\examples{
plotYieldRelative(NS_sim, NS_params)
plotYieldRelative(NS_sim, NS_params, species = c("Cod", "Sole"))
}
