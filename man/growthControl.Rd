% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growthControl.R
\name{growthControl}
\alias{growthControl}
\alias{growthControlUI}
\title{A single slider that adjusts both \code{h} and \code{gamma}}
\usage{
growthControl(input, output, session, params, params_old, flags, ...)

growthControlUI(p, input)
}
\arguments{
\item{input}{Reactive holding the inputs}

\item{output}{Reactive holding the outputs}

\item{session}{Shiny session}

\item{params}{Reactive value holding updated MizerParams object}

\item{params_old}{Reactive value holding non-updated MizerParams object}

\item{flags}{Environment holding flags to skip certain observers}

\item{...}{Unused}

\item{p}{The MizerParams object currently being tuned.}
}
\description{
A single slider that adjusts both \code{h} and \code{gamma}
}
