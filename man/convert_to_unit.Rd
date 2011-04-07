\name{convert_to_unit}
\alias{convert_to_unit}
\title{Convert timings to different units.}
\usage{convert_to_unit(x, unit=c("ns", "us", "ms", "s", "hz", "khz", "mhz", "eps"))
}
\description{Convert timings to different units.}
\details{The following units of time are supported \describe{
\item{\dQuote{ns}}{Nanoseconds.}
\item{\dQuote{us}}{Microseconds.}
\item{\dQuote{ms}}{Milliseconds.}
\item{\dQuote{s}}{Seconds.}
\item{\dQuote{hz}}{Hertz / evaluations per second.}
\item{\dQuote{eps}}{Evaluations per second / Hertz.}
\item{\dQuote{khz}}{Kilohertz / 1000s of evaluations per second.}
\item{\dQuote{mhz}}{Megahertz / 1000000s of evaluations per second.}
}}
\value{A matrix containing the converted time values with an
attribute \code{unit} which is a printable name of the unit of
time.}
\author{Olaf Mersmann \email{olafm@datensplitter.net}}
\arguments{\item{x}{An \code{microbenchmark} object.}
\item{unit}{A unit of time. See details.}
}

