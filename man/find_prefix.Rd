\name{find_prefix}
\alias{find_prefix}
\title{Find SI prefix for unit...}
\usage{find_prefix(x, f=min, minexp=-Inf, maxexp=Inf, mu=TRUE)
}
\description{Find SI prefix for unit}
\value{character with the SI prefix}
\author{Claudia Beleites}
\arguments{\item{x}{a numeric}
\item{f}{function that produces the number from \code{x} that is used to determine the
prefix, e.g. \code{\link[base]{min}} or \code{\link[stats]{median}}.}
\item{minexp}{minimum (decimal) exponent to consider, 
e.g. -3 to suppress prefixes smaller than milli (m).}
\item{maxexp}{maximum (decimal) exponent to consider, 
e.g. 3 to suppress prefixes larger than kilo (k).}
\item{mu}{if \code{TRUE}, should a proper mu is used for micro, otherwise use
u as ASCII-compatible replacement}
}

