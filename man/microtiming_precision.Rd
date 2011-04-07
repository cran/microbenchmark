\name{microtiming_precision}
\alias{microtiming_precision}
\title{Estimate precision of timing routines.}
\usage{microtiming_precision(rounds=100, warmup=2^18)
}
\description{Estimate precision of timing routines.}
\details{This function is currently experimental. Its main use is to judge
the quality of the underlying timer implementation of the
operating system. The function measures the overhead of timing a C
function call \code{rounds} times and returns all non-zero timings
observed. This can be used to judge the granularity and resolution
of the timing subsystem.}
\value{A vector of observed non-zero timings.}
\author{Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}}
\arguments{\item{rounds}{Number of measurements used to estimate the precision.}
\item{warmup}{Number of iterations used to warmup the CPU.}
}

