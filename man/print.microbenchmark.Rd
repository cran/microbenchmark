\name{print.microbenchmark}
\alias{print.microbenchmark}
\title{Print nanosecound timings.}
\usage{\method{print}{microbenchmark}(x, unit=c("ns", "ms", "eps", "slowdown", "speedup"), ...)
}
\description{Print nanosecound timings.}
\details{The available units are nanoseconds (\code{"ns"}), milliseconds
(\code{"ms"}), evaluations per seconds (\code{"eps"}), slowdown
compared to the fastest expression (\code{"slowdown"}) and speedup
compared to slowest expression (\code{"speedup"}).}
\author{Olaf Mersmann \email{olafm@datensplitter.net}}
\arguments{\item{x}{An object of class \code{microbenchmark}.}
\item{unit}{What unit to print the timings in.}
\item{...}{Ignored.}
}

