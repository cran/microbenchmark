\name{print.microbenchmark}
\alias{print.microbenchmark}
\title{Print microbenchmark timings.}
\usage{\method{print}{microbenchmark}(x, unit="t", ...)
}
\description{Print \code{microbenchmark} timings.}
\details{The available units are nanoseconds (\code{"ns"}), microseconds
(\code{"us"}), milliseconds (\code{"ms"}), seconds (\code{"s"})
and evaluations per seconds (\code{"eps"}).}
\seealso{\code{\link{boxplot.microbenchmark}} for a plot method.}
\author{Olaf Mersmann \email{olafm@datensplitter.net}}
\arguments{\item{x}{An object of class \code{microbenchmark}.}
\item{unit}{What unit to print the timings in.}
\item{...}{Ignored.}
}

