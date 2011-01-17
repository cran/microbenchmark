\name{melt.microbenchmark}
\alias{melt.microbenchmark}
\title{Convert / melt a microbenchmark object into a...}
\usage{\method{melt}{microbenchmark}(data, unit=c("ns", "ms", "eps", "slowdown", "speedup"), ...)
\method{as.data.frame}{microbenchmark}(x, ...)
}
\description{Convert / melt a \code{microbenchmark} object into a
\code{data.frame} ready to be \code{\link{cast}}.}
\value{\code{melt.microbenchmark}: A \code{data.frame} with columns \sQuote{run},
\sQuote{expr} and \sQuote{value}, containing the run number,
expression (as a string) and the time in nanoseconds.

\code{as.data.frame.microbenchmark}: The same \code{data.frame} returned by \code{melt}.


}
\author{Olaf Mersmann \email{olafm@datensplitter.net}}
\arguments{\item{data}{A \code{microbenchmark} object.}
\item{x}{A \code{microbenchmark} object.}
\item{unit}{Unit in which the results be plotted.}
\item{...}{Ignored.}
}
\alias{as.data.frame.microbenchmark}

