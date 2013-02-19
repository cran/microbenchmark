\name{print.microbenchmark}
\alias{print.microbenchmark}
\title{Print \code{microbenchmark} timings.}
\usage{
  \method{print}{microbenchmark} (x, unit, order = NULL,
    ...)
}
\arguments{
  \item{x}{An object of class \code{microbenchmark}.}

  \item{unit}{What unit to print the timings in.}

  \item{order}{If present, order results according to this
  column of the output.}

  \item{...}{Passed to \code{print.data.frame}}
}
\description{
  The available units are nanoseconds (\code{"ns"}),
  microseconds (\code{"us"}), milliseconds (\code{"ms"}),
  seconds (\code{"s"}) and evaluations per seconds
  (\code{"eps"}) and relative runtime compared to the best
  median time (\code{"relative"}).
}
\author{
  Olaf Mersmann \email{olafm@p-value.net}
}
\seealso{
  \code{\link{boxplot.microbenchmark}} for a plot method.
}

