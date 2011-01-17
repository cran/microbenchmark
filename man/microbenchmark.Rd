\name{microbenchmark}
\alias{microbenchmark}
\title{Nanosecond accurate timing of expression evaluation.}
\usage{microbenchmark(..., list, times=100)
}
\description{Nanosecond accurate timing of expression evaluation.}
\details{\code{nanobenchmark} serves as a more accurate replacement of the
often seen \code{system.time(replicate(1000, expr))}
expression. It tries hard to accurately measure only the time it
takes to evaluate \code{expr}. To achieved this, the
sub-millisecond (supposedly nanosecond) accurate timing functions
most modern operating systems provide are used. Additionally all
evaluations of the expressions are done in C code to minimze any
overhead.

This function is only meant for micro-benchmarking small pieces of
source code and to compare their relative performance
characteristics. You should generally avoid benchmarking larger
chunks of your code using this function. Instead, try using the R
profiler to detect hot spots and consider rewriting them in C/C++
or FORTRAN.}
\note{Depending on the underlying operating system, different
methods are used for timing. On Windows the
\code{QueryPerformanceCounter} interface is used to measure the
time passed. For Linux the \code{clock_gettime} API is used and on
Solaris the \code{gethrtime} function. Finally on MacOS X the,
undocumented, \code{mach_absolute_time} function is used to avoid
a dependency on the CoreServices Framework.

Before evaluating each expression \code{times} times, the overhead
of calling the timing functions and the C function call overhead
are estimated. This estimated overhead is subtracted from each
measured evaluation time. Should the resulting timing be negative,
a warning is thrown and the respective value is replaced by
\code{NA}.}
\value{Object of class \sQuote{microbenchmark}, a matrix with one
column per exoression. Each row contains the time it took to
evaluate the respective expression one time in nanoseconds.}
\seealso{\code{\link{print.microbenchmark}} to display,
\code{\link{boxplot.microbenchmark}} or
\code{\link{ggplot.microbenchmark}} to plot and
\code{\link{as.data.frame.microbenchmark}} or
\code{\link{melt.microbenchmark}} to convert \code{microbenchmark}
objects.}
\author{Olaf Mersmann \email{olafm@datensplitter.ner}}
\arguments{\item{...}{Expressions to benchmark.}
\item{list}{List of unevaluated expression to benchmark.}
\item{times}{Number of times to evaluate the expression.}
}
\examples{## Measure the time it takes to dispatch a simple function call
## compared to simply evaluating the constant \code{NULL}
f <- function() NULL
res <- microbenchmark(NULL, f(), times=1000L)

## Print results:
print(res)

## Plot results:
boxplot(res)

## Pretty plot:
if (require("ggplot2")) {
plt <- ggplot(res)
plt <- plt + geom_line() + scale_y_log10()
print(plt)
}}

