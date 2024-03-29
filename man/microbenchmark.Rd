% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbenchmark.R
\name{microbenchmark}
\alias{microbenchmark}
\title{Sub-millisecond accurate timing of expression evaluation.}
\usage{
microbenchmark(
  ...,
  list = NULL,
  times = 100L,
  unit = NULL,
  check = NULL,
  control = list(),
  setup = NULL
)
}
\arguments{
\item{...}{Expressions to benchmark.}

\item{list}{List of unevaluated expressions to benchmark.}

\item{times}{Number of times to evaluate each expression.}

\item{unit}{Default unit used in \code{summary} and \code{print}.}

\item{check}{A function to check if the expressions are equal. By default \code{NULL} which omits the check.
In addition to a function, a string can be supplied.
The string \sQuote{equal} will compare all values using \code{\link{all.equal}}, \sQuote{equivalent} will compare all values using \code{\link{all.equal}} and check.attributes = FALSE, and \sQuote{identical} will compare all values using \code{\link{identical}}.}

\item{control}{List of control arguments. See Details.}

\item{setup}{An unevaluated expression to be run (untimed) before each benchmark expression.}
}
\value{
Object of class \sQuote{microbenchmark}, a data frame with
columns \code{expr} and \code{time}. \code{expr} contains the
deparsed expression as passed to \code{microbenchmark} or the name
of the argument if the expression was passed as a named
argument. \code{time} is the measured execution time of the
expression in nanoseconds. The order of the observations in the
data frame is the order in which they were executed.
}
\description{
\code{microbenchmark} serves as a more accurate replacement of the
often seen \code{system.time(replicate(1000, expr))}
expression. It tries hard to accurately measure only the time it
takes to evaluate \code{expr}. To achieved this, the
sub-millisecond (supposedly nanosecond) accurate timing functions
most modern operating systems provide are used. Additionally all
evaluations of the expressions are done in C code to minimize any
overhead.
}
\details{
This function is only meant for micro-benchmarking small pieces of
source code and to compare their relative performance
characteristics. You should generally avoid benchmarking larger
chunks of your code using this function. Instead, try using the R
profiler to detect hot spots and consider rewriting them in C/C++
or FORTRAN.

The \code{control} list can contain the following entries:
\describe{
\item{order}{the order in which the expressions are evaluated.
  \dQuote{random} (the default) randomizes the execution order,
  \dQuote{inorder} executes each expression in order and
  \dQuote{block} executes all repetitions of each expression
    as one block.}
\item{warmup}{the number of warm-up iterations performed before
  the actual benchmark. These are used to estimate the timing
  overhead as well as spinning up the processor from any sleep
  or idle states it might be in. The default value is 2.}
}
}
\note{
Depending on the underlying operating system, different
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
\code{0}. If the timing is zero, a warning is raised.
Should all evaluations result in one of the two error conditions described above, an error is raised.

One platform on which the clock resolution is known to be too low to measure short runtimes with the required precision is 
Oracle\if{html}{\out{&reg;}}\if{latex}{\out{\textregistered\ }}\if{text}{(R)}
Solaris 
on some 
SPARC\if{html}{\out{&reg;}}\if{latex}{\out{\textregistered\ }}\if{text}{(R)}
hardware.
Reports of other platforms with similar problems are welcome.
Please contact the package maintainer.
}
\examples{
## Measure the time it takes to dispatch a simple function call
## compared to simply evaluating the constant \code{NULL}
f <- function() NULL
res <- microbenchmark(NULL, f(), times=100L)

## Print results:
print(res)

dput(res)

## Plot results:
boxplot(res)

## Pretty plot:
if (requireNamespace("ggplot2")) {
  ggplot2::autoplot(res)
}

## Example check usage
my_check <- function(values) {
  all(sapply(values[-1], function(x) identical(values[[1]], x)))
}

f <- function(a, b)
  2 + 2

a <- 2
## Check passes
microbenchmark(2 + 2, 2 + a, f(2, a), f(2, 2), check=my_check)
\dontrun{
a <- 3
## Check fails
microbenchmark(2 + 2, 2 + a, f(2, a), f(2, 2), check=my_check)
}
## Example setup usage
set.seed(21)
x <- rnorm(10)
microbenchmark(x, rnorm(10), check=my_check, setup=set.seed(21))
## Will fail without setup
\dontrun{
microbenchmark(x, rnorm(10), check=my_check)
}
## using check
a <- 2
microbenchmark(2 + 2, 2 + a, sum(2, a), sum(2, 2), check='identical')
microbenchmark(2 + 2, 2 + a, sum(2, a), sum(2, 2), check='equal')
attr(a, 'abc') <- 123
microbenchmark(2 + 2, 2 + a, sum(2, a), sum(2, 2), check='equivalent')
## check='equal' will fail due to difference in attribute
\dontrun{
microbenchmark(2 + 2, 2 + a, sum(2, a), sum(2, 2), check='equal')
}
}
\seealso{
\code{\link{print.microbenchmark}} to display and
\code{\link{boxplot.microbenchmark}} or
\code{\link{autoplot.microbenchmark}} to plot the results.
}
\author{
Olaf Mersmann
}
