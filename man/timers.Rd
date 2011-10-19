\name{timing_issues}
\alias{timing_issues}
\title{Timing issues}
\description{Platform specific tips to deal with strange timing issues.}
\details{
  Measuring short time intervals accurately on modern machines remains a
  non-trivial task. There are many factors which can invalidate the
  results.

  The biggest problem is that time can appear to go backwards.  There
  are several causes for this. The most likely are

  \itemize{
    \item You are running under a hypervisor. This can do all sorts of
    things to the timing functions of your operating system.

    \item You have frequency scaling turned on. Most modern CPUs can
    reduce their core frequency if they are not busy. microbenchmark
    tries hard to spin up the CPU before the actual timing, but there is
    no guarantee this works, so you are advised to disable this
    feature. Under Linux this can be done using the 'cpufreq' utilities.

    \item You have a machine with many CPU cores and the timers provided
    by your operating system are not synchronized across cores. Your
    best bet is to peg your R process to a single core. On Linux
    systems, this can be achieved using the 'taskset' utility.

    \item Your machine is super fast. If the difference between the
    estimated overhead and the actual execution time is zero (or
    possibly even negative), you will get this error. Sorry, you're out
    of luck, benchmark complexer code.
  }
 
  Please contact the package maintainer if none of these issues are the
  root cause of the timing issues you are experiencing. 
}
