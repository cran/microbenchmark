#include <stdint.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>
#include <R_ext/Memory.h>

#include "sexp_macros.h"

/* FIXME: Find portable 64 bit integer type. */
typedef uint64_t nanotime_t;

#if defined(WIN32) 
  #include "nanotimer_windows.h"
#elif defined(__MACH__) || defined(__APPLE__)
  #include "nanotimer_macosx.h"
#elif defined(linux) || defined(__linux)
  #include "nanotimer_linux.h"
#elif defined(sun) || defined(__sun) || defined(_AIX)
  #include "nanotimer_rtposix.h"
#else /* Unsupported OS */
  #error "Unsupported OS."
#endif

#if defined(__GNUC__)
  #define NOINLINE  __attribute__((noinline))
#else
  #define NOINLINE
#endif

SEXP do_nothing(SEXP a, SEXP b) NOINLINE;

SEXP do_nothing(SEXP a, SEXP b) {
    return a;
}

nanotime_t estimate_overhead(SEXP s_rho, int rounds) {
    int i;
    SEXP s_expr;
    volatile SEXP s_tmp;    
    /* Estimate minimal overhead and warm up the machine ... */
    nanotime_t start, end, overhead = 1 << 31;
    for (i = 0; i < rounds; ++i) {
        start = get_nanotime();
        // s_tmp = do_nothing(s_expr, s_rho);
        end = get_nanotime();

        const nanotime_t diff = end - start;
        if (diff > 0 && diff < overhead)
            overhead = diff;
    }
    return overhead;
}

SEXP do_microtiming_precision(SEXP s_rho, SEXP s_times, SEXP s_warmup) {
    UNPACK_INT(s_warmup, warmup); 
    UNPACK_INT(s_times, times);
    int j, i, n=0;
    int tmp;
    nanotime_t overhead = estimate_overhead(s_rho, warmup);
    nanotime_t start, end, precision = 0;
    SEXP s_ret;

    PROTECT(s_ret = allocVector(REALSXP, times));
    while (n < times) {
        start = get_nanotime();
        end = get_nanotime();
        if (end - start > 0) {
            REAL(s_ret)[n] = end - start - overhead;
            n++;
        }
    }    
    UNPROTECT(1); /* s_ret */
    return s_ret;
}

SEXP do_microtiming(SEXP s_exprs, SEXP s_rho, SEXP s_warmup) {
    nanotime_t start, end, overhead;
    int i, n_under_overhead = 0;
    R_len_t n_exprs = 0;
    volatile SEXP s_tmp;    
    SEXP s_ret, s_expr;
    double *ret;
   
    UNPACK_INT(s_warmup, warmup);

    /* Expressions */
    n_exprs = LENGTH(s_exprs);

    /* Environment in which to evaluate */
    if(!isEnvironment(s_rho)) 
        error("'s_rho' should be an environment");

    /* Return value: */
    PROTECT(s_ret = allocVector(REALSXP, n_exprs));
    ret = REAL(s_ret);
    
    /* Estimate minimal overhead and warm up the machine ... */
    overhead = estimate_overhead(s_rho, warmup);

    /* Actual timing... */
    for (i = 0; i < n_exprs; ++i) {
        s_expr = VECTOR_ELT(s_exprs, i);
        start = get_nanotime();
        s_tmp = eval(s_expr, s_rho);
        end = get_nanotime();
        
        const nanotime_t diff = end - start;
        if (diff < overhead) {
            ret[i] = R_NaReal;
            n_under_overhead++;
        } else {
            ret[i] = diff - overhead;
        }

        /* Housekeeping */
        R_CheckUserInterrupt();
        /* R_gc(); */
    }

    /* Issue waring if we observed some timings below the estimated
     * overhead.
     */
    if (n_under_overhead > 0) {
        if (n_under_overhead == 1) {
            warning("Estimated overhead was greater than measured evaluation time inr 1 run.");
        } else {
            warning("Estimated overhead was greater than measured evaluation time in %i runs.", 
                    n_under_overhead);
        }
    }
    
    UNPROTECT(1); /* s_ret */
    return s_ret;
}
