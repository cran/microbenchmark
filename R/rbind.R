#' @method rbind microbenchmark
rbind.microbenchmark <-
function(..., deparse.level = 1)
{
    args <- list(...)
    is_mb <- sapply(args, inherits, "microbenchmark")
    if(!all(is_mb)) {
        stop("can only combine microbenchmark objects")
    }

    output <- args
    arg_names <- as.character(substitute(alist(...))[-1])
    all_levels <- NULL
    for(i in seq_along(output)) {
        # expressions and levels for object 'i'
        i_expr <- output[[i]]$expr
        i_levels <- levels(i_expr)
        # all levels so far...
        all_levels <- c(all_levels, i_levels)
        # add object name to suffix for duplicated expressions
        has_dups <- utils::tail(duplicated(all_levels), -nlevels(i_expr))
        if (any(has_dups)) {
            levels(output[[i]]$expr) <- paste(i_levels, arg_names[i], sep = ".")
        }
    }
    output <- do.call("rbind.data.frame", output)
    class(output) <- class(args[[1]])
    return(output)
}


    #arg_names <- names(args)  # if called via do.call()
    #arg_symbols <- as.character(substitute(alist(...))[-1])
    #if (is.null(arg_names)) {
    #    arg_names <- arg_symbols
    #}
    #no_name <- arg_names == ""
    #arg_names[no_name] <- seq_along(arg_names)[no_name]

    #all_expr <- lapply(args, function(x) x$expr)
    #all_levels <- sapply(all_expr, levels)
    #is_dupe <- duplicated(all_levels)
    #positions <- rep(seq_along(args), times = sapply(all_expr, nlevels))
    #unique_levels <- make.unique(all_levels)
