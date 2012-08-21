
## ceeboo 2008

setMethod("support", signature(x = "sequences"),
    function(x, transactions, type = c("relative", "absolute"), control = NULL)
    {
        ## FIXME allow sequences?
        if (!inherits(transactions, "transactions"))
            stop("'transactions' not of class transactions")

        type <- match.arg(type)
        verbose <- if (is.null(control$verbose)) FALSE else control$verbose

        if (!is.null(control$method))
            warning("method is not supported")

#        if (!length(x))
#            return (switch(type, relative = double(),
#                                 absolute = integer()))
#        if (!length(transactions))
#            return (switch(type, relative = rep(0,  length(x)),
#                                 absolute = rep(0L, length(x))))

        if (verbose) {
            t0 <- proc.time()
            cat("preprocessing ...")
        }

        ## FIXME this is inefficient as we currently
        ##       use the ordering information only.
        y <- as(transactions, "timedsequences")

        ## conform
        k <- match(y@elements, x@elements)
        n <- which(is.na(k))
        if (length(n)) {
            k[n] <- length(x@elements) + seq(length(n))
            x@data@Dim[1] <- x@data@Dim[1] + length(n)
            x@elements <- c(x@elements, y@elements[n])
        }
        if (any(k != seq_len(length(k))))
            y@data <- .Call(R_recode_ngCMatrix, y@data, k)
        if (y@data@Dim[1] <  x@data@Dim[1])
            y@data@Dim[1] <- x@data@Dim[1]
        ## order
        k <- order(.Call(R_colSums_ngCMatrix, x@elements@items@data),
                   .Call(R_pnindex, x@elements@items@data, NULL, FALSE))
        if (any(k != seq_len(length(k)))) {
            x@elements <- x@elements[k]
            k[k] <- seq_len(length(k))
            x@data <- .Call(R_recode_ngCMatrix, x@data, k)
            y@data <- .Call(R_recode_ngCMatrix, y@data, k)
        }

        if (verbose) {
            t1 <- proc.time()
            cat("[", t1[1]-t0[1], "s]\n", sep = "")
        }

        supports <- .Call(R_pnscount, x@data, y@data,
            x@elements@items@data, verbose)

        switch(type,
            relative = supports / y@data@Dim[2],
            absolute = supports)
    }
)

###
