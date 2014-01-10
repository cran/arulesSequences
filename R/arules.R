
## missing implementations
##
## ceeboo 2014

##
setAs("list", "tidLists",
    function(from) {
	if (!all(sapply(from, is.atomic)))
	    stop("item(s) not atomic")
        i <- lapply(from, sort)
	names(i) <- NULL
        p <- sapply(i, length)
        p <- cumsum(p)
        i <- unclass(factor(unlist(i)))
        l <- attr(i, "levels")
        attr(i, "levels") <- NULL
        i <- new("ngCMatrix", p   = c(0L, p),
                              i   = i - 1L, 
                              Dim = c(length(l), length(p)))
        new("tidLists", 
            data            = i,
            itemInfo        = data.frame(labels = 
		    if (!is.null(names(from)))
			I(names(from))
		),
            transactionInfo = data.frame(labels = I(l))
        )
    }
)

##
