
## ceeboo 2008

setGeneric("similarity",
    function(x, y = NULL, ...) standardGeneric("similarity")) 

## FIXME includes timed sequences
setMethod("similarity", signature(x = "sequences"),
    function(x, y = NULL, method = c("jaccard", "dice", "cosine", "subset"),
        strict = FALSE)
    {
        method <- match(match.arg(method), eval(formals()$method)) - 1L

        if (!is.null(y)) {
            if (!is(y, "sequences"))
                stop("'y' not of class sequences")
            ## conform
            k <- match(y@elements, x@elements)
            n <- which(is.na(k))
            if (length(n)) {
                k[n] <- length(x@elements) + seq(length(n))
                x@data@Dim[1] <- x@data@Dim[1] + length(n)
                if (!strict)
                    x@elements <- c(x@elements, y@elements[n])
            }
            if (any(k != seq_len(length(k))))
                y@data <- .Call(R_recode_ngCMatrix, y@data, k)
            if (y@data@Dim[1] <  x@data@Dim[1])
                y@data@Dim[1] <- x@data@Dim[1]

            dimnames(y@data) <- list(NULL, y@sequenceInfo[["sequenceID"]])
            y <- y@data
        }
        dimnames(x@data) <- list(NULL, x@sequenceInfo[["sequenceID"]])

        .Call(R_similarity_sgCMatrix,
              x@data, y, if (strict) NULL else x@elements@items@data, method)
    }
)

setMethod("is.subset", signature(x = "sequences"),
    function(x, y = NULL, proper = FALSE) {
        ## FIXME inefficient
        s <- similarity(x, y, method = "subset") > 0;
        if (proper) {
            if (!is.null(y))
                r <- similarity(y, x, method = "subset") > 0
            else 
                r <- s
            ## FIXME R-2.7.0 bug 
            s <- s & !selectMethod("t", class(s))(s)
        }
        s
    }
)

setMethod("is.superset", signature(x = "sequences"),
    function(x, y = NULL, proper = FALSE) t(is.subset(x, y, proper)))

##

setAs("dsCMatrix", "dist",
    function(from)
        .Call(R_as_dist_dsCMatrix, if (from@uplo == "L") from else t(from)))

###
