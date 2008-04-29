
## sequencerules 
##
## ceeboo 2007

setClass("sequencerules",
    representation(
        elements = "itemsets",
        lhs      = "sgCMatrix",
        rhs      = "sgCMatrix",
        info     = "data.frame"
    ),
    contains = "associations",

    prototype(quality = data.frame(), info = data.frame()),

    validity = function(object) {
        if (dim(object@lhs)[2] != dim(object@rhs)[2])
            stop("slots 'lhs' and 'rhs' do not conform")

        if (dim(object@lhs)[1] != dim(object@elements@items)[1])
            return("slots 'items' and 'lhs' do not conform")
        if (dim(object@rhs)[1] != dim(object@elements@items)[1])
            return("slots 'items' and 'rhs' do not conform")

        if (length(object@quality) &&
            dim(object@quality)[1] != dim(object@lhs)[2])
            stop("slot 'quality' and number of rules do not conform")
        if (length(object@info) &&
            dim(object@info)[1] != dim(object@lhs)[2])
            stop("slot 'info' and number of rules do not conform")

        TRUE
    } 
)

##

setMethod("length", signature(x = "sequencerules"),
    function(x) x@lhs@Dim[2])

setMethod("[", signature(x = "sequencerules", i = "ANY", j = "missing", drop = "ANY"),
    function(x, i, j, ..., drop) {
        if (!missing(j))
            stop("incorrect number of dimensions (j not possible)")
        if (missing(i))
            return(x)
        x@lhs <- x@lhs[,i]
        x@rhs <- x@rhs[,i]
        if (length(quality))
            x@quality <- x@quality[i,, drop = FALSE]
        if (length(x@info))
            x@info <- x@info[i,, drop = FALSE]
        validObject(x, complete = TRUE) 
        x
    }
)

## this needs to be different than in arules.
## however, from the numerical point of view
## this is a bad idea.

setMethod("lhs", signature(x = "sequencerules"),
    function(x) {
        q <- if (length(x@quality) > 0)
                 data.frame(support = x@quality$confidence / 
                                      x@quality$support)
             else
                 NULL

        new("sequences", elements  = x@elements,
                         data      = x@lhs,
                         quality   = q)
    }
)

setMethod("rhs", signature(x = "sequencerules"),
    function(x) {
        q <- if (length(x@quality) > 0)
                 data.frame(support = x@quality$confidence / 
                                      x@quality$lift)
             else
                 NULL
        
        new("sequences", elements  = x@elements,
                         data      = x@rhs,
                         quality   = q)
    }
)

setMethod("labels", signature(object = "sequencerules"),
    function(object, setSep = ",", seqStart = "<", seqEnd = ">", ruleSep = " => ", decode = TRUE, ...) {
        paste(labels(lhs(object), setSep, seqStart, seqEnd, decode, ...),
              ruleSep, 
              labels(rhs(object), setSep, seqStart, seqEnd, decode, ...),
              sep = "")
    }
)

setAs("sequencerules", "data.frame",
    function(from) {
        if (!length(from))
            return(data.frame())
        if (!length(from@quality))
            return(data.frame(rule = labels(from)))
        data.frame(rule = labels(from), from@quality)
    }
)

## currently we only represent rules of the form
## <A1,A2,...> => <C> with Ai and C elements and
## A1 < A2 < ... < C.

setMethod("inspect", signature(x = "sequencerules"),
    function(x, setSep = ",", seqStart = "<", seqEnd = ">", ruleSep = "=>", decode = TRUE) {
        if (!length(x))
            return()

        q <- quality(x)

        # fixme: inefficient
        lx <- LIST(lhs(x), decode)
        rx <- LIST(rhs(x), decode)

        x <- rbind(lx, rx)
        
        i <- sapply(unlist(x, recursive = FALSE), length)
        i <- cumsum(i)

        s <- sapply(x, function(x) sum(sapply(x, length)))

        ## mark left sequences
        k <- rep(c(TRUE,FALSE), length.out = length(s))
        k <- rep(k, s)
        
        s <- cumsum(s)

        x <- unlist(x)

        tmp <- matrix(" ", 5, length(x))

        tmp[3,] <- x

        tmp[4,]   <- ","            # item separator
        tmp[4, i] <- "}"            # itemset closing delimiter
        tmp[5, i] <- setSep         # itemset (element) seperator
        tmp[5, s] <- seqEnd         # sequence closing delimiter
       
        lc <- s[seq(1,length(s),2)] # left sequence closing

        i <- c(1, i[-length(i)]+1)
        s <- c(1, s[-length(s)]+1)

        lo <- s[seq(1,length(s),2)] # right sequence opening
        
        tmp[2, i] <- "{"            # item opening delimiter
        tmp[1, s] <- seqStart       # sequence opening delimiter

        tmp[1,] <- format(tmp[1,])

        tmp <- apply(tmp, 2, paste, collapse = "")
        tmp <- format(tmp)
        
        out <- matrix("", 5+length(q), length(x)-length(rx)+1)
        
        ro <- lc + 1                # right sequence opening

        out[2, which(c(TRUE, k[-ro]))] <- c("lhs", tmp[ k])
        out[4, which(c(TRUE,!k[-lo]))] <- c("rhs", tmp[!k])

        out[2,] <- format(out[2,])
        out[4,] <- format(out[4,])
        
        # fixme: original sequence numbers
       
        k  <- seq(length(lo))
        lo <- lo - k + 2            # header
       
        out[1, lo] <- k
        out[1,]    <- format(out[1,], justify = "right")
        
        lc <- lc - k + 2            # header
     
        out[3, lc] <- ruleSep
        out[3,]    <- format(out[3,])

        for (i in seq(q)) {
            tmp <- format(q[[i]], justify = "right")
            out[4+i, c(1, lc)] <- 
                format(c(names(q)[i], tmp), justify = "right")
        }
            
        out[4+i+1,] <- "\n"
        
        cat("", out, "\n")
    }
)

#

setMethod("info", signature(object = "sequencerules"),
    function(object) object@info)

setReplaceMethod("info", signature(object = "sequencerules"),
    function(object, value) {
        object@info <- value
        validObject(value)
        object
    }
)

# fixme: extend to lhs and rhs statistics

setClass("summary.sequencerules",
    representation(
        sizes   = "table",
        lengths = "table"
    ),
    contains = "summary.associations"
)

setMethod("summary", signature(object = "sequencerules"),
    function(object) {
        if (!length(object))
            return(new("summary.sequencerules", length = 0L))

        s <- size(lhs(object)) + size(rhs(object))
        l <- size(lhs(object), "items") + 
             size(rhs(object), "items")

        q <- if (length(object) > 0)
                 summary(object@quality)
             else
                 summary(NULL)

        new("summary.sequencerules", length  = length(object),
                                     sizes   = table(sizes = s),
                                     lengths = table(lengths = l),
                                     quality = q)
    }
)

setMethod("show", signature(object = "summary.sequencerules"),
    function(object) {
        cat("set of", object@length, "sequencerules with\n")
        if (object@length) {
            cat("\nrule size distribution (lhs + rhs)\n")
            print(object@sizes)

            cat("\nrule length distribution (lhs + rhs)\n")
            print(object@lengths)

            cat("\nsummary of quality measures:\n")
            print(object@quality)
        }
    }
)

## induce sequential rules, i.e. rules of the form
## <A1, A2, ...> => <C> with Ai, and C elements and
## A1 < A2 < ... < C.
##
## todo: (1) verbose (2) transactions.

setMethod("ruleInduction", signature(x = "sequences"),
    function(x, transactions, confidence = 0.8, control = NULL) {
        if (!missing(transactions))
            stop("'transactions' not implemented")
        if (confidence < 0 || confidence > 1)
            stop("'confidence' invalid range")

        if (is.null(quality(x)))
            stop("cannot induce rules because support is missing")
            
        if (is.null(control$verbose))
            control$verbose <- FALSE
        else 
        if (!is.logical(control$verbose))
            stop("'verbose invalid range'")
        if (is.null(control$maximal))
            control$maximal <- FALSE
        else 
        if (!is.logical(control$maximal))
            stop("'maximal' invalid range")

        r <- data.frame(.Call("R_pnsindex", x@data, NULL, control$verbose))
        names(r) <- c("i", "li", "ri")

        if (!all(r$li) || !all(r$ri))
            stop("cannot induce rules because the set of sequences is incomplete")

        r$support    <- x@quality$support[r$i]
        r$confidence <- r$support /
                        x@quality$support[r$li]
        # filter
        if (control$maximal)
            r <- r[is.maximal(x),]
        r <- r[r$confidence >= confidence,]
        if (dim(r)[1] == 0)
            return(new("sequencerules"))
        r$lift       <- r$confidence / x@quality$support[r$ri]

        new("sequencerules", elements   = x@elements,
                             lhs        = x@data[,r$li],
                             rhs        = x@data[,r$ri],
                             quality    = r[4:6])
    }
)

# generatingItemsets is a misnomer
setAs("sequencerules", "sequences",
    function(from) {
        d <- .Call("R_colAppend_sgCMatrix", from@lhs, from@rhs, FALSE)
        new("sequences", elements  = from@elements,
                         data      = d,
                         quality   = from@quality["support"])
    }
)

setMethod("duplicated", signature(x = "sequencerules"),
    function(x, incomparables = FALSE) {
        i <- .Call("R_colAppend_sgCMatrix", x@lhs, x@rhs, TRUE)
        i <- .Call("R_pnindex", i, NULL, FALSE)
        duplicated(i, incomparables)
    }
)

setMethod("unique", signature(x = "sequencerules"),
    function(x, incomparables = FALSE) x[!duplicated(x)])

setMethod("match", signature(x = "sequencerules", table = "sequencerules"),
    function(x, table, nomatch = NA_integer_, incomparables = NULL) {
        k <- match(x@elements, table@elements)
        n <- which(is.na(k))
        if (length(n)) {
            k[n] <- table@lhs@Dim[1] + seq(length(n))
            table@lhs@Dim[1] <- 
            table@rhs@Dim[1] <- table@lhs@Dim[1] + length(n)
        }
        if (any(k != seq_len(length(k)))) {
            x@lhs <- .Call("R_recode_sgCMatrix", x@lhs, k)
            x@rhs <- .Call("R_recode_sgCMatrix", x@rhs, k)
        }
        if (x@lhs@Dim[1] <  table@lhs@Dim[1])
            x@lhs@Dim[1] <- 
            x@rhs@Dim[1] <- table@lhs@Dim[1]
        table <- .Call("R_colAppend_sgCMatrix", table@lhs, table@rhs, TRUE)
        x     <- .Call("R_colAppend_sgCMatrix", x@lhs, x@rhs, TRUE)
        i <- .Call("R_pnindex", table, x, FALSE)
        match(i, seq(table@Dim[2]), nomatch, incomparables)
    }
)

setMethod("c", signature(x = "sequencerules"),
    function(x, ..., recursive = FALSE) {
        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!is(y, "sequencerules"))
                stop("can only combine sequencerules")
            x <- .combineMeta(x, y, "quality")
            x <- .combineMeta(x, y, "info")
            k <- match(y@elements, x@elements)
            n <- which(is.na(k))
            if (length(n)) {
                k[n] <- x@lhs@Dim[1] + seq(length(n))
                x@lhs@Dim[1] <- 
                x@rhs@Dim[1] <- x@lhs@Dim[1] + length(n)
                x@elements <- c(x@elements, y@elements[n])
            }
            if (any(k != seq_len(length(k)))) {
                y@lhs <- .Call("R_recode_sgCMatrix", y@lhs, k)
                y@rhs <- .Call("R_recode_sgCMatrix", y@rhs, k)
            }
            if (y@lhs@Dim[1] <  x@lhs@Dim[1])
                y@lhs@Dim[1] <- y@rhs@Dim[1] <- x@lhs@Dim[1]

            x@lhs <- .Call("R_cbind_ngCMatrix", x@lhs, y@lhs)
            x@rhs <- .Call("R_cbind_ngCMatrix", x@rhs, y@rhs)
        }
        validObject(x, complete = TRUE)
        x
    }
)

# why not define for associations in arules?

setMethod("coverage", signature(x = "sequencerules"),
    function(x) {
        q <- quality(x)
        if (!all(c("support", "confidence") %in% names(q)))
            stop("support and/or confidence missing in slot 'quality'")
        q$support / q$confidence
    }
)

# fixme: we cannot dispatch as sequences.

setMethod("subset", signature(x = "sequencerules"),
    function(x, subset) {
        if (missing(subset))
            return(x)
        i <- eval(substitute(subset), 
                  envir = c(x@quality, x@info))
        x[i]
    }
)

###
