\name{SPcontrol-class}
\docType{class}
\alias{SPcontrol-class}
\alias{coerce,NULL,SPcontrol-method}
\alias{coerce,list,SPcontrol-method}
\alias{coerce,SPcontrol,character-method}
\alias{coerce,SPcontrol,data.frame-method}
\alias{coerce,SPcontrol,list-method}
\alias{coerce,SPcontrol,vector-method}
\alias{format,SPcontrol-method}
\alias{initialize,SPcontrol-method}
\alias{show,SPcontrol-method}
\title{Class "SPcontrol" --- cSPADE Control Parameters}
\description{
    Provides control parameters for the cSPADE algorithm for mining 
    frequent sequences.
}
\section{Objects from the Class}{
    A suitable default parameter object will be automatically created
    by a call to \code{\link{cspade}}. However, the values can be replaced
    by specifying a named list with the names (partially) matching the
    slot names of the \code{\linkS4class{SPparameter}} class.

    Objects can be created by calls of the form 
    \code{new("SPcontrol", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{memsize}:}{an integer value specifying the maximum amount
	of memory to use (default none [32 MB], range >= 16).}
    \item{\code{numpart}:}{an integer value specifying the number of
	database partitions to use (default auto, range > 1).}
    \item{\code{bfstype}:}{a logical value specifying if a breadth-first
	type of search should be performed (default \code{FALSE} [DFS]).}
    \item{\code{verbose}:}{a logical value specifying if progress and
	runtime information should be displayed (default \code{FALSE}).}
    \item{\code{summary}:}{a logical value specifying if summary 
	information should be logged (default \code{FALSE}).}
  }
}
\section{Methods}{
\describe{
    \item{\code{coerce}}{
        \code{signature(from = "NULL", to = "SPcontrol")}}
    \item{\code{coerce}}{
        \code{signature(from = "list", to = "SPcontrol")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPcontrol, to = "character")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPcontrol, to = "data.frame")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPcontrol, to = "list")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPcontrol, to = "vector")}}
    \item{\code{format}}{
	\code{signature(x = "SPcontrol")}}
  }
}
\author{Christian Buchta}
\note{
    User-supplied values are silently coerced to the target class, e.g.
    \code{integer}.

    Parameters with no (default) value are not supplied to the mining
    algorithm, i.e., take the default values implemented there. A
    default can be unset using \code{NULL}.

    The value of \code{memsize} implicitly determines the number of
    database partitions used unless overridden by \code{numpart}.
    Usually, the more partitions the less the runtime in the mining stage.
    However, there may be a trade-off with preprocessing time.

    If \code{summary = TRUE} output on the console from the system
    calls in the preprocessing and mining steps are written to the
    file \emph{summary.out} in the current working directory.
}
\seealso{
    Class 
    \code{\linkS4class{SPparameter}},
    function
    \code{\link{cspade}}.
}
\examples{
## coerce from list 
p <- as(list(verbose = TRUE), "SPcontrol")
p

## coerce to
as(p, "vector")
as(p, "data.frame")
}
\keyword{classes}
