\name{SPparameter-class}
\docType{class}
\alias{SPparameter-class}
\alias{coerce,NULL,SPparameter-method}
\alias{coerce,list,SPparameter-method}
\alias{coerce,SPparameter,character-method}
\alias{coerce,SPparameter,data.frame-method}
\alias{coerce,SPparameter,list-method}
\alias{coerce,SPparameter,vector-method}
\alias{format,SPparameter-method}
\alias{initialize,SPparameter-method}
\alias{show,SPparameter-method}
\title{Class "SPparameter" --- cSPADE Mining Parameters}
\description{
    Provides the constraint parameters for the cSPADE algorithm for mining
    frequent sequences.
}
\section{Objects from the Class}{
    A suitable default parameter object will be automatically created
    by a call to \code{\link{cspade}}. However, the values can be replaced
    by specifying a named list with the names (partially) matching the 
    slot names of the \code{\linkS4class{SPparameter}} class.

    Objects can be created by calls of the form 
    \code{new("SPparameter", support, ...)}.
}
\section{Slots}{
\describe{
    \item{\code{support}:}{a numeric value specifying the minimum support
	of a sequence
	(default 0.1, range [0,1]).}
    \item{\code{maxsize}:}{an integer value specifying the maximum number
	of items of an element of a sequence
	(default 10, range > 0).}
    \item{\code{maxlen}:}{an integer value specifying the maximum number
	of elements of a sequence (default 10, range > 0).}
    \item{\code{mingap}:}{an integer value specifying the minimum time 
	difference between consecutive elements of a sequence
	(default none, range >= 1).}
    \item{\code{maxgap}:}{an integer value specifying the maximum time
	difference between consecutive elements of a sequence
	(default none, range >= 0).}
    \item{\code{maxwin}:}{an integer value specifying the maximum time
	difference between any two elements of a sequence
	(default none, range >= 0).}
  }
}
\section{Methods}{
\describe{
    \item{\code{coerce}}{
	\code{signature(from = "NULL", to = "SPparameter")}}
    \item{\code{coerce}}{
	\code{signature(from = "list", to = "SPparameter")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPparameter", to = "character")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPparameter", to = "data.frame")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPparameter", to = "list")}}
    \item{\code{coerce}}{
	\code{signature(from = "SPparameter", to = "vector")}}
    \item{\code{format}}{
	\code{signature(x = "SPparameter")}}
  }
}
\author{Christian Buchta}
\note{ 
    User-supplied values are silently coerced to the target class, e.g.
    \code{integer}.

    Parameters with no (default) value are not supplied to the mining
    algorithm, i.e., take the default values implemented there. A value
    can be unset using \code{NULL}.
}
\seealso{
    Class
    \code{\linkS4class{SPcontrol}},
    function
    \code{\link{cspade}}.
}
\examples{
## coerce from list 
p <- as(list(maxsize = NULL, maxwin = 5), "SPparameter")
p

## coerce to
as(p, "vector")
as(p, "data.frame")
}
\keyword{classes}
