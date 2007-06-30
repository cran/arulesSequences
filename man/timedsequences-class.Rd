\name{timedsequences-class}
\docType{class}
\alias{timedsequences-class}
\alias{coerce,transactions,timedsequences-method}
\alias{coerce,timedsequences,transactions-method}
%\alias{[,timedsequences,ANY,ANY,ANY-method}
%\alias{c,timedsequences-method}
%\alias{dim,timedsequences-method}
%\alias{inspect,timedsequences-method}
%\alias{labels,timedsequences-method}
\alias{LIST,timedsequences-method}
\alias{show,timedsequences-method}
\alias{show,summary.timedsequences-method}
\alias{summary.timedsequences-class}
\alias{summary,timedsequences-method}
%\alias{timeFrequency,timedsequences-method}
%\alias{timeInfo<-,timedsequences-method}
%\alias{timeInfo,timedsequences-method}
%\alias{timeTable,timedsequences-method}
%\alias{times,timedsequences-method}
\alias{timesets}
\alias{timesets,timedsequences-method}

\title{Class "timedsequences" --- Collections of Sequences with Timining Information}
\description{
    Represents a collection of (observed) sequences and the associated timing
    information.
}
\section{Objects from the Class}{
    Typically, objects are created by coercion from an object of class
    \code{\link[arules:transactions-class]{transactions}}.

    Objects can also be created by calls of the form
    \code{new("timedsequences", ...)}.
}
\section{Slots}{
\describe{
    \item{\code{time}:}{an object of class
	\code{\link[Matrix:ngCMatrix-class]{ngCMatrix"}} containing a sparse
	representation of the event times of the elements of the sequences.
	note that the storage layout is the same as for slot \code{data}.}
    \item{\code{timeInfo}:}{a data frame containing the set of time
	identifiers (column \code{eventID}) and possibly distinct labels.}
    \item{\code{elements}:}{inherited from class \code{\linkS4class{sequences}}.}
    \item{\code{data}:}{inherited from class \code{\linkS4class{sequences}}.}
    \item{\code{info}:}{inherited from class \code{\linkS4class{sequences}}.}
    \item{\code{quality}:}{inherited from class \code{\linkS4class{sequences}},
	usually empty.}
  }
}
\section{Extends}{
    Class \code{"\linkS4class{sequences}"}, directly.
    Class \code{"\link[arules:associations-class]{associations}"}, by class
    "sequences", distance 2.
}
\section{Methods}{
\describe{
    \item{\code{coerce}}{\code{signature(from = "transactions", to = "timedsequences")}}
    \item{\code{coerce}}{\code{signature(from = "timedsequences", to = "transactions")}}
    \item{\code{c}}{\code{signature(x = "timedsequences")}}
    \item{\code{dim}}{\code{signature(x = "timedsequences")}}
    \item{\code{labels}}{\code{signature(object = "timedsequences")}}
    \item{\code{LIST}}{\code{signature(x = "timedsequences")}}
    \item{\code{inspect}}{\code{signature(x = "timedsequences")}}
    \item{\code{show}}{\code{signature(object = "timedsequences")}}
    \item{\code{summary}}{\code{signature(object = "timedsequences")}}
    \item{\code{timeFrequency}}{\code{signature(x = "timedsequences")}}
    \item{\code{timeInfo<-}}{\code{signature(object = "timedsequences")}}
    \item{\code{timeInfo}}{\code{signature(object = "timedsequences")}}
    \item{\code{timesets}}{\code{signature(object = "timedsequences")}}
    \item{\code{times}}{\code{signature(x = "timedsequences")}}
    \item{\code{timesets}}{\code{signature(x = "timedsequences")};
	returns a collection of sequences of event times as an object of
	class \code{\link[arules:itemMatrix]{itemMatrix}}.}
    \item{\code{timeTable}}{\code{signature(x = "timedsequences")}}
    }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christian Buchta}
\note{
    The temporal information is taken from components \code{sequenceID} 
    and \code{eventID} of slot\cr\code{transactionInfo}. It may be either
    on an ordinal or metric scale. The former is always assumed if column
    \code{eventID} is a factor.

    Note that a sequence must not contain two or more events with the
    same \code{eventID}.
    
    Coercion from an object of class \code{\linkS4class{sequences}} is
    not provided as this class does not contain timing information.

    FIXME currently objects of this class cannot be used in sequence
    mining, i.e. are for analysis only.
}
\seealso{
    Class
    \code{\link[arules:itemMatrix]{itemMatrix}},
    \code{\link[arules:transactions]{transactions}},
    \code{\linkS4class{sequences}}.
}
\examples{
## use example data
data(zaki)

## coerce
z <- as(zaki, "timedsequences")
z

## get time sequences
summary(timesets(z))

## coerce back
as(z, "transactions")
}
\keyword{classes}
