\name{sequences-class}
\docType{class}
\alias{sequences-class}
\alias{coerce,sequences,list-method}
\alias{coerce,sequences,data.frame-method}
\alias{coerce,list,sequences-method}
%\alias{[,sequences,ANY,ANY,ANY-method}
%\alias{\%in\%,sequences,character-method}
%\alias{\%ain\%,sequences,character-method}
%\alias{\%pin\%,sequences,character-method}
%\alias{\%ein\%,sequences,character-method}
%\alias{c,sequences-method}
%\alias{dim,sequences-method}
%\alias{duplicated,sequences-method}
%\alias{sequenceInfo,sequences-method}
%\alias{sequenceInfo<-,sequences-method}
%\alias{inspect,sequences-method}
\alias{is.closed,sequences-method}
\alias{is.maximal,sequences-method}
%\alias{is.subset,sequences-method}
%\alias{is.superset,sequences-method}
%\alias{itemFrequency,sequences-method}
%\alias{itemInfo,sequences-method}
%\alias{itemInfo<-,sequences-method}
%\alias{itemLabels,sequences-method}
%\alias{itemLabels<-,sequences-method}
%\alias{itemLabels<-,sequences-method}
\alias{itemsets}
\alias{itemsets,sequences-method}
%\alias{itemTable,sequences-method}
%\alias{itemTable}
%\alias{labels,sequences-method}
%\alias{length,sequences-method}
\alias{LIST,sequences-method}
%\alias{match,sequences,sequences-method}
%\alias{nitems,sequences-method}
%\alias{ruleInduction,sequences-method}
\alias{show,sequences-method}
\alias{show,summary.sequences-method}
%\alias{size,sequences-method}
\alias{summary.sequences-class}
\alias{summary,sequences-method}
%\alias{support,sequences-method}
%\alias{unique,sequences-method}
%\alias{subset,sequences-method}
%\alias{tidLists,sequences-method}
\title{Class "sequences" --- Collections of Sequences}
\description{
    Represents a collection of sequences and the associated quality measures.
}
\section{Objects from the Class}{
    Most frequently, objects are created by a sequence mining algorithm
    such as cSPADE as the return value.

    Objects can also be created by calls of the form 
    \code{new("sequences", ...)}.
}
\section{Slots}{
\describe{
    \item{\code{elements}:}{an object of class
	\code{\link[arules:itemsets-class]{itemsets}}
	containing a sparse representation of the unique elements of a 
	sequence.}
    \item{\code{data}:}{an object of class \code{\linkS4class{sgCMatrix}}
	containing a sparse representation of ordered \emph{lists} 
	(collections of) indexes into the unique elements.}
    \item{\code{sequenceInfo}:}{a data frame which may contain additional
	information on a sequence.}
    \item{\code{quality}:}{a data.frame containing the quality measures
	of a sequence.}
    \item{\code{tidLists}:}{an object of class \code{\link[arules:tidLists-class]{tidLists}}
	mapping supporting sequences, or \code{NULL}.}
  }
}
\section{Extends}{
    Class \code{"\link[arules:associations-class]{associations}"}, directly.
}
\section{Methods}{
    \describe{
	\item{\code{coerce}}{\code{signature(from = "sequences", to = "list")}}
	\item{\code{coerce}}{\code{signature(from = "sequences", to = "data.frame")}}
	\item{\code{coerce}}{\code{signature(from = "list", to = "sequences")}}
	\item{\code{\%in\%}}{\code{signature(x = "sequences", table = "character")}}
	\item{\code{\%ain\%}}{\code{signature(x = "sequences", table = "character")}}
	\item{\code{\%pin\%}}{\code{signature(x = "sequences", table = "character")}}
	\item{\code{\%ein\%}}{\code{signature(x = "sequences", table = "character")}}
	\item{\code{c}}{\code{signature(x = "sequences")}}
	\item{\code{dim}}{\code{signature(x = "sequences")}}
	\item{\code{duplicated}}{\code{signature(x = "sequences")}}
	\item{\code{labels}}{\code{signature(object = "sequences")}}
	\item{\code{length}}{\code{signature(x = "sequences")}}
	\item{\code{LIST}}{\code{signature(x = "sequences")}}
	\item{\code{match}}{\code{signature(x = "sequences")}}
	\item{\code{nitems}}{\code{signature(x = "sequences")}}
	\item{\code{sequenceInfo}}{\code{signature(object = "sequences")}}
	\item{\code{sequenceInfo<-}}{\code{signature(object = "sequences")}}
	\item{\code{inspect}}{\code{signature(x = "sequences")}}
	\item{\code{is.closed}}{\code{signature(x = "sequences")};
	    returns a logical vector indicating if a sequence has
	    no proper superset in \code{x} which has the same support.}
	\item{\code{is.maximal}}{\code{signature(x = "sequences")};
	    returns a logical vector indicating if a sequence is not a
	    subsequence of any other sequence in \code{x}.}
	\item{\code{is.subset}}{\code{signature(x = "sequences")}}
	\item{\code{is.superset}}{\code{signature(x = "sequences")}}
	\item{\code{itemFrequency}}{\code{signature(x = "sequences")}}
	\item{\code{itemInfo}}{\code{signature(object = "sequences")}}
	\item{\code{itemInfo<-}}{\code{signature(object = "sequences")}}
	\item{\code{itemLabels}}{\code{signature(object = "sequences")}}
	\item{\code{itemLabels<-}}{\code{signature(object = "sequences")}}
	\item{\code{itemTable}}{\code{signature(x = "sequences")}}
	\item{\code{itemsets}}{\code{signature(x = "sequences")};
	    returns the reference set of distinct
	    \code{\link[arules:itemsets-class]{itemsets} (elements)}.}
	\item{\code{ruleInduction}}{\code{signature(x = "sequences")}}
	\item{\code{show}}{\code{signature(object = "sequences")}}
	\item{\code{size}}{\code{signature(x = "sequences")}}
	\item{\code{subset}}{\code{signature(x = "sequences")}}
	\item{\code{summary}}{\code{signature(object = "sequences")}}
	\item{\code{support}}{\code{signature(x = "sequences")}}
	\item{\code{unique}}{\code{signature(x = "sequences")}}
    }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christian Buchta}
\note{
    Coercion from an object of class
    \code{\link[arules:transactions-class]{transactions}} with
    temporal information to an object of class \code{\linkS4class{sequences}}
    is not provided as this information would be lost. Use class
    \code{\linkS4class{timedsequences}} instead.

    Currently, a general method for concatenation of sequences similar
    to \code{cbind}, is not provided.
}
\seealso{
    Class
    \code{\linkS4class{sgCMatrix}},
    \code{\linkS4class{timedsequences}},
    \code{\link[arules:itemsets-class]{itemsets}},
    \code{\link[arules:associations-class]{associations}},
    method 
    \code{\link{ruleInduction}},
    FIXME, 
    function 
    \code{\link{cspade}},
    data
    \code{\link{zaki}}.
}
\examples{
## 3 example sequences
x <- list("01" = list(c("A","B"), "C"), 
          "02" = list("C"), 
	  "03" = list("B", "B"))

## coerce
s <- as(x, "sequences")
as(s, "data.frame")

## get reference set
as(itemsets(s), "data.frame")
}
\keyword{classes}
