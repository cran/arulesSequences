\name{sgCMatrix-class}
\docType{class}
\alias{sgCMatrix-class}
\alias{coerce,sgCMatrix,list-method}
\alias{coerce,list,sgCMatrix-method}
\alias{coerce,ngCMatrix,sgCMatrix-method}
\alias{[,sgCMatrix,ANY,ANY,ANY-method}
\alias{dim,sgCMatrix-method}
\alias{dimnames,sgCMatrix-method}
\alias{dimnames<-,sgCMatrix,ANY-method}
\alias{show,sgCMatrix-method}
\title{Class "sgCMatrix" -- Sparse Ordered Lists of Symbols}
\description{
    Sparse pseudo matrices in column-compressed form for storing ordered
    \emph{lists} of symbols.
}
\section{Objects from the Class}{
    Most frequently, an object is created upon creation of an object of
    class \code{\linkS4class{sequences}} or\cr
    \code{\linkS4class{sequencerules}}.

    Objects can also be created by calls of the form 
    \code{new("sgCMatrix", ...)}.
}
\section{Slots}{
\describe{
    \item{\code{p}:}{an integer vector of length the number of columns
	in the matrix plus one. These are \emph{zero-based pointers} into
	\code{i}, i.e. to the first element of a list. However, note that
	the last element contains the number of elements of \code{i}.}
    \item{\code{i}:}{an integer vector of length the number of non-zero
	elements in the matrix. These are \emph{zero-based} symbol
	indexes, i.e. pointers into the row names if such exist.}
    \item{\code{Dim}:}{an integer vector representing the number of symbols
	and the number of lists.}
    \item{\code{Dimnames}:}{a list with components for symbol and
	\emph{list} labels.}
    \item{\code{factors}:}{unused, for compatibility with package
	\pkg{Matrix} only.} 
  }
}
\section{Methods}{
\describe{
    \item{\code{coerce}}{\code{signature(from = "sgCMatrix", to = "list")}}
    \item{\code{coerce}}{\code{signature(from = "list", to = "sgCMatrix")}}
    \item{\code{coerce}}{\code{signature(from = "ngCMatrix", to = "sgCMatrix")}}
    \item{\code{dim}}{\code{signature(x = "sgCMatrix")}}
    \item{\code{dimnames}}{\code{signature(x = "sgCMatrix")}}
    \item{\code{dimnames<-}}{\code{signature(x = "sgCMatrix", value = "ANY")}}
    \item{\code{show}}{\code{signature(x = "sgCMatrix")}}
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christian Buchta}
\note{
    The number of rows can be larger than the number of symbols actually
    occurring. Thus \code{i} need not be recoded upon subsetting or two
    collections of lists with the same index base can be easily combined
    (column or \emph{row}-wise).

    Many of the \emph{methods} of this class implemented in C are
    currently not interfaced as R methods.
}

\seealso{
    Class
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    \code{\linkS4class{sequencerules}}.
}
\examples{
## 3 example sequences
x <- list("01" = list(c("A","B"), "C"), 
          "02" = list("C"), 
	  "03" = list("B", "B"))

## uses paste
s <- as(x, "sgCMatrix")
s

##
dim(s)
dimnames(s)
}
\keyword{classes}
