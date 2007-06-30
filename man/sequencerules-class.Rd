\name{sequencerules-class}
\docType{class}
\alias{sequencerules-class}
\alias{coerce,sequencerules,list-method}
\alias{coerce,sequencerules,data.frame-method}
\alias{coerce,sequencerules,sequences-method}
\alias{generatingItemsets}
%\alias{[,sequencerules,ANY,missing,ANY-method}
%\alias{c,sequencerules-method}
\alias{coverage,sequencerules-method}
%\alias{duplicated,sequencerules-method}
%\alias{info,sequencerules-method}
%\alias{info<-,sequencerules-method}
%\alias{inspect,sequencerules-method}
%\alias{labels,sequencerules-method}
%\alias{length,sequencerules-method}
%\alias{lhs,sequencerules-method}
%\alias{lhs}
%\alias{match,sequencerules,sequencerules-method}
\alias{summary,sequencerules-method}
%\alias{rhs,sequencerules-method}
%\alias{rhs}
\alias{show,sequencerules-method}
\alias{show,summary.sequencerules-method}
\alias{summary.sequencerules-class}
\alias{summary,sequencerules-method}
%\alias{subset,sequencerules-method}
%\alias{unique,sequencerules-method}
\title{Class "sequencerules" --- Collections of Sequential Rules}
\description{
    Represents a collection of sequential rules and their associated quality
    measure. That is, the elements in the consequent occur at a later
    time than the elements of the antecedent.
}
\section{Objects from the Class}{
    Typically objects are created by a sequence rule mining algorithm as the
    result value, e.g. method \code{\link{ruleInduction}}.

    Objects can be created by calls of the form 
    \code{new("sequencerules", ...)}.
}
\section{Slots}{
\describe{
    \item{\code{elements}:}{an object of class
	\code{\link[arules:itemsets-class]{itemsets}}
	containing a sparse representation of the unique elements of a 
	sequence.}
    \item{\code{lhs}:}{an object of class \code{\linkS4class{sgCMatrix}}
	containing a sparse representation of the left-hand sides of the
	rules (antecedent sequences).}
    \item{\code{rhs}:}{an object of class \code{\linkS4class{sgCMatrix}}
	containing a sparse representation of the right-hand sides of the
	rules (consequent sequences).}
    \item{\code{info}:}{a data.frame which may contain additional
	information on a sequence rule.}
    \item{\code{quality}:}{a data.frame containing the quality measures
	of a sequence rule.}
  }
}
\section{Extends}{
Class \code{"\link[arules:associations-class]{associations}"}, directly.
}
\section{Methods}{
\describe{
    \item{\code{coerce}}{\code{signature(from = "sequencerules", to = "list")}}
    \item{\code{coerce}}{
	\code{signature(from = "sequencerules", to = "data.frame")}}
    \item{\code{coerce}}{
	\code{signature(from = "sequencerules", to = "sequences")};
	coerce a collection of sequence rules to a collection of sequences
	by appending to each left-hand (antecedent) sequence its right-hand
	(consequent) sequence.}
    \item{\code{c}}{\code{signature(x = "sequencerules")}}
    \item{\code{coverage}}{\code{signature(x = "sequencerules")};
	returns the support values of the left-hand side (antecedent)
	sequences.}
    \item{\code{duplicated}}{\code{signature(x = "sequencerules")}}
    \item{\code{labels}}{\code{signature(x = "sequencerules")}}
    \item{\code{info}}{\code{signature(object = "sequencerules")}}
    \item{\code{info<-}}{\code{signature(object = "sequencerules")}}
    \item{\code{inspect}}{\code{signature(x = "sequencerules")}}
    \item{\code{labels}}{\code{signature(object = "sequencerules")}}
    \item{\code{length}}{\code{signature(x = "sequencerules")}}
    \item{\code{lhs}}{\code{signature(x = "sequencerules")}}
    \item{\code{match}}{\code{signature(x = "sequencerules")}}
    \item{\code{rhs}}{\code{signature(x = "sequencerules")}}
    \item{\code{show}}{\code{signature(object = "sequencerules")}}
    \item{\code{size}}{\code{signature(x = "sequencerules")}}
    \item{\code{subset}}{\code{signature(x = "sequencerules")}}
    \item{\code{summary}}{\code{signature(object = "sequencerules")}}
    \item{\code{unique}}{\code{signature(x = "sequencerules")}}
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christian Buchta}
\note{
    Some of the methods for sequences are not implemented as objects of
    this class can be coerced to sequences.
}
\seealso{
    Class
    \code{\linkS4class{sgCMatrix}},
    \code{\link[arules:itemsets-class]{itemsets}},
    \code{\link[arules:associations-class]{associations}},
    \code{\linkS4class{sequences}},
    method
    \code{\link{ruleInduction}},
    function
    \code{\link{cspade}}
}
\examples{
## continue example
example(ruleInduction, package = "arulesSequences")
as(r2, "data.frame")

## coerce to sequences
as(as(r2, "sequences"), "data.frame")
}
\keyword{classes}
