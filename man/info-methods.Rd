\name{info-methods}
\docType{methods}
\alias{sequenceInfo}
\alias{sequenceInfo,sequences-method}
\alias{sequenceInfo<-}
\alias{sequenceInfo<-,sequences-method}
\alias{ruleInfo}
\alias{ruleInfo,sequencerules-method}
\alias{ruleInfo<-}
\alias{ruleInfo<-,sequencerules-method}
\alias{itemInfo}
\alias{itemInfo,sequences-method}
\alias{itemInfo<-}
\alias{itemInfo<-,sequences-method}
\alias{timeInfo}
\alias{timeInfo,timedsequences-method}
\alias{timeInfo<-}
\alias{timeInfo<-,timedsequences-method}

\title{Get/Set Object Information}
\description{
    \code{sequenceInfo} gets or sets information on the elements of a
    collection of sequences
    
    \code{ruleInfo} gets or sets information on the elements of a
    collection of sequence rules.

    \code{itemInfo} gets or sets information on the set of distinct items
    associated with a collection of sequences.

    \code{timeInfo} gets or sets information on the event times of a
    collection of timed sequences.
}
\usage{
\S4method{sequenceInfo}{sequences}(object)

## S4 method for signature 'sequences':
sequenceInfo(object) <- value

\S4method{ruleInfo}{sequencerules}(object)

## S4 method for signature 'sequencerules':
ruleInfo(object) <- value

\S4method{itemInfo}{sequences}(object)

## S4 method for signature 'sequences':
itemInfo(object) <- value

\S4method{timeInfo}{timedsequences}(object)

## S4 method for signature 'timedsequences':
timeInfo(object) <- value
}
\arguments{
  \item{object}{an object.}
  \item{value}{a data frame corresponding with the \emph{elements} or 
    \emph{times} of \code{object}.}
%  \item{\dots}{not used.}
}
%\details{}
\value{
    For method \code{sequenceInfo}
    and method \code{ruleInfo} a data frame of information on and
    corresponding with the elements of \code{object}.

    For method \code{itemInfo} a data frame of information on and
    corresponding with the distinct items of \code{object}. 

    For method \code{timeInfo} a data frame of information on and
    corresponding with the distinct event times of \code{object}.
}
\author{Christian Buchta}
\note{
    For reasons of efficiency the reference set of distinct itemsets
    may contain unreferenced \emph{elements}, i.e. items that do not
    occur in any sequence.

    Unique item identifiers must be provided in column \code{labels}.

    Unique event time identifiers must be provided in columns \code{labels}
    and \code{eventID}. Note that the latter is used for compuatation
    of gaps, etc.
}
\seealso{
    Class 
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    \code{\linkS4class{sequencerules}}.
}
\examples{
## continue example
example(ruleInduction, package = "arulesSequences")

## empty
sequenceInfo(s2) <- sequenceInfo(s2)
ruleInfo(r2) <- ruleInfo(r2)

## item info
itemInfo(s2)


## time info
z <- as(zaki, "timedsequences")
timeInfo(z)
}
\keyword{manip}
\keyword{attribute}
