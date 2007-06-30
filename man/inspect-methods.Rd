\name{inspect-methods}
\docType{methods}
\alias{inspect}
\alias{inspect,sequences-method}
\alias{inspect,timedsequences-method}
\alias{inspect,sequencerules-method}
\alias{labels}
\alias{labels,sequences-method}
\alias{labels,timedsequences-method}
\alias{labels,sequencerules-method}
\alias{itemLabels}
\alias{itemLabels,sequences-method}
\alias{itemLabels<-}
\alias{itemLabels<-,sequences-method}
\title{Display Objects}
\description{
    \code{inspect} displays a collection of (timed) sequences or sequence
    rules and their associated quality measures formatted for online
    inspection.

    \code{labels} retrieves the string representations of a collection of
    (timed) sequences or sequence rules.

    \code{itemLabels} gets the string representations of the set of distinct
    items or itemsets (elements) associated  with a collection of sequences,
    or sets item labels.
}
\usage{
\S4method{inspect}{sequences}(x, setSep = ",", seqStart = "<", seqEnd = ">",
	decode = TRUE)

\S4method{inspect}{timedsequences}(x, setSep = ",", seqStart = "<", seqEnd = ">", 
	decode = TRUE)

\S4method{inspect}{sequencerules}(x, setSep = ",", seqStart = "<", seqEnd = ">",
	ruleSep = "=>", decode = TRUE)


\S4method{labels}{sequences}(object, setSep = ",", seqStart = "<", seqEnd = ">",
       decode = TRUE, ...)

\S4method{labels}{timedsequences}(object, timeStart = "[", timeEnd = "]", setSep = ",",
       seqStart = "<", seqEnd = ">", decode = TRUE, ...)
       

\S4method{labels}{sequencerules}(object, setSep = ",", seqStart = "<", seqEnd = ">", 
       ruleSep = " => ", decode = TRUE, ...)

\S4method{itemLabels}{sequences}(object, itemsets = FALSE, ...)

## S4 method for signature 'sequences, character':
itemLabels(object) <- value
}
\arguments{
  \item{x, object}{an object.}
  \item{setSep}{a string value specifying the itemset (element) separator.}
  \item{seqStart}{a string value specifying the left sequence delimiter.}
  \item{seqEnd}{a string value specifying the right sequence delimiter.}
  \item{ruleSep}{a string value specifying the separator of the left-hand
    (antecedent) and the right-hand side (consequent) sequence.}
  \item{timeStart}{a string value specifying the left event time delimiter.}
  \item{timeEnd}{a string value specifying the right event time delimiter.}
  \item{decode}{a logical value specifying if the item indexes should be
    replaced by item labels.}
  \item{itemsets}{a logical value specifying the type of labels.}
  \item{\dots}{arguments specifying the markup of itemsets:
    \code{itemSep = ","}, \code{setStart = "\{"}, or 
    \code{setEnd = "\}"}.}
  \item{value}{a character vector of length the number of items of
    \code{object}.}
}
%\details{}
\value{
    For method \code{inspect} returns \code{x} invisibly.
    
    For method \code{labels} a character vector corresponding with
    the \emph{elements} of \code{x}.

    For method \code{itemLabels} a character vector corresponding
    with the distinct items or itemsets of \code{object}.
}
\author{Christian Buchta}
\note{
    For compatibility with package \pkg{arules} the markup of itemsets
    is not customizable in the inspect methods.

    For reasons of efficiency the reference set of distinct itemsets
    may contain unreferenced \emph{elements}, e.g. after subsetting.
}
\seealso{
    Class 
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    \code{\linkS4class{sequencerules}},
    method
    \code{\link{subset}}.
}
\examples{
## continue example
example(ruleInduction, package = "arulesSequences")

## stacked style
inspect(s2)
inspect(s2, setSep = "->", seqStart = "", seqEnd = "")

## economy style
labels(s2, setSep = "->", seqStart = "", seqEnd = "",
	   itemSep = " ", setStart = "", setEnd = "")

## rules
inspect(r2)

## alternate style
labels(r2, ruleSep = " + ")

## itemset labels
itemLabels(s2, itemsets = TRUE)
itemLabels(s2[reduce = TRUE], itemsets = TRUE)

## item labels
itemLabels(s2) <- tolower(itemLabels(s2))
itemLabels(s2)

## timed
z <- as(zaki, "timedsequences") 
labels(z)
inspect(z)
}
\keyword{print}
