\name{subset-methods}
\docType{methods}
\alias{subset}
\alias{subset,sequences-method}
\alias{subset,sequencerules-method}
\alias{[}
\alias{[,sequences,ANY,ANY,ANY-method}
\alias{[,timedsequences,ANY,ANY,ANY-method}
\alias{[,sequencerules,ANY,missing,ANY-method}
\alias{unique,sequences-method}
\alias{unique,sequencerules-method}
\alias{unique}
\alias{lhs,sequencerules-method}
\alias{lhs}
\alias{rhs,sequencerules-method}
\alias{rhs}
\title{Subset Objects}
\description{
    \code{subset} extracts a subset of a collection of sequences or sequence
    rules which meet conditions specified with respect to their associated
    (or derived) quality measures, additional information, or patterns of
    items or itemsets.

    \code{[} extracts subsets from a collection of (timed) sequences or
    sequence rules.

    \code{unique} extracts the unique set of sequences or sequence rules
    from a collection of sequences or sequence rules.

    \code{lhs, rhs} extract the left-hand (antecedent) or right-hand side
    (consequent) sequences from a collection of sequence rules.
}
\usage{
\S4method{subset}{sequences}(x, subset)

\S4method{subset}{sequencerules}(x, subset)

## S4 method for signature 'sequences, ANY, ANY, ANY':
x[i, j, ..., reduce = FALSE, drop = FALSE]

## S4 method for signature 'timedsequences, ANY, ANY, ANY':
x[i, j, k, ..., reduce = FALSE, drop = FALSE]

## S4 method for signature 'sequencerules, ANY, missing, ANY':
x[i, j, ..., drop = FALSE]

\S4method{unique}{sequences}(x, incomparables = FALSE, ...)

\S4method{unique}{sequencerules}(x, incomparables = FALSE, ...)

\S4method{lhs}{sequencerules}(x)

\S4method{rhs}{sequencerules}(x)
}
\arguments{
  \item{x}{an object.}
  \item{subset}{an expression specifying the conditions where the columns
    in quality and info must be referenced by their names, and the object
    itself as \code{x}.}
  \item{i}{a vector specifying the subset of \emph{elements} to be extracted.}
  \item{k}{a vector specifying the subset of event times to be extracted.}
  \item{reduce}{a logical value specifying if the reference set of
      distinct itemsets should be reduced if possible.}
  \item{j, \dots, drop}{unused arguments (for compatibility with package
    \pkg{Matrix} only).}
  \item{incomparables}{not used.}
}
%\details{}
\value{
    For \code{subset}, \code{[}, and \code{unique} returns an object of the
    same class as \code{x}.
    
    For \code{lhs} and \code{rhs} returns an object of class
    \code{\linkS4class{sequences}}. 
}
\author{Christian Buchta}
\note{
    In package \pkg{arules}, somewhat confusingly, the object itself has
    to be referenced as \code{items}. We do not provide this, as well as
    any of the references \code{items}, \code{lhs}, or \code{rhs}.

    After extraction the reference set of distinct itemsets may be larger
    than the set actually referred to unless reduction to this set is 
    explicitly requested. However, this may increase memory consumption.

    Event time indexes of mode character are matched against the time
    labels. Any duplicate indexes are ignored and their order does not
    matter, i.e. reordering of a sequence is not possible.

    The accessors \code{lhs} and \code{rhs} impute the support of
    a sequence from the support and confidence of a rule. This may
    lead to numerically inaccuracies over back-to-back derivations.
}
\seealso{
    Class
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    \code{\linkS4class{sequencerules}},
    method
    \code{\link{lhs}},
    \code{\link{rhs}},
    \code{\link{match}},
    \code{\link{nitems}},
    \code{\link{c}}.
}
\examples{
## continue example
example(ruleInduction, package = "arulesSequences")

## matching a pattern
as(subset(s2, size(x) > 1), "data.frame")
as(subset(s2, x \%ain\% c("B", "F")), "data.frame")

## as well as a measure
as(subset(s2, x \%ain\% c("B", "F") & support == 1), "data.frame")

## matching a pattern in the left-hand side
as(subset(r2, lhs(x) \%ain\% c("B", "F")), "data.frame")

## matching a derived measure
as(subset(r2, coverage(x) == 1), "data.frame")

## reduce
s <- s2[11, reduce = TRUE]
itemLabels(s)
itemLabels(s2)

## drop initial events
z <- as(zaki, "timedsequences")
summary(z[1,,-1])
}
\keyword{manip}
