\name{match-methods}
\docType{methods}
\alias{match}
\alias{match,sequences,sequences-method}
\alias{match,sequencerules,sequencerules-method}
\alias{\%in\%,sequences,character-method} 
\alias{\%in\%}
\alias{\%ain\%,sequences,character-method} 
\alias{\%ain\%}
\alias{\%pin\%,sequences,character-method} 
\alias{\%pin\%}
\alias{\%ein\%,sequences,character-method}
\alias{\%ein\%}
\alias{duplicated,sequences-method}
\alias{duplicated,sequencerules-method}
\alias{duplicated}
\title{Match Objects}
\description{
    \code{match} finds the positions of first matches of a collection of
    sequences or sequence rules in an object of the same class. 
    
    \code{\%in\%} indicates matches of the left in the right operand.
    If the right operand is a vector of item labels indicates if a
    sequence contains any of the items given.

    \code{\%ain\%} indicates if a sequence contains all the items given as
    the right operand. 

    \code{\%pin\%} indicates if a sequence contains any item matching
    the regular expression given as the right operand.

    \code{\%ein\%} indicates if a sequence contains any itemset 
    containing all the items given as the right operand.

    \code{duplicated} indicates duplicate occurrences of sequences
    or sequence rules.
}
\usage{
\S4method{match}{sequences,sequences}(x, table, nomatch = NA_integer_, incomparables = NULL)

\S4method{match}{sequencerules,sequencerules}(x, table, nomatch = NA_integer_, incomparables = NULL)

## S4 methods for signature 'sequences, character':
x \%in\%  table
x \%ain\% table
x \%pin\% table
x \%ein\% table

\S4method{duplicated}{sequences}(x, incomparables = FALSE)

\S4method{duplicated}{sequencerules}(x, incomparables = FALSE)
}
\arguments{
  \item{x}{an object.}
  \item{table}{an object (of the same class as \code{x}).}
  \item{nomatch}{the value to be returned in the case of no match. }
  \item{incomparables}{not used.}
}
%\details{}
\value{
    For \code{match} returns an integer vector of the same length as 
    \code{x} containing the position in \code{table} of the first match,
    or if there is no match the value of \code{nomatch}.

    For \code{\%in\%}, \code{\%ain\%}, and \code{\%pin\%} returns a 
    logical vector indicating for each \emph{element} of \code{x} if
    a match was found in the right operand.

    For \code{duplicated} a logical vector corresponding with the
    \emph{elements} of \code{x}.
}
\author{Christian Buchta}
\note{
    For practical reasons, the item labels given in the right operand
    must match the item labels associated with \code{x} exactly.

    Currently, an operator for matching against the labels of a set of
    sequences is not provided. For example, it could be defined as

    \code{"\%lin\%" <- function(l, r) match(r, labels(l)) > 0}

    with the caveat of being too general.

    FIXME curently matching of timed sequences does not take event
    times into consideration.
}
\seealso{
    Class 
    \code{\linkS4class{sequences}},
    \code{\linkS4class{sequencerules}},
    method
    \code{\link{labels}},
    \code{\link{itemLabels}}.
}
\examples{
## continue example
example(cspade)

## match
labels(s1[match(s2, s1)])
labels(s1[s1 \%in\% s2])	    # the same

## match items
labels(s2[s2 \%in\%  c("B", "F")])
labels(s2[s2 \%ain\% c("B", "F")])
labels(s2[s2 \%pin\% "F"])

## match itemsets
labels(s1[s1 \%ein\% c("F","B")])
}
\keyword{manip}
