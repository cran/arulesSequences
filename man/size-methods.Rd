\name{size-methods}
\docType{methods}
\alias{size}
\alias{size,sequences-method}
\alias{ritems}
\alias{ritems,sequences-method}
\title{Compute the Size of Sequences}
\description{
    \code{size} computes the size of a sequence. This can be either the number
    of (distinct) itemsets (elements) or items occurring in a sequence.

    \code{ritems} compute the minimum (maximum) number an item or itemset
    (element) is repeatedly occurring in a sequence.
}
\usage{
\S4method{size}{sequences}(x, type = c("size", "itemsets", "length", "items"))

\S4method{ritems}{sequences}(x, type = c("min", "max"), itemsets = FALSE)
}
\arguments{
    \item{x}{an object.}
    \item{type, itemsets}{as string (logical) value specifying the type of
	count to be computed.}
}
\value{
    Returns a vector of counts corresponding with the \emph{elements}
    of object \code{x}.
}
\author{Christian Buchta}
\note{
    The total number of items occurring in a sequence is often referred
    to as the \emph{length} of the sequence. Similarly, we refer to the
    total number of itemsets as the \code{size} of the sequence. Note
    that we follow this terminology in the summary methods. 

    For use with a collection of rules use the accessors \code{\link{lhs}}
    or \code{\link{rhs}}, or coerce to sequences.
}
\seealso{
    Class
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}}.
}
\examples{
## continue example
example(cspade)

## default size
size(s2)
size(s2, "itemsets")
size(s2, "length")
size(s2, "items")

## crosstab
table(length = size(s1, "length"),
      items  = size(s1, "items"))

## repetitions
ritems(s1)
ritems(s1, "max")
ritems(s1, "max", TRUE)
}
\keyword{methods}
