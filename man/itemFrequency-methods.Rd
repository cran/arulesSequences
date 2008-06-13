\name{itemFrequency-methods}
\docType{methods}
\alias{itemFrequency}
\alias{itemFrequency,sequences-method}
\alias{itemTable,sequences-method}
\alias{itemTable}
\alias{nitems}
\alias{nitems,sequences-method}
\alias{dim}
\alias{dim,sequences-method}
\alias{dim,timedsequences-method}
\alias{length,sequences-method}
\alias{length,sequencerules-method}
\alias{length}
\title{Count Items or Itemsets}
\description{
    \code{itemFrequency} counts the number of distinct occurrences of items
    or itemsets (elements) in a collection of sequences. That is, multiple
    occurrences within a sequence are ignored.

    \code{itemTable} cross-tabulates the counts an item or itemset
    occurs in a sequence.

    \code{nitems} computes the total number of distinct occurrences of items
    or itemsets in a collection of sequences.

    \code{dim} retrieves the dimensions of an object of class
    \code{\linkS4class{sequences}} or \code{\linkS4class{timedsequences}}.

    \code{length} retrieves the number of elements of a collection of
    sequences or sequence rules.
}
\usage{
\S4method{itemFrequency}{sequences}(x, itemsets = FALSE, type = c("absolute", "relative"))

\S4method{itemTable}{sequences}(x, itemsets = FALSE)

\S4method{nitems}{sequences}(x, itemsets = FALSE)

\S4method{dim}{sequences}(x)

\S4method{dim}{timedsequences}(x)

\S4method{length}{sequences}(x)

\S4method{length}{sequencerules}(x)
}
\arguments{
  \item{x}{an object.}
  \item{itemsets}{a logical value specifying the type of count.}
  \item{type}{a string value specifying the scale of count.}
}
%\details{}
\value{
    For \code{itemFrequency} returns a vector of counts corresponding with
    the reference set of distinct items or itemsets.

    For \code{itemTable} returns a table with the rownames corresponding
    with the reference set of distinct items or itemsets.

    For \code{nitmes} a scalar value.

    For \code{dim} and class \code{sequences} a vector of length three
    containing the number of sequences and the dimension of the reference
    set of distinct itemsets. For class \code{timedsequences} the fourth
    element contains the number of distinct event times.

    For \code{length} a scalar value.
}
\author{Christian Buchta}
\note{
    For efficiency reasons, the reference set of distinct itemsets can
    be larger than the set actually referenced by a collection of sequences.
    Thus, the counts of some items or itemsets may be zero.

    Method \code{nitems} is provided for efficiency; method \code{dim} for
    technical information.

    For analysis of a set of rules use the accessors \code{\link{lhs}}
    or \code{\link{rhs}}, or coerce to sequences.
}
\seealso{
    Class
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    method
    \code{\link{size}},
    \code{\link{subset}}.
}
\examples{
## continue example
example(cspade)

##
itemFrequency(s2)
itemFrequency(s2, itemsets = TRUE)

##
itemTable(s2)
itemTable(s2, itemsets = TRUE)

##
nitems(s2)
nitems(s2, itemsets = TRUE)

##
length(s2)
dim(s2)

##
z <- as(zaki, "timedsequences")
dim(z)
}
\keyword{models}
