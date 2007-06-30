\name{timeFrequency-methods}
\docType{methods}
\alias{firstOrder}
\alias{firstOrder,timedsequences-method}
\alias{timeFrequency}
\alias{timeFrequency,timedsequences-method}
\alias{timeTable}
\alias{timeTable,timedsequences-method}
\title{Count Event Times}
\description{
    \code{timeFrequency} counts the number of occurrences of event times, of
    the time gaps between the events of a sequence, the minimum or maximum
    gap of a sequence, or the span of a sequence.

    \code{timeTable} cross-tabulates the above statistics for items or
    itemsets. For items the sequences are reduced to the events containing
    the item.

    \code{firstOrder} computes a first order model, i.e. a table of counts
    of state changes among a collection of timed sequences, where the
    elements or the times can be the states.
}
\usage{
\S4method{timeFrequency}{timedsequences}(x, type = c("times", "gaps", "mingap", "maxgap",
                          "span"))

\S4method{timeTable}{timedsequences}(x, type = c("times","gaps", "mingap", "maxgap", "span"),
	  itemsets = FALSE)

\S4method{firstOrder}{timedsequences}(x, times = FALSE)
}
\arguments{
  \item{x}{an object.}
  \item{type, itemsets, times}{a string (logical) value specifying the type
    of count.}
}
%\details{}
\value{
    For \code{timeFrequency} returns a vector of counts corresponding with
    the set of distinct event times, the set of gaps or spans as indicated
    by the names attribute.

    For \code{timeTable} returns a table of counts with the rownames
    corresponding with the reference set of distinct items or itemsets.

    For \code{firstOrder} a matrix of counts corresponding with the set of
    distinct itemsets or event times.
}
\author{Christian Buchta}
\note{
    Undefined values are not included in the counts, e.g. the \code{mingap}
    of a sequence with one element only. Thus, except for \code{times} and
    \code{gaps} the counts (per item or itemset) always add up to less than
    or equal the number of sequences, i.e. \code{length(x)}.
}
\seealso{
    Class
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    method
    \code{\link{size}},
    \code{\link{times}},
    \code{\link{itemFrequency}}.
}
\examples{
## continue example
example("timedsequences-class")

## totals
timeFrequency(z)
timeFrequency(z, "gaps")
timeFrequency(z, "span")

## default items
timeTable(z)
timeTable(z, "gaps")
timeTable(z, "span")

## beware of large data sets
timeTable(z, itemsets = TRUE)


## first order models
firstOrder(z)
firstOrder(z, times = TRUE)
}
\keyword{models}
