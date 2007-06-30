\name{times-methods}
\docType{methods}
\alias{times}
\alias{times,timedsequences-method}
\title{Compute Time Statistics of Sequences}
\description{
    Computes the gaps, the minimum or maximum gap, or the span of sequence.
}
\usage{
\S4method{times}{timedsequences}(x, type = c("times", "gaps", "mingap", "maxgap", "span"))
}
\arguments{
  \item{x}{an object.}
  \item{type}{a string value specifying the type of statistic.}
}
%\details{}
\value{
    If \code{type = "items"} returns a list of vectors of events times
    corresponding with the elements of a sequence.
    
    If \code{type = "gaps"} returns a list of vectors of time differences
    between consecutive elements of a sequence.
    
    Otherwise, a vector corresponding with the \emph{elements} of \code{x}.
}
\author{Christian Buchta}
\note{
    Gap statistics are not defined for sequences of size one, i.e. which
    contain a single element. \code{NA} is used for undefined values.

    FIXME lists are silently reduced to vector if possible.
}
\seealso{
    Class
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    method
    \code{\link{size}},
    \code{\link{itemFrequency}},
    \code{\link{timeFrequency}}.
}
\examples{
## continue example
example("timedsequences-class")

##
times(z)
times(z, "gaps")

## all defined
times(z, "span")

## crosstab
table(size = size(z), span = times(z, "span"))
}
%fuck the keywords!
\keyword{methods}
