\name{c-methods}
\alias{c}
\alias{c,sequences-method}
\alias{c,timedsequences-method}
\alias{c,sequencerules-method}
\title{Combining Objects}
\description{
    \code{c} combines a collection of (timed) sequences or sequence rules
    into a single object.
}
\usage{
\S4method{c}{sequences}(x, ..., recursive = FALSE)

\S4method{c}{timedsequences}(x, ..., recursive = FALSE)

\S4method{c}{sequencerules}(x, ..., recursive = FALSE)
}
\arguments{
  \item{x}{an object.}
  \item{...}{(a list of) further objects of the same class as \code{x}}.
  \item{recursive}{a logical value specifying if the function should
    descend through lists.}
}
%\details{}
\value{
    For \code{c} and \code{unique} an object of the same class as \code{x}.
}
\author{Christian Buchta}
\note{
    Method \code{c} is similar to \code{rbind} but with the added twist
    that objects are internally conformed matching their item labels.
    That is, an object based on the union of item labels is created.

    For timed sequences event times are currently conformed as follows:
    if the union of all labels can be cast to integer the labels are
    sorted. Otherwise, labels not occuring in \code{x} are appended.

    The default setting does not allow any object to be of a class
    other than \code{x}, i.e. the objects are not combined into a 
    list.
}
\seealso{
    Class
    \code{\linkS4class{sequences}},
    \code{\linkS4class{timedsequences}},
    \code{\linkS4class{sequencerules}},
    method
    \code{\link{match}}.
}
\examples{
## continue example
example(ruleInduction, package = "arulesSequences")
s <- c(s1, s2)
s
match(unique(s), s1)

## combine rules
r <- c(r2, r2[1:2])
r
match(unique(r), r2)

## combine timed sequences
z <- as(zaki, "timedsequences")
match(z, c(z[1], z[-1]))
}
\keyword{manip}
\keyword{attribute}
