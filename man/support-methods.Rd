\name{support-methods}
\docType{methods}
\alias{support}
\alias{support,sequences-method}
\title{Support Counting for Sequences}
\description{
    Compute the relative or absolute support of an arbitrary collection
    of sequences among a set of transactions with additional sequence and
    temporal information.
}
\usage{
\S4method{support}{sequences}(x, transactions, type= c("relative", "absolute"),
           control = NULL)
}
\arguments{
  \item{x}{an object.}
  \item{transactions}{an object of class
  \code{\link[arules:transactions-class]{transactions}}.}
  \item{type}{a character value specifying the scale of support (relative
    or absolute).}
% this is awkward
  \item{control}{a named list with logical component \code{verbose}
    specifying if progress and runtime information should be displayed.}
}
\value{
    Returns a \code{numeric} vector the elements of which correspond
    with the elements of \code{x}.
}
\author{Christian Buchta}
\note{
    Currently, only prefix-tree counting is implemented. This approach
    uses the ordering information of the elements of a sequence only.
% FIXME
    Therefore, the counts might be higher than those computed by
    \code{\link{cspade}}.
}
\seealso{
    Class 
    \code{\linkS4class{sequences}},
    method 
    \code{\link{ruleInduction}},
    function 
    \code{\link{cspade}},
    \code{\link{read_baskets}}.
}
\examples{
## continue example
example(cspade)

## recompute support
s <- support(s2, zaki, control = list(verbose = TRUE))
cbind(as(s2, "data.frame"), upper = s)
}
\keyword{models}
