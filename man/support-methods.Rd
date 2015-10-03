\name{support-methods}
\docType{methods}
\alias{support}
\alias{support,sequences-method}
\alias{supportingTransactions,sequences-method}
\title{Support Counting for Sequences}
\description{
    Compute the relative or absolute support of an arbitrary collection
    of sequences among a set of transactions with additional sequence and
    temporal information.
}
\usage{
\S4method{support}{sequences}(x, transactions, type= c("relative", "absolute"),
           control = NULL)

\S4method{supportingTransactions}{sequences}(x, transactions, \dots)
}
\arguments{
  \item{x}{an object.}
  \item{transactions}{an object of class
  \code{\link[arules:transactions-class]{transactions}} or
  \code{\linkS4class{sequences}}.}
  \item{type}{a character value specifying the scale of support (relative
    or absolute).}
% this is awkward
  \item{control}{a named list with logical component \code{verbose}
    specifying if progress and runtime information should be displayed
    and list component \code{parameter} (see details).}
  \item{\dots}{currently not used.}
}
\details{
    Provides support counting using either method ptree (default), or
    idlists (for details see the reference in \code{\link{cspade}}) and
    timing constraints.

    \code{parameter} can be an object of class
    \code{\linkS4class{SPparameter}} or a named list with corresponding
    components. Note that constraints which do not relate to the timing
    information of \code{transactions} are ignored.

    If sequences are used for \code{transactions} missing event times
    are replaced with the order indexes of events.

    The supporting sequences are all sequences (of transactions) of which
    the sequence representing the association is a subset of.

    Note that \code{supportingTransactions} does not support timing
    constraints.
}
\value{
    For \code{support} a \code{numeric} vector the elements of which
    correspond with the elements of \code{x}.

    For \code{supportingTransactions} an object of class
    \code{\link[arules]{tidLists}} containing one sequence ID list per
    association in \code{x}.
}
\author{Christian Buchta}
%\note{
%}
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
s <- support(s2, zaki, control = list(verbose = TRUE, 
                     parameter = list(maxwin = 5)))
data.frame(as(s2, "data.frame"), support = s)

## use default method
k <- support(s2, zaki, control = list(verbose = TRUE))
table(size(s2), sign(k - s))

## the same
s <- supportingTransactions(s2, zaki)
itemFrequency(s)
}
\keyword{models}
