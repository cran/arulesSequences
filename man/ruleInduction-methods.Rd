\name{ruleInduction-methods}
\docType{methods}
\alias{ruleInduction}
\alias{ruleInduction,sequences-method}
\title{Induce Sequence Rules}
\description{
    Induce a set of strong sequence rules from a set of frequent 
    sequences, i.e. which (1) satisfy the minimum confidence threshold
    and (2) which contain the last element of the generating sequence as
    the right-hand side (consequent) sequence.
}
\usage{
\S4method{ruleInduction}{sequences}(x, transactions, confidence = 0.8, control = NULL)
}
\arguments{
  \item{x}{an object.}
  \item{transactions}{currently not used.}
  \item{confidence}{a numeric value specifying the minimum confidence
    threshold.}
% this is awkward
  \item{control}{a list with logical components \code{maximal} specifying
    if rules should be induced from maximally frequent sequences only, and
    \code{verbose} if progress and runtime information should be displayed.}
}
\value{
    Returns an object of class \code{\linkS4class{sequencerules}}.
}
\author{Christian Buchta}
\note{
    Currently, the collection of sequences supplied must be closed with
    respect to the rules to be induced. That is, the left- and the
    right-hand side sequence of each candidate rule must be contained in
    the collection of sequences. However, using timing constraints in the
    mining step the set of frequent sequences may not be closed under
    rule induction.
}
\seealso{
    Class 
    \code{\linkS4class{sequences}},
    \code{\linkS4class{sequencerules}},
    function 
    \code{\link{cspade}}.
}
\examples{
## continue example
example(cspade)

## mine rules
r2 <- ruleInduction(s2, confidence = 0.5,
			control    = list(verbose = TRUE))
summary(r2)
as(r2, "data.frame")
}
\keyword{models}
