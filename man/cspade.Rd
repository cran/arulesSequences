\name{cspade}
\alias{cspade}
\title{Mining Associations with cSPADE}
\description{
    Mining frequent sequential patterns with the cSPADE algorithm.
    This algorithm utilizes temporal joins along with efficient
    lattice search techniques and provides for timing constraints.
}
\usage{
cspade(data, parameter = NULL, control = NULL)
}
\arguments{
  \item{data}{an object of class
    \code{\link[arules:transactions-class]{transactions}} with
    temporal information.}
  \item{parameter}{an object of class \code{\linkS4class{SPparameter}}.}
  \item{control}{an object of class \code{\linkS4class{SPcontrol}}.}
}
\details{
    Interfaces the command-line tools for preprocessing and mining
    frequent sequences with the cSPADE algorithm by M. Zaki via a proper
    chain of system calls.

    The temporal information is taken from components \code{sequenceID} 
    (sequence or customer identifier) and \code{eventID} (event identifier)
    of slot \code{transactionInfo}.
    
    The utility function \code{\link{read_baskets}} provides for reading
    of text files with temporal transaction data.
}
\value{
    Returns an object of class \code{\linkS4class{sequences}}.
}
\references{
    M. J. Zaki. (2001). SPADE: An Efficient Algorithm for Mining
    Frequent Sequences. \emph{Machine Learning Journal}, \bold{42},
    31--60. 
}
\author{Christian Buchta, Michael Hahsler}
\note{
    Temporary files may not deleted until the end of the R session if the
    call is interrupted.

    \code{sequenceID} and \code{eventID} are coerced to factor if necessary.
}
\seealso{
    Class 
    \code{\link[arules:transactions-class]{transactions}},
    \code{\linkS4class{sequences}},
    \code{\linkS4class{SPparameter}},
    \code{\linkS4class{SPcontrol}},
    method 
    \code{\link{ruleInduction}},
    function
    \code{\link{read_baskets}}.
}
\examples{
## use example data from paper
data(zaki)
## mine frequent sequences
s1 <- cspade(zaki, parameter = list(support = 0.4), 
		   control   = list(verbose = TRUE))
summary(s1)
as(s1, "data.frame")
## use timing constraint
s2 <- cspade(zaki, parameter = list(support = 0.4, maxwin = 5))
as(s2, "data.frame")
}
\keyword{models}
