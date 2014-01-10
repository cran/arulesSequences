\name{cspade}
\alias{cspade}
\title{Mining Associations with cSPADE}
\description{
    Mining frequent sequential patterns with the cSPADE algorithm.
    This algorithm utilizes temporal joins along with efficient
    lattice search techniques and provides for timing constraints.
}
\usage{
cspade(data, parameter = NULL, control = NULL, tmpdir = tempdir())
}
\arguments{
  \item{data}{an object of class
    \code{\link[arules:transactions-class]{transactions}} with
    temporal information.}
  \item{parameter}{an object of class \code{\linkS4class{SPparameter}}
    or a named list with corresponding components.}
  \item{control}{an object of class \code{\linkS4class{SPcontrol}}
    or a named list with corresponding components.}
  \item{tmpdir}{a non-empty character vector giving the directory name
    where temporary files are written.}
}
\details{
    Interfaces the command-line tools for preprocessing and mining
    frequent sequences with the cSPADE algorithm by M. Zaki via a proper
    chain of system calls.

    The temporal information is taken from components \code{sequenceID} 
    (sequence or customer identifier) and \code{eventID} (event identifier)
    of slot \code{transactionInfo}. Both identifiers must be in (blockwise)
    ascending order.

    The amount of disk space used by temporary files is reported in
    verbose mode (see class \code{\linkS4class{SPcontrol}}).

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
    Temporary files may not be deleted until the end of the R session if
    the call is interrupted.

    The current working directory (see \code{\link{getwd}}) must be writable.

    \code{sequenceID} and \code{eventID} are coerced to factor if not integer.

#ifdef windows
    If this interface does not work with \code{Rgui} use \code{Rterm} as
    fallback.
#endif
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
		   control   = list(verbose = TRUE, tidLists = TRUE))
summary(s1)
as(s1, "data.frame")

##
summary(tidLists(s1))
transactionInfo(tidLists(s1))

## use timing constraint
s2 <- cspade(zaki, parameter = list(support = 0.4, maxwin = 5))
as(s2, "data.frame")

## replace timestamps
t <- zaki
transactionInfo(t)$eventID <-
    unlist(tapply(seq(t), transactionInfo(t)$sequenceID,
	function(x) x - min(x) + 1), use.names = FALSE)
as(t, "data.frame")
s0 <- cspade(t, parameter = list(support = 0.4))
s0
identical(as(s1, "data.frame"), as(s0, "data.frame"))

\dontrun{
## use generated data
t <- read_baskets(con  = system.file("misc", "test.txt", package =
				      "arulesSequences"),
		  info = c("sequenceID","eventID","SIZE"))
summary(t)
## use low support
s3 <- cspade(t, parameter = list(support=0.03), 
		control   = list(verbose=TRUE))
summary(s3)
}
}
\keyword{models}
