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
    of \emph{transactionInfo}. Note that integer identifiers must be
    positive and that \code{transactions} must be ordered by
    \code{sequenceID} and \code{eventID}.

    Class information (on sequences or customers) is taken from component
    \code{classID}, if available. 

    The amount of disk space used by temporary files is reported in
    verbose mode (see class \code{\linkS4class{SPcontrol}}).

    If specified \code{timeout} is passed to \code{\link{system2}} (see
    details there and class \code{\linkS4class{SPcontrol}}).
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
    the call is interrupted. Use timeouts to avoid this problem.

    The current working directory (see \code{\link{getwd}}) must be writable.

#ifdef windows
    If this interface does not work with \code{Rgui} use \code{Rterm} as
    fallback.
#endif
}
\section{Warning}{
    The implementation of the \code{maxwin} constraint in the command-line
    tools seems to be broken. To avoid confusion it is disabled with a 
    warning.
}
\seealso{
    Class 
    \code{\link[arules:transactions-class]{transactions}},
    \code{\linkS4class{sequences}},
    \code{\linkS4class{SPparameter}},
    \code{\linkS4class{SPcontrol}},
    method 
    \code{\link{ruleInduction}},
    \code{\link{support}},
    function
    \code{\link{read_baskets}}.
}
\examples{
## use example data from paper
data(zaki)
## get support bearings
s0 <- cspade(zaki, parameter = list(support = 0,
                                    maxsize = 1, maxlen = 1),
                   control   = list(verbose = TRUE))
as(s0, "data.frame")
## mine frequent sequences
s1 <- cspade(zaki, parameter = list(support = 0.4), 
		   control   = list(verbose = TRUE, tidLists = TRUE))
summary(s1)
as(s1, "data.frame")

##
summary(tidLists(s1))
transactionInfo(tidLists(s1))

## use timing constraint
s2 <- cspade(zaki, parameter = list(support = 0.4, maxgap = 5))
as(s2, "data.frame")

## use classification
t <- zaki
transactionInfo(t)$classID <-
    as.integer(transactionInfo(t)$sequenceID) \%\% 2 + 1L
s3 <- cspade(t, parameter = list(support = 0.4, maxgap = 5))
as(s3, "data.frame")

## replace timestamps
t <- zaki
transactionInfo(t)$eventID <-
    unlist(tapply(seq(t), transactionInfo(t)$sequenceID,
	function(x) x - min(x) + 1), use.names = FALSE)
as(t, "data.frame")
s4 <- cspade(t, parameter = list(support = 0.4))
s4
identical(as(s1, "data.frame"), as(s4, "data.frame"))

## work around
s5 <- cspade(zaki, parameter = list(support = .25, maxgap = 5))
length(s5)
k <- support(s5, zaki, control   = list(verbose = TRUE,
                       parameter = list(maxwin = 5)))
table(size(s5[k == 0]))

\dontrun{
## use generated data
t <- read_baskets(con  = system.file("misc", "test.txt", package =
				      "arulesSequences"),
		  info = c("sequenceID", "eventID", "SIZE"))
summary(t)
## use low support
s6 <- cspade(t, parameter = list(support = 0.0133), 
		control   = list(verbose = TRUE, timeout = 15))
summary(s6)

## check
k <- support(s6, t, control = list(verbose = TRUE))
table(size(s6), sign(quality(s6)$support -k))

## use low confidence
r6 <- ruleInduction(s6, confidence = .5,
			control    = list(verbose = TRUE))
summary(r6)
}
}
\keyword{models}
