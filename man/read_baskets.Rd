\name{read_baskets}
\alias{read_baskets}
\title{Read Transaction Data}
\description{
    Read transaction data in basket format (with additional temporal
    or other information) and create an object of class 
    \code{\link[arules:transactions-class]{transactions}}. 
}
\usage{
read_baskets(con, sep = "[ \t]+", info = NULL, iteminfo = NULL)
}
\arguments{
  \item{con}{an object of class \code{connection} or file name.}
  \item{sep}{a regular expression specifying how fields are separated
    in the data file.}
  \item{info}{a character vector specifying the header for columns with
    additional transaction information.}
  \item{iteminfo}{a data frame specifying (additional) item information.} 
}
\details{
    Each line of text represents a transaction where items are
    separated by a pattern matching the regular expression specified
    by \code{sep}.

    Columns with additional information such as customer or time (event)
    identifiers are required to come before any item identifiers and must
    be specified by \code{info}.

    Sequential data are identified by the presence of the column identifiers
    \code{sequenceID} (sequence or customer identifier) and \code{eventID}
    (time or event identifier) of slot \code{transactionInfo}.

    The row names of \code{iteminfo} must match the item identifiers
    present in the data. However, \code{iteminfo} need not contain a
    labels column.
}
\value{
    An object of class \code{\link[arules:transactions-class]{transactions}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christian Buchta}
\note{
    Currently, it is not checked if column \code{eventID} defines a
    temporal order. \code{sequenceID} and \code{eventID} will be coerced
    to factor if not integer.

    For efficiency, the item labels are not sorted and, thus, are in the
    order they appear in the data.
}
\seealso{
    Class
    \code{\linkS4class{timedsequences}},
    \code{\link[arules:transactions-class]{transactions}},
    function
    \code{\link{cspade}}.
}
\examples{
## read example data
x <- read_baskets(con  = system.file("misc", "zaki.txt", package = 
                                     "arulesSequences"),
		  info = c("sequenceID","eventID","SIZE"))
as(x, "data.frame")
}
\keyword{file}
