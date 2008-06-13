\name{zaki}
\alias{zaki}
\docType{data}
\title{Zaki Data Set}
\description{
    A small example database for sequence mining provided as an object
    of class \code{\link[arules:transactions-class]{transactions}} and
    as a text file.
}
\usage{data(zaki)}
\details{
    The data set contains the sequential database described in the
    paper by M. J. Zaki for illustration of the concepts of sequence
    mining. \code{sequenceID} and \code{eventID} denote the sequence
    and event (time) identifiers of the transactions.
}
\source{
    M. J. Zaki. (2001). SPADE: An Efficient Algorithm for Mining
    Frequent Sequences. \emph{Machine Learning Journal}, \bold{42},
    31--60.
}
\seealso{
    Class
    \code{\link[arules:transactions-class]{transactions}},
    \code{\linkS4class{sequences}},
    function  
    \code{\link{cspade}}.
}
\examples{
data(zaki)
summary(zaki)
as(zaki, "data.frame")
}
\keyword{datasets}
