\name{similarity-methods}
\docType{methods}
\alias{similarity}
\alias{similarity,sequences-method}
\alias{is.subset}
\alias{is.subset,sequences-method}
\alias{is.superset}
\alias{is.superset,sequences-method}
\alias{coerce,dsCMatrix,dist-method}
\title{Compute Similarities}
\description{
    Provides the generic function \code{similarity} and the S4 method
    to compute similarities among a collection of sequences.

    \code{is.subset, is.superset} find subsequence or supersequence
    relationships among a collection of sequences.
}
\usage{
similarity(x, y = NULL, ...)

\S4method{similarity}{sequences}(x, y = NULL,
           method = c("jaccard", "dice", "cosine", "subset"),
	   strict = FALSE)

\S4method{is.subset}{sequences}(x, y = NULL, proper = FALSE)
\S4method{is.superset}{sequences}(x, y = NULL, proper = FALSE)
}
\arguments{
  \item{x, y}{an object.}
  \item{\dots}{further (unused) arguments.}
  \item{method}{a string specifying the similarity measure to use (see
    details).}
  \item{strict}{a logical value specifying if strict itemset matching
    should be used.}
  \item{proper}{a logical value specifying if only strict relationships
    (omitting equality) should be indicated.}
}
\details{
    Let the number of \emph{common} elements of two sequences refer to
    those that occur in a longest common subsequence. The following
    similarity measures are implemented:
\describe{
    \item{\code{jaccard}:}{The number of common elements divided by the
	total number of elements (the sum of the lengths of the sequences
	minus the length of the longest common subsequence).}
    \item{\code{dice}:}{Uses two times the number of common elements.}
    \item{\code{cosine}:}{Uses the square root of the product of the
	sequence lengths for the denominator.} 
    \item{\code{subset}:}{Zero if the first sequence is not a subsequence
	of the second. Otherwise the number of common elements divided by
	the number of elements in the first sequence.}
    }
    If \code{strict = TRUE} the elements (itemsets) of the sequences must
    be equal to be matched. Otherwise matches are quantified by the
    similarity of the itemsets (as specified by \code{method}) thresholded
    at 0.5, and the common sequence by the sum of the similarities.
}
\value{
    For \code{similarity}, returns an object of class 
    \code{\link[Matrix:dsCMatrix-class]{dsCMatrix}} if the result
    is symmetric (or \code{method = "subset"}) and and object of
    class \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} otherwise.

    For \code{is.subset, is.superset} returns an object of class
    \code{\link[Matrix:lgCMatrix-class]{lgCMatrix}}. 
}
\author{Christian Buchta}
\note{
    Computation of the longest common subsequence of two sequences of
    length \code{n, m} takes \code{O(n*m)} time.

    The supported set of operations for the above matrix classes depends
    on package \pkg{Matrix}. In case of problems, expand to full storage
    representation using \code{as(x, "matrix")} or \code{as.matrix(x)}.

    For efficiency use \code{as(x, "dist")} to convert a symmetric
    result matrix for clustering.
}
\seealso{
    Class
    \code{\linkS4class{sequences}},
    method
    \code{\link[arules:dissimilarity]{dissimilarity}}.
}
\examples{
## use example data
data(zaki)
z <- as(zaki, "timedsequences")
similarity(z)

# require equality
similarity(z, strict = TRUE)

## emphasize common
similarity(z, method = "dice")

## 
is.subset(z)
is.subset(z, proper = TRUE)
}
\keyword{manip}
