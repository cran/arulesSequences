
### ported from arules 1.7-7

library("arulesSequences")

e <- new("ngCMatrix")
x <- new("ngCMatrix", p   = as.integer(c(0,3,5,7,7,10)),
                      i   = as.integer(c(1,2,4,1,2,0,4,0,2,5)),
                      Dim = as.integer(c(6,5)))
rownames(x) <- paste("I", 1:6, sep = "")
colnames(x) <- paste("T", 1:5, sep = "")
#x

##
#unclass(x)

## validity
stopifnot(getValidity(getClassDef("CsparseMatrix"))(x))
.Call(arulesSequences:::R_valid_ngCMatrix, x)
.Call(arulesSequences:::R_valid_ngCMatrix, e)

## t
identical(.Call(arulesSequences:::R_transpose_ngCMatrix, x), t(x))
identical(.Call(arulesSequences:::R_transpose_ngCMatrix, e), t(e))

## column/row subset (index can only be integer now)
s <- as.integer(c(1,1,3,4))
stopifnot(all.equal(x[,s], .Call(arulesSequences:::R_colSubset_ngCMatrix, x, s)))

#
stopifnot(identical(.Call(arulesSequences:::R_colSubset_ngCMatrix, e, integer()), e))
stopifnot(all.equal(x[s,], .Call(arulesSequences:::R_rowSubset_ngCMatrix, x, s)))

#
stopifnot(identical(.Call(arulesSequences:::R_rowSubset_ngCMatrix, e, integer()), e))

## reorder
stopifnot(all.equal(.Call(arulesSequences:::R_recode_ngCMatrix, x, 6:1), x[6:1,]))
stopifnot(all.equal(.Call(arulesSequences:::R_recode_ngCMatrix, e, integer()), e))

## recode (add columns)
stopifnot(identical(.Call(arulesSequences:::R_recode_ngCMatrix, x, c(1L,3:7)),
  as(rbind(x[1,,drop=FALSE], 
    Matrix(0, ncol = 5, sparse = TRUE), 
    x[2:6,, drop = FALSE]), "nsparseMatrix")))

## cbind
stopifnot(identical(.Call(arulesSequences:::R_cbind_ngCMatrix, e, e), cbind(e,e)))
stopifnot(identical(.Call(arulesSequences:::R_cbind_ngCMatrix, x, x), cbind(x, x)))

## logical OR
identical(.Call(arulesSequences:::R_or_ngCMatrix, x, x), 
  as(x | x, "nsparseMatrix"))
identical(.Call(arulesSequences:::R_or_ngCMatrix, e, e), 
  as(e | e, "nsparseMatrix"))

## row sums
stopifnot(all.equal(.Call(arulesSequences:::R_rowSums_ngCMatrix, x), rowSums(x)))
stopifnot(all.equal(.Call(arulesSequences:::R_rowSums_ngCMatrix, e), rowSums(e)))

## column sums
stopifnot(all.equal(colSums(x), .Call(arulesSequences:::R_colSums_ngCMatrix, x)))
stopifnot(all.equal(.Call(arulesSequences:::R_colSums_ngCMatrix, e), colSums(e)))

## crossprod and tcrossprod
all.equal(.Call(arulesSequences:::R_crosstab_ngCMatrix, x, NULL, TRUE),
  as(tcrossprod(as(as(x, "ngCMatrix"), "dsparseMatrix")), "matrix"))
all.equal(.Call(arulesSequences:::R_crosstab_ngCMatrix, x, NULL, FALSE),
  as(crossprod(as(as(x, "ngCMatrix"), "dsparseMatrix")), "matrix"))

###

