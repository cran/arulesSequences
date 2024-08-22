
## cebboo 2024
##
## stuff no longer maintained in arules

## t in Matrix is now faster
##setMethod("t", signature(x = "ngCMatrix"),
##    function(x) .Call(R_transpose_ngCMatrix, x))

