
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP R_makebin(SEXP x, SEXP R_file);
extern SEXP R_rowSums_sgCMatrix(SEXP x);
extern SEXP R_rowSubset_sgCMatrix(SEXP x, SEXP s);
extern SEXP R_colAppend_sgCMatrix(SEXP x, SEXP y, SEXP R_s);
extern SEXP R_valid_sgCMatrix(SEXP x);
extern SEXP R_firstOrder_sgCMatrix(SEXP x);
extern SEXP R_similarity_sgCMatrix(SEXP x, SEXP y, SEXP R_e, SEXP R_method);
extern SEXP R_as_dist_dsCMatrix(SEXP x);
extern SEXP R_ilscount(SEXP x, SEXP R_tid, SEXP R_sid, SEXP R_eid,
		       SEXP R_mingap, SEXP R_maxgap, SEXP R_maxwin,
		       SEXP R_verbose, SEXP R_supporting);
extern SEXP R_pnscount(SEXP R_x, SEXP R_t, SEXP R_e, SEXP R_v);
extern SEXP R_pnsclosed(SEXP R_x, SEXP R_e, SEXP R_c, SEXP R_v);
extern SEXP R_pnsredundant(SEXP R_x, SEXP R_e, SEXP R_c, SEXP R_v);
extern SEXP R_pnssuperset(SEXP R_x, SEXP R_y, SEXP R_e, SEXP R_p, SEXP R_v);

static const R_CallMethodDef CallEntries[] = {
    {"R_makebin",		(DL_FUNC) &R_makebin,		    2},
    {"R_rowSums_sgCMatrix",	(DL_FUNC) &R_rowSums_sgCMatrix,	    1},
    {"R_rowSubset_sgCMatrix",	(DL_FUNC) &R_rowSubset_sgCMatrix,   2},
    {"R_colAppend_sgCMatrix",	(DL_FUNC) &R_colAppend_sgCMatrix,   3},
    {"R_valid_sgCMatrix",	(DL_FUNC) &R_valid_sgCMatrix,	    1},
    {"R_firstOrder_sgCMatrix",  (DL_FUNC) &R_firstOrder_sgCMatrix,  1},
    {"R_similarity_sgCMatrix",  (DL_FUNC) &R_similarity_sgCMatrix,  4},
    {"R_as_dist_dsCMatrix",	(DL_FUNC) &R_as_dist_dsCMatrix,	    1},
    {"R_ilscount",		(DL_FUNC) &R_ilscount,		    9},
    {"R_pnscount",		(DL_FUNC) &R_pnscount,		    4},
    {"R_pnsclosed",		(DL_FUNC) &R_pnsclosed,		    4},
    {"R_pnsredundant",		(DL_FUNC) &R_pnsredundant,	    4},
    {"R_pnssuperset",		(DL_FUNC) &R_pnssuperset,	    5},
    {NULL, NULL, 0}
};

void R_init_arulesSequences(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

