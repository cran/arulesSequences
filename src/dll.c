
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

extern SEXP R_transpose_ngCMatrix(SEXP x);
extern SEXP R_crosstab_ngCMatrix(SEXP x, SEXP y, SEXP t);
extern SEXP R_rowSums_ngCMatrix(SEXP x);
extern SEXP R_colSums_ngCMatrix(SEXP x);
extern SEXP R_colSubset_ngCMatrix(SEXP x, SEXP s);
extern SEXP R_rowSubset_ngCMatrix(SEXP x, SEXP s);
extern SEXP R_asList_ngCMatrix(SEXP x, SEXP d);
extern SEXP R_cbind_ngCMatrix(SEXP x, SEXP y);
extern SEXP R_recode_ngCMatrix(SEXP x, SEXP s);
extern SEXP R_or_ngCMatrix(SEXP x, SEXP y);
extern SEXP R_valid_ngCMatrix(SEXP x);

void R_init_arulesSequences(DllInfo *dll) {

    const R_CallMethodDef CallEntries[] = {
	{"R_makebin",		    (DL_FUNC) R_makebin,		2},
	{"R_rowSums_sgCMatrix",	    (DL_FUNC) R_rowSums_sgCMatrix,	1},
	{"R_rowSubset_sgCMatrix",   (DL_FUNC) R_rowSubset_sgCMatrix,	2},
	{"R_colAppend_sgCMatrix",   (DL_FUNC) R_colAppend_sgCMatrix,	3},
	{"R_valid_sgCMatrix",	    (DL_FUNC) R_valid_sgCMatrix,	1},
	{"R_firstOrder_sgCMatrix",  (DL_FUNC) R_firstOrder_sgCMatrix,	1},
	{"R_similarity_sgCMatrix",  (DL_FUNC) R_similarity_sgCMatrix,	4},
	{"R_as_dist_dsCMatrix",	    (DL_FUNC) R_as_dist_dsCMatrix,	1},
	{"R_ilscount",		    (DL_FUNC) R_ilscount,		9},
	{"R_pnscount",		    (DL_FUNC) R_pnscount,		4},
	{"R_pnsclosed",		    (DL_FUNC) R_pnsclosed,		4},
	{"R_pnsredundant",	    (DL_FUNC) R_pnsredundant,		4},
	{"R_pnssuperset",	    (DL_FUNC) R_pnssuperset,		5},

	{"R_transpose_ngCMatrix",   (DL_FUNC) R_transpose_ngCMatrix,	1},
        {"R_crosstab_ngCMatrix",    (DL_FUNC) R_crosstab_ngCMatrix,	3},
	{"R_rowSums_ngCMatrix",	    (DL_FUNC) R_rowSums_ngCMatrix,	1},
	{"R_colSums_ngCMatrix",	    (DL_FUNC) R_colSums_ngCMatrix,	1},
	{"R_colSubset_ngCMatrix",   (DL_FUNC) R_colSubset_ngCMatrix,	2},
	{"R_rowSubset_ngCMatrix",   (DL_FUNC) R_rowSubset_ngCMatrix,	2},
	{"R_asList_ngCMatrix",	    (DL_FUNC) R_asList_ngCMatrix,	2},
	{"R_cbind_ngCMatrix",	    (DL_FUNC) R_cbind_ngCMatrix,	2},
	{"R_recode_ngCMatrix",	    (DL_FUNC) R_recode_ngCMatrix,	2},
        {"R_or_ngCMatrix",	    (DL_FUNC) R_or_ngCMatrix,		2},
        {"R_valid_ngCMatrix",	    (DL_FUNC) R_valid_ngCMatrix,	1},

	{"R_pnindex",
	    (DL_FUNC) R_GetCCallable("arules", "R_pnindex"),		3},
	{"R_pnrindex",
	    (DL_FUNC) R_GetCCallable("arules", "R_pnrindex"),		2}, 
	{NULL, NULL, 0}
    };

    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

