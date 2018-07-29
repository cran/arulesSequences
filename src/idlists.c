#include <R.h>
#include <Rdefines.h>
#include <time.h>

// #define __DEBUG

// support counting of sequences using id lists.
//
// The approach used here is slow but allows to handle more
// complex constraints on event times, notably maxwin.
//
// Supports list components of x.
//
// see  M. J. Zaki. (2001). SPADE: An Efficient Algorithm for
//      Mining Frequent Sequences. Machine Learning Journal, 
//      42, pp. 10-11.
//
// (C) ceeboo 2015/8
//
SEXP R_ilscount(SEXP x, SEXP R_tid, SEXP R_sid, SEXP R_eid, SEXP R_mingap, SEXP R_maxgap, SEXP R_maxwin, SEXP R_verbose, SEXP R_supporting) {
    if (TYPEOF(x) != VECSXP)
	error("'x' not of type list");
    if (!inherits(R_tid, "ngCMatrix"))
	error("'tid' not of class 'ngCMatrix'");
    if (TYPEOF(R_sid) != INTSXP)
	error("'sid' not of type integer");
    if (TYPEOF(R_eid) != INTSXP)
	error("'eid' not of type integer");
    if (TYPEOF(R_verbose) != LGLSXP)
	error("'verbose' not of type logical");

    // return identifiers of supporting sequences 
    // instead of counts
    Rboolean supporting = FALSE;
    if (!isNull(R_supporting))
	supporting = LOGICAL(R_supporting)[0];

    int ns, nr = INTEGER(getAttrib(R_tid, install("Dim")))[0];

    if (LENGTH(R_sid) != nr)
	error("'sid' invalid length");

    if (LENGTH(R_eid) != nr)
	error("'eid' invalid length");

    int *sid = INTEGER(R_sid),
	*eid = INTEGER(R_eid);

    int h, i, j, k, l, m, n;

    // see makebin.c

    n = 0;
    l = 0;
    m = 0;
    for (j = 0; j < nr; j++) {
	k = sid[j];
	if (k < 1)
	    error("'sid' invalid");
	if (k > l) {
	    if (m == NA_INTEGER)
		error("'eid' invalid");
	    m = 0;
	    l = k;
	    n++;
	} else
	    if (k < l)
		error("'sid' invalid (order)");
	i = eid[j];
	if (i <= m) {
	    if (i < 1)
		error("'eid' invalid");
	    else
		error("'eid' invalid (strict order)");
	}
	m = i;
    }
    if (l == NA_INTEGER)
	error("'sid' invalid");
    if (m == NA_INTEGER)
	error("'eid' invalid");
    ns = n;

    int  nc = INTEGER(getAttrib(R_tid, install("Dim")))[1],
	*pt = INTEGER(getAttrib(R_tid, install("p"))),
	*it = INTEGER(getAttrib(R_tid, install("i")));	// 0-based

    // maximum number of entries in a colum
    n = 0;
    l = pt[0];
    for (j = 1; j <= nc; j++) {
	k = pt[j];
	m = k - l;
	if (m > n)
	    n = m;
	l = k;
    }

    int *ii = INTEGER(PROTECT(allocVector(INTSXP, n))),
	*jj = INTEGER(PROTECT(allocVector(INTSXP, n))),
	*kk = NULL,
	*di = NULL,
	*dj = NULL;

    int mingap = 1,
	maxgap = INT_MAX,
	maxwin = 0;

    if (!isNull(R_mingap)) {
	if (INTEGER(R_mingap)[0] >= 1) 
	    mingap = INTEGER(R_mingap)[0];
	else
	    error("'mingap' invalid");
    }
    if (!isNull(R_maxgap)) {
	if (INTEGER(R_maxgap)[0] >= 0 &&
	    INTEGER(R_maxgap)[0] <= INT_MAX)
	    maxgap =  INTEGER(R_maxgap)[0];
	else
	    error("'maxgap' invalid");
    }
    
    if (!isNull(R_maxwin)) {
	// error("'maxwin' not supported");

	if (INTEGER(R_maxwin)[0] >= 0 &&
	    INTEGER(R_maxwin)[0] <= INT_MAX) {
	    if (INTEGER(R_maxwin)[0] > 0) {
		maxwin = INTEGER(R_maxwin)[0];
		if (maxgap > maxwin)
		    maxgap = maxwin;
	    } else
		maxgap = 0;
	} else
	    error("'maxwin' invalid");
    }

    if (maxwin) {
	di = INTEGER(PROTECT(allocVector(INTSXP, n)));
	dj = INTEGER(PROTECT(allocVector(INTSXP, n)));
    }

    SEXP r = PROTECT(allocVector((supporting) ? VECSXP : INTSXP, LENGTH(x)));
    setAttrib(r, R_NamesSymbol, getAttrib(x, R_NamesSymbol));

#ifdef _TIME_H
    clock_t t2, t1;

    t1 = clock();
    if (LOGICAL(R_verbose)[0])
	Rprintf("counting ... ");
#ifdef __DEBUG
    Rprintf("\n");

    Rprintf("mingap %d\n", mingap);
    Rprintf("maxgap %d\n", maxgap);
    Rprintf("maxwin %d\n", maxwin);
#endif
#endif

    int ne = 0, nt = 0;
    for (n = 0; n < LENGTH(x); n++) {
	SEXP s = VECTOR_ELT(x, n);

	if (LENGTH(s) == 0) {		    // empty
#ifdef __DEBUG
	    Rprintf(" %6d  [%d]\n", n+1, ns);
#endif
	    INTEGER(r)[n] = ns;
	    continue;
	}

	int ni = 0, nj, nk;

	h = 0;
	while (h < LENGTH(s)) {
	    SEXP t = VECTOR_ELT(s, h++);
	    if (LENGTH(t) == 0)
		error("'x' invalid component");

	    m = 0;
	    if (h == 1) {		    // initial list
		k = INTEGER(t)[m++];	    // 1-based
		l = k;
		if (k < 1 || k > nc)
		    error("'x' invalid index");
		i  = pt[k-1];
		ni = pt[k] - i;
#ifdef __DEBUG
		Rprintf(" %6d  %d (%d)", n+1, k, ni);
#endif
		if (LENGTH(s) == 1 &&
		    LENGTH(t) == 1) {	    // shortcut
		    kk = ii;
		    ii = it + i;
		    break;
		}			    // copy
		memcpy(ii, it + i, sizeof(int) * ni);
		if (maxwin)
		    memset(di, 0, sizeof(int) * ni);
	    }

	    if (!m) {			    // temporal
		k = INTEGER(t)[m++];
		l = k;
#ifdef __DEBUG
		Rprintf(" %d", k);
#endif
		if (k < 1 || k > nc)
		    error("'x' invalid index (zero)");
		l = k;

		i  = 0;
		nk = 0;
		j  = pt[k-1];
		nj = pt[k];
		while (i < ni && j < nj) {
		    if (sid[ii[i]] > sid[it[j]])
			j++;
		    else
		    if (sid[ii[i]] < sid[it[j]])
			i++;
		    else {
			int d = eid[it[j]] - eid[ii[i]];
			if (d < mingap)	    // not positive 
			    j++;
			else
			if (d > maxgap)
			    i++;
			else { 
			    if (maxwin) {
				d += di[i];
				if (d > maxwin) {
				    i++;
				    continue;
				}
				dj[nk] = d;
			    }
			    jj[nk] = it[j];
			    j++;
			    nk++;
			}
		    }
		}
		kk = ii;
		ii = jj;
		jj = kk;

		if (maxwin) {
		    kk = di;
		    di = dj;
		    dj = kk;
		}
		ni = nk;
		nt++;
#ifdef __DEBUG
		Rprintf(" (%d)", ni);
#endif
		if (!ni)
		    break;
	    }
	    while (m < LENGTH(t)) {	    // equality
		k = INTEGER(t)[m++];
#ifdef __DEBUG
		Rprintf(" %d", k);
#endif
		if (k <= l || k > nc)	    // not ascending
		    error("'x' invalid index (order)");
		l = k;

		i  = 0;
		nk = 0;
		j  = pt[k-1];
		nj = pt[k];
		while (i < ni && j < nj) {
		    if (ii[i] > it[j])
			j++;
		    else 
		    if (ii[i] < it[j])
			i++;
		    else {		    // match
			ii[nk] = ii[i];	    // omit check
			i++;
			j++;
			nk++;
		    }
		}
		ni = nk;
		ne++;
#ifdef __DEBUG
		Rprintf(" (%d)", ni);
#endif
		if (!ni)
		    break;
	    }
	    if (!ni)
		break;
	}
	// count unique identifiers
	nk = 0;
	l  = 0;
	for (i = 0; i < ni; i++) {
	    k = sid[ii[i]];
	    if (k > l) {
		l = k;
		nk++;
	    }
	}
	if (supporting) {
	    SEXP s;
	    SET_VECTOR_ELT(r, n, s = allocVector(INTSXP, nk));
	    nk = 0;
	    l  = 0;
	    for (i = 0; i < ni; i++) {
		k = sid[ii[i]];
		if (k > l) {
		    INTEGER(s)[nk] = k;
		    l = k;
		    nk++;
		}
	    }
	} else
	    INTEGER(r)[n] = nk;
#ifdef __DEBUG
	Rprintf(" [%d]\n", nk);
#endif
	if (h == 1 && 
	    m == 1)
	    ii = kk;

	R_CheckUserInterrupt();
    }
#ifdef _TIME_H
    t2 = clock();

    if (LOGICAL(R_verbose)[0])
	Rprintf("%i sequence(s), processed %i/%i join(s) [%.2fs]\n",
	    LENGTH(x), ne, nt,
	    ((double) t2 - t1) / CLOCKS_PER_SEC);
#endif
    if (maxwin)
	UNPROTECT(2);
    UNPROTECT(3);

    return r; 
}

