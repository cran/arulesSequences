#include <R.h>
#include <Rdefines.h>
#include <time.h>

// support counting of sequences using memory-efficient
// prefix-trees. 
//
// fixme: free on user interrupt is impossible.
//
// (C) ceeboo 2007, 2015, 2016

typedef struct pnode {
    int index;
    int count;
    int visit;
    struct pnode *pl;
    struct pnode *pr;
} PN;

static PN *nq, **nb = NULL;		    // node pointers
static int npn, cpn, apn;		    // node counters

static void pnfree(PN *p) {
    if (p == NULL)
	return;
    pnfree(p->pl);
    pnfree(p->pr);
    free(p);
    apn--;
}

static void nbfree(void) {
    pnfree(*nb);
      free( nb);
    nb = NULL;
}

static PN *pnadd(PN *p, int *x, int n) {
    if (n == 0)
	return p;
    cpn++;
    if (p == NULL) {			    // append node
	p = nq = (PN *) malloc(sizeof(PN));
	if (p) {
	    apn++;
	    p->index = *x;
	    p->count = 0;
	    p->visit = 0;
	    p->pl = pnadd(NULL, x+1, n-1);
	    p->pr = NULL;
	} else
	    npn = 1;
    } else
    if (p->index == *x) {		    // existing node
	nq = p;
	p->pl = pnadd(p->pl, x+1, n-1);
    } else
    if (p->index < *x) {		    // search right subtree
	nq = p;
	p->pr = pnadd(p->pr, x, n);
    } else {				    // prepend node
	PN *q = nq = (PN *) malloc(sizeof(PN));
	if (q) {
	    apn++;
	    q->index = *x;
	    q->count = 0;
	    q->visit = 0;
	    q->pl = pnadd(NULL, x+1, n-1);
	    q->pr = p;
	    p = q;
	} else
	    npn = 1;
    }
    return p;
}


// retrieve count

static int pnget(PN *p, int *x, int n) {
    if (p == NULL || n == 0)
	return 0;
    cpn++;
    if (p->index == *x) {
	npn++;
	if (n == 1)
	    return p->count;
	return pnget(p->pl, x+1, n-1);
    }
    if (p->index < *x) 
	return pnget(p->pr, x, n);
    return 0;				    // set not found
}

// count sequence
//
// NOTE item indexes in itemsets must be in 
//	ascending order.
//

static int dpn, sn;
static int ct, cn, cx;
static int *cb;

static void pnscount(PN *p, int *x, int n) {
    if (p == NULL || n == 0)
	return;
    cpn++;
    // search
    if (p->index < *x) {	// *x > -1
	p = p->pr;
	while (p && p->index < *x) {
	    cpn++;
	    p = p->pr;
	}
	if (!p) {
	    if (!nq)
		return;
	    while (n && *x > -1) {
		x++;
		n--;
	    }
	    if (!n)
		return;
	    p = nq;
	}
    }
    if (p->index == *x) { 
	if (p->visit < sn) {
	    dpn++;
	    p->visit = sn + n;
	    switch (ct) {
		case 0: p->count++;
			break;
		case 1: if (cn && 
			    p->count < cx)
			    p->count = cx;
			cn--;
			break;
		case 2: if (cn &&
			    cx < p->count)
			    cx = p->count;
			cn--;
			break;
		case 3: if (cx && 
			    p->count)
			    cb[cn++] = p->count;
			cx--;
			break;
	    }
	} else {
	    npn++;
	    if (p->visit == sn + n)
		return;		// x identical
	}
	if (p->index > -1) {
	    if (p->pl) {
		PN *q = nq;
		if (p->pl->index > -1)
		    nq = NULL;
		else
		    nq = p->pl;
		pnscount(p->pl, x+1, n-1);
		nq = q;
	    }
	    pnscount(p, x+1, n-1);
	} else {
	    nq = p->pl;
	    pnscount(nq, x+1, n-1);
	}
    } else			// p->index > -1
	if (*x > -1)
	    pnscount(p, x+1, n-1);
	else
	    if (nq) {
		if (nq->index > -1)
		    pnscount(nq, x+1, n-1);
		else
		    pnscount(nq, x, n);
	    }
}

// map sequence
//
// NOTE itemset indexes must be 0-based
//	and item indexes must not be
//	negative
//
static int *eb = NULL;			    // buffer pointer
static int  ne;				    // buffer size

static void ebfree(void) {
    if (eb == NULL)
	return;
    free(eb);
    eb = NULL;
    ne = 0;
}

static int eballoc(void) {
    int *q = eb;
    if (!q)
	ne = 1024;			    // initial size
    else
	ne = ne * 2;
    eb = realloc(q, sizeof(int) * ne);
    if (!eb) {
	eb = q;
	ebfree();
	return 0;
    }
    return ne; 
}

static int emap(int *x, int nx, int *pe, int *ie) {
    int i, k, f, l, n;
    n = 0;
    if (pe) {
	for (i = 0; i < nx; i++) {
	    l = x[i];
	    f = pe[l];
	    l = pe[l+1];
	    if (n + l - f >= ne && !eballoc())	    // FIXME
		return 0;
	    for (k = f; k < l; k++)
		eb[n++] = ie[k];
	    eb[n++] = -1;			    // delimiter
	}
    } else {
	if (2 * nx > ne && !eballoc())
	    return 0;
	for (i = 0; i < nx; i++) {
	    eb[n++] = x[i];
	    eb[n++] = -1;			    // delimiter
	}
    }
    if (n)
	n--;
    return n;
}

SEXP R_pnscount(SEXP R_x, SEXP R_t, SEXP R_e, SEXP R_v) {
    if (!inherits(R_x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    if (!inherits(R_t, "sgCMatrix"))
	error("'t' not of class sgCMatrix");
    if (INTEGER(GET_SLOT(R_x, install("Dim")))[0] != 
	INTEGER(GET_SLOT(R_t, install("Dim")))[0])
	error("the number of rows of 'x' and 't' do not conform");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, f, l, k, n, nr, e;
    int *x = NULL;
    SEXP px, ix, pt, it;
    SEXP r; 
#ifdef _TIME_H
    clock_t t4, t3, t2, t1;

    t1 = clock();
    
    if (LOGICAL(R_v)[0] == TRUE)
	Rprintf("preparing ... ");
#endif
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    pt = GET_SLOT(R_t, install("p"));
    it = GET_SLOT(R_t, install("i"));

    int *pe = NULL, *ie = NULL;
    if (!isNull(R_e)) {
        if (nr != INTEGER(GET_SLOT(R_e, install("Dim")))[1])
            error("the number of rows of 'x' and columns of 'e' do not conform");
        pe = INTEGER(GET_SLOT(R_e, install("p")));
        ie = INTEGER(GET_SLOT(R_e, install("i")));

	if (!eballoc())
	    error("buffer allocation failed");
    }

    if (nb != NULL) 
	nbfree();
    nb = (PN **) malloc(sizeof(PN *) * (nr+1));
    if (nb == NULL)
	error("pointer array allocation failed");

    cpn = apn = npn = 0;

    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
	nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
	nbfree();
	error("node allocation failed");
    }

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0)
	    continue;
	n = emap(INTEGER(ix)+f, n, pe, ie);
	if (!n) {
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	if (n > 1) {
	    pnadd(nb[*x], x, n);
	    if (npn) {
		nbfree();
		ebfree();
		error("node allocation failed");
	    }
	}
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t2 = clock();
    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i sequences, created %i (%.2f) nodes [%.2fs]\n",
		LENGTH(px) - 1, apn, (double) apn / cpn,
		((double) t2 - t1) / CLOCKS_PER_SEC);
	Rprintf("counting ... ");
    }
#endif

    cpn = npn = dpn = sn = 0;
     ct = 0;

    k = 0;
    f = 0;
    for (i = 1; i < LENGTH(pt); i++) {
	l = INTEGER(pt)[i];
	n = l-f;
	if (n == 0)
	    continue;
	k += n;
	n = emap(INTEGER(it)+f, n, pe, ie);
	if (!n) {
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	sn++;
	nq = *nb;
	pnscount(nb[*x], x, n);
	sn += n;
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t3 = clock();
    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i transactions (%i), processed %i (%.2f, %.2f) nodes [%.2fs]\n",
		k, LENGTH(pt) - 1, cpn, (double) dpn / cpn, (double)  npn / cpn,
		((double) t3 - t2) / CLOCKS_PER_SEC);
	Rprintf("writing ... ");
    }
#endif
 
    PROTECT(r = allocVector(INTSXP, LENGTH(px)-1));

    cpn = npn = 0;
    
    e = LENGTH(pt) - 1;
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    INTEGER(r)[i-1] = e;
	    continue;
	}
	n = emap(INTEGER(ix)+f, n, pe, ie);
	if (!n) {			    // never
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	n = pnget(nb[*x], x, n);
	INTEGER(r)[i-1] = n; 
	f = l;
	R_CheckUserInterrupt();
    }
  
    nbfree();
    ebfree();

    if (apn)
	error("node deallocation imbalance %i", apn);
    
#ifdef _TIME_H
    t4 = clock();

    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i counts, ", LENGTH(px)-1);
	Rprintf("processed %i (%.2f) nodes [%.2fs]\n", cpn, (double) npn / cpn,
		((double) t4 - t3) / CLOCKS_PER_SEC);
    }
#endif

    UNPROTECT(1);

    return r;
}

// NOTE node statistics are omitted.
//

SEXP R_pnsclosed(SEXP R_x, SEXP R_e, SEXP R_c, SEXP R_v) {
    if (!inherits(R_x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    if (TYPEOF(R_c) != INTSXP)
	error("'c' not of storage type integer");
    if (LENGTH(R_c) != INTEGER(GET_SLOT(R_x, install("Dim")))[1])
	error("'x' and 'c' not the same length");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, f, l, k, n, nr, e;
    int *x = NULL;
    SEXP px, ix;
    SEXP r; 
#ifdef _TIME_H
    clock_t t4, t3, t2, t1;

    t1 = clock();
    
    if (LOGICAL(R_v)[0] == TRUE)
	Rprintf("checking ... ");
#endif
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    int *pe = NULL, *ie = NULL;
    if (!isNull(R_e)) {
        if (nr != INTEGER(GET_SLOT(R_e, install("Dim")))[1])
            error("the number of rows of 'x' and columns of 'e' do not conform");
        pe = INTEGER(GET_SLOT(R_e, install("p")));
        ie = INTEGER(GET_SLOT(R_e, install("i")));

	if (!eballoc())
	    error("buffer allocation failed");
    }

    for (k = 0; k < LENGTH(R_c); k++) {
	e = INTEGER(R_c)[k];
	if (e == NA_INTEGER || e < 1)
	    error("'c' invalid value");
    }

    if (nb != NULL) 
	nbfree();
    nb = (PN **) malloc(sizeof(PN *) * (nr+1));
    if (nb == NULL)
	error("pointer array allocation failed");

    cpn = apn = npn = 0;

    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
	nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
	nbfree();
	error("node allocation failed");
    }

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0)
	    continue;
	n = emap(INTEGER(ix)+f, n, pe, ie);
	if (!n) {
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	if (n > 1) {
	    pnadd(nb[*x], x, n);
	    if (npn) {
		nbfree();
		ebfree();
		error("node allocation failed");
	    }
	}
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t2 = clock();
#endif

    cpn = npn = dpn = sn = 0;
     ct = 1;

    e = 0;
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0)
	    continue;
	n = emap(INTEGER(ix)+f, n, pe, ie);
	if (!n) {			    // never
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	sn++;
	nq = *nb;
	cn = n - 1;
	cx = INTEGER(R_c)[i-1];
	if (e < cx)
	    e = cx;
	pnscount(nb[*x], x, n);
	sn += n;
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t3 = clock();
#endif

    PROTECT(r = allocVector(LGLSXP, LENGTH(px)-1));

    cpn = npn = 0;
    
    k = 0;
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    if (e < INTEGER(R_c)[i-1])
		LOGICAL(r)[i-1] = TRUE;
	    else {
		LOGICAL(r)[i-1] = FALSE;
		if (e > INTEGER(R_c)[i-1])
		    k++;
	    }
	    continue;
	}
	n = emap(INTEGER(ix)+f, n, pe, ie);
	if (!n) {			    // never
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	n = pnget(nb[*x], x, n);
	if (n < INTEGER(R_c)[i-1])
	    LOGICAL(r)[i-1] = TRUE;
	else {
	    LOGICAL(r)[i-1] = FALSE;
	    if (n > INTEGER(R_c)[i-1])
		k++;
	}
	f = l;
	R_CheckUserInterrupt();
    }
  
    if (k)
	warning("'c' not closed");

    nbfree();
    ebfree();

    if (apn)
	error("node deallocation imbalance %i", apn);
    
#ifdef _TIME_H
    t4 = clock();

    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i counts [%.2fs, %.2fs]\n", LENGTH(px)-1,
		((double) t4 - t1) / CLOCKS_PER_SEC,
		((double) t3 - t2) / CLOCKS_PER_SEC);
    }
#endif

    UNPROTECT(1);

    return r;
}


SEXP R_pnsredundant(SEXP R_x, SEXP R_e, SEXP R_c, SEXP R_v) {
    if (!inherits(R_x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    if (TYPEOF(R_c) != INTSXP)
	error("'c' not of storage type integer");
    if (LENGTH(R_c) != INTEGER(GET_SLOT(R_x, install("Dim")))[1])
	error("'x' and 'c' not the same length");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, f, l, k, n, nr, e;
    int *x = NULL;
    SEXP px, ix;
    SEXP r; 
#ifdef _TIME_H
    clock_t t3, t2, t1;

    t1 = clock();
    
    if (LOGICAL(R_v)[0] == TRUE)
	Rprintf("checking ... ");
#endif
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    int *pe = NULL, *ie = NULL;
    if (!isNull(R_e)) {
        if (nr != INTEGER(GET_SLOT(R_e, install("Dim")))[1])
            error("the number of rows of 'x' and columns of 'e' do not conform");
        pe = INTEGER(GET_SLOT(R_e, install("p")));
        ie = INTEGER(GET_SLOT(R_e, install("i")));

	if (!eballoc())
	    error("buffer allocation failed");
    }

    for (k = 0; k < LENGTH(R_c); k++) {
	e = INTEGER(R_c)[k];
	if (e == NA_INTEGER || e < 1)
	    error("'c' invalid value");
    }

    if (nb != NULL) 
	nbfree();
    nb = (PN **) malloc(sizeof(PN *) * (nr+1));
    if (nb == NULL)
	error("pointer array allocation failed");

    cpn = apn = npn = 0;

    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
	nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
	nbfree();
	error("node allocation failed");
    }

    e = 0;
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    e = INTEGER(R_c)[i-1];
	    continue;
	}
	n = emap(INTEGER(ix)+f, n, pe, ie);
	if (!n) {
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	if (n > 1) {
	    pnadd(nb[*x], x, n);
	    if (npn) {
		nbfree();
		ebfree();
		error("node allocation failed");
	    }
	} else
	    nq = nb[*x];
	nq->count = INTEGER(R_c)[i-1];
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t2 = clock();
#endif

    PROTECT(r = allocVector(LGLSXP, LENGTH(px)-1));

    cpn = npn = dpn = sn = 0;
     ct = 2;

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    LOGICAL(r)[i-1] = FALSE;
	    continue;
	}
	if (e < INTEGER(R_c)[i-1]) {
	    n = emap(INTEGER(ix)+f, n, pe, ie);
	    if (!n) {			    // never
		nbfree();
		ebfree();
		error("buffer allocation failed");
	    }
	    x = eb;
	    sn++;
	    nq = *nb;
	    cn = n - 1;
	    cx = 0;
	    pnscount(nb[*x], x, n);
	    sn += n;
	    if (cx < INTEGER(R_c)[i-1])
		LOGICAL(r)[i-1] = FALSE;
	    else
		LOGICAL(r)[i-1] = TRUE;
	} else
	    LOGICAL(r)[i-1] = TRUE;
	f = l;
	R_CheckUserInterrupt();
    }

    nbfree();
    ebfree();

    if (apn)
	error("node deallocation imbalance %i", apn);
    
#ifdef _TIME_H
    t3 = clock();

    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i counts [%.2fs, %.2fs]\n", LENGTH(px)-1,
		((double) t3 - t1) / CLOCKS_PER_SEC,
		((double) t3 - t2) / CLOCKS_PER_SEC);
    }
#endif

    UNPROTECT(1);

    return r;
}

// duplicates
//
// NOTE protect variables already in use
//
static PN **db = NULL;
static int nd;

static void dbfree(void) {
    if (db == NULL)
	return;
    for (int k = 0; k < nd; k++)
	pnfree(db[k]);
    free(db);
    db = NULL;
    nd = 0;
}


SEXP R_pnssuperset(SEXP R_x, SEXP R_y, SEXP R_e, SEXP R_p, SEXP R_v) {
    if (!inherits(R_x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    if (!isNull(R_y) && 
	!inherits(R_y, "sgCMatrix"))
	error("'y' not of class sgCMatrix");
    if (TYPEOF(R_p) != LGLSXP)
	error("'p' not of type logical");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, f, l, k, n, nr, e;
    int *x = NULL;
    SEXP px, ix, py, iy;
    SEXP r; 
#ifdef _TIME_H
    clock_t t3, t2, t1;

    t1 = clock();
    
    if (LOGICAL(R_v)[0] == TRUE)
	Rprintf("checking ... ");
#endif
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    if (!isNull(R_y)) {
	if (nr != INTEGER(GET_SLOT(R_y, install("Dim")))[0])
	    error("the number of rows of 'x' and 'y' do not conform");
	py = GET_SLOT(R_y, install("p"));
	iy = GET_SLOT(R_y, install("i"));
    } else {
	py = px;
	iy = ix;
    }

    int *pe = NULL, *ie = NULL;
    if (!isNull(R_e)) {
        if (nr != INTEGER(GET_SLOT(R_e, install("Dim")))[1])
            error("the number of rows of 'x' and columns of 'e' do not conform");
        pe = INTEGER(GET_SLOT(R_e, install("p")));
        ie = INTEGER(GET_SLOT(R_e, install("i")));

	if (!eballoc())
	    error("buffer allocation failed");
    }

    dpn = 0;

    if (db != NULL)
	dbfree();
    nd = LENGTH(py);
    db = (PN **) malloc(sizeof(PN *) * nd);
    if (db == NULL)
	error("pointer array allocation failed");
    for (k = 0; k < nd; k++)
	db[k] = NULL;

    if (nb != NULL) 
	nbfree();
    nb = (PN **) malloc(sizeof(PN *) * (nr+1));
    if (nb == NULL) {
	dbfree();
	error("pointer array allocation failed");
    }

    cpn = apn = npn = 0;

    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
	nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
	dbfree();
	nbfree();
	error("node allocation failed");
    }

    e = 0;
    f = 0;
    for (i = 1; i < LENGTH(py); i++) {
	l = INTEGER(py)[i];
	n = l-f;
	if (n == 0) {
	    if (e) {
		db[e] = pnadd(db[e], &i, 1);
		dpn++;
		if (npn) {
		    dbfree();
		    nbfree();
		    ebfree();
		    error("node allocation failed");
		}
	    } else
		e = i;
	    continue;
	}
	n = emap(INTEGER(iy)+f, n, pe, ie);
	if (!n) {
	    dbfree();
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	if (n > 1) {
	    pnadd(nb[*x], x, n);
	    if (npn) {
		dbfree();
		nbfree();
		ebfree();
		error("node allocation failed");
	    }
	} else
	    nq = nb[*x];
	k = nq->count;
	if (k) {
	    db[k] = pnadd(db[k], &i, 1);
	    dpn++;
	    if (npn) {
		dbfree();
		nbfree();
		ebfree();
		error("node allocation failed");
	    }
	} else
	    nq->count = i;
	f = l;
	R_CheckUserInterrupt();
    }

    if (dpn)
	warning("duplicate element(s)");
    else
	dbfree();

#ifdef _TIME_H
    t2 = clock();
#endif

    PROTECT(r = allocVector(VECSXP, LENGTH(px)-1));

    cpn = npn = dpn = sn = 0;
     ct = 3;

    cb = INTEGER(PROTECT(allocVector(INTSXP, LENGTH(py) - 1)));

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    if (LOGICAL(R_p)[0])
		SET_VECTOR_ELT(r, i-1, allocVector(INTSXP, 0));
	    else {
		cb[n++] = e;
		if (db) {
		    PN *q = db[cb[e]];
		    while (q) {
			cb[n++] = q->index;
			q = q->pr;
		    }
		}
		SEXP t;

		SET_VECTOR_ELT(r, i-1, (t = allocVector(INTSXP, n)));
		memcpy(INTEGER(t), cb, sizeof(int) * n);
	    }
	    continue;
	}
	n = emap(INTEGER(ix)+f, n, pe, ie);
	if (!n) {			    // never
	    dbfree();
	    nbfree();
	    ebfree();
	    error("buffer allocation failed");
	}
	x = eb;
	sn++;
	nq = *nb;
	cn = 0;
	if (e)
	    cb[cn++] = e;
	cx = -1;
	if (LOGICAL(R_p)[0])
	    cx += n;
	pnscount(nb[*x], x, n);
	sn += n;
	{
	    if (db != NULL) {
		n = cn;
		for (k = 0; k < cn; k++) {
		    PN *q = db[cb[k]];
		    while (q) {
			cb[n++] = q->index;
			q = q->pr;
		    }
		}
		cn = n;	
	    }
	    SEXP t;

	    SET_VECTOR_ELT(r, i-1, (t = allocVector(INTSXP, cn)));
	    memcpy(INTEGER(t), cb, sizeof(int) * cn);
	    R_isort(INTEGER(t), cn);
	}
	f = l;
	R_CheckUserInterrupt();
    }

    UNPROTECT(1);

    dbfree();
    nbfree();
    ebfree();

    if (apn)
	error("node deallocation imbalance %i", apn);
    
#ifdef _TIME_H
    t3 = clock();

    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i counts [%.2fs, %.2fs]\n", LENGTH(px)-1,
		((double) t3 - t1) / CLOCKS_PER_SEC,
		((double) t3 - t2) / CLOCKS_PER_SEC);
    }
#endif

    UNPROTECT(1);

    return r;
}

