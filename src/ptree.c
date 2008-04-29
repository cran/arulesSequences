#include <R.h>
#include <Rdefines.h>
#include <time.h>


// support counting of sequences using memory-efficient
// prefix-trees. 
//
// note that for events we require a superset to have
// a higher index than any of its subsets.
//
// todo: partial reordering of events for runtime 
//	 reduction.
//
// fixme: free on user interrupt is impossible.
//
// (C) ceeboo 2007

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

static void nbfree() {
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
	    p->pr = NULL;
	    p->pl = pnadd(NULL, x+1, n-1);
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
	    q->pr = p;
	    q->pl = pnadd(NULL, x+1, n-1);
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
    return -1;				    // set not found
}

static int issubset(int *x, int *y, int nx, int ny) {
    if (nx == 0)
	return 1;
    if (ny == 0)
	return 0;
    if (*x == *y)
	return issubset(x+1, y+1, nx-1, ny-1);
    if (*x > *y)
	return issubset(x, y+1, nx, ny-1);
    return 0;
}

static int *pe, *ie;

static int is_subset(int x, int y) {
    if (pe == NULL)
	return  (x == y);
    int px, py;

    px = pe[x];
    py = pe[y];

    if (px == py)
	return 1;

    return issubset(ie+px, ie+py, pe[x+1]-px, pe[y+1]-py); 
}

static int is_atom(int x) {
    if (pe == NULL)
	return 1;
    return (pe[x+1] == pe[x] + 1);
}

static int dpn, tc;

// count sequence

static void pnscount(PN *p, int *x, int n, PN *q, int r) {
    if (p == NULL || n == 0)
	return;
    cpn++;
    if (is_subset(p->index, *x)) {
	npn++;
	if (p->visit < tc) {
	    p->count++;
	    p->visit = tc;
	    pnscount(p->pl, x+1, n-1, p->pl, 1);
	} else
	    dpn++;
	if (p->index < *x)
	    pnscount(p->pr, x, n, q, 0);
	if (r && n > 1) {
	    if (is_atom(*(x+1)) && *x < *(x+1))
		pnscount(p->pr, x+1, n-1, q, r);
	    else
		pnscount(q, x+1, n-1, q, r);
	}
    } else
    if (p->index < *x) 
	pnscount(p->pr, x, n, q, r);
    else
	pnscount(p, x+1, n-1, q, r);
}

SEXP R_pnscount(SEXP R_x, SEXP R_t, SEXP R_e, SEXP R_v) {
    if (!inherits(R_x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    if (!inherits(R_t, "sgCMatrix"))
	error("'t' not of class sgCMatrix");
    if (INTEGER(GET_SLOT(R_x, install("Dim")))[0] != 
	INTEGER(GET_SLOT(R_t, install("Dim")))[0])
	error("the number of rows of 'x' and 't' do not conform");
    if (!isNull(R_e) && !inherits(R_e, "ngCMatrix"))
	error("e not of class ngCMatrix");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, f, l, k, n, nr, e;
    int *x;
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

    if (!isNull(R_e)) {
	if (nr != INTEGER(GET_SLOT(R_e, install("Dim")))[1])
	    error("the number of rows of 'x' and columns of 'e' do not conform");
	pe = INTEGER(GET_SLOT(R_e, install("p")));
	ie = INTEGER(GET_SLOT(R_e, install("i")));
    }
    else
	pe = NULL;

    cpn = apn = npn = 0;

    if (nb != NULL) 
	nbfree();
    nb = (PN **) malloc(sizeof(PN *) * (nr+1));
    if (nb == NULL)
	error("pointer array allocation failed");

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
	if (n > 1) {
	    x = INTEGER(ix)+f;
	    pnadd(nb[*x], x, n);
	    if (npn) {
		nbfree();
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
    
    cpn = npn = dpn = tc = 0;

    f = 0;
    for (i = 1; i < LENGTH(pt); i++) {
	l = INTEGER(pt)[i];
	n = l-f;
	if (n == 0)
	    continue;
	tc++;
	x = INTEGER(it)+f;
	pnscount(is_atom(*x) ? nb[*x] : *nb, x, n, *nb, 1);
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t3 = clock();
    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i transactions, processed %i (%.2f, %.2f) nodes [%.2fs]\n",
		LENGTH(pt) - 1, cpn, (double) dpn / npn, (double) npn / cpn,
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
	x = INTEGER(ix)+f;
	INTEGER(r)[i-1] = pnget(nb[*x], x, n); 
	f = l;
	R_CheckUserInterrupt();
    }
  
    nbfree();

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

// fixme: extend to items

SEXP R_pnsindex(SEXP R_x, SEXP R_e, SEXP R_v) {
    if (!inherits(R_x, "sgCMatrix"))
        error("'x' not of class ngCMatrix");
    if (!isNull(R_e))
        error("item based rules not yet supported");
    if (TYPEOF(R_v) != LGLSXP)
        error("'v' not of type logical");
    int i, k, f, l, m, n, nr;
    int *x;
    SEXP px, ix;
    SEXP r, is, ir, il;
#ifdef _TIME_H
    clock_t t2, t1 = clock();

    if (LOGICAL(R_v)[0] == TRUE) 
        Rprintf("processing ... ");
#endif
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    cpn = apn = npn = 0;
    
    if (nb != NULL)
        nbfree();
    nb = (PN **) malloc(sizeof(PN *) * (nr+1));
    if (nb == NULL)
        error("pointer array allocation failed");

    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
        nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
        nbfree();
        error("node allocation failed");
    }
  
    m = k = 0;
    
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
        l = INTEGER(px)[i];
        n = l-f;
        if (n == 0) 
            continue;
        x = INTEGER(ix)+f;
        pnadd(nb[*x], x, n);
        if (npn) {
            nbfree();
            error("node allocation failed");
        }
	if (nq->count == 0)
            nq->count = i;
	if (n > 1)
	    m++;
	if (n > k)
	    k = n;
        f = l;
        R_CheckUserInterrupt();
    }

    PROTECT(r = allocVector(VECSXP, 3));

    SET_VECTOR_ELT(r, 0, (is = allocVector(INTSXP, m)));
    SET_VECTOR_ELT(r, 1, (il = allocVector(INTSXP, m)));
    SET_VECTOR_ELT(r, 2, (ir = allocVector(INTSXP, m)));
  
    cpn = npn = 0;

    m = 0;
    
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0)
	    continue;
	if (n > 1) {
	    x = INTEGER(ix)+f;
	    INTEGER(is)[m] = i;
	    INTEGER(il)[m] = pnget(nb[x[0]], x, n-1);
	    INTEGER(ir)[m] = pnget(nb[x[n-1]], x+n-1, 1);
	    m++;
	}
	f = l;
	R_CheckUserInterrupt();
    }

    nbfree();

    if (apn)
        error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
    t2 = clock();
    if (LOGICAL(R_v)[0] == TRUE)
        Rprintf(" %i itemsets, %i rules [%.2fs]\n", LENGTH(px) - 1, m,
                ((double) t2-t1) / CLOCKS_PER_SEC);
#endif
    
    UNPROTECT(1);
 
    return r;
}

//

