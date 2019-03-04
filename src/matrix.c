#include <R.h>
#include <R_ext/Utils.h>
#include <Rdefines.h>

// arraySubscript.c
extern SEXP _int_array_subscript(int, SEXP, const char *, const char *, SEXP, Rboolean, SEXP);

// some low-level utilities that speed up 
// operations with sgCMatrix.
//
// version: 0.1-3
//
// ceeboo 2007, 2008, 2011

SEXP R_rowSums_sgCMatrix(SEXP x) {
    if (!inherits(x, "sgCMatrix"))
	error("'x' not of class 'sgCMatrix'");
    int h, i, j, k, f, l, n; 
    SEXP r, px, ix;

    n = INTEGER(getAttrib(x, install("Dim")))[0];

    px = getAttrib(x, install("p"));

    PROTECT(ix = duplicate(getAttrib(x, install("i"))));

    PROTECT(r = allocVector(INTSXP, n));
    memset(INTEGER(r), 0, sizeof(int) * n);

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0)
	    continue;
	R_isort(INTEGER(ix)+f, n);
	h = INTEGER(ix)[f];
	INTEGER(r)[h]++;
	for (k = f+1; k < l; k++) {
	    j = INTEGER(ix)[k];
	    if (j == h)
		continue;
	    INTEGER(r)[j]++;
	    h = j;
	}
	f = l;
    }
    setAttrib(r, R_NamesSymbol, VECTOR_ELT(getAttrib(x, install("Dimnames")), 0));

    UNPROTECT(2);

    return r;
}

// strict subsets that preserve the
// order of the sequences.

SEXP R_rowSubset_sgCMatrix(SEXP x, SEXP s) {
    if (!inherits(x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    int i, j, k, f, l, n, *o;
    SEXP r, dx, px, ix, pr, ir;
	        
    dx = getAttrib(x, install("Dimnames"));
#ifdef _COMPAT_
    r = CONS(dx, ATTRIB(x));
    SET_TAG(r, R_DimNamesSymbol);
    SET_ATTRIB(x, r);

    PROTECT(s = arraySubscript(0, s, getAttrib(x, install("Dim")), getAttrib, (STRING_ELT), x));
    
    SET_ATTRIB(x, CDR(r));
#else
    PROTECT(s = _int_array_subscript(0, s, "Dim", "Dimnames", x, TRUE, R_NilValue));
#endif

    n = INTEGER(getAttrib(x, install("Dim")))[0];

    o = INTEGER(PROTECT(allocVector(INTSXP, n)));
    memset(o, 0, sizeof(int) * n);

    l = 1;
    for (i = 0; i < LENGTH(s); i++) {
	j = INTEGER(s)[i];
	if (j == NA_INTEGER)
	    error("invalid subscript(s)");
	if (j < l)
	    error("invalid subscript(s)");
	if (o[j-1] == 0)
	    o[j-1] = i+1;
	else
	    error("invalid subscript(s)");
	l = j;
    }

    ix = getAttrib(x, install("i"));

    n = 0;
    if (LENGTH(s))
	for (i = 0; i < LENGTH(ix); i++)
	    if (o[INTEGER(ix)[i]])
		n++;

    px = getAttrib(x, install("p"));

    PROTECT(r = NEW_OBJECT(PROTECT(MAKE_CLASS("sgCMatrix"))));
    setAttrib(r, install("p"), PROTECT(pr = allocVector(INTSXP, LENGTH(px))));
    setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, n)));
    UNPROTECT(2);

    f = n = INTEGER(pr)[0] = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	if (LENGTH(s))
	    for (k = f; k < l; k++) {
		j = o[INTEGER(ix)[k]];
		if (j)
		    INTEGER(ir)[n++] = j-1;
	    }
	INTEGER(pr)[i] = n;
	f = l;
    }
    
    setAttrib(r, install("Dim"), PROTECT(ir = allocVector(INTSXP, 2)));
    UNPROTECT(1);
    INTEGER(ir)[0] = LENGTH(s);
    INTEGER(ir)[1] = LENGTH(px)-1;

    if (isNull((ix = VECTOR_ELT(dx, 0))))
	setAttrib(r, install("Dimnames"), dx);
    else {
	setAttrib(r, install("Dimnames"), PROTECT(ir = allocVector(VECSXP, 2)));
	UNPROTECT(1);
	setAttrib(ir, R_NamesSymbol, getAttrib(dx, R_NamesSymbol));
	SET_VECTOR_ELT(ir, 1, VECTOR_ELT(dx, 1));
	if (LENGTH(s) > 0) {
	    SET_VECTOR_ELT(ir, 0, (pr = allocVector(STRSXP, LENGTH(s))));
	    for (i = 0; i < LENGTH(s); i++)
		SET_STRING_ELT(pr, i, STRING_ELT(ix, INTEGER(s)[i]-1));
	} else
	    SET_VECTOR_ELT(ir, 0, R_NilValue);
    }

    UNPROTECT(4);

    return r;
}

// for each column of x append the corresponding
// column of y to the column of x. thus, the objects
// must conform with respect to both dimensions.
//
// we provide for a seperator, i.e. we append a 
// row and use its code as seperator.

SEXP R_colAppend_sgCMatrix(SEXP x, SEXP y, SEXP R_s) {
    if (!inherits(x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    if (!inherits(y, "sgCMatrix"))
	error("'y' not of class sgCMatrix");
    if (INTEGER(getAttrib(x, install("Dim")))[1] !=
	INTEGER(getAttrib(y, install("Dim")))[1])
	error("the number of columns of 'x' and 'y' do not conform");
    if (TYPEOF(R_s) != LGLSXP)
	error("'s' not of storage type logical");
    int i, k, fx, lx, fy, ly, nr, n;
    SEXP r, pr, ir, px, ix, py, iy;

    nr = INTEGER(getAttrib(x, install("Dim")))[0];
    if (nr != INTEGER(getAttrib(y, install("Dim")))[0])
	error("the number of rows of 'x' and 'y' do not conform");

    px = getAttrib(x, install("p"));
    py = getAttrib(y, install("p"));
    if (LENGTH(px) != LENGTH(py))
	error("slots p of 'x' and 'y' do not conform");

    ix = getAttrib(x, install("i"));
    iy = getAttrib(y, install("i"));

    n = (LOGICAL(R_s)[0] == FALSE) ? 0 : LENGTH(px)-1;	    // seperators

    PROTECT(r = NEW_OBJECT(PROTECT(MAKE_CLASS("sgCMatrix"))));
    setAttrib(r, install("p"), PROTECT(pr = allocVector(INTSXP, LENGTH(px))));
    setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, LENGTH(ix)+LENGTH(iy)+n)));
    UNPROTECT(2);

    fx = fy = n = INTEGER(pr)[0] = 0;
    for (i = 1; i < LENGTH(px); i++) {
	lx = INTEGER(px)[i];
	for (k = fx; k < lx; k++)
	    INTEGER(ir)[n++] = INTEGER(ix)[k];
	ly = INTEGER(py)[i];
	if (LOGICAL(R_s)[0] == TRUE)
	    INTEGER(ir)[n++] = nr;
	for (k = fy; k < ly; k++)
	    INTEGER(ir)[n++] = INTEGER(iy)[k];
	INTEGER(pr)[i] = n;
	fx = lx;
	fy = ly;
    }
    setAttrib(r, install("Dim"), PROTECT(ir = allocVector(INTSXP, 2)));
    UNPROTECT(1);
    INTEGER(ir)[0] = (LOGICAL(R_s)[0] == FALSE) ? nr : nr + 1;
    INTEGER(ir)[1] = LENGTH(pr)-1;

    setAttrib(r, install("Dimnames"), PROTECT(ir = allocVector(VECSXP, 2)));
    UNPROTECT(1);
    
    ix = getAttrib(x, install("Dimnames"));
    iy = getAttrib(y, install("Dimnames"));

    if (isNull((px = VECTOR_ELT(ix, 0))))
	px = VECTOR_ELT(iy, 0);
    if (isNull(px) || LOGICAL(R_s)[0] == FALSE)
	SET_VECTOR_ELT(ir, 0, px);
    else {
	SEXP s;

	SET_VECTOR_ELT(ir, 0, (s = allocVector(STRSXP, nr+1)));
	for (k = 0; k < nr; k++)
	    SET_STRING_ELT(s, k, VECTOR_ELT(px, k));
	SET_STRING_ELT(s, k, R_BlankString);
    }

    if (isNull((px = VECTOR_ELT(ix, 1))))
	SET_VECTOR_ELT(ir, 1, VECTOR_ELT(iy, 1));
    else
	SET_VECTOR_ELT(ir, 1, px);

    if (isNull((ix = getAttrib(ix, R_NamesSymbol))))
	setAttrib(ir, R_NamesSymbol, getAttrib(iy, R_NamesSymbol));
    else
	setAttrib(ir, R_NamesSymbol, ix);

    UNPROTECT(2);

    return r;
}

SEXP R_valid_sgCMatrix(SEXP x) {
    if (!inherits(x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    int i, k, f, l;
    SEXP px, ix, dx;

    px = getAttrib(x, install("p"));
    ix = getAttrib(x, install("i"));
    dx = getAttrib(x, install("Dim"));
    
    if (isNull(px) || isNull(ix) || isNull(dx))
	return mkString("slot p, i, or Dim is NULL");

    if (TYPEOF(px) != INTSXP || TYPEOF(ix) != INTSXP || TYPEOF(dx) != INTSXP)
	return mkString("slot p, i, or Dim not of storage type integer");

    if (LENGTH(dx) != 2 || INTEGER(dx)[0] < 0 || INTEGER(dx)[1] < 0)
	return mkString("slot Dim invalid");

    if (INTEGER(dx)[1] != LENGTH(px)-1)
	return mkString("slot p and Dim do not conform");

    f = l = INTEGER(px)[0];
    if (f != 0)
	return mkString("slot p invalid");

    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	if (l < f)
	    return mkString("slot p invalid");
	f = l;
    }
    if (l != LENGTH(ix))
	return mkString("slot p and i do not conform");

    if (l > 0) {
	f = l = INTEGER(ix)[0];
	for (i = 1; i < LENGTH(ix); i++) {
	    k = INTEGER(ix)[i];
	    if (k < f)
		f = k;
	    else
	    if (k > l)
		l = k;
	}
	if (f < 0 || l > INTEGER(dx)[0]-1)
	    return mkString("slot i invalid");
    }

    ix = getAttrib(x, install("Dimnames"));

    if (LENGTH(ix) != 2 || TYPEOF(ix) != VECSXP)
	return mkString("slot Dimnames invalid");

    px = VECTOR_ELT(ix, 0);
    if (!isNull(px)) {
	if (TYPEOF(px) != STRSXP)
	    return mkString("slot Dimnames invalid");
	if (LENGTH(px) != INTEGER(dx)[0])
	    return mkString("slot Dim and Dimnames do not conform");
    }

    px = VECTOR_ELT(ix, 1);
    if (!isNull(px)) {
	if (TYPEOF(px) != STRSXP)
	    return mkString("slot Dimnames invalid");
	if (LENGTH(px) != INTEGER(dx)[1])
	    return mkString("slot Dim and Dimnames do not conform");
    }

    return ScalarLogical(TRUE);
}

// first-order model (cross-tabulation)

SEXP R_firstOrder_sgCMatrix(SEXP x) {
    if (!inherits(x, "sgCMatrix") && 
	!inherits(x, "ngCMatrix"))
	error("'x' not of class sgCMatrix");
    SEXP r, px, ix;
    int i, f, k, l, i0, i1, nr;

    px = getAttrib(x, install("p"));
    ix = getAttrib(x, install("i"));

    nr = INTEGER(getAttrib(x, install("Dim")))[0]; 

    PROTECT(r = allocMatrix(INTSXP, nr, nr));
    memset(INTEGER(r), 0, sizeof(int) * nr * nr);

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	if (f == l)
	    continue;
	i0 = INTEGER(ix)[f];
	INTEGER(r)[i0 + i0 * nr]++;
	for (k = f+1; k < l; k++) {
	    i1 = INTEGER(ix)[k];
	    INTEGER(r)[i0 + i1 * nr]++;
	    INTEGER(r)[i1 + i1 * nr]++;
	    i0 = i1;
	}
	f = l;
    }

    ix = VECTOR_ELT(getAttrib(x, install("Dimnames")), 0);
    if (!isNull(ix)) {
	setAttrib(r, R_DimNamesSymbol, (px = allocVector(VECSXP, 2)));
	SET_VECTOR_ELT(px, 0, ix);
	SET_VECTOR_ELT(px, 1, ix);
	if (!isNull(ix = PROTECT(getAttrib(ix, R_NamesSymbol)))) {
	    SEXP t;
	    setAttrib(px, R_NamesSymbol, (t = allocVector(STRSXP, 2)));
	    // FIXME
	    SET_STRING_ELT(t, 0, STRING_ELT(ix, 0));
	    SET_STRING_ELT(t, 1, STRING_ELT(ix, 0));
	}
	UNPROTECT(1);
    }

    UNPROTECT(1);

    return r;
}

//
