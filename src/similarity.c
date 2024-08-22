#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

// compute the similarity of two sets
// represented in column-sparse format.

static int *pe, *ie;

static int simset(int x, int y, int t) {
    if (x == y)
	return 1;
    if (!pe)
	return 0;
    int fx, lx, fy, ly, c;

    fx = pe[x];
    fy = pe[y];
    lx = pe[x+1];
    ly = pe[y+1];

    c = 0;
    for (x = fx, y = fy; x < lx && y < ly; )
	if (ie[x]  < ie[y])
	    x++;
	else {
	    if (ie[x] == ie[y]) {
		c++;
		x++;
	    }
	    y++;
	}
    if (c)
	switch (t) {
	case 0:					    // Jaccard
	    c /= (lx - fx + ly - fy - c);
	    break;
	case 1:					    // Dice
	    c = 2 * c / (lx - fx + ly - fy);
	    break;
	case 2:					    // Cosine
	    c /= sqrt(lx - fx) * sqrt(ly - fy);
	    break;
	case 3:					    // is subset
	    c = (c == (lx - fx));
	    break;
	}
    return c;
}

// compute the number or the sum of similarities of
// the elements in the longest common subsequence.
//
// the contribution of an insert or delete operation
// is zero. the similarity between two sets is scaled
// to the interval [-1,1]. thus, two sets are matched
// only if the scaled similarity is positive.
//
// c.f. edist_ow in package cba
//
// ceeboo 2008

static double esim_lcs(int *x, int *y, int nx, int ny, int t)
{
    int i, j, x0 = 0, y0 = 0;
    double z1 = 0, z2 = 0, s0 = 0, s2 = 0;
    double z0[ny+1];

    for (i = 0; i < nx+1; i++) {
	for (j = 0; j < ny+1; j++)
	    if (i == 0)
		z2 = z0[j] = 0;
	    else
	    if (j == 0) {
		x0 = x[i-1];
		z1 = z2 = 0;
	    }
	    else {
		y0 = y[j-1];
		s0 = z0[j];
		s2 = z0[j-1] + 2 * simset(x0, y0, t) - 1;
		z2 = (s0 > z1) ? s0 : z1;
		z2 = (z2 > s2) ? z2 : s2;
		z0[j-1] = z1;
		z1 = z2;
	    }
	z0[ny] = z2;
    }
    return z2;
}

// Compute auto- or cross-similarities over the columns
// of sgCMatrix objects. Currently, Jaccard, Dice, Cosine,
// and Subset similarities are implemented. Note that if
// two columns are all-zeros the similarity is set to one
// for clustering.
//
// This code avoids double computations and returns an
// object of class dsCMatrix (symmetric) or dgCMatrix.
//
// FIXME weighting of sequence elements and / or items
//       is not implemented.
//
// ceeboo 2008, 2016

SEXP R_similarity_sgCMatrix(SEXP x, SEXP y, SEXP R_e, SEXP R_method) {
    if (!x || isNull(x) || !inherits(x, "sgCMatrix"))
	error("'x' not of class sgCMatrix");
    if (!y || (!isNull(y) && !inherits(y, "sgCMatrix")))
	error("'y' not of class sgCMatrix");
    if (!R_e || (!isNull(R_e) && !inherits(R_e, "ngCMatrix")))
	error("'e' not of class ngCMatrix");
    if (!R_method || isNull(R_method) || TYPEOF(R_method) != INTSXP)
	error("'method' not of storage type integer");
    int i, j, fx, lx, fy, ly, n, m = 0, a = 0;
    double *zx, zy, z;
    SEXP r, pr, ir, xr, px, ix, py, iy;

    if (isNull(y)) {
	y = x;
	if (INTEGER(R_method)[0] == 3)
	    m = 1;
    } else
	m = 1;

    n = INTEGER(getAttrib(x, install("Dim")))[0];
    if (n != INTEGER(getAttrib(y, install("Dim")))[0])
	error("the number of rows of 'x' and 'y' does not conform");

    if (!isNull(R_e)) {
	if (INTEGER(getAttrib(R_e, install("Dim")))[1] != n)
	    error("the number of rows of 'x' and columns of 'e' do not conform");

	pe = INTEGER(getAttrib(R_e, install("p")));
	ie = INTEGER(getAttrib(R_e, install("i")));
    } else
	pe = 0;

    px = getAttrib(x, install("p"));
    ix = getAttrib(x, install("i"));

    py = getAttrib(y, install("p"));
    iy = getAttrib(y, install("i"));

    PROTECT(r = NEW_OBJECT(PROTECT(MAKE_CLASS((m) ? "dgCMatrix" : "dsCMatrix"))));

    if (!m) {
	setAttrib(r, install("uplo"), PROTECT(mkString("L")));
	UNPROTECT(1);
    }

    // FIXME can we bound the initial memory allocation
    //       to less than full storage representation?
    n = (m) ? (LENGTH(px)-1) * (LENGTH(py)-1)
	    : (LENGTH(px)-1) *  LENGTH(px) / 2;

    if (n > 1024) {
	n = LENGTH(px) + LENGTH(py);
	a = 1;
    }

    setAttrib(r, install("p"), PROTECT(pr = allocVector(INTSXP, LENGTH(py))));
    setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, n)));
    setAttrib(r, install("x"), PROTECT(xr = allocVector(REALSXP, n)));
    UNPROTECT(3);

    // precompute
    zx = REAL(PROTECT(allocVector(REALSXP, LENGTH(px))));
    fx = 0;
    for (i = 1; i < LENGTH(px); i++) {
	lx = INTEGER(px)[i];
	zx[i] = lx - fx;
	fx = lx;
    }

    fy = n = INTEGER(pr)[0] = 0;
    for (j = 1; j < LENGTH(py); j++) {

	// reallocate
	if (a &&
	    LENGTH(ir) - n < LENGTH(px)) {
	    SEXP t;

	    PROTECT(t = ir);
	    setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, LENGTH(ir) * 2)));
	    memcpy(INTEGER(ir), INTEGER(t), sizeof(int) * n);

	    UNPROTECT(2);

	    PROTECT(t = xr);
	    setAttrib(r, install("x"), PROTECT(xr = allocVector(REALSXP, LENGTH(ir))));
	    memcpy(REAL(xr), REAL(t), sizeof(double) * n);

	    UNPROTECT(2);
	}

	ly = INTEGER(py)[j];
	if (m) {
	    zy = ly - fy;
	    i = 1;
	} else {
	    zy = zx[j];
	    i = j + 1;
	    // set the diagonal
	    REAL(xr)[n] = 1;
	    INTEGER(ir)[n++] = j-1;
	}
	fx = INTEGER(px)[i-1];
	for (; i < LENGTH(px); i++) {
	    lx = INTEGER(px)[i];
	    if (!zx[i] && !zy) {	    // all-zeros
		REAL(xr)[n] = 1;
		INTEGER(ir)[n++] = i-1;
	    }
	    else {
		z = esim_lcs(INTEGER(ix)+fx, INTEGER(iy)+fy, lx-fx, ly-fy,
			     INTEGER(R_method)[0]);
		if (z) {
		    switch(INTEGER(R_method)[0]) {
		    case 0:			    // Jaccard
			z /= zx[i] + zy - z;
			break;
		    case 1:			    // Dice
			z = 2 * z / (zx[i] + zy);
			break;
		    case 2:			    // Cosine
			z /= sqrt(zx[i]) * sqrt(zy);
			break;
		    case 3:			    // Subset
			z = (zx[i] > z) ? 0 : z / zy;
			break;
		    // add further measures here!
		    default:
			error("type not implemented");
		    }
		    if (z) {
			REAL(xr)[n] = z;
			INTEGER(ir)[n++] = i-1;
		    }
		}
	    } 
	    fx = lx;
	}
	INTEGER(pr)[j] = n;
	fy = ly;
	R_CheckUserInterrupt();
    }

    UNPROTECT(1);

    if (n < LENGTH(ir)) {
	PROTECT(ix = ir);
	setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, n)));
	memcpy(INTEGER(ir), INTEGER(ix), sizeof(int) * n);

	UNPROTECT(2);

	PROTECT(ix = xr);
	setAttrib(r, install("x"), PROTECT(xr = allocVector(REALSXP, n)));
	memcpy(REAL(xr), REAL(ix), sizeof(double) * n);

	UNPROTECT(2);
    }

    ix = getAttrib(r, install("Dim"));

    INTEGER(ix)[0] = LENGTH(px)-1;
    INTEGER(ix)[1] = LENGTH(py)-1;

    ir = getAttrib(r, install("Dimnames"));

    ix = getAttrib(x, install("Dimnames"));
    SET_VECTOR_ELT(ir, 0, VECTOR_ELT(ix, 1));
    iy = getAttrib(y, install("Dimnames"));
    SET_VECTOR_ELT(ir, 1, VECTOR_ELT(iy, 1));

    ix = PROTECT(getAttrib(ix, R_NamesSymbol));
    iy = PROTECT(getAttrib(iy, R_NamesSymbol));

    if (!isNull(iy) || !isNull(ix)) {
        setAttrib(ir, R_NamesSymbol, (pr = allocVector(STRSXP, 2)));
        SET_STRING_ELT(pr, 0, isNull(ix) ? R_BlankString : STRING_ELT(ix, 1));
        SET_STRING_ELT(pr, 1, isNull(iy) ? R_BlankString : STRING_ELT(iy, 1));
    }
    
    UNPROTECT(4);

    return r;
}

// helper

SEXP R_as_dist_dsCMatrix(SEXP x) {
    if (!x || isNull(x) || !inherits(x, "dsCMatrix"))
	error("'x' not of class dsCMatrix");
    int i, j, k, f, l, n;
    SEXP r, px, ix, xx;

    ix = getAttrib(x, install("uplo"));
    if (isNull(ix) || strncmp(CHAR(STRING_ELT(ix, 0)), "L", 1) != 0)
	error("uplo invalid");

    px = getAttrib(x, install("p"));
    ix = getAttrib(x, install("i"));
    xx = getAttrib(x, install("x"));

    n = (LENGTH(px)-1) * (LENGTH(px)-2) / 2;

    r = PROTECT(allocVector(REALSXP, n));
    memset(REAL(r), 0, sizeof(double) * n);

    f = n = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	for (k = f; k < l; k++)
	    if ((j = INTEGER(ix)[k]) >= i)
		REAL(r)[j-i+n] = REAL(xx)[k];
	n += LENGTH(px)-i-1;
	f = l;
    }

    setAttrib(r, install("Size"), PROTECT(ScalarInteger(LENGTH(px)-1)));
    UNPROTECT(1);

    ix = getAttrib(x, install("Dimnames")); 
    if (!isNull((ix = VECTOR_ELT(ix, 0))))
	setAttrib(r, install("Labels"), ix);

    setAttrib(r, R_ClassSymbol, PROTECT(mkString("dist")));

    UNPROTECT(2);

    return r;
}

//
