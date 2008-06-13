
#include <R.h>
#include <Rinternals.h>

// supported by MinGW

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

// write the binary sequence data and configuration
// file directly from R (see TPOSE/makebin.cc and
// TPOSE/getconf.cc).
//
// as input we expect an ngCMatrix object with two
// integer attributes containing the sequence and
// event/time identifiers.
// 
// returns TRUE if data has been written and FALSE
// otherwise.
//
// NOTE
//
// the item identifiers are offset to start with one
// as this facilitates importing the mining results
// later.
//
// ceeboo 2008

#define __IBUF_SIZE 1024
static int ibuf[__IBUF_SIZE];	// integer buffer

SEXP R_makebin(SEXP x, SEXP R_file) {
    if (!x || !inherits(x, "ngCMatrix"))
	error("'x' not of type ngCMatrix");
    if (!R_file || isNull(R_file) || TYPEOF(R_file) != STRSXP)
	error("'file' not of type character");
    int f, h, i, k, l, n, nr, nc, ns;
    int fd, max_sid, min_sid;
    int off_it = 1;		// see note above
    char *file;
    SEXP px, ix, sx, ex;

    ix = getAttrib(x, install("Dim"));
    nr = INTEGER(ix)[0];	// number of items
    nc = INTEGER(ix)[1];	// number of transactions

    if (!nr || !nc)
	return ScalarLogical(FALSE);

    px = getAttrib(x, install("p"));
    ix = getAttrib(x, install("i"));

    sx = getAttrib(x, install("sid"));
    if (isNull(sx) || TYPEOF(sx) != INTSXP)
	error("attribute 'sid' not of type integer");
    if (LENGTH(sx) != nc)
	error("attribute 'sid' invalid length");

    ex = getAttrib(x, install("eid"));
    if (isNull(ex) || TYPEOF(ex) != INTSXP)
	error("attribute 'eid' not of type integer");
    if (LENGTH(ex) != nc)
	error("attribute 'eid' invalid length");

    // check if the sequence and event/time 
    // identifiers are in ascending order and
    // determine the number of sequences.
    ns = l = h = 0;
    for (i = 0; i < LENGTH(sx); i++) {
	if ((k = INTEGER(sx)[i]) < l)
	    error("'sid' invalid");
	if (k > l) {
	    l = k;
	    if (h == NA_INTEGER)
		error("'eid' invalid");
	    h = 0;
	    ns++;
	} else {
	    if ((f = INTEGER(ex)[i] <= h))
		error("'eid' invalid");
	    h = f;
	}
    }
    if (h == NA_INTEGER)
	error("'eid' invalid");
    if (l == NA_INTEGER)
	error("'sid' invalid");
    max_sid = l;
    min_sid = INTEGER(sx)[0];

    if (!LENGTH(R_file))
	error("'file' invalid length");
    R_file = STRING_ELT(R_file, 0);
    if (!LENGTH(R_file) || R_file == NA_STRING)
	error("'file' invalid");

    file = (char *) CHAR(PROTECT(allocVector(CHARSXP, LENGTH(R_file)+6)));

    // NOTE
    //
    // mmap might be more efficient but this is
    // not supported by MinGW.
    sprintf(file, "%s.data", CHAR(R_file));
    fd = open(file, (O_WRONLY|O_CREAT|O_TRUNC|O_APPEND), 0666);
    if (fd < 0)
	error("EOPEN %s", file);

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l - f;
	if (n == 0)
	    continue;
	if (n > __IBUF_SIZE - 3)
	    error("cannot copy data to buffer");
	k = i - 1;
	ibuf[0] = INTEGER(sx)[k];
	ibuf[1] = INTEGER(ex)[k];
	ibuf[2] = n;

	n = 3;
	for (k = f; k < l; k++)
	    ibuf[n++] = INTEGER(ix)[k] + off_it;

	if (write(fd, ibuf, sizeof(int) * n) / sizeof(int) != n) {
	    close(fd);
	    error("EWRITE %s", file);
	}
	f = l;
    }
    close(fd);

    // write the configuration file
    sprintf(file, "%s.conf", CHAR(R_file));
    fd = open(file, (O_WRONLY|O_CREAT|O_TRUNC),0666);
    if (fd < 0)
	error("EOPEN %s", file);

    {
	// FIXME
	//
	// the -a option is not implemented
	int   max_iid = nr + off_it;
	float avg_seq = (float) nc / ns,
	      avg_tr  = (float) LENGTH(ix) / nc;

	if (write(fd,&ns,      sizeof(int))   != sizeof(int)   ||
	    write(fd,&max_iid, sizeof(int))   != sizeof(int)   ||	
	    write(fd,&avg_seq, sizeof(float)) != sizeof(float) ||	
	    write(fd,&avg_tr,  sizeof(float)) != sizeof(float) ||	
	    write(fd,&nc,      sizeof(int))   != sizeof(int)   ||	
	    write(fd,&min_sid, sizeof(int))   != sizeof(int)   ||	
	    write(fd,&max_sid, sizeof(int))   != sizeof(int))
	{
	    close(fd);
	    error("EWRITE %s", file);
	}
    }
    close(fd);

    UNPROTECT(1);

    return ScalarLogical(TRUE);
}

//
