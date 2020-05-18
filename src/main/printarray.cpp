/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000--2016	The R Core Team
 *  Copyright (C) 2001--2012	The R Foundation
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 *
 *  EXPORTS	printMatrix()
 *		printArray()
 *
 *  See ./printutils.cpp	 for general remarks on Printing
 *			 and the Encode.. utils.
 *
 *  See ./format.cpp	 for the  format_FOO_  functions used below.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <numeric>  // For accumulate.

#include <Defn.h>
#include <Localization.h>
#include <Print.h>

#include <numeric>
#include <stdlib.h> /* for div() */
#include "rho/GCStackRoot.hpp"
#include "rho/StringVector.hpp"

using namespace std;
using namespace rho;

/* We need display width of a string.
   Used only for row/column names found by GetMatrixDimnames,
   so in native encoding.  (NULL ones from do_prmatrix are skipped.)
*/
int Rstrwid(const char *str, int slen, cetype_t enc, int quote);  /* from printutils.cpp */
#define strwidth(x) Rstrwid(x, (int) strlen(x), CE_NATIVE, 0)

/* ceil_DIV(a,b) :=  ceil(a / b)  in _int_ arithmetic : */
static R_INLINE
int ceil_DIV(int a, int b)
{
    div_t div_res = div(a, b);
    return div_res.quot + ((div_res.rem != 0) ? 1 : 0);
}

/* moved from printutils.cpp */

static void MatrixColumnLabel(SEXP cl, int j, int w)
{
    if (!Rf_isNull(cl)) {
	SEXP tmp = STRING_ELT(cl, j);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	Rprintf("%*s%s", w-l, "",
		Rf_EncodeString(tmp, l, 0, Rprt_adj_left));
    }
    else {
	Rprintf("%*s[,%ld]", w-Rf_IndexWidth(j+1)-3, "", j+1);
    }
}

static void RightMatrixColumnLabel(SEXP cl, int j, int w)
{
    if (!Rf_isNull(cl)) {
	SEXP tmp = STRING_ELT(cl, j);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	/* This does not work correctly at least on FC3
	Rprintf("%*s", R_print.gap+w,
		Rf_EncodeString(tmp, l, 0, Rprt_adj_right)); */
	Rprintf("%*s%s", R_print.gap+w-l, "",
		Rf_EncodeString(tmp, l, 0, Rprt_adj_right));
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", j+1, w-Rf_IndexWidth(j+1)-3, "");
    }
}

static void LeftMatrixColumnLabel(SEXP cl, int j, int w)
{
    if (!Rf_isNull(cl)) {
	SEXP tmp = STRING_ELT(cl, j);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	Rprintf("%*s%s%*s", R_print.gap, "",
		Rf_EncodeString(tmp, l, 0, Rprt_adj_left), w-l, "");
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", j+1, w-Rf_IndexWidth(j+1)-3, "");
    }
}

static void MatrixRowLabel(SEXP rl, int i, int rlabw, int lbloff)
{
    if (!Rf_isNull(rl)) {
	SEXP tmp = STRING_ELT(rl, i);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	Rprintf("\n%*s%s%*s", lbloff, "",
		Rf_EncodeString(tmp, l, 0, Rprt_adj_left),
		rlabw-l-lbloff, "");
    }
    else {
	Rprintf("\n%*s[%ld,]", rlabw-3-Rf_IndexWidth(i + 1), "", i+1);
    }
}



/* This is the first (of 6)  print<TYPE>Matrix()  functions.
 * We define macros that will be re-used in the other functions,
 * and comment the common code here (only):
*/
static void printLogicalMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			       StringVector* rl, SEXP cl,
			       const char *rn, const char *cn,
			       Rboolean print_ij)
{
/* initialization; particularly of row labels, rl= dimnames(.)[[1]] and
 * rn = names(dimnames(.))[1] : */
#define _PRINT_INIT_rl_rn				\
    int *w = (int *) R_alloc(c, sizeof(int));		\
    int width, rlabw = -1, clabw = -1; /* -Wall */	\
    int i, j, jmin = 0, jmax = 0, lbloff = 0;		\
							\
    if (!Rf_isNull(rl)) {					\
	StringVector::const_iterator beg = rl->begin(); \
        rlabw = std::accumulate(beg, beg + r, 0, stringWidth);  \
    } else						\
	rlabw = Rf_IndexWidth(r + 1) + 3;			\
							\
    if (rn) {						\
	int rnw = strwidth(rn);				\
	if ( rnw < rlabw + R_MIN_LBLOFF )		\
	    lbloff = R_MIN_LBLOFF;			\
	else						\
	    lbloff = rnw - rlabw;			\
							\
	rlabw += lbloff;				\
    }

#   define _COMPUTE_W2_(_FORMAT_j_, _LAST_j_)				\
    /* compute w[j] = column-width of j(+1)-th column : */		\
    for (j = 0; j < c; j++) {						\
	if(print_ij) { _FORMAT_j_; } else w[j] = 0;			\
									\
	if (!Rf_isNull(cl)) {						\
	    const void *vmax = vmaxget();				\
	    if(STRING_ELT(cl, j) == NA_STRING)				\
		clabw = R_print.na_width_noquote;			\
	    else clabw = strwidth(Rf_translateChar(STRING_ELT(cl, j)));	\
	    vmaxset(vmax);						\
	} else								\
	    clabw = Rf_IndexWidth(j + 1) + 3;				\
									\
	if (w[j] < clabw)						\
	    w[j] = clabw;						\
	_LAST_j_;							\
    }

#   define _COMPUTE_W_(F_j) _COMPUTE_W2_(F_j, w[j] += R_print.gap)
    //                               _LAST_j  ------------------- for all but String

#   define _PRINT_ROW_LAB			\
						\
    if (cn != NULL)				\
	Rprintf("%*s%s\n", rlabw, "", cn);	\
    if (rn != NULL)				\
	Rprintf("%*s", -rlabw, rn);		\
    else					\
	Rprintf("%*s", rlabw, "")

#   define _PRINT_MATRIX_(_W_EXTRA_, DO_COLUMN_LABELS, ENCODE_I_J)	\
									\
    if (c == 0) {							\
	_PRINT_ROW_LAB;							\
	for (i = 0; i < r; i++)						\
	    MatrixRowLabel(rl, i, rlabw, lbloff);			\
	Rprintf("\n");							\
    }									\
    else while (jmin < c) {						\
	/* print columns  jmin:(jmax-1)	 where jmax has to be determined first */ \
									\
	width = rlabw;							\
	/* initially, jmax = jmin */					\
	do {								\
	    width += w[jmax] _W_EXTRA_;					\
	    jmax++;							\
	}								\
	while (jmax < c && width + w[jmax] _W_EXTRA_ < R_print.width);	\
									\
	_PRINT_ROW_LAB;							\
									\
	DO_COLUMN_LABELS;						\
									\
	for (i = 0; i < r_pr; i++) {					\
	    MatrixRowLabel(rl, i, rlabw, lbloff); /* starting with an "\n" */ \
	    if(print_ij) for (j = jmin; j < jmax; j++) {		\
		ENCODE_I_J;						\
	    }								\
	}								\
	Rprintf("\n");							\
	jmin = jmax;							\
    }

#   define STD_ColumnLabels			\
	for (j = jmin; j < jmax ; j++)		\
	    MatrixColumnLabel(cl, j, w[j])

    _PRINT_INIT_rl_rn;
    const int *x = LOGICAL_RO(sx) + offset;

    _COMPUTE_W_(formatLogical(&x[j * r], (R_xlen_t) r, &w[j]));

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%s", EncodeLogical(x[i + j * r], w[j])));

}

static void printIntegerMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			       StringVector* rl, SEXP cl, const char *rn,
			       const char *cn, Rboolean print_ij)
{
    _PRINT_INIT_rl_rn;
    const int *x = INTEGER_RO(sx) + offset;

    _COMPUTE_W_( formatInteger(&x[j * r], (R_xlen_t) r, &w[j]) );

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%s", EncodeInteger(x[i + j * r], w[j])));
}

static void printRealMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			    StringVector* rl, SEXP cl,
			    const char *rn, const char *cn,
			    Rboolean print_ij)
{
    _PRINT_INIT_rl_rn;
    const double *x = REAL_RO(sx) + offset;
    int *d = (int *) R_alloc(c, sizeof(int)),
	*e = (int *) R_alloc(c, sizeof(int));

    _COMPUTE_W_( formatReal(&x[j * r], (R_xlen_t) r, &w[j], &d[j], &e[j], 0) );

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%s", EncodeReal0(x[i + j * r], w[j], d[j], e[j], OutDec)) );
}

static void printComplexMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			       StringVector* rl, SEXP cl,
			       const char *rn, const char *cn,
			       Rboolean print_ij)
{
    _PRINT_INIT_rl_rn;
    const Rcomplex *x = COMPLEX_RO(sx) + offset;
    int *dr = (int *) R_alloc(c, sizeof(int)),
	*er = (int *) R_alloc(c, sizeof(int)),
	*wr = (int *) R_alloc(c, sizeof(int)),
	*di = (int *) R_alloc(c, sizeof(int)),
	*ei = (int *) R_alloc(c, sizeof(int)),
	*wi = (int *) R_alloc(c, sizeof(int));

    /* Determine the column widths */
    _COMPUTE_W_( formatComplex(&x[j * r], (R_xlen_t) r,
			       &wr[j], &dr[j], &er[j],
			       &wi[j], &di[j], &ei[j], 0);
		 w[j] = wr[j] + wi[j] + 2 );

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   if (ISNA(x[i + j * r].r) || ISNA(x[i + j * r].i))
		       Rprintf("%s", EncodeReal0(NA_REAL, w[j], 0, 0, OutDec));
		   else
		       /* Note that the label printing may modify w[j], so wr[j] is not 
		          necessarily still valid, and we use w[j] - wi[j] - 2  */
		       Rprintf("%s",
			       EncodeComplex(x[i + j * r],
					     w[j] - wi[j] - 2, dr[j], er[j],
					     wi[j], di[j], ei[j], OutDec)) )
}

static
void formatString(StringVector::const_iterator begin,
		  R_xlen_t n, int *fieldwidth, int quote)
{
    StringVector::const_iterator end = begin + n;
    int xmax = 0;
    int l;

    for (auto item = begin; item != end; ++item) {
	if (*item == NA_STRING) {
	    l = quote ? R_print.na_width : R_print.na_width_noquote;
	} else l = Rstrlen(*item, quote) + (quote ? 2 : 0);
	if (l > xmax) xmax = l;
    }
    *fieldwidth = xmax;
}

static void printStringMatrix(const StringVector* sx, int offset, int r_pr,
			      int r, int c, int quote, int right,
			      StringVector* rl, SEXP cl,
			      const char *rn, const char *cn, Rboolean print_ij)
{
    _PRINT_INIT_rl_rn;

    StringVector::const_iterator x = sx->begin() + offset;

    _COMPUTE_W2_( formatString(x + j * r, (R_xlen_t) r, &w[j], quote), );

    _PRINT_MATRIX_( + R_print.gap,
	           /* DO_COLUMN_LABELS = */
		   if (right) {
		       for (j = jmin; j < jmax ; j++)
			   RightMatrixColumnLabel(cl, j, w[j]);
		   }
		   else {
		       for (j = jmin; j < jmax ; j++)
			   LeftMatrixColumnLabel(cl, j, w[j]);
		   },
		   /* ENCODE_I = */
		   Rprintf("%*s%s", R_print.gap, "",
			   Rf_EncodeString(*(x + i + j * r),
					w[j], quote, Rprt_adj(right))) );
}

static void printRawMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			   StringVector* rl, SEXP cl,
			   const char *rn, const char *cn,
			   Rboolean print_ij)
{
    _PRINT_INIT_rl_rn;
    const Rbyte *x = RAW_RO(sx) + offset;

    _COMPUTE_W_( formatRaw(&x[j * r], (R_xlen_t) r, &w[j]) )

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%*s%s", w[j]-2, "", Rf_EncodeRaw(x[i + j * r], "")) );
}

/* rm and cn are found by GetMatrixDimnames so in native encoding */
attribute_hidden
void printMatrix(SEXP x, int offset, SEXP dim, int quote, int right,
		 SEXP rl, SEXP cl, const char *rn, const char *cn)
{
/* 'rl' and 'cl' are dimnames(.)[[1]] and dimnames(.)[[2]]  whereas
 * 'rn' and 'cn' are the  names(dimnames(.))
 */
    const void *vmax = vmaxget();
    const int *pdim = INTEGER_RO(dim);
    int r = pdim[0];
    int c = pdim[1], r_pr;
    /* PR#850 */
    if ((rl != nullptr) && (r > Rf_length(rl)))
	Rf_error(_("too few row labels"));
    if ((cl != nullptr) && (c > Rf_length(cl)))
	Rf_error(_("too few column labels"));
    if (r == 0 && c == 0) { // FIXME?  names(dimnames(.)) :
	Rprintf("<0 x 0 matrix>\n");
	return;
    }
    r_pr = r;
    StringVector* rlabs = SEXP_downcast<StringVector*>(rl);
    if(c > 0 && R_print.max / c < r) /* avoid integer overflow */
	/* using floor(), not ceil(), since 'c' could be huge: */
	r_pr = R_print.max / c;
    switch (TYPEOF(x)) {
    case LGLSXP:
	printLogicalMatrix(x, offset, r_pr, r, c, rlabs, cl, rn, cn, TRUE);
	break;
    case INTSXP:
	printIntegerMatrix(x, offset, r_pr, r, c, rlabs, cl, rn, cn, TRUE);
	break;
    case REALSXP:
	printRealMatrix	  (x, offset, r_pr, r, c, rlabs, cl, rn, cn, TRUE);
	break;
    case CPLXSXP:
	printComplexMatrix(x, offset, r_pr, r, c, rlabs, cl, rn, cn, TRUE);
	break;
    case STRSXP:
	{
	    if (quote) quote = '"';
	    const StringVector* sv = SEXP_downcast<StringVector*>(x);
	    printStringMatrix (sv, offset, r_pr, r, c, quote,
			       right, rlabs, cl, rn, cn, TRUE);
	    break;
	}
    case RAWSXP:
	printRawMatrix	  (x, offset, r_pr, r, c, rlabs, cl, rn, cn, TRUE);
	break;
    default:
	UNIMPLEMENTED_TYPE("printMatrix", x);
    }
#ifdef ENABLE_NLS
    if(r_pr < r) // number of formats must be consistent here
	Rprintf(n_(" [ reached getOption(\"max.print\") -- omitted %d row ]\n",
			 " [ reached getOption(\"max.print\") -- omitted %d rows ]\n",
			 r - r_pr),
		r - r_pr);
#else
    if(r_pr < r)
	Rprintf(" [ reached getOption(\"max.print\") -- omitted %d rows ]\n",
		r - r_pr);
#endif
    vmaxset(vmax);
}

attribute_hidden
void printArray(SEXP x, SEXP dim, int quote, int right, SEXP dimnames)
{
/* == printArray(.) */
    const void *vmax = vmaxget();
    int ndim = LENGTH(dim);
    const char *rn = NULL, *cn = NULL;

    if (ndim == 1)
	printVector(x, 1, quote);
    else if (ndim == 2) {
	SEXP rl, cl;
	Rf_GetMatrixDimnames(x, &rl, &cl, &rn, &cn);
	printMatrix(x, 0, dim, quote, 0, rl, cl, rn, cn);
    }
    else { /* ndim >= 3 */
	SEXP dn, dnn, dn1;
	StringVector* dn0;
	const int *dims = INTEGER_RO(dim);
	int i, j, nb, nb_pr, nr_last,
	    nr = dims[0], nc = dims[1],
	    b = nr * nc;
	bool max_reached, has_dimnames = (dimnames != nullptr),
	    has_dnn = has_dimnames;

	if (!has_dimnames) {
	    dn0 = nullptr;
	    dn1 = nullptr;
	    dnn = nullptr; /* -Wall */
	}
	else {
	    dn0 = SEXP_downcast<StringVector*>(VECTOR_ELT(dimnames, 0));
	    dn1 = VECTOR_ELT(dimnames, 1);
	    dnn = Rf_getAttrib(dimnames, Symbols::NamesSymbol);
	    has_dnn = !Rf_isNull(dnn);
	    if ( has_dnn ) {
		rn = (char *) Rf_translateChar(STRING_ELT(dnn, 0));
		cn = (char *) Rf_translateChar(STRING_ELT(dnn, 1));
	    }
	}
	/* nb := #{entries} in a slice such as x[1,1,..] or equivalently,
	 *       the number of matrix slices   x[ , , *, ..]  which
	 *       are printed as matrices -- if options("max.print") allows */
	for (i = 2, nb = 1; i < ndim; i++)
	    nb *= dims[i];
	max_reached = (b > 0 && R_print.max / b < nb);
	if (max_reached) { /* i.e., also  b > 0, nr > 0, nc > 0, nb > 0 */
	    /* nb_pr := the number of matrix slices to be printed */
	    nb_pr = ceil_DIV(R_print.max, b);
	    /* for the last, (nb_pr)th matrix slice, use only nr_last rows;
	     *  using floor(), not ceil(), since 'nc' could be huge: */
	    nr_last = (R_print.max - b * (nb_pr - 1)) / nc;
	    if(nr_last == 0) { nb_pr--; nr_last = nr; }
	} else {
	    nb_pr = (nb > 0) ? nb : 1; // do print *something* when dim = c(a,b,0)
	    nr_last = nr;
	}
	for (i = 0; i < nb_pr; i++) {
	    Rboolean do_ij = Rboolean(nb > 0);
	    Rboolean i_last = Rboolean(i == nb_pr - 1); /* for the last slice */
	    int use_nr = i_last ? nr_last : nr;
	    if(do_ij) {
		int k = 1;
		Rprintf(", ");
		for (j = 2 ; j < ndim; j++) {
		    int l = (i / k) % dims[j] + 1;
		    if (has_dimnames &&
			((dn = VECTOR_ELT(dimnames, j)) != nullptr)) {
			if ( has_dnn )
			    Rprintf(", %s = %s",
				    Rf_translateChar(STRING_ELT(dnn, j)),
				    Rf_translateChar(STRING_ELT(dn, l - 1)));
			else
			    Rprintf(", %s", Rf_translateChar(STRING_ELT(dn, l - 1)));
		    } else
			Rprintf(", %d", l);
		    k *= dims[j];
		}
		Rprintf("\n\n");
	    } else { // nb == 0 -- e.g. <2 x 3 x 0 array of logical>
		for (i = 0; i < ndim; i++)
		    Rprintf("%s%d", (i == 0) ? "<" : " x ", dims[i]);
		Rprintf(" array of %s>\n", R_CHAR(Rf_type2str_nowarn(TYPEOF(x))));
	    }
	    switch (TYPEOF(x)) {
	    case LGLSXP:
		printLogicalMatrix(x, i * b, use_nr, nr, nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case INTSXP:
		printIntegerMatrix(x, i * b, use_nr, nr, nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case REALSXP:
		printRealMatrix   (x, i * b, use_nr, nr, nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case CPLXSXP:
		printComplexMatrix(x, i * b, use_nr, nr, nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case STRSXP:
	    {
		if (quote) quote = '"';
		const StringVector* sv = SEXP_downcast<StringVector*>(x);
		printStringMatrix (sv, i * b, use_nr, nr, nc,
				   quote, right, dn0, dn1, rn, cn, do_ij);
		break;
	    }
	    case RAWSXP:
		printRawMatrix    (x, i * b, use_nr, nr, nc, dn0, dn1, rn, cn, do_ij);
		break;
		default: Rf_error(_("invalid type")); break;
	    }
	    Rprintf("\n");
	}

	if(max_reached && nb_pr < nb) {
	    Rprintf(" [ reached getOption(\"max.print\") -- omitted");
	    if(nr_last < nr) Rprintf(" %d row(s) and", nr - nr_last);
	    Rprintf(" %d matrix slice(s) ]\n", nb - nb_pr);
	}
    }
    vmaxset(vmax);
}
