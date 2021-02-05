/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2017  The R Core Team.
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
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
 *  EXPORTS	printVector()
 *		printNamedVector()
 *		printRealVector()
 *		printIntegerVector()
 *		printComplexVector()
 *
 *  See ./printutils.cpp	 for remarks on Printing and the Encoding utils.
 *  See ./format.cpp	 for the formatXXXX functions used below.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdarg>
#include <Print.h>
#include <rho/StringVector.hpp>

#include <numeric>

using namespace std;
using namespace rho;

#define DO_first_lab                     \
	if (indx)                            \
	{                                    \
		labwidth = Rf_IndexWidth(n) + 2; \
		/* labwidth may well be          \
		   one more than desired ..*/    \
		VectorIndex(1, labwidth);        \
		width = labwidth;                \
	}                                    \
	else                                 \
		width = 0

#define DO_newline                    \
	Rprintf("\n");                    \
	if (indx)                         \
	{                                 \
		VectorIndex(i + 1, labwidth); \
		width = labwidth;             \
	}                                 \
	else                              \
		width = 0

static
void printLogicalVector(const int *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatLogical(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%s", EncodeLogical(x[i], w));
	width += w;
    }
    Rprintf("\n");
}

HIDDEN
void printIntegerVector(const int *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatInteger(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%s", EncodeInteger(x[i], w));
	width += w;
    }
    Rprintf("\n");
}

// used in uncmin.c
HIDDEN
void printRealVector(const double *x, R_xlen_t n, int indx)
{
    int w, d, e, labwidth=0, width;

    DO_first_lab;
    formatReal(x, n, &w, &d, &e, 0);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%s", EncodeReal0(x[i], w, d, e, OutDec));
	width += w;
    }
    Rprintf("\n");
}

HIDDEN
void printComplexVector(const Rcomplex *x, R_xlen_t n, int indx)
{
    int w, wr, dr, er, wi, di, ei, labwidth=0, width;

    DO_first_lab;
    formatComplex(x, n, &wr, &dr, &er, &wi, &di, &ei, 0);

    w = wr + wi + 2;	/* +2 for "+" and "i" */
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	if (ISNA(x[i].r) || ISNA(x[i].i))
	    Rprintf("%s", EncodeReal0(R_NaReal, w, 0, 0, OutDec));
	else
	    Rprintf("%s", EncodeComplex(x[i], wr + R_print.gap , dr, er,
					wi, di, ei, OutDec));
	width += w;
    }
    Rprintf("\n");
}

static void printStringVector(const StringVector* sv, R_xlen_t n, int quote, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    StringVector::const_iterator beg = sv->begin();
    w = accumulate(beg, beg + n, 0, (quote ? stringWidthQuote : stringWidth));

    for (R_xlen_t i = 0; i < n; i++) {
	String* str = const_cast<String*>((*sv)[i].get());
	if (i > 0 && width + w + R_print.gap > R_print.width) {
	    DO_newline;
	}
	Rprintf("%*s%s", R_print.gap, "",
		Rf_EncodeString(str, w, quote, Rprt_adj(R_print.right)));
	width += w + R_print.gap;
    }
    Rprintf("\n");
}

static
void printRawVector(const Rbyte *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatRaw(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w > R_print.width) {
	    DO_newline;
	}
	Rprintf("%*s%s", R_print.gap, "", Rf_EncodeRaw(x[i], ""));
	width += w;
    }
    Rprintf("\n");
}

void printVector(SEXP x, int indx, int quote)
{
/* print R vector x[];	if(indx) print indices; if(quote) quote strings */
    R_xlen_t n;

    if ((n = XLENGTH(x)) != 0) {
	R_xlen_t n_pr = (n <= R_print.max +1) ? n : R_print.max;
	/* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printLogicalVector(LOGICAL_RO(x), n_pr, indx);
	    break;
	case INTSXP:
	    printIntegerVector(INTEGER_RO(x), n_pr, indx);
	    break;
	case REALSXP:
	    printRealVector(REAL_RO(x), n_pr, indx);
	    break;
	case STRSXP:
	    {
		const StringVector* sv = SEXP_downcast<StringVector*>(x);
		printStringVector(sv, n_pr, (quote ? '"' : 0), indx);
		break;
	    }
	case CPLXSXP:
	    printComplexVector(COMPLEX_RO(x), n_pr, indx);
	    break;
	case RAWSXP:
	    printRawVector(RAW_RO(x), n_pr, indx);
	    break;
	default:  // -Wswitch
	    break;
	}
	if(n_pr < n)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			n - n_pr);
    }
    else
#define PRINT_V_0						\
	switch (TYPEOF(x)) {					\
	case LGLSXP:	Rprintf("logical(0)\n");	break;	\
	case INTSXP:	Rprintf("integer(0)\n");	break;	\
	case REALSXP:	Rprintf("numeric(0)\n");	break;	\
	case CPLXSXP:	Rprintf("complex(0)\n");	break;	\
	case STRSXP:	Rprintf("character(0)\n");	break;	\
	case RAWSXP:	Rprintf("raw(0)\n");		break;	\
	default:                                        break;  \
	}
	PRINT_V_0;
}

#undef DO_first_lab
#undef DO_newline


/* The following code prints vectors which have every element named.

 * Primitives for each type of vector are presented first, followed
 * by the main (dispatching) function.
 * 1) These primitives are almost identical... ==> use PRINT_N_VECTOR macro
 * 2) S prints a _space_ in the first column for named vectors; we dont.
 */

#define PRINT_N_VECTOR(INI_FORMAT, PRINT_1)                                 \
	{                                                                       \
		int i, j, k, nlines, nperline, w, wn;                               \
		INI_FORMAT;                                                         \
                                                                            \
		{                                                                   \
			StringVector::const_iterator beg = names->begin();              \
			wn = accumulate(beg, beg + n, 0, stringWidth);                  \
		}                                                                   \
		if (w < wn)                                                         \
			w = wn;                                                         \
		nperline = R_print.width / (w + R_print.gap);                       \
		if (nperline <= 0)                                                  \
			nperline = 1;                                                   \
		nlines = n / nperline;                                              \
		if (n % nperline)                                                   \
			nlines += 1;                                                    \
                                                                            \
		for (i = 0; i < nlines; i++)                                        \
		{                                                                   \
			if (i)                                                          \
				Rprintf("\n");                                              \
			for (j = 0; j < nperline && (k = i * nperline + j) < n; j++)    \
				Rprintf("%s%*s",                                            \
						Rf_EncodeString((*names)[k], w, 0, Rprt_adj_right), \
						R_print.gap, "");                                   \
			Rprintf("\n");                                                  \
			for (j = 0; j < nperline && (k = i * nperline + j) < n; j++)    \
				PRINT_1;                                                    \
		}                                                                   \
		Rprintf("\n");                                                      \
	}

static void printNamedLogicalVector(const int * x, int n, const StringVector* names)
    PRINT_N_VECTOR(formatLogical(x, n, &w),
		   Rprintf("%s%*s", EncodeLogical(x[k],w), R_print.gap,""))

static void printNamedIntegerVector(const int * x, int n, const StringVector* names)
    PRINT_N_VECTOR(formatInteger(x, n, &w),
		   Rprintf("%s%*s", EncodeInteger(x[k],w), R_print.gap,""))

#undef INI_F_REAL
#define INI_F_REAL	int d, e; formatReal(x, n, &w, &d, &e, 0)

static void printNamedRealVector(const double * x, int n, const StringVector* names)
    PRINT_N_VECTOR(INI_F_REAL,
		   Rprintf("%s%*s",
			   EncodeReal0(x[k],w,d,e, OutDec),R_print.gap,""))

#undef INI_F_CPLX
#define INI_F_CPLX					\
    int wr, dr, er, wi, di, ei;				\
    formatComplex(x, n, &wr, &dr, &er, &wi, &di, &ei, 0);	\
    w = wr + wi + 2

#undef P_IMAG_NA
#define P_IMAG_NA				\
	    if(std::isnan(x[k].i))			\
		Rprintf("+%si", "NaN");		\
	    else

static void printNamedComplexVector(const Rcomplex * x, int n, const StringVector* names)
    PRINT_N_VECTOR(INI_F_CPLX,
	{ /* PRINT_1 */
	    if(j) Rprintf("%*s", R_print.gap, "");
	    if (ISNA(x[j].r) || ISNA(x[j].i)) {
		Rprintf("%s", EncodeReal0(R_NaReal, w, 0, 0, OutDec));
	    }
	    else {
		Rprintf("%s", EncodeReal0(x[k].r, wr, dr, er, OutDec));
		P_IMAG_NA
		if (x[k].i >= 0)
		    Rprintf("+%si", EncodeReal0(x[k].i, wi, di, ei, OutDec));
		else
		    Rprintf("-%si", EncodeReal0(-x[k].i, wi, di, ei, OutDec));
	    }
	})

#undef INI_F_STRING
#define INI_F_STRING                                          \
    StringVector::const_iterator beg = sv->begin();           \
    w = accumulate(beg, beg + n, 0,                           \
                   (quote ? stringWidthQuote : stringWidth));

static void printNamedStringVector(const StringVector* sv, int n, int quote,
				   const StringVector* names)
    PRINT_N_VECTOR(INI_F_STRING,
		   Rprintf("%s%*s",
			   Rf_EncodeString((*sv)[k], w, quote, Rprt_adj_right),
			   R_print.gap, ""))

static void printNamedRawVector(const Rbyte * x, int n, const StringVector* names)
    PRINT_N_VECTOR(formatRaw(x, n, &w),
		   Rprintf("%*s%s%*s", w - 2, "",
			   Rf_EncodeRaw(x[k], ""), R_print.gap,""))

HIDDEN
void printNamedVector(SEXP x, SEXP names, int quote, const char *title)
{
    int n;

    if (title != nullptr)
	 Rprintf("%s\n", title);

    StringVector* namesv = SEXP_downcast<StringVector*>(names);
    if ((n = LENGTH(x)) != 0) {
	int n_pr = (n <= R_print.max +1) ? n : R_print.max;
	/* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printNamedLogicalVector(LOGICAL_RO(x), n_pr, namesv);
	    break;
	case INTSXP:
	    printNamedIntegerVector(INTEGER_RO(x), n_pr, namesv);
	    break;
	case REALSXP:
	    printNamedRealVector(REAL_RO(x), n_pr, namesv);
	    break;
	case CPLXSXP:
	    printNamedComplexVector(COMPLEX_RO(x), n_pr, namesv);
	    break;
	case STRSXP:
	    {
		if(quote) quote = '"';
		StringVector* sv = SEXP_downcast<StringVector*>(x);
	        printNamedStringVector(sv, n_pr, quote, namesv);
		break;
	    }
	case RAWSXP:
	    printNamedRawVector(RAW_RO(x), n_pr, namesv);
	    break;
	default:  // -Wswitch
	    break;
	}
	if(n_pr < n)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			n - n_pr);

    }
    else {
	Rprintf("named ");
	PRINT_V_0;
    }
}
