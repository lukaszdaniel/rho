/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 2017  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2016  The R Core Team
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
 */

/* This is currently restricted to vectors of length < 2^30 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include "basedecl.h"
#include <rho/ArgMatcher.hpp>
#include <rho/ClosureContext.hpp>
#include <rho/DottedArgs.hpp>
#include <rho/Promise.hpp>
#include <rho/RAllocStack.hpp>
#include <rho/BuiltInFunction.hpp>

using namespace rho;

#define NIL -1

/* interval at which to check interrupts */
// constexpr R_xlen_t NINTERRUPT = 1000000;


/* Hash function and equality test for keys */
typedef struct _HashData HashData;

struct _HashData {
    int K;
    R_xlen_t M;
    R_xlen_t nmax;
#ifdef LONG_VECTOR_SUPPORT
    Rboolean isLong;
#endif
    size_t (*hash)(SEXP, R_xlen_t, HashData *);
    bool (*equal)(SEXP, R_xlen_t, SEXP, R_xlen_t);
    SEXP HashTable;

    int nomatch;
    Rboolean useUTF8;
    Rboolean useCache;
};

#define HTDATA_INT(d) (INTEGER0((d)->HashTable))
#define HTDATA_DBL(d) (REAL0((d)->HashTable))


/*
   Integer keys are hashed via a random number generator
   based on Knuth's recommendations.  The high order K bits
   are used as the hash code.

   NB: lots of this code relies on M being a power of two and
   on silent integer overflow mod 2^32.

   <FIXME> Integer keys are wasteful for logical and raw vectors, but
   the tables are small in that case.  It would be much easier to
   implement long vectors, though.
*/

/*  Currently the hash table is implemented as a (signed) integer
    array.  So there are two 31-bit restrictions, the length of the
    array and the values.  The values are initially NIL (-1).  O-based
    indices are inserted by isDuplicated, and invalidated by setting
    to R_NaInt.
*/

static size_t scatter(unsigned int key, HashData *d)
{
    return 3141592653U * key >> (32 - d->K);
}

static size_t lhash(SEXP x, R_xlen_t indx, HashData *d)
{
    int xi = LOGICAL_ELT(x, indx);
    if (xi == R_NaLog) return 2U;
    return size_t(xi);
}

R_INLINE static size_t ihash(SEXP x, R_xlen_t indx, HashData *d)
{
    int xi = INTEGER_ELT(x, indx);
    if (xi == R_NaInt) return 0;
    return scatter(static_cast<unsigned int>(xi), d);
}

/* We use unions here because Solaris gcc -O2 has trouble with
   casting + incrementing pointers.  We use tests here, but R currently
   assumes int is 4 bytes and double is 8 bytes.
 */
union foo {
    double d;
    unsigned int u[2];
};

R_INLINE static size_t rhash(SEXP x, R_xlen_t indx, HashData *d)
{
    /* There is a problem with signed 0s under IEC60559 */
    double xi = REAL_ELT(x, indx);
    double tmp = (xi == 0.0) ? 0.0 : xi;
    /* need to use both 32-byte chunks or endianness is an issue */
    /* we want all NaNs except NA equal, and all NAs equal */
    if (R_IsNA(tmp)) tmp = R_NaReal;
    else if (R_IsNaN(tmp)) tmp = R_NaN;
#if 2*SIZEOF_INT == SIZEOF_DOUBLE
    {
	union foo tmpu;
	tmpu.d = tmp;
	return scatter(tmpu.u[0] + tmpu.u[1], d);
    }
#else
    return scatter(*((unsigned int *) (&tmp)), d);
#endif
}

static Rcomplex unify_complex_na(Rcomplex z) {
    Rcomplex ans;
    ans.r = (z.r == 0.0) ? 0.0 : z.r;
    ans.i = (z.i == 0.0) ? 0.0 : z.i;
    if (R_IsNA(ans.r) || R_IsNA(ans.i))
	ans.r = ans.i = R_NaReal;
    else if (R_IsNaN(ans.r) || R_IsNaN(ans.i))
	ans.r = ans.i = R_NaN;
    return ans;
}

static size_t chash(SEXP x, R_xlen_t indx, HashData *d)
{
    Rcomplex tmp = unify_complex_na(COMPLEX_ELT(x, indx));

#if 2*SIZEOF_INT == SIZEOF_DOUBLE
    {
	unsigned int u;
	union foo tmpu;
	tmpu.d = tmp.r;
	u = tmpu.u[0] ^ tmpu.u[1];
	tmpu.d = tmp.i;
	u ^= tmpu.u[0] ^ tmpu.u[1];
	return scatter(u, d);
    }
#else
	return scatter((*((unsigned int *)(&tmp.r)) ^
			(*((unsigned int *)(&tmp.i)))), d);
#endif
}

/* Hash CHARSXP by address.  Hash values are int, For 64bit pointers,
 * we do (upper ^ lower) */
R_INLINE static size_t cshash(SEXP x, R_xlen_t indx, HashData *d)
{
    intptr_t z = intptr_t( STRING_ELT(x, indx));
    unsigned int z1 = static_cast<unsigned int>((z & 0xffffffff)), z2 = 0;
#if SIZEOF_LONG == 8
    z2 = static_cast<unsigned int>(z/0x100000000L);
#endif
    return scatter(z1 ^ z2, d);
}

R_INLINE static size_t shash(SEXP x, R_xlen_t indx, HashData *d)
{
    unsigned int k;
    const char *p;
    if(!d->useUTF8 && d->useCache) return cshash(x, indx, d);
    const void *vmax = vmaxget();
    /* Not having d->useCache really should not happen anymore. */
    p = Rf_translateCharUTF8(STRING_ELT(x, indx));
    k = 0;
    while (*p++)
	k = 11 * k + static_cast<unsigned int>(*p); /* was 8 but 11 isn't a power of 2 */
    vmaxset(vmax); /* discard any memory used by translateChar */
    return scatter(k, d);
}

inline static bool lequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return (LOGICAL_ELT(x, i) == LOGICAL_ELT(y, j));
}


inline static bool iequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return (INTEGER_ELT(x, i) == INTEGER_ELT(y, j));
}

/* BDR 2002-1-17  We don't want NA and other NaNs to be equal */
inline static bool requal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return 0;
    double xi = REAL_ELT(x, i);
    double yj = REAL_ELT(y, j);
    if (!std::isnan(xi) && !std::isnan(yj))
	return (xi == yj);
    else if (R_IsNA(xi) && R_IsNA(yj)) return true;
    else if (R_IsNaN(xi) && R_IsNaN(yj)) return true;
    else return false;
}

/* This is differentiating {NA,1}, {NA,0}, {NA, NaN}, {NA, NA},
 * but R's print() and format()  render all as "NA" */
inline static bool cplx_eq(const Rcomplex &x, const Rcomplex &y)
{
    if (!std::isnan(x.r) && !std::isnan(x.i) && !std::isnan(y.r) && !std::isnan(y.i))
	return x.r == y.r && x.i == y.i;
    else if (R_IsNA(x.r) || R_IsNA(x.i)) // x is NA
	return (R_IsNA(y.r) || R_IsNA(y.i)) ? true : false;
    else if (R_IsNA(y.r) || R_IsNA(y.i)) // y is NA but x is not
	return false;
    // else : none is NA but there's at least one NaN;  hence  std::isnan(.) == R_IsNaN(.)
    return
	(((std::isnan(x.r) && std::isnan(y.r)) || (!std::isnan(x.r) && !std::isnan(y.r) && x.r == y.r)) && // Re
	 ((std::isnan(x.i) && std::isnan(y.i)) || (!std::isnan(x.i) && !std::isnan(y.i) && x.i == y.i))    // Im
	    ) ? true : false;
}

inline static bool cequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return cplx_eq(COMPLEX_ELT(x, i), COMPLEX_ELT(y, j));
}

inline static bool sequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    SEXP xi = STRING_ELT(x, i);
    SEXP yj = STRING_ELT(y, j);
    /* Two strings which have the same address must be the same,
       so avoid looking at the contents */
    if (xi == yj) return true;
    /* Then if either is NA the other cannot be */
    /* Once all CHARSXPs are cached, Seql will handle this */
    if (xi == R_NaString || yj == R_NaString)
	return false;
    /* another pre-test to avoid the call to Seql */
    if (/*IS_CACHED(xi) && IS_CACHED(yj) && */ ENC_KNOWN(xi) == ENC_KNOWN(yj))
	return false;
    return bool(Rf_Seql(xi, yj));
}

static size_t rawhash(SEXP x, R_xlen_t indx, HashData *d)
{
    return size_t(RAW_ELT(x, indx));
}

inline static bool rawequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return (RAW_ELT(x, i) == RAW_ELT(y, j));
}

static size_t vhash(SEXP x, R_xlen_t indx, HashData *d)
{
    int i;
    unsigned int key;
    SEXP _this = VECTOR_ELT(x, indx);

    key = OBJECT(_this) + 2*TYPEOF(_this) + 100U*static_cast<unsigned int>(Rf_length(_this));
    /* maybe we should also look at attributes, but that slows us down */
    switch (TYPEOF(_this)) {
    case LGLSXP:
	/* This is not too clever: pack into 32-bits and then scatter? */
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= lhash(_this, i, d);
	    key *= 97;
	}
	break;
    case INTSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= ihash(_this, i, d);
	    key *= 97;
	}
	break;
    case REALSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= rhash(_this, i, d);
	    key *= 97;
	}
	break;
    case CPLXSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= chash(_this, i, d);
	    key *= 97;
	}
	break;
    case STRSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= shash(_this, i, d);
	    key *= 97;
	}
	break;
    case RAWSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= scatter(static_cast<unsigned int>(rawhash(_this, i, d)), d);
	    key *= 97;
	}
	break;
    case VECSXP:
	for(i = 0; i < LENGTH(_this); i++) {
	    key ^= vhash(_this, i, d);
	    key *= 97;
	}
	break;
    default:
	break;
    }
    return scatter(key, d);
}

inline static bool vequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return R_compute_identical(VECTOR_ELT(x, i), VECTOR_ELT(y, j), 0);
}

/*
  Choose M to be the smallest power of 2
  not less than 2*n and set K = log2(M).
  Need K >= 1 and hence M >= 2, and 2^M <= 2^31 -1, hence n <= 2^29.

  Dec 2004: modified from 4*n to 2*n, since in the worst case we have
  a 50% full table, and that is still rather efficient -- see
  R. Sedgewick (1998) Algorithms in C++ 3rd edition p.606.
*/
static void MKsetup(R_xlen_t n, HashData *d, R_xlen_t nmax)
{
#ifdef LONG_VECTOR_SUPPORT
    /* M = 2^32 is safe, hence n <= 2^31 -1 */
    if(n < 0) /* protect against overflow to -ve */
	Rf_error(_("length %d is too large for hashing"), n);
#else
    if(n < 0 || n >= 1073741824) /* protect against overflow to -ve */
	Rf_error(_("length %d is too large for hashing"), n);
#endif

    if (nmax != R_NaInt && nmax != 1) n = nmax;
    R_xlen_t n2 = 2U * n;
    d->M = 2;
    d->K = 1;
    while (d->M < n2) {
	d->M *= 2;
	d->K++;
    }
    d->nmax = n;
}

#define IMAX 4294967296L
static void HashTableSetup(SEXP x, HashData *d, R_xlen_t nmax)
{
    d->useUTF8 = FALSE;
    d->useCache = TRUE;
    switch (TYPEOF(x)) {
    case LGLSXP:
	d->hash = lhash;
	d->equal = lequal;
	d->nmax = d->M = 4;
	d->K = 2; /* unused */
	break;
    case INTSXP:
    {
	d->hash = ihash;
	d->equal = iequal;
#ifdef LONG_VECTOR_SUPPORT
	R_xlen_t nn = XLENGTH(x);
	if (nn > IMAX) nn = IMAX;
	MKsetup(nn, d, nmax);
#else
	MKsetup(LENGTH(x), d, nmax);
#endif
    }
	break;
    case REALSXP:
	d->hash = rhash;
	d->equal = requal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case CPLXSXP:
	d->hash = chash;
	d->equal = cequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case STRSXP:
	d->hash = shash;
	d->equal = sequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case RAWSXP:
	d->hash = rawhash;
	d->equal = rawequal;
	d->nmax = d->M = 256;
	d->K = 8; /* unused */
	break;
    case VECSXP:
	d->hash = vhash;
	d->equal = vequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    default:
	UNIMPLEMENTED_TYPE("HashTableSetup", x);
    }
#ifdef LONG_VECTOR_SUPPORT
    d->isLong = Rboolean(IS_LONG_VEC(x));
    if (d->isLong) {
	d->HashTable = Rf_allocVector(REALSXP, d->M);
	for (R_xlen_t i = 0; i < d->M; i++) HTDATA_DBL(d)[i] = NIL;
    } else
#endif
    {
	d->HashTable = Rf_allocVector(INTSXP, d->M);
	for (R_xlen_t i = 0; i < d->M; i++) HTDATA_INT(d)[i] = NIL;
    }
}

/* Open address hashing */
/* Collision resolution is by linear probing */
/* The table is guaranteed large so this is sufficient */

static int isDuplicated(SEXP x, R_xlen_t indx, HashData *d)
{
#ifdef LONG_VECTOR_SUPPORT
    if (d->isLong) {
	double *h = HTDATA_DBL(d);
	size_t i = d->hash(x, indx, d);
	while (h[i] != NIL) {
	    if (d->equal(x, R_xlen_t(h[i]), x, indx))
		return h[i] >= 0 ? 1 : 0;
	    i = (i + 1) % d->M;
	}
	if (d->nmax-- < 0) Rf_error("hash table is full");
	h[i] = double(indx);
    } else 
#endif
    {
	int *h = HTDATA_INT(d);
	size_t i = d->hash(x, indx, d);
	while (h[i] != NIL) {
	    if (d->equal(x, h[i], x, indx))
		return h[i] >= 0 ? 1 : 0;
	    i = (i + 1) % d->M;
	}
	if (d->nmax-- < 0) Rf_error("hash table is full");
	h[i] = int(indx);
    }
    return 0;
}

static void removeEntry(SEXP table, SEXP x, R_xlen_t indx, HashData *d)
{
#ifdef LONG_VECTOR_SUPPORT
    if (d->isLong) {
	double *h = HTDATA_DBL(d);
	size_t i = d->hash(x, indx, d);
	while (h[i] >= 0) {
	    if (d->equal(table, R_xlen_t(h[i]), x, indx)) {
		h[i] = R_NaInt;  /* < 0, only index values are inserted */
		return;
	    }
	    i = (i + 1) % d->M;
	}
    } else
#endif
    {
	int *h = HTDATA_INT(d);
	size_t i = d->hash(x, indx, d);
	while (h[i] >= 0) {
	    if (d->equal(table, h[i], x, indx)) {
		h[i] = R_NaInt;  /* < 0, only index values are inserted */
		return;
	    }
	    i = (i + 1) % d->M;
	}
    }
}

#define DUPLICATED_INIT						\
    HashData data;						\
    HashTableSetup(x, &data, nmax);				\
    if(TYPEOF(x) == STRSXP) {					\
	data.useUTF8 = FALSE; data.useCache = TRUE;		\
	for(i = 0; i < n; i++) {				\
	    if(IS_BYTES(STRING_ELT(x, i))) {			\
		data.useUTF8 = FALSE; break;			\
	    }							\
	    if(ENC_KNOWN(STRING_ELT(x, i))) {			\
		data.useUTF8 = TRUE;				\
	    }							\
	}							\
    }

/* used in scan() */
SEXP Rf_duplicated(SEXP x, Rboolean from_last)
{
    SEXP ans;
    int *v, nmax = R_NaInt;

    if (!Rf_isVector(x)) Rf_error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    UNPROTECT(2);
    return ans;
}

static SEXP Duplicated(SEXP x, Rboolean from_last, int nmax)
{
    SEXP ans;
    int *v;

    if (!Rf_isVector(x)) Rf_error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    UNPROTECT(2);
    return ans;
}

/* simpler version of the above : return 1-based index of first, or 0 : */
R_xlen_t Rf_any_duplicated(SEXP x, Rboolean from_last)
{
    R_xlen_t result = 0;
    int nmax = R_NaInt;

    if (!Rf_isVector(x)) Rf_error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);

    DUPLICATED_INIT;
    PROTECT(data.HashTable);

    if(from_last) {
	for (i = n-1; i >= 0; i--) {
	    if(isDuplicated(x, i, &data)) { result = ++i; break; }
	}
    } else {
	for (i = 0; i < n; i++) {
	    if(isDuplicated(x, i, &data)) { result = ++i; break; }
	}
    }
    UNPROTECT(1);
    return result;
}

static SEXP duplicated3(SEXP x, SEXP incomp, Rboolean from_last, int nmax)
{
    SEXP ans;
    int *v, j, m;

    if (!Rf_isVector(x)) Rf_error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    if(Rf_length(incomp)) {
	PROTECT(incomp = Rf_coerceVector(incomp, TYPEOF(x)));
	m = Rf_length(incomp);
	for (i = 0; i < n; i++)
	    if(v[i]) {
		for(j = 0; j < m; j++)
		    if(data.equal(x, i, incomp, j)) {v[i] = 0; break;}
	    }
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return ans;
}

/* return (1-based) index of first duplication, or 0 : */
R_xlen_t Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last)
{
    int j, m = Rf_length(incomp), nmax = R_NaInt;

    if (!Rf_isVector(x)) Rf_error(_("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;
    PROTECT(data.HashTable);

    if(!m) Rf_error(_("Rf_any_duplicated3(., <0-length incomp>)"));

    PROTECT(incomp = Rf_coerceVector(incomp, TYPEOF(x)));
    m = Rf_length(incomp);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
#define IS_DUPLICATED_CHECK				\
	    if(isDuplicated(x, i, &data)) {		\
		Rboolean isDup = TRUE;			\
		for(j = 0; j < m; j++)			\
		    if(data.equal(x, i, incomp, j)) {	\
			isDup = FALSE; break;		\
		    }					\
		if(isDup) {				\
		    UNPROTECT(2);			\
		    return ++i;				\
		}					\
		/* else continue */			\
	    }
	    IS_DUPLICATED_CHECK;
	}
    else {
	for (i = 0; i < n; i++) {
	    IS_DUPLICATED_CHECK;
	}
    }

    UNPROTECT(2);
    return 0;
}

#undef IS_DUPLICATED_CHECK
#undef DUPLICATED_INIT


/* .Internal(duplicated   (x, incomparables, fromLast, nmax))	  [op=0]
   .Internal(unique       (x, incomparables, fromLast, nmax))	  [op=1]
   .Internal(anyDuplicated(x, incomparables, fromLast))		  [op=2]
*/
HIDDEN SEXP do_duplicated(/*const*/ Expression* call, const BuiltInFunction* op, int num_args, ...)
{
    SEXP dup, ans;
    int fromLast, nmax = R_NaInt;
    R_xlen_t i, k, n;

    va_list args;
    va_start(args, num_args);
    SEXP x = NEXT_ARG;
    SEXP incomp = NEXT_ARG;
    SEXP fromLast_ = NEXT_ARG;
    if (op->variant() <= 1) {
	nmax = Rf_asInteger(NEXT_ARG);
    }
    va_end(args);

    if (Rf_length(fromLast_) < 1)
	Rf_error(_("'fromLast' must be length 1"));
    fromLast = Rf_asLogical(fromLast_);
    if (fromLast == R_NaLog)
	Rf_error(_("'fromLast' must be TRUE or FALSE"));

    Rboolean fL = Rboolean( fromLast);

    /* handle zero length vectors, and NULL */
    if ((n = Rf_xlength(x)) == 0)
	return(op->variant() <= 1
	       ? Rf_allocVector(op->variant() != 1 ? LGLSXP : TYPEOF(x), 0)
	       : Rf_ScalarInteger(0));

    if (!Rf_isVector(x)) {
	Rf_error(_("%s() applies only to vectors"),
	      (op->variant() == 0 ? "duplicated" :
	       (op->variant() == 1 ? "unique" : /* 2 */ "anyDuplicated")));
    }
    if (op->variant() <= 1) {
	if (nmax != R_NaInt && nmax <= 0)
	    Rf_error(_("'nmax' must be positive"));
    }

    if(Rf_length(incomp) && /* S has FALSE to mean empty */
       !(Rf_isLogical(incomp) && Rf_length(incomp) == 1 &&
	 LOGICAL_ELT(incomp, 0) == 0)) {
	if(op->variant() == 2) {
	    /* return R's 1-based index :*/
	    R_xlen_t ind  = Rf_any_duplicated3(x, incomp, fL);
	    if(ind > INT_MAX) return Rf_ScalarReal((double) ind);
	    else return Rf_ScalarInteger((int)ind);
	} else
	    dup = duplicated3(x, incomp, fL, nmax);
    }
    else {
	if(op->variant() == 2) {
	    R_xlen_t ind  = Rf_any_duplicated(x, fL);
	    if(ind > INT_MAX) return Rf_ScalarReal((double) ind);
	    else return Rf_ScalarInteger((int)ind);
	} else
	    dup = Duplicated(x, fL, nmax);
    }
    if (op->variant() == 0) /* "Rf_duplicated()" */
	return dup;
    /*	ELSE
	use the results of "duplicated" to get "unique" */

    /* count unique entries */
    k = 0;
    for (i = 0; i < n; i++)
	if (LOGICAL_ELT(dup, i) == 0)
	    k++;

    PROTECT(dup);
    PROTECT(ans = Rf_allocVector(TYPEOF(x), k));

    k = 0;
    switch (TYPEOF(x)) {
    case LGLSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		LOGICAL0(ans)[k++] = Rboolean(LOGICAL_ELT(x, i));
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		INTEGER0(ans)[k++] = INTEGER_ELT(x, i);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		REAL0(ans)[k++] = REAL_ELT(x, i);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		COMPLEX0(ans)[k++] = COMPLEX_ELT(x, i);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		SET_STRING_ELT(ans, k++, STRING_ELT(x, i));
	break;
    case VECSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		SET_VECTOR_ELT(ans, k++, VECTOR_ELT(x, i));
	break;
    case RAWSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		RAW0(ans)[k++] = RAW_ELT(x, i);
	break;
    default:
	UNIMPLEMENTED_TYPE("duplicated", x);
    }
    UNPROTECT(2);
    return ans;
}

/* Build a hash table, ignoring information on duplication */
static void DoHashing(SEXP table, HashData *d)
{
    R_xlen_t i, n = XLENGTH(table);
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	(void) isDuplicated(table, i, d);
    }
}

/* invalidate entries: normally few */
static void UndoHashing(SEXP x, SEXP table, HashData *d)
{
    for (R_xlen_t i = 0; i < XLENGTH(x); i++) removeEntry(table, x, i, d);
}

#define DEFLOOKUP(NAME, HASHFUN, EQLFUN)                                       \
    R_INLINE static int NAME(SEXP table, SEXP x, R_xlen_t indx, HashData* d)   \
    {                                                                          \
	int* h = HTDATA_INT(d);                                                \
	size_t i = HASHFUN(x, indx, d);                                        \
	while (h[i] != NIL) {                                                  \
	    if (EQLFUN(table, h[i], x, indx))                                  \
		return h[i] >= 0 ? h[i] + 1 : d->nomatch;                      \
	    i = (i + 1) % d->M;                                                \
	}                                                                      \
	return d->nomatch;                                                     \
    }

/* definitions to help the C compiler to inline of most important cases */
DEFLOOKUP(iLookup, ihash, iequal)
DEFLOOKUP(rLookup, rhash, requal)
DEFLOOKUP(sLookup, shash, sequal)

/* definition for the general case */
DEFLOOKUP(Lookup, d->hash, d->equal)

/* Now do the table lookup */
static SEXP HashLookup(SEXP table, SEXP x, HashData *d)
{
    SEXP ans;
    R_xlen_t i, n;

    n = XLENGTH(x);
    PROTECT(ans = Rf_allocVector(INTSXP, n));
    int *pa = INTEGER0(ans);

    switch (TYPEOF(x)) {
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = iLookup(table, x, i, d);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = rLookup(table, x, i, d);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    pa[i] = sLookup(table, x, i, d);
	break;
    default:
	for (i = 0; i < n; i++)
	    pa[i] = Lookup(table, x, i, d);
    }

    UNPROTECT(1);
    return ans;
}

static SEXP match_transform(SEXP s, SEXP env)
{
    if(OBJECT(s)) {
	if(Rf_inherits(s, "factor")) return Rf_asCharacterFactor(s);
	else if(Rf_inherits(s, "POSIXlt")) { /* and maybe more classes in the future:
					   * Call R's (generic)	 as.character(s) : */
	    SEXP call, r;
	    PROTECT(call = Rf_lang2(Symbols::AsCharacterSymbol, s));
	    r = Rf_eval(call, env);
	    UNPROTECT(1);
	    return r;
	}
    }
    /* else */
    return Rf_duplicate(s);
}

// workhorse of R's match() and hence also  " ix %in% itable "
extern "C"  // Used by the fastmatch package.
SEXP match5(SEXP itable, SEXP ix, int nmatch, SEXP incomp, SEXP env)
{
    SEXP ans, x, table;
    SEXPTYPE type;
    HashData data;

    R_xlen_t n = Rf_xlength(ix);

    /* handle zero length arguments */
    if (n == 0) return Rf_allocVector(INTSXP, 0);
    if (Rf_length(itable) == 0) {
	ans = Rf_allocVector(INTSXP, n);
	int *pa = INTEGER0(ans);
	for (R_xlen_t i = 0; i < n; i++) pa[i] = nmatch;
	return ans;
    }

    int nprot = 0;
    PROTECT(x	  = match_transform(ix,	    env)); nprot++;
    PROTECT(table = match_transform(itable, env)); nprot++;
    /* or should we use PROTECT_WITH_INDEX and REPROTECT below ? */

    /* Coerce to a common type; type == NILSXP is ok here.
     * Note that above we coerce factors and "POSIXlt", only to character.
     * Hence, coerce to character or to `higher' type
     * (given that we have "Vector" or NULL) */
    if(TYPEOF(x) >= STRSXP || TYPEOF(table) >= STRSXP) type = STRSXP;
    else type = TYPEOF(x) < TYPEOF(table) ? TYPEOF(table) : TYPEOF(x);
    PROTECT(x	  = Rf_coerceVector(x,	type)); nprot++;
    PROTECT(table = Rf_coerceVector(table, type)); nprot++;

    // special case scalar x -- for speed only :
    if(XLENGTH(x) == 1 && !incomp) {
      int val = nmatch;
      int nitable = LENGTH(itable);
      switch (type) {
      case STRSXP: {
	  SEXP x_val = STRING_ELT(x,0);
	  for (int i=0; i < nitable; i++) if (Rf_Seql(STRING_ELT(table,i), x_val)) {
		  val = i + 1; break;
	      }
	  break; }
      case LGLSXP:
      case INTSXP: {
	  int x_val = INTEGER_ELT(x, 0),
	      *table_p = INTEGER(table);
	  for (int i=0; i < nitable; i++) if (table_p[i] == x_val) {
		  val = i + 1; break;
	      }
	  break; }
      case REALSXP: {
	  double xv = REAL_ELT(x, 0);
	  // pblm with signed 0s under IEC60559
	  double x_val = (xv == 0.) ? 0. : xv;
	  double *table_p = REAL(table);
	  /* we want all NaNs except NA equal, and all NAs equal */
	  if (R_IsNA(x_val)) {
	      for (int i=0; i < nitable; i++) if (R_IsNA(table_p[i])) {
		      val = i + 1; break;
		  }
	  }
	  else if (R_IsNaN(x_val)) {
	      for (int i=0; i < nitable; i++) if (R_IsNaN(table_p[i])) {
		      val = i + 1; break;
		  }
	  }
	  else {
	      for (int i=0; i < nitable; i++) if (table_p[i] == x_val) {
		      val = i + 1; break;
	      }
	  }
	  break; }
      case CPLXSXP: {
	  Rcomplex x_val = COMPLEX_ELT(x, 0),
	      *table_p = COMPLEX(table);
	  for (int i=0; i < nitable; i++)
	      if (cplx_eq(table_p[i], x_val)) {
		  val = i + 1; break;
	      }
	  break; }
      case RAWSXP: {
	  Rbyte x_val = RAW_ELT(x, 0),
	      *table_p = RAW(table);
	  for (int i=0; i < nitable; i++) if (table_p[i] == x_val) {
		  val = i + 1; break;
	      }
	  break; }
	default: Rf_error(_("invalid type")); break;
      }
      PROTECT(ans = Rf_ScalarInteger(val)); nprot++;
    }
    else { // regular case

	if (incomp) { PROTECT(incomp = Rf_coerceVector(incomp, type)); nprot++; }
	data.nomatch = nmatch;
	HashTableSetup(table, &data, R_NaInt);
	if(type == STRSXP) {
	    Rboolean useBytes = FALSE;
	    Rboolean useUTF8 = FALSE;
	    Rboolean useCache = TRUE;
	    for(R_xlen_t i = 0; i < Rf_length(x); i++) {
		SEXP s = STRING_ELT(x, i);
		if(IS_BYTES(s)) {
		    useBytes = TRUE;
		    useUTF8 = FALSE;
		    break;
		}
		if(ENC_KNOWN(s)) {
		    useUTF8 = TRUE;
		}
	    }
	    if(!useBytes || useCache) {
		for(int i = 0; i < Rf_length(table); i++) {
		    SEXP s = STRING_ELT(table, i);
		    if(IS_BYTES(s)) {
			useBytes = TRUE;
			useUTF8 = FALSE;
			break;
		    }
		    if(ENC_KNOWN(s)) {
			useUTF8 = TRUE;
		    }
		}
	    }
	    data.useUTF8 = useUTF8;
	    data.useCache = useCache;
	}
	PROTECT(data.HashTable); nprot++;
	DoHashing(table, &data);
	if (incomp) UndoHashing(incomp, table, &data);
	ans = HashLookup(table, x, &data);
    }
    UNPROTECT(nprot);
    return ans;
} // end{ match5 }

SEXP Rf_matchE(SEXP itable, SEXP ix, int nmatch, SEXP env)
{
    return match5(itable, ix, nmatch, nullptr, env);
}

/* used from other code, not here: */
SEXP Rf_match(SEXP itable, SEXP ix, int nmatch)
{
    return match5(itable, ix, nmatch, nullptr, R_BaseEnv);
}


// .Internal(match(x, table, nomatch, incomparables)) :
HIDDEN SEXP do_match(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x, RObject* table, RObject* nomatch_, RObject* incomparables)
{
    if ((!Rf_isVector(x) && !Rf_isNull(x))
	|| (!Rf_isVector(table) && !Rf_isNull(table)))
	Rf_error(_("'match' requires vector arguments"));

    int nomatch = Rf_asInteger(nomatch_);

    if (Rf_isNull(incomparables) || /* S has FALSE to mean empty */
	(Rf_length(incomparables) == 1 && Rf_isLogical(incomparables) &&
	 LOGICAL_ELT(incomparables, 0) == 0))
	return match5(table, x, nomatch, NULL, R_BaseEnv);
    else
	return match5(table, x, nomatch, incomparables, R_BaseEnv);
}

/* pmatch and charmatch return integer positions, so cannot be used
   for long vector tables */

/* Partial Matching of Strings */
/* Fully S Compatible version. */

/* Hmm, this was not all S compatible!	The desired behaviour is:
 * First do exact matches, and mark elements as used as they are matched
 *   unless dup_ok is true.
 * Then do partial matching, from left to right, using up the table
 *   unless dup_ok is true.  Multiple partial matches are ignored.
 * Empty strings are unmatched			      BDR 2000/2/16
 */

HIDDEN SEXP do_pmatch(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* table_, RObject* nomatch_, RObject* duplicates_ok_)
{
    SEXP ans, input, target;
    int mtch, n_target, mtch_count, dups_ok, no_match;
    size_t temp;
    int *used = nullptr, *ians;
    const char **in, **tar;
    Rboolean no_dups;
    Rboolean useBytes = FALSE, useUTF8 = FALSE;

    input = x_;
    R_xlen_t n_input = XLENGTH(input);
    target = table_;
    n_target = LENGTH(target); // not allowed to be long
    no_match = Rf_asInteger(nomatch_);
    dups_ok = Rf_asLogical(duplicates_ok_);
    if (dups_ok == R_NaLog)
	Rf_error(_("invalid '%s' argument"), "duplicates.ok");
    no_dups = Rboolean(!dups_ok);

    if (!Rf_isString(input) || !Rf_isString(target))
	Rf_error(_("argument is not of mode character"));

    if(no_dups) {
	used = static_cast<int *>(RHO_alloc(size_t(n_target), sizeof(int)));
	for (int j = 0; j < n_target; j++) used[j] = 0;
    }

    for(R_xlen_t i = 0; i < n_input; i++) {
	if(IS_BYTES(STRING_ELT(input, i))) {
	    useBytes = TRUE;
	    useUTF8 = FALSE;
	    break;
	} else if(ENC_KNOWN(STRING_ELT(input, i))) {
	    useUTF8 = TRUE;
	}
    }
    if(!useBytes) {
	for(R_xlen_t i = 0; i < n_target; i++) {
	    if(IS_BYTES(STRING_ELT(target, i))) {
		useBytes = TRUE;
		useUTF8 = FALSE;
		break;
	    } else if(ENC_KNOWN(STRING_ELT(target, i))) {
		useUTF8 = TRUE;
	    }
	}
    }

    in = static_cast<const char **>(RHO_alloc(size_t(n_input), sizeof(char *)));
    tar = static_cast<const char **>(RHO_alloc(size_t(n_target), sizeof(char *)));
    PROTECT(ans = Rf_allocVector(INTSXP, n_input));
    ians = INTEGER0(ans);
    if(useBytes) {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = R_CHAR(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = R_CHAR(STRING_ELT(target, j));
    }
    else if(useUTF8) {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = Rf_translateCharUTF8(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = Rf_translateCharUTF8(STRING_ELT(target, j));
    } else {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = Rf_translateChar(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = Rf_translateChar(STRING_ELT(target, j));
    }
    /* First pass, exact matching */
    R_xlen_t nexact = 0;
    /* Compromise when hashing used changed in 3.2.0 (PR#15697) */
    if (n_input <= 100 || n_target <= 100) {
	for (R_xlen_t i = 0; i < n_input; i++) {
	    const char *ss = in[i];
	    if (strlen(ss) == 0) continue;
	    for (int j = 0; j < n_target; j++) {
		if (no_dups && used[j]) continue;
		if (streql(ss, tar[j])) {
		    ians[i] = j + 1;
		    if (no_dups) used[j] = 1;
		    nexact++;
		    break;
		}
	    }
	}
    } else {
	HashData data;
	HashTableSetup(target, &data, R_NaInt);
	data.useUTF8 = useUTF8;
	data.nomatch = 0;
	DoHashing(target, &data);
	for (R_xlen_t i = 0; i < n_input; i++) {
	    if (strlen(in[i]) == 0) /* don't look up "" */
		continue;
	    int j = Lookup(target, input, i, &data);
	    if ((j == 0) || (no_dups && used[j - 1])) continue;
	    if (no_dups) used[j - 1] = 1;
	    ians[i] = j;
	    nexact++;
	}
    }

    if(nexact < n_input) {
	/* Second pass, partial matching */
	for (R_xlen_t i = 0; i < n_input; i++) {
	    const char *ss;
	    if (ians[i]) continue;
	    ss = in[i];
	    temp = strlen(ss);
	    if (temp == 0) continue;
	    mtch = 0;
	    mtch_count = 0;
	    for (int j = 0; j < n_target; j++) {
		if (no_dups && used[j]) continue;
		if (streqln(ss, tar[j], temp)) {
		    mtch = j + 1;
		    mtch_count++;
		}
	    }
	    if (mtch > 0 && mtch_count == 1) {
		if(no_dups) used[mtch - 1] = 1;
		ians[i] = mtch;
	    }
	}
	/* Third pass, set no matches */
	for (R_xlen_t i = 0; i < n_input; i++)
	    if(ians[i] == 0) ians[i] = no_match;

    }
    UNPROTECT(1);
    return ans;
}


/* Partial Matching of Strings */
/* Based on Therneau's charmatch. */

HIDDEN SEXP do_charmatch(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* table_, RObject* nomatch_)
{
    SEXP ans, input, target;
    const char *ss, *st;
    Rboolean useBytes = FALSE, useUTF8 = FALSE;

    input = x_;
    R_xlen_t n_input = LENGTH(input);
    target = table_;
    int n_target = LENGTH(target);

    if (!Rf_isString(input) || !Rf_isString(target))
	Rf_error(_("argument is not of mode character"));
    int no_match = Rf_asInteger(nomatch_);

    for(R_xlen_t i = 0; i < n_input; i++) {
	if(IS_BYTES(STRING_ELT(input, i))) {
	    useBytes = TRUE;
	    useUTF8 = FALSE;
	    break;
	} else if(ENC_KNOWN(STRING_ELT(input, i))) {
	    useUTF8 = TRUE;
	}
    }
    if(!useBytes) {
	for(int i = 0; i < n_target; i++) {
	    if(IS_BYTES(STRING_ELT(target, i))) {
		useBytes = TRUE;
		useUTF8 = FALSE;
		break;
	    } else if(ENC_KNOWN(STRING_ELT(target, i))) {
		useUTF8 = TRUE;
	    }
	}
    }

    PROTECT(ans = Rf_allocVector(INTSXP, n_input));
    int *ians = INTEGER0(ans);

    const void *vmax = vmaxget();  // prudence: .Internal does this too.
    for(R_xlen_t i = 0; i < n_input; i++) {
	if(useBytes)
	    ss = R_CHAR(STRING_ELT(input, i));
	else if(useUTF8)
	    ss = Rf_translateCharUTF8(STRING_ELT(input, i));
	else
	    ss = Rf_translateChar(STRING_ELT(input, i));
	size_t temp = strlen(ss);
	int imatch = R_NaInt;
	Rboolean perfect = FALSE;
	/* we could reset vmax here too: worth it? */
	for(int j = 0; j < n_target; j++) {
	    if(useBytes)
		st = R_CHAR(STRING_ELT(target, j));
	    else if(useUTF8)
		st = Rf_translateCharUTF8(STRING_ELT(target, j));
	    else
		st = Rf_translateChar(STRING_ELT(target, j));
	    int k = strncmp(ss, st, temp);
	    if (k == 0) {
		if (strlen(st) == temp) {
		    if (perfect)
			imatch = 0;
		    else {
			perfect = TRUE;
			imatch = j + 1;
		    }
		}
		else if (!perfect) {
		    if (imatch == R_NaInt)
			imatch = j + 1;
		    else
			imatch = 0;
		}
	    }
	}
	ians[i] = (imatch == R_NaInt) ? no_match : imatch;
	vmaxset(vmax);
    }
    UNPROTECT(1);
    return ans;
}


/* Functions for matching the supplied arguments to the */
/* formal arguments of functions.  The returned value */
/* is a list with all components named. */

static SEXP StripUnmatched(SEXP s)
{
    if (s == nullptr) return s;

    if (CAR(s) == R_MissingArg) {
	return StripUnmatched(CDR(s));
    }
    else if (CAR(s) == Symbols::DotsSymbol ) {
	return StripUnmatched(CDR(s));
    }
    else {
	SETCDR(s, StripUnmatched(CDR(s)));
	return s;
    }
}

static SEXP ExpandDots(SEXP s, int expdots)
{
    SEXP r;
    // The call to ConsCell::convert below will allocate memory:
    GCStackRoot<> sr(s);
    if (s == nullptr)
	return s;
    if (TYPEOF(CAR(s)) == DOTSXP ) {
	// Convert CAR(s) to a PairList:
	{
	    ConsCell* cc = SEXP_downcast<ConsCell*>(CAR(s));
	    SETCAR(s, ConsCell::convert<PairList>(cc));
	}
	if (expdots) {
	    r = CAR(s);
	    while (CDR(r) != nullptr ) {
		r = CDR(r);
	    }
	    SETCDR(r, ExpandDots(CDR(s), expdots));
	    return CAR(s);
	}
    }
    SETCDR(s, ExpandDots(CDR(s), expdots));
    return s;
}
static SEXP subDots(SEXP rho)
{
    SEXP rval, dots, a, b, t;
    int len,i;

    dots = Rf_findVar(Symbols::DotsSymbol, rho);

    if (dots == R_UnboundValue)
	Rf_error(_("... used in a situation where it does not exist"));

    if (dots == R_MissingArg)
	return dots;

    if (!Rf_isPairList(dots))
	Rf_error(_("... is not a pairlist"));

    len = Rf_length(dots);
    PROTECT(rval=Rf_allocList(len));
    for(a = dots, b = rval, i = 1; i <= len; a = CDR(a), b = CDR(b), i++) {
	SET_TAG(b, TAG(a));
	t = CAR(a);
	while (TYPEOF(t) == PROMSXP)
	    t = PREXPR(t);
	if( Rf_isSymbol(t) || Rf_isLanguage(t) )
	    SETCAR(b, Symbol::obtainDotDotSymbol(i));
	else
	    SETCAR(b, t);
    }
    UNPROTECT(1);
    return rval;
}


HIDDEN SEXP do_matchcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP formals, actuals, rlist;
    SEXP funcall, f, b, rval, sysp, t1, t2, tail;
    int expdots;

    funcall = CADR(args);

    if (TYPEOF(funcall) == EXPRSXP)
	funcall = VECTOR_ELT(funcall, 0);

    if (TYPEOF(funcall) != LANGSXP)
	Rf_error(_("invalid '%s' argument"), "call");

    b = CAR(args);
    if (TYPEOF(b) != CLOSXP)
	Rf_error(_("invalid '%s' argument"), "definition");

    sysp = CAR(CDDDR(args));
    if (!Rf_isEnvironment(sysp))
	Rf_error(_("'envir' must be an environment"));

    /* Do we expand ... ? */

    expdots = Rf_asLogical(CAR(CDDR(args)));
    if (expdots == R_NaLog)
	Rf_error(_("invalid '%s' argument"), "expand.dots");

    /* Get the formals and match the actual args */

    formals = FORMALS(b);
    PROTECT(actuals = Rf_duplicate(CDR(funcall)));

    /* If there is a ... symbol then expand it out in the sysp env
       We need to take some care since the ... might be in the middle
       of the actuals  */

    t2 = R_MissingArg;
    for (t1=actuals ; t1!=nullptr ; t1 = CDR(t1) ) {
	if (CAR(t1) == Symbols::DotsSymbol) {
	    t2 = subDots(sysp);
	    break;
	}
    }
    /* now to splice t2 into the correct spot in actuals */
    if (t2 != R_MissingArg ) {	/* so we did something above */
	if( CAR(actuals) == Symbols::DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = Rf_listAppend(t2, CDR(actuals));
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=nullptr; t1=CDR(t1)) {
		if( CADR(t1) == Symbols::DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, t2);
		    Rf_listAppend(actuals,tail);
		    break;
		}
	    }
	}
    } else { /* get rid of it */
	if( CAR(actuals) == Symbols::DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = CDR(actuals);
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=nullptr; t1=CDR(t1)) {
		if( CADR(t1) == Symbols::DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, tail);
		    break;
		}
	    }
	}
    }
    ArgList actuals_list(SEXP_downcast<PairList*>(actuals), ArgList::RAW);
    ArgMatcher* matcher = new ArgMatcher(SEXP_downcast<PairList*>(formals));
    rlist = matcher->matchToPairList(actuals_list,
                                     SEXP_downcast<Expression*>(call));

    /* Attach the argument names as tags */

    for (f = formals, b = rlist; b != nullptr; b = CDR(b), f = CDR(f)) {
	SET_TAG(b, TAG(f));
    }


    /* Handle the dots */

    PROTECT(rlist = ExpandDots(rlist, expdots));

    /* Eliminate any unmatched formals and any that match R_DotSymbol */
    /* This needs to be after ExpandDots as the DOTSXP might match ... */

    rlist = StripUnmatched(rlist);

    PROTECT(rval = new Expression);
    SETCAR(rval, Rf_duplicate(CAR(funcall)));
    SETCDR(rval, rlist);
    UNPROTECT(3);
    return rval;
}


#include <R_ext/RS.h> /* for Memzero */

#ifdef _AIX  /*some people just have to be different: is this still needed? */
#    include <memory.h>
#endif

static SEXP
rowsum(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP rn)
{
    SEXP matches,ans;
    int n, p, ng, narm;
    R_xlen_t offset = 0, offsetg = 0;
    HashData data;
    data.nomatch = 0;

    n = LENGTH(g);
    ng = Rf_length(uniqueg);
    narm = Rf_asLogical(snarm);
    if(narm == R_NaLog) Rf_error("'na.rm' must be TRUE or FALSE");
    if(Rf_isMatrix(x)) p = Rf_ncols(x); else p = 1;

    HashTableSetup(uniqueg, &data, R_NaInt);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));
    int *pmatches = INTEGER(matches);

    PROTECT(ans = Rf_allocMatrix(TYPEOF(x), ng, p));

    switch(TYPEOF(x)){
    case REALSXP:
	Memzero(REAL0(ans), ng*p);
	for(int i = 0; i < p; i++) {
	    double *pa = REAL0(ans);
	    for(int j = 0; j < n; j++) {
		double xjpo = REAL_ELT(x, j + offset);
		if(!narm || !std::isnan(xjpo))
		    pa[pmatches[j] - 1 + offsetg] += xjpo;
	    }
	    offset += n;
	    offsetg += ng;
	}
	break;
    case INTSXP:
	Memzero(INTEGER0(ans), ng*p);
	for(int i = 0; i < p; i++) {
	    int *pa = INTEGER0(ans);
	    for(int j = 0; j < n; j++) {
		int xjpo = INTEGER_ELT(x, j + offset);
		if (xjpo == R_NaInt) {
		    if(!narm)
			pa[pmatches[j] - 1 + offsetg] = R_NaInt;
		} else if (pa[pmatches[j] - 1 + offsetg] != R_NaInt) {
		    /* check for integer overflows */
		    int itmp = pa[pmatches[j] - 1 + offsetg];
		    double dtmp = itmp;
		    dtmp += xjpo;
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = R_NaInt;
		    else itmp += xjpo;
		    pa[pmatches[j] - 1 + offsetg] = itmp;
		}
	    }
	    offset += n;
	    offsetg += ng;
	}
	break;
    default:
	Rf_error(_("non-numeric matrix in rowsum(): this should not happen"));
    }

    if (TYPEOF(rn) != STRSXP) Rf_error("row names are not character");
    SEXP dn = Rf_allocVector(VECSXP, 2), dn2, dn3;
    Rf_setAttrib(ans, Symbols::DimNamesSymbol, dn);
    SET_VECTOR_ELT(dn, 0, rn);
    dn2 = Rf_getAttrib(x, Symbols::DimNamesSymbol);
    if(Rf_length(dn2) >= 2 &&
       !Rf_isNull(dn3 = VECTOR_ELT(dn2, 1))) SET_VECTOR_ELT(dn, 1, dn3);

    UNPROTECT(3); /* HashTable, matches, ans */
    return ans;
}

static SEXP
rowsum_df(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP rn)
{
    SEXP matches,ans,col,xcol;
    int p, narm;
    HashData data;
    data.nomatch = 0;

    R_xlen_t n = XLENGTH(g);
    p = LENGTH(x);
    R_xlen_t ng = XLENGTH(uniqueg);
    narm = Rf_asLogical(snarm);
    if(narm == R_NaLog) Rf_error("'na.rm' must be TRUE or FALSE");

    HashTableSetup(uniqueg, &data, R_NaInt);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));
    int *pmatches = INTEGER(matches);

    PROTECT(ans = Rf_allocVector(VECSXP, p));

    for(int i = 0; i < p; i++) {
	xcol = VECTOR_ELT(x,i);
	if (!Rf_isNumeric(xcol))
	    Rf_error(_("non-numeric data frame in rowsum"));
	switch(TYPEOF(xcol)){
	case REALSXP:
	    PROTECT(col = Rf_allocVector(REALSXP,ng));
	    Memzero(REAL0(col), ng);
	    for(R_xlen_t j = 0; j < n; j++) {
		double xj = REAL_ELT(xcol, j);
		if(!narm || !std::isnan(xj))
		    REAL0(col)[pmatches[j] - 1] += xj;
	    }
	    SET_VECTOR_ELT(ans,i,col);
	    UNPROTECT(1);
	    break;
	case INTSXP:
	    PROTECT(col = Rf_allocVector(INTSXP, ng));
	    Memzero(INTEGER0(col), ng);
	    for(R_xlen_t j = 0; j < n; j++) {
		int xj = INTEGER_ELT(xcol, j);
		if (xj == R_NaInt) {
		    if(!narm)
			INTEGER0(col)[pmatches[j] - 1] = R_NaInt;
		} else if (INTEGER0(col)[pmatches[j] - 1] != R_NaInt) {
		    int itmp = INTEGER0(col)[pmatches[j] - 1];
		    double dtmp = itmp;
		    dtmp += xj;
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = R_NaInt;
		    else itmp += xj;
		    INTEGER0(col)[pmatches[j] - 1] = itmp;
		}
	    }
	    SET_VECTOR_ELT(ans, i, col);
	    UNPROTECT(1);
	    break;

	default:
	    Rf_error(_("this cannot happen"));
	}
    }
    Rf_namesgets(ans, Rf_getAttrib(x, Symbols::NamesSymbol));

    if (TYPEOF(rn) != STRSXP) Rf_error("row names are not character");
    Rf_setAttrib(ans, Symbols::RowNamesSymbol, rn);
    Rf_classgets(ans, Rf_mkString("data.frame"));

    UNPROTECT(3); /* HashTable, matches, ans */
    return ans;
}

HIDDEN SEXP do_rowsum(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* group_, RObject* unique_groups_, RObject* na_rm_, RObject* unique_group_names_)
{
    if(op->variant() == 1)
	return rowsum_df(x_, group_, unique_groups_, na_rm_,
			 unique_group_names_);
    else
	return rowsum(x_, group_, unique_groups_, na_rm_, 
		      unique_group_names_);
}


/* returns 1-based duplicate no */
static int isDuplicated2(SEXP x, int indx, HashData *d)
{
    int *h = HTDATA_INT(d);
    size_t i = d->hash(x, indx, d);
    while (h[i] != NIL) {
	if (d->equal(x, h[i], x, indx))
	    return h[i] + 1;
	i = (i + 1) % d->M;
    }
    h[i] = indx;
    return 0;
}

static SEXP duplicated2(SEXP x, HashData *d)
{
    SEXP ans;
    int n;

    n = LENGTH(x);
    HashTableSetup(x, d, R_NaInt);
    PROTECT(d->HashTable);
    PROTECT(ans = Rf_allocVector(INTSXP, n));

    int *h = HTDATA_INT(d);
    int *v = INTEGER0(ans);
    for (R_xlen_t i = 0; i < d->M; i++) h[i] = NIL;
    for (int i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	v[i] = isDuplicated2(x, i, d);
    }
    UNPROTECT(2);
    return ans;
}

HIDDEN SEXP do_makeunique(/*const*/ Expression* call, const BuiltInFunction* op, RObject* names_, RObject* sep_)
{
    SEXP names, sep, ans, dup, newx;
    int i, cnt, *cnts, dp;
    int n, len, maxlen = 0;
    HashData data;
    const char *csep, *ss;
    const void *vmax;

    names = names_;
    if(!Rf_isString(names))
	Rf_error(_("'names' must be a character vector"));
    n = LENGTH(names);
    sep = sep_;
    if(!Rf_isString(sep) || LENGTH(sep) != 1)
	Rf_error(_("'%s' must be a character string"), "sep");
    csep = Rf_translateChar(STRING_ELT(sep, 0));
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    vmax = vmaxget();
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(ans, i, STRING_ELT(names, i));
	len = int(strlen(Rf_translateChar(STRING_ELT(names, i))));
	if(len > maxlen) maxlen = len;
	vmaxset(vmax);
    }
    if(n > 1) {
	/* +2 for terminator and rounding error */
	char buf[maxlen + int(strlen(csep))
                 + int(log(double(n))/log(10.0)) + 2];
	if(n < 10000) {
	    R_CheckStack2(size_t(n) * sizeof(int));
	    cnts = static_cast<int *>(alloca((size_t(n)) * sizeof(int)));
	} else {
	    /* This is going to be slow so use expensive allocation
	       that will be recovered if interrupted. */
	    cnts = static_cast<int *>(RHO_alloc(size_t(n),  sizeof(int)));
	}
	for(i = 0; i < n; i++) cnts[i] = 1;
	data.nomatch = 0;
	PROTECT(newx = Rf_allocVector(STRSXP, 1));
	PROTECT(dup = duplicated2(names, &data));
	PROTECT(data.HashTable);
	vmax = vmaxget();
	for(i = 1; i < n; i++) { /* first cannot be a duplicate */
	    dp = INTEGER_ELT(dup, i); /* 1-based number of first occurrence */
	    if(dp == 0) continue;
	    ss = Rf_translateChar(STRING_ELT(names, i));
	    /* Try appending 1,2,3, ..., n-1 until it is not already in use */
	    for(cnt = cnts[dp - 1]; cnt < n; cnt++) {
		sprintf(buf, "%s%s%d", ss, csep, cnt);
		SET_STRING_ELT(newx, 0, Rf_mkChar(buf));
		if(Lookup(ans, newx, 0, &data) == data.nomatch) break;
	    }
	    SET_STRING_ELT(ans, i, STRING_ELT(newx, 0));
	    /* insert it */ (void) isDuplicated(ans, i, &data);
	    cnts[dp - 1] = cnt+1; /* cache the first unused cnt value */
	    vmaxset(vmax);
	}
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return ans;
}

/* Use hashing to improve object.size. Here we want equal CHARSXPs,
   not equal contents. */

inline static bool csequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    return STRING_ELT(x, i) == STRING_ELT(y, j);
}

static void HashTableSetup1(SEXP x, HashData *d)
{
    d->hash = cshash;
    d->equal = csequal;
#ifdef LONG_VECTOR_SUPPORT
    d->isLong = FALSE;
#endif
    MKsetup(XLENGTH(x), d, R_NaInt);
    d->HashTable = Rf_allocVector(INTSXP, d->M);
    for (R_xlen_t i = 0; i < d->M; i++) HTDATA_INT(d)[i] = NIL;
}

/* used in utils */
SEXP Rf_csduplicated(SEXP x)
{
    if(TYPEOF(x) != STRSXP)
	Rf_error(_("C function 'csduplicated' not called on a STRSXP"));
    R_xlen_t n = XLENGTH(x);
    HashData data;
    HashTableSetup1(x, &data);
    PROTECT(data.HashTable);
    SEXP ans = PROTECT(Rf_allocVector(LGLSXP, n));
    int *v = LOGICAL(ans);

    for (R_xlen_t i = 0; i < n; i++) v[i] = isDuplicated(x, i, &data);

    UNPROTECT(2);
    return ans;
}

#include <R_ext/Random.h>

// sample.int(.) --> .Internal(sample2(n, size)) :
HIDDEN SEXP do_sample2(/*const*/ Expression* call, const BuiltInFunction* op, RObject* n_, RObject* size_)
{
    SEXP ans;
    double dn = Rf_asReal(n_);
    int k = Rf_asInteger(size_);
    if (!std::isfinite(dn) || dn < 0 || dn > 4.5e15 || (k > 0 && dn == 0)) 
	Rf_error(_("invalid first argument"));
    if (k < 0) Rf_error(_("invalid '%s' argument"), "size");
    if (k > dn/2) Rf_error("This algorithm is for size <= n/2");
    HashData data;
    GetRNGstate();
    if (dn > INT_MAX) {
	ans = PROTECT(Rf_allocVector(REALSXP, k));
	double *ry = REAL0(ans);
	HashTableSetup(ans, &data, R_NaInt);
	PROTECT(data.HashTable);
	for (int i = 0; i < k; i++)
	    for(int j = 0; j < 100; j++) { // average < 2
		ry[i] = R_unif_index(dn) + 1;
		if(!isDuplicated(ans, i, &data)) break;
	    }
   } else {
	ans = PROTECT(Rf_allocVector(INTSXP, k));
	int *iy = INTEGER0(ans);
	HashTableSetup(ans, &data, R_NaInt);
	PROTECT(data.HashTable);
	for (int i = 0; i < k; i++)
	    for(int j = 0; j < 100; j++) { // average < 2
		iy[i] = R_unif_index(dn) + 1;
		if(!isDuplicated(ans, i, &data)) break;
	    }
    }
    PutRNGstate();
    UNPROTECT(2);
    return ans;
}
