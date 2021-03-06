/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2018  The R Core Team
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Pulic License as published by
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

/** @file character.cpp
 *
 * All sorts of character manipulation, including grep and agrep.
 */

/* The character functions in this file are

nzchar nchar substr substr<- abbreviate tolower toupper chartr strtrim

and the utility

make.names

The regex functions

strsplit grep [g]sub [g]regexpr agrep

here prior to 2.10.0 are now in grep.cpp and agrep.cpp

make.unique, duplicated, unique, match, pmatch, charmatch are in unique.cpp
iconv is in sysutils.cpp

Character strings in R are less than 2^31-1 bytes, so we use int not size_t.

Support for UTF-8-encoded strings in non-UTF-8 locales
======================================================

Comparison is done directly unless you happen to be comparing the same
string in different encodings.

nzchar and nchar(, "bytes") are independent of the encoding
nchar(, "char") nchar(, "width") handle UTF-8 directly, translate Latin-1
substr substr<-  handle UTF-8 and Latin-1 directly
tolower toupper chartr  translate UTF-8 to wchar, rest to current charset
  which needs Unicode wide characters
abbreviate strtrim  translate

All the string matching functions handle UTF-8 directly, otherwise
translate (latin1 to UTF-8, otherwise to native).

Support for "bytes" marked encoding
===================================

nzchar and nchar(, "bytes") are independent of the encoding.

nchar(, "char") nchar(, "width") give NA (if allowed) or error.
substr substr<-  work in bytes

abbreviate chartr make.names strtrim tolower toupper give error.

*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <errno.h>
#include <R_ext/RS.h>  /* for Calloc/Free */
#include <R_ext/Itermacros.h>
#include <rlocale.h>

#include <rho/RAllocStack.hpp>
#include <rho/BuiltInFunction.hpp>

using namespace rho;

/* We use a shared buffer here to avoid reallocing small buffers, and
   keep a standard-size (MAXELTSIZE = 8192) buffer allocated shared
   between the various functions.

   If we want to make this thread-safe, we would need to initialize an
   instance non-statically in each using function, but this would add
   to the overhead.
 */

#include "RBufferUtils.h"
static R_StringBuffer cbuff = { nullptr, 0, MAXELTSIZE };

/* Functions to perform analogues of the standard C string library. */
/* Most are vectorized */

/* primitive */
HIDDEN SEXP do_nzchar(/*const*/ Expression* call, const BuiltInFunction* op, int num_args, ...)
{
    SEXP x, ans;

    // checkArity(op, args);  .Primitive()  &  may have 1 or 2 args now
    if (num_args < 1 || num_args > 2)
	Rf_errorcall(call,
		  n_("%d argument passed to '%s' which requires %d to %d",
			   "%d arguments passed to '%s' which requires %d to %d",
			   (unsigned long) num_args),
		  num_args, op->name(), 1, 2);
    call->check1arg("x");

    // Unpack the arguments.
    va_list args;
    va_start(args, num_args);
    x = NEXT_ARG;

    int keepNA = FALSE; // the default
    if(num_args > 1) {
	RObject* keepNA_ = NEXT_ARG;
	keepNA = Rf_asLogical(keepNA_);
	if (keepNA == R_NaLog) keepNA = FALSE;
    }
    va_end(args);

    if (Rf_isFactor(x))
	Rf_error(_("'%s' requires a character vector"), "nzchar()");
    PROTECT(x = Rf_coerceVector(x, STRSXP));
    if (!Rf_isString(x))
	Rf_error(_("'%s' requires a character vector"), "nzchar()");

    R_xlen_t i, len = XLENGTH(x);
    PROTECT(ans = Rf_allocVector(LGLSXP, len));
    if (keepNA)
	for (i = 0; i < len; i++) {
	    SEXP sxi = STRING_ELT(x, i);
	    LOGICAL(ans)[i] = (sxi == R_NaString) ? R_NaLog : LENGTH(sxi) > 0;
	}
    else
	for (i = 0; i < len; i++)
	    LOGICAL(ans)[i] = LENGTH(STRING_ELT(x, i)) > 0;
    UNPROTECT(2);
    return ans;
}

/* R strings are limited to 2^31 - 1 bytes on all platforms */
int R_nchar(SEXP string, nchar_type type_,
	    Rboolean allowNA, Rboolean keepNA, const char* msg_name)
{
    if (string == R_NaString)
	return keepNA ? R_NaInt : 2;
    // else :
    switch(type_) {
    case Bytes:
	return LENGTH(string);
	break;
    case Chars:
	if (IS_UTF8(string)) {
	    const char *p = R_CHAR(string);
	    if (!utf8Valid(p)) {
		if (!allowNA)
		    Rf_error(_("invalid multibyte string, %s"), msg_name);
		return R_NaInt;
	    } else {
		int nc = 0;
		for( ; *p; p += utf8clen(*p)) nc++;
		return nc;
	    }
	} else if (IS_BYTES(string)) {
	    if (!allowNA) /* could do chars 0 */
		Rf_error(_("number of characters is not computable in \"bytes\" encoding, %s"),
		      msg_name);
	    return R_NaInt;
	} else if (mbcslocale) {
	    int nc = (int) mbstowcs(NULL, Rf_translateChar(string), 0);
	    if (!allowNA && nc < 0)
		Rf_error(_("invalid multibyte string, %s"), msg_name);
	    return (nc >= 0 ? nc : R_NaInt);
	} else
	    return ((int) strlen(Rf_translateChar(string)));
	break;
    case Width:
	if (IS_UTF8(string)) {
	    const char* p = R_CHAR(string);
	    if (!utf8Valid(p)) {
		if (!allowNA)
		    Rf_error(_("invalid multibyte string, %s"), msg_name);
		return R_NaInt;
	    } else {
		wchar_t wc1;
		Rwchar_t ucs;
		int nc = 0;
		for( ; *p; p += utf8clen(*p)) {
		    Rf_utf8toucs(&wc1, p);
		    if (IS_HIGH_SURROGATE(wc1)) 
		    	ucs = utf8toucs32(wc1, p);
		    else
		    	ucs = wc1;
		    nc += Ri18n_wcwidth(ucs); 
		}
		return nc;
	    }
	} else if (IS_BYTES(string)) {
	    if (!allowNA) /* could do width 0 */
		Rf_error(_("width is not computable for %s in \"bytes\" encoding"),
		      msg_name);
	    return R_NaInt;
	} else if (mbcslocale) {
	    const char* xi = Rf_translateChar(string);
	    int nc = (int) mbstowcs(NULL, xi, 0);
	    if (nc >= 0) {
		const void *vmax = vmaxget();
		wchar_t *wc = (wchar_t *)
		    R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
		mbstowcs(wc, xi, nc + 1);
		int nci18n = Ri18n_wcswidth(wc, 2147483647);
		vmaxset(vmax);
		return (nci18n < 1) ? nc : nci18n;
	    } else if (allowNA)
		Rf_error(_("invalid multibyte string, %s"), msg_name);
	    else
		return R_NaInt;
	} else
	    return (int) strlen(Rf_translateChar(string));

    } // switch
    return R_NaInt; // -Wall
} // R_nchar()

HIDDEN SEXP do_nchar(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* stype, RObject* allowNA_, RObject* keepNA_)
{
    SEXP d, s, x;

    if (Rf_isFactor(x_))
	Rf_error(_("'%s' requires a character vector"), "nchar()");
    PROTECT(x = Rf_coerceVector(x_, STRSXP));
    if (!Rf_isString(x))
	Rf_error(_("'%s' requires a character vector"), "nchar()");
    R_xlen_t len = XLENGTH(x);
    if (!Rf_isString(stype) || LENGTH(stype) != 1)
	Rf_error(_("invalid '%s' argument"), "type");
    const char *type = R_CHAR(STRING_ELT(stype, 0)); /* always ASCII */
    size_t ntype = strlen(type);
    if (ntype == 0) Rf_error(_("invalid '%s' argument"), "type");
    nchar_type type_;
    if (streqln(type, "bytes", ntype))	 type_ = Bytes;
    else if (streqln(type, "chars", ntype)) type_ = Chars;
    else if (streqln(type, "width", ntype)) type_ = Width;
    else Rf_error(_("invalid '%s' argument"), "type");
    int allowNA = Rf_asLogical( allowNA_);
    if (allowNA == R_NaLog) allowNA = 0;
    int keepNA = Rf_asLogical( keepNA_);
    if (keepNA == R_NaLog) // default
	keepNA = (type_ == Width) ? FALSE : TRUE;
    PROTECT(s = Rf_allocVector(INTSXP, len));
    int *s_ = INTEGER(s);
    for (R_xlen_t i = 0; i < len; i++) {
	SEXP sxi = STRING_ELT(x, i);
	char msg_i[38];
	sprintf(msg_i, "element %ld", (long)i + 1);
	s_[i] = R_nchar(sxi, type_, Rboolean(allowNA), Rboolean(keepNA), msg_i);
    }
    R_FreeStringBufferL(&cbuff);
    if ((d = Rf_getAttrib(x, Symbols::NamesSymbol)) != nullptr)
	Rf_setAttrib(s, Symbols::NamesSymbol, d);
    if ((d = Rf_getAttrib(x, Symbols::DimSymbol)) != nullptr)
	Rf_setAttrib(s, Symbols::DimSymbol, d);
    if ((d = Rf_getAttrib(x, Symbols::DimNamesSymbol)) != nullptr)
	Rf_setAttrib(s, Symbols::DimNamesSymbol, d);
    UNPROTECT(2);
    return s;
}

static void substr(char *buf, const char *str, int ienc, int sa, int so,
                   R_xlen_t idx)
{
/* Store the substring	str [sa:so]  into buf[] */
    int i, j, used;

    if (ienc == CE_UTF8) {
	if (!utf8Valid(str)) {
	    char msg[30];
	    sprintf(msg, "element %ld", (long)idx+1);
	    Rf_error(_("invalid multibyte string, %s"), msg);
	}
	const char* end = str + strlen(str);
	for (i = 0; i < so && str < end; i++) {
	    int used = utf8clen(*str);
	    if (i < sa - 1) { str += used; continue; }
	    for (j = 0; j < used; j++) *buf++ = *str++;
	}
    } else if (ienc == CE_LATIN1 || ienc == CE_BYTES) {
	for (str += (sa - 1), i = sa; i <= so; i++) *buf++ = *str++;
    } else {
	if (mbcslocale && !Rf_strIsASCII(str)) {
	    const char* end = str + strlen(str);
	    mbstate_t mb_st;
	    mbs_init(&mb_st);
	    for (i = 1; i < sa; i++) str += Rf_mbrtowc(nullptr, str, MB_CUR_MAX, &mb_st);
	    for (i = sa; i <= so && str < end; i++) {
		used = int(Rf_mbrtowc(nullptr, str, MB_CUR_MAX, &mb_st));
		for (j = 0; j < used; j++) *buf++ = *str++;
	    }
	} else
	    for (str += (sa - 1), i = sa; i <= so; i++) *buf++ = *str++;
    }
    *buf = '\0';
}

HIDDEN SEXP do_substr(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* start_, RObject* stop_)
{
    SEXP s;
    RObject* x = x_;

    if (!Rf_isString(x))
	Rf_error(_("extracting substrings from a non-character object"));
    R_xlen_t len = XLENGTH(x);
    PROTECT(s = Rf_allocVector(STRSXP, len));
    if (len > 0) {
	SEXP sa = start_,
	    so = stop_;
	int
	    k = LENGTH(sa),
	    l = LENGTH(so);
	if (!Rf_isInteger(sa) || !Rf_isInteger(so) || k == 0 || l == 0)
	    Rf_error(_("invalid substring arguments"));

	for (R_xlen_t i = 0; i < len; i++) {
	    int start = INTEGER(sa)[i % k],
		stop  = INTEGER(so)[i % l];
	    SEXP el = STRING_ELT(x,i);
	    if (el == R_NaString || start == R_NaInt || stop == R_NaInt) {
		SET_STRING_ELT(s, i, R_NaString);
		continue;
	    }
	    cetype_t ienc = Rf_getCharCE(el);
	    const char *ss = R_CHAR(el);
	    size_t slen = strlen(ss); /* FIXME -- should handle embedded nuls */
	    char* buf = static_cast<char*>(R_AllocStringBuffer(slen+1, &cbuff));
	    if (start < 1) start = 1;
	    if (start > stop || start > int(slen)) {
		buf[0] = '\0';
	    } else {
		if (stop > int(slen)) stop = int(slen);
		substr(buf, ss, ienc, start, stop, i);
	    }
	    SET_STRING_ELT(s, i, Rf_mkCharCE(buf, ienc));
	}
	R_FreeStringBufferL(&cbuff);
    }
    SHALLOW_DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return s;
}

// .Internal( startsWith(x, prefix) )  and
// .Internal( endsWith  (x, suffix) )
HIDDEN SEXP
do_startsWith(Expression* call, const BuiltInFunction* op,
	      RObject* x,
	      RObject* Xfix /* 'prefix' or 'suffix' */)
{
    if (!Rf_isString(x) || !Rf_isString(Xfix))
	Rf_error(_("non-character object(s)"));
    R_xlen_t
	n1 = XLENGTH(x),
	n2 = XLENGTH(Xfix),
	n = (n1 > 0 && n2 > 0) ? ((n1 >= n2) ? n1 : n2) : 0;
    if (n == 0) return Rf_allocVector(LGLSXP, 0);
    SEXP ans = PROTECT(Rf_allocVector(LGLSXP, n));

    using cp = const char*;
    if (n2 == 1) { // optimize the most common case
	SEXP el = STRING_ELT(Xfix, 0);
	if (el == R_NaString) {
	    for (R_xlen_t i = 0; i < n1; i++)
		LOGICAL(ans)[i] = R_NaLog;
	} else {
	    // ASCII matching will do for ASCII Xfix except in non-UTF-8 MBCS
	    Rboolean need_translate = TRUE;
	    if (Rf_strIsASCII(R_CHAR(el)) && (utf8locale || !mbcslocale)) 
		need_translate = FALSE;
	    cp y0 = need_translate ? Rf_translateCharUTF8(el) : R_CHAR(el);
	    int ylen = (int) strlen(y0);
	    for (R_xlen_t i = 0; i < n1; i++) {
		SEXP el = STRING_ELT(x, i);
		if (el == R_NaString) {
		    LOGICAL(ans)[i] = R_NaLog;
		} else {
		    cp x0 = need_translate ? Rf_translateCharUTF8(el) : R_CHAR(el);
		    if(op->variant() == 0) { // startsWith
			LOGICAL(ans)[i] = streqln(x0, y0, ylen);
		    } else { // endsWith
			int off = (int)strlen(x0) - ylen;
			if (off < 0)
			    LOGICAL(ans)[i] = 0;
			else {
			    LOGICAL(ans)[i] = memcmp(x0 + off, y0, ylen) == 0;
			}
		    }
		}
	    }
	}
    } else { // n2 > 1
	// convert both inputs to UTF-8
	cp* x0 = (cp*)R_alloc(n1, sizeof(char*));
	cp* y0 = (cp*)R_alloc(n2, sizeof(char*));
	// and record lengths, -1 for NA
	int* x1 = (int*)R_alloc(n1, sizeof(int*));
	int* y1 = (int*)R_alloc(n2, sizeof(int*));
	for (R_xlen_t i = 0; i < n1; i++) {
	    SEXP el = STRING_ELT(x, i);
	    if (el == R_NaString)
		x1[i] = -1;
	    else {
		x0[i] = Rf_translateCharUTF8(el);
		x1[i] = (int) strlen(x0[i]);
	    }
	}
	for (R_xlen_t i = 0; i < n2; i++) {
	    SEXP el = STRING_ELT(Xfix, i);
	    if (el == R_NaString)
		y1[i] = -1;
	    else {
		y0[i] = Rf_translateCharUTF8(el);
		y1[i] = (int) strlen(y0[i]);
	    }
	}
	R_xlen_t i, i1, i2;
	if(op->variant() == 0) { // 0 = startsWith, 1 = endsWith
	    MOD_ITERATE2(n, n1, n2, i, i1, i2, {
		    if (x1[i1] < 0 || y1[i2] < 0)
			LOGICAL(ans)[i] = R_NaLog;
		    else if (x1[i1] < y1[i2])
			LOGICAL(ans)[i] = 0;
		    else // memcmp should be faster than strncmp
			LOGICAL(ans)[i] = 
			    memcmp(x0[i1], y0[i2], y1[i2]) == 0;
		});
	} else { // endsWith
	    MOD_ITERATE2(n, n1, n2, i, i1, i2, {
		    if (x1[i1] < 0 || y1[i2] < 0)
			LOGICAL(ans)[i] = R_NaLog;
		    else {
			int off = x1[i1] - y1[i2];
			if (off < 0)
			    LOGICAL(ans)[i] = 0;
			else {
			    LOGICAL(ans)[i] = 
				memcmp(x0[i1] + off, y0[i2], y1[i2]) == 0;
			}
		    }
		});
	}
    }
    UNPROTECT(1);
    return ans;
}


static void substrset(char *buf, const char *const str, cetype_t ienc, size_t sa, size_t so,
          R_xlen_t xidx, R_xlen_t vidx)
{
    /* Replace the substring buf[sa:so] by str[] */
    //int i;
	size_t in = 0, out = 0;

    if (ienc == CE_UTF8) {
	if (!utf8Valid(buf)) {
	    char msg[30];
	    sprintf(msg, "element %ld", (long)xidx+1);
	    Rf_error(_("invalid multibyte string, %s"), msg);
	}
	if (!utf8Valid(str)) {
	    char msg[30];
	    sprintf(msg, "value element %ld", (long)vidx+1);
	    Rf_error(_("invalid multibyte string, %s"), msg);
	}
	for (size_t i = 1; i < sa; i++) buf += utf8clen(*buf);
	for (size_t i = sa; i <= so && in < strlen(str); i++) {
	    in +=  utf8clen(str[in]);
	    out += utf8clen(buf[out]);
	    if (!str[in]) break;
	}
	if (in != out) memmove(buf+in, buf+out, strlen(buf+out)+1);
	memcpy(buf, str, in);
    } else if (ienc == CE_LATIN1 || ienc == CE_BYTES) {
	in = strlen(str);
	out = so - sa + 1;
	memcpy(buf + sa - 1, str, (in < out) ? in : out);
    } else {
	/* This cannot work for stateful encodings */
	if (mbcslocale) {
	    for (size_t i = 1; i < sa; i++) buf += Rf_mbrtowc(nullptr, buf, MB_CUR_MAX, nullptr);
	    /* now work out how many bytes to replace by how many */
	    for (size_t i = sa; i <= so && in < strlen(str); i++) {
		in += Rf_mbrtowc(nullptr, str+in, MB_CUR_MAX, nullptr);
		out += Rf_mbrtowc(nullptr, buf+out, MB_CUR_MAX, nullptr);
		if (!str[in]) break;
	    }
	    if (in != out) memmove(buf+in, buf+out, strlen(buf+out)+1);
	    memcpy(buf, str, in);
	} else {
	    in = strlen(str);
	    out = so - sa + 1;
	    memcpy(buf + sa - 1, str, (in < out) ? in : out);
	}
    }
}

HIDDEN SEXP do_substrgets(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* start_, RObject* stop_, RObject* value_)
{
    SEXP s, x, sa, so, value, el, v_el;
    R_xlen_t i, len;
    int start, stop, k, l, v;
    size_t slen;
    cetype_t ienc, venc;
    const char *ss, *v_ss;
    char *buf;
    const void *vmax;

    x = x_;
    sa = start_;
    so = stop_;
    value = value_;
    k = LENGTH(sa);
    l = LENGTH(so);

    if (!Rf_isString(x))
	Rf_error(_("replacing substrings in a non-character object"));
    len = LENGTH(x);
    PROTECT(s = Rf_allocVector(STRSXP, len));
    if (len > 0) {
	if (!Rf_isInteger(sa) || !Rf_isInteger(so) || k == 0 || l == 0)
	    Rf_error(_("invalid substring arguments"));

	v = LENGTH(value);
	if (!Rf_isString(value) || v == 0) Rf_error(_("invalid value"));

	vmax = vmaxget();
	for (i = 0; i < len; i++) {
	    el = STRING_ELT(x, i);
	    v_el = STRING_ELT(value, i % v);
	    start = INTEGER(sa)[i % k];
	    stop = INTEGER(so)[i % l];
	    if (el == R_NaString || v_el == R_NaString ||
		start == R_NaInt || stop == R_NaInt) {
		SET_STRING_ELT(s, i, R_NaString);
		continue;
	    }
	    ienc = Rf_getCharCE(el);
	    ss = R_CHAR(el);
	    slen = strlen(ss);
	    if (start < 1) start = 1;
	    if (stop > int(slen)) stop = int(slen); /* SBCS optimization */
	    if (start > stop) {
		/* just copy element across */
		SET_STRING_ELT(s, i, STRING_ELT(x, i));
	    } else {
		cetype_t ienc2 = ienc;
		v_ss = R_CHAR(v_el);
		/* is the value in the same encoding?
		   FIXME: could prefer UTF-8 here
		 */
		venc = Rf_getCharCE(v_el);
		if (venc != ienc && !Rf_strIsASCII(v_ss)) {
		    ss = Rf_translateChar(el);
		    slen = strlen(ss);
		    v_ss = Rf_translateChar(v_el);
		    ienc2 = CE_NATIVE;
		}
		/* might expand under MBCS */
		buf = static_cast<char*>(R_AllocStringBuffer(slen+strlen(v_ss), &cbuff));
		strcpy(buf, ss);
		substrset(buf, v_ss, ienc2, start, stop, i, i % v);
		SET_STRING_ELT(s, i, Rf_mkCharCE(buf, ienc2));
	    }
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    }
    UNPROTECT(1);
    return s;
}

/* Abbreviate
   long names in the S-designated fashion:
   1) spaces
   2) lower case vowels
   3) lower case consonants
   4) upper case letters
   5) special characters.

   Letters are dropped from the end of words
   and at least one letter is retained from each word.

   If unique abbreviations are not produced letters are added until the
   results are unique (duplicated names are removed prior to entry).
   names, minlength, use.classes, dot
*/

namespace {
    inline bool FIRSTCHAR(const char* buff1, unsigned int i)
    {
	return isspace(int(buff1[i-1]));
    }

    inline bool LASTCHAR(const char* buff1, unsigned int i)
    {
	return !isspace(int(buff1[i-1]))
	    && (!buff1[i+1] || isspace(int(buff1[i+1])));
    }

    inline bool LC_VOWEL(const char* buff1, unsigned int i)
    {
	return buff1[i] == 'a' || buff1[i] == 'e' || buff1[i] == 'i' ||
	    buff1[i] == 'o' || buff1[i] == 'u';
    }

    inline size_t UPPER(const char* s) {
	return strlen(s) - 1;
    }
}

/* memmove does allow overlapping src and dest */
static void mystrcpy(char *dest, const char *src)
{
    memmove(dest, src, strlen(src)+1);
}

static SEXP stripchars(const char * const inchar, size_t minlen, int usecl)
{
    int i, j;
	size_t nspace = 0;
    char *s = cbuff.data;

    /* The R wrapper removed leading and trailing spces */
    mystrcpy(s, inchar);
    if (strlen(s) < minlen) goto donesc;

    /* The for() loops never touch the first character */

    /*  record spaces for removal later (as they act as word boundaries) */
    for (i = UPPER(s), j = 1; i > 0; i--) {
	if (isspace((int)s[i])) {
	    if (j) s[i] = '\0'; // trailing space
	    else nspace++;
	} else j = 0;
	if (strlen(s) - nspace <= minlen)
	    goto donesc;
    }

    if(usecl) {
	/* remove l/case vowels,
	   which are not at the beginning of a word but are at the end */
	for (i = UPPER(s); i > 0; i--) {
	    if (LC_VOWEL(s, i) && LASTCHAR(s, i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}

	/* remove those not at the beginning of a word */
	for (i = UPPER(s); i > 0; i--) {
	    if (LC_VOWEL(s, i) && !FIRSTCHAR(s, i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}

	/* Now do the same for remaining l/case chars */
	for (i = UPPER(s); i > 0; i--) {
	    if (islower((int)s[i]) && LASTCHAR(s, i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}

	for (i = UPPER(s); i > 0; i--) {
	    if (islower((int)s[i]) && !FIRSTCHAR(s, i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}
    }

    /* all else has failed so we use brute force */

    for (i = UPPER(s); i > 0; i--) {
	if (!FIRSTCHAR(s, i) && !isspace((int)s[i]))
	    mystrcpy(s + i, s + i + 1);
	if (strlen(s) - nspace <= minlen)
	    goto donesc;
    }

donesc:
    {  // remove internal spaces as required
	size_t upper = strlen(s);
	if (upper > minlen)
	    for (i = upper - 1; i > 0; i--)
		if (isspace((int)s[i]))
		    mystrcpy(s + i, s + i + 1);
    }

    return Rf_mkChar(s);
}

#define FIRSTCHARW(i) (iswspace((int)wc[i-1]))
#define LASTCHARW(i) (!iswspace((int)wc[i-1]) && (!wc[i+1] || iswspace((int)wc[i+1])))
#define WUP (int)(wcslen(wc) - 1)

// lower-case vowels in English plus accented versions
static int vowels[] = {
    0x61, 0x65, 0x69, 0x6f, 0x75,
    0xe0, 0xe1, 0x2e, 0xe3, 0xe4, 0xe5,
    0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc,
    0x101, 0x103, 0x105, 0x113, 0x115, 0x117, 0x118, 0x11b,
    0x129, 0x12b, 0x12d, 0x12f, 0x131, 0x14d, 0x14f, 0x151,
    0x169, 0x16b, 0x16d, 0x16f, 0x171, 0x173
};

static Rboolean iswvowel(wchar_t w)
{
    int v = (int) w, n = sizeof(vowels)/sizeof(int);
    Rboolean found = FALSE;
    for(int i = 0; i < n; i++)
	if(v == vowels[i]) {found = TRUE; break;}

    return found;
}

static void mywcscpy(wchar_t *dest, const wchar_t *src)
{
    memmove(dest, src, sizeof(wchar_t) * (wcslen(src)+1));
}

static SEXP wstripchars(const wchar_t * const inchar, size_t minlen, int usecl)
{
    int i, j;
	size_t nspace = 0;
    wchar_t *wc = (wchar_t *)cbuff.data;

    mywcscpy(wc, inchar);
    if (wcslen(wc) < minlen) goto donewsc;

    for (i = WUP, j = 1; i > 0; i--) {
	if (iswspace((int)wc[i])) {
	    if (j) wc[i] = '\0' ; else nspace++;
	} else j = 0;
	if (wcslen(wc) - nspace <= minlen)
	    goto donewsc;
    }

    if(usecl) {
	for (i = WUP; i > 0; i--) {
	    if (iswvowel(wc[i]) && LASTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}

	for (i = WUP; i > 0; i--) {
	    if (iswvowel(wc[i]) && !FIRSTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}

	for (i = WUP; i > 0; i--) {
	    if (islower((int)wc[i]) && LASTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}

	for (i = WUP; i > 0; i--) {
	    if (islower((int)wc[i]) && !FIRSTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}
    }

    for (i = WUP; i > 0; i--) {
	if (!FIRSTCHARW(i) && !iswspace((int)wc[i]))
	    mywcscpy(wc + i, wc + i + 1);
	if (wcslen(wc) - nspace <= minlen)
	    goto donewsc;
    }

donewsc:

    {
	size_t upper = wcslen(wc);
	if (upper > minlen)
	    for (i = upper - 1; i > 0; i--)
		if (iswspace((int)wc[i])) mywcscpy(wc + i, wc + i + 1);
    }

    int nb = (int) Rf_wcstoutf8(NULL, wc, INT_MAX);
    char *cbuf = CallocCharBuf(nb);
    Rf_wcstoutf8(cbuf, wc, nb);
    SEXP ans = Rf_mkCharCE(cbuf, CE_UTF8);
    Free(cbuf);
    return ans;
}

HIDDEN SEXP do_abbrev(/*const*/ Expression* call, const BuiltInFunction* op, RObject* these_, RObject* minlength_, RObject* use_classes_)
{
    RObject* x = these_;

    if (!Rf_isString(x))
	Rf_error(_("the first argument must be a character vector"));
    int minlen = Rf_asInteger(minlength_);
    if (minlen == R_NaInt)
	Rf_error(_("invalid '%s' argument"), "minlength");
    int usecl = Rf_asLogical(use_classes_);
    if (usecl == R_NaInt)
	Rf_error(_("invalid '%s' argument"), "use.classes");

    R_xlen_t len = XLENGTH(x);
    SEXP ans = PROTECT(Rf_allocVector(STRSXP, len));
    const void *vmax = vmaxget();
    Rboolean warn = FALSE;
    for (R_xlen_t i = 0 ; i < len ; i++) {
	SEXP el = STRING_ELT(x, i);
	if (el  == R_NaString)
	    SET_STRING_ELT(ans, i, R_NaString);
	else {
	    const char *s = R_CHAR(el);
	    if (Rf_strIsASCII(s)) {
		if(int(strlen(s)) > minlen) {
		    R_AllocStringBuffer(strlen(s)+1, &cbuff);
		    SET_STRING_ELT(ans, i, stripchars(s, minlen, usecl));
		} else SET_STRING_ELT(ans, i, el);
	    } else {
		s = Rf_translateCharUTF8(el);
		int nc = (int) Rf_utf8towcs(NULL, s, 0);
		if (nc > minlen) {
		    warn = TRUE;
		    const wchar_t *wc = Rf_wtransChar(el);
		    nc = (int) wcslen(wc);
		    R_AllocStringBuffer(sizeof(wchar_t)*(nc+1), &cbuff);
		    SET_STRING_ELT(ans, i, wstripchars(wc, minlen, usecl));
		} else SET_STRING_ELT(ans, i, el);
	    }
	}
	vmaxset(vmax); // this throws away the result of wtransChar
    }
    if (usecl && warn) Rf_warning(_("abbreviate used with non-ASCII chars"));
    SHALLOW_DUPLICATE_ATTRIB(ans, x);
    /* This copied the class, if any */
    R_FreeStringBufferL(&cbuff);
    UNPROTECT(1);
    return ans;
}

HIDDEN SEXP do_makenames(/*const*/ Expression* call, const BuiltInFunction* op, RObject* names_, RObject* allow__)
{
    SEXP arg, ans;
    R_xlen_t i, n;
    int l, allow_;
    char *p, *tmp = nullptr, *cbuf;
    const char *This;
    Rboolean need_prefix;
    const void *vmax;

    arg = names_;
    if (!Rf_isString(arg))
	Rf_error(_("non-character names"));
    n = XLENGTH(arg);
    allow_ = Rf_asLogical(allow__);
    if (allow_ == R_NaLog)
	Rf_error(_("invalid '%s' value"), "allow_");
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
	This = Rf_translateChar(STRING_ELT(arg, i));
	l = int(strlen(This));
	/* need to prefix names not beginning with alpha or ., as
	   well as . followed by a number */
	need_prefix = FALSE;
	if (mbcslocale && This[0]) {
	    int nc = l, used;
	    wchar_t wc;
	    mbstate_t mb_st;
	    const char *pp = This;
	    mbs_init(&mb_st);
	    used = int(Rf_mbrtowc(&wc, pp, MB_CUR_MAX, &mb_st));
	    pp += used; nc -= used;
	    if (wc == L'.') {
		if (nc > 0) {
		    Rf_mbrtowc(&wc, pp, MB_CUR_MAX, &mb_st);
		    if (iswdigit(wc))  need_prefix = TRUE;
		}
	    } else if (!iswalpha(wc)) need_prefix = TRUE;
	} else {
	    if (This[0] == '.') {
		if (l >= 1 && isdigit(0xff & int(This[1]))) need_prefix = TRUE;
	    } else if (!isalpha(0xff & int(This[0]))) need_prefix = TRUE;
	}
	if (need_prefix) {
	    tmp = Calloc(l+2, char);
	    strcpy(tmp, "X");
	    strcat(tmp, Rf_translateChar(STRING_ELT(arg, i)));
	} else {
	    tmp = Calloc(l+1, char);
	    strcpy(tmp, Rf_translateChar(STRING_ELT(arg, i)));
	}
	if (mbcslocale) {
	    /* This cannot lengthen the string, so safe to overwrite it.
	       Would also be possible a char at a time.
	     */
	    int nc = int(mbstowcs(nullptr, tmp, 0));
	    wchar_t *wstr = Calloc(nc+1, wchar_t), *wc;
	    if (nc >= 0) {
		mbstowcs(wstr, tmp, nc+1);
		for (wc = wstr; *wc; wc++) {
		    if (*wc == L'.' || (allow_ && *wc == L'_'))
			/* leave alone */;
		    else if (!iswalnum(int(*wc))) *wc = L'.';
		    /* If it changes into dot here,
		     * length will become short on mbcs.
		     * The name which became short will contain garbage.
		     * cf.
		     *   >  make.names(c("\u30fb"))
		     *   [1] "X.\0"
		     */
		}
		wcstombs(tmp, wstr, strlen(tmp)+1);
		Free(wstr);
	    } else Rf_error(_("invalid multibyte string %d"), i+1);
	} else {
	    for (p = tmp; *p; p++) {
		if (*p == '.' || (allow_ && *p == '_')) /* leave alone */;
		else if (!isalnum(0xff & int(*p))) *p = '.';
		/* else leave alone */
	    }
	}
	l = int(strlen(tmp));        /* needed? */
	SET_STRING_ELT(ans, i, Rf_mkChar(tmp));
	/* do we have a reserved word?  If so the name is invalid */
	if (!Rf_isValidName(tmp)) {
	    /* FIXME: could use R_Realloc instead */
	    cbuf = CallocCharBuf(strlen(tmp) + 1);
	    strcpy(cbuf, tmp);
	    strcat(cbuf, ".");
	    SET_STRING_ELT(ans, i, Rf_mkChar(cbuf));
	    Free(cbuf);
	}
	Free(tmp);
	vmaxset(vmax);
    }
    UNPROTECT(1);
    return ans;
}


HIDDEN SEXP do_tolower(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    SEXP x, y;
    R_xlen_t i, n;
    int ul;
    char *p;
    SEXP el;
    cetype_t ienc;
    Rboolean use_UTF8 = FALSE;
    const void *vmax;

    ul = op->variant(); /* 0 = tolower, 1 = toupper */

    x = x_;
    /* coercion is done in wrapper */
    if (!Rf_isString(x)) Rf_error(_("non-character argument"));
    n = XLENGTH(x);
    PROTECT(y = Rf_allocVector(STRSXP, n));
#if defined(Win32) || defined(__STDC_ISO_10646__) || defined(__APPLE__) || defined(__FreeBSD__)
    /* utf8towcs is really to UCS-4/2 */
    for (i = 0; i < n; i++)
	if (Rf_getCharCE(STRING_ELT(x, i)) == CE_UTF8) use_UTF8 = TRUE;
    if (mbcslocale || use_UTF8 == TRUE)
#else
    if (mbcslocale)
#endif
    {
	int nb, nc, j;
	wctrans_t tr = wctrans(ul ? "toupper" : "tolower");
	wchar_t * wc;
	char * cbuf;

	vmax = vmaxget();
	/* the translated string need not be the same length in bytes */
	for (i = 0; i < n; i++) {
	    el = STRING_ELT(x, i);
	    if (el == R_NaString) SET_STRING_ELT(y, i, R_NaString);
	    else {
		const char *xi;
		ienc = Rf_getCharCE(el);
		if (use_UTF8 && ienc == CE_UTF8) {
		    xi = R_CHAR(el);
		    nc = int(Rf_utf8towcs(nullptr, xi, 0));
		} else {
		    xi = Rf_translateChar(el);
		    nc = int(mbstowcs(nullptr, xi, 0));
		    ienc = CE_NATIVE;
		}
		if (nc >= 0) {
		    /* FIXME use this buffer for new string as well */
		    wc = static_cast<wchar_t *>
			(R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff));
		    if (ienc == CE_UTF8) {
			Rf_utf8towcs(wc, xi, nc + 1);
			for (j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = int(Rf_wcstoutf8(nullptr, wc, INT_MAX));
			cbuf = CallocCharBuf(nb);
			Rf_wcstoutf8(cbuf, wc, nb);
			SET_STRING_ELT(y, i, Rf_mkCharCE(cbuf, CE_UTF8));
		    } else {
			mbstowcs(wc, xi, nc + 1);
			for (j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = int(wcstombs(nullptr, wc, 0));
			cbuf = CallocCharBuf(nb);
			wcstombs(cbuf, wc, nb + 1);
			SET_STRING_ELT(y, i, Rf_markKnown(cbuf, el));
		    }
		    Free(cbuf);
		} else {
		    Rf_error(_("invalid multibyte string %d"), i+1);
		}
	    }
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    } else {
	char *xi;
	vmax = vmaxget();
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(x, i) == R_NaString)
		SET_STRING_ELT(y, i, R_NaString);
	    else {
		xi = CallocCharBuf(strlen(R_CHAR(STRING_ELT(x, i))));
		strcpy(xi, Rf_translateChar(STRING_ELT(x, i)));
		for (p = xi; *p != '\0'; p++)
		    *p = char(ul ? toupper(*p) : tolower(*p));
		SET_STRING_ELT(y, i, Rf_markKnown(xi, STRING_ELT(x, i)));
		Free(xi);
	    }
	    vmaxset(vmax);
	}
    }
    SHALLOW_DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}


enum wtr_type { WTR_INIT, WTR_CHAR, WTR_RANGE };
struct wtr_spec {
    wtr_type type;
    struct wtr_spec *next;
    union {
	wchar_t c;
	struct {
	    wchar_t first;
	    wchar_t last;
	} r;
    } u;
};

static void
wtr_build_spec(const wchar_t *s, struct wtr_spec *trs) {
    int i, len = int(wcslen(s));
    struct wtr_spec *This, *_new;

    This = trs;
    for (i = 0; i < len - 2; ) {
	_new = Calloc(1, struct wtr_spec);
	_new->next = nullptr;
	if (s[i + 1] == L'-') {
	    _new->type = WTR_RANGE;
	    if (s[i] > s[i + 2])
		Rf_error(_("decreasing range specification ('%lc-%lc')"),
		      s[i], s[i + 2]);
	    _new->u.r.first = s[i];
	    _new->u.r.last = s[i + 2];
	    i = i + 3;
	} else {
	    _new->type = WTR_CHAR;
	    _new->u.c = s[i];
	    i++;
	}
	This = This->next = _new;
    }
    for ( ; i < len; i++) {
	_new = Calloc(1, struct wtr_spec);
	_new->next = nullptr;
	_new->type = WTR_CHAR;
	_new->u.c = s[i];
	This = This->next = _new;
    }
}

static void
wtr_free_spec(struct wtr_spec *trs) {
    struct wtr_spec *This, *next;
    This = trs;
    while(This) {
	next = This->next;
	Free(This);
	This = next;
    }
}

static wchar_t
wtr_get_next_char_from_spec(struct wtr_spec **p) {
    wchar_t c;
    struct wtr_spec *This;

    This = *p;
    if (!This)
	return('\0');
    switch(This->type) {
	/* Note: this code does not deal with the WTR_INIT case. */
    case WTR_CHAR:
	c = This->u.c;
	*p = This->next;
	break;
    case WTR_RANGE:
	c = This->u.r.first;
	if (c == This->u.r.last) {
	    *p = This->next;
	} else {
	    (This->u.r.first)++;
	}
	break;
    default:
	c = L'\0';
	break;
    }
    return(c);
}

enum tr_spec_type { TR_INIT, TR_CHAR, TR_RANGE };
struct tr_spec {
    tr_spec_type type;
    struct tr_spec *next;
    union {
	unsigned char c;
	struct {
	    unsigned char first;
	    unsigned char last;
	} r;
    } u;
};

static void
tr_build_spec(const char *s, struct tr_spec *trs) {
    int i, len = int(strlen(s));
    struct tr_spec *This, *_new;

    This = trs;
    for (i = 0; i < len - 2; ) {
	_new = Calloc(1, struct tr_spec);
	_new->next = nullptr;
	if (s[i + 1] == '-') {
	    _new->type = TR_RANGE;
	    if (s[i] > s[i + 2])
		Rf_error(_("decreasing range specification ('%c-%c')"),
		      s[i], s[i + 2]);
	    _new->u.r.first = s[i];
	    _new->u.r.last = s[i + 2];
	    i = i + 3;
	} else {
	    _new->type = TR_CHAR;
	    _new->u.c = s[i];
	    i++;
	}
	This = This->next = _new;
    }
    for ( ; i < len; i++) {
	_new = Calloc(1, struct tr_spec);
	_new->next = nullptr;
	_new->type = TR_CHAR;
	_new->u.c = s[i];
	This = This->next = _new;
    }
}

static void
tr_free_spec(struct tr_spec *trs) {
    struct tr_spec *This, *next;
    This = trs;
    while(This) {
	next = This->next;
	Free(This);
	This = next;
    }
}

static unsigned char
tr_get_next_char_from_spec(struct tr_spec **p) {
    unsigned char c;
    struct tr_spec *This;

    This = *p;
    if (!This)
	return('\0');
    switch(This->type) {
	/* Note: this code does not deal with the TR_INIT case. */
    case TR_CHAR:
	c = This->u.c;
	*p = This->next;
	break;
    case TR_RANGE:
	c = This->u.r.first;
	if (c == This->u.r.last) {
	    *p = This->next;
	} else {
	    (This->u.r.first)++;
	}
	break;
    default:
	c = '\0';
	break;
    }
    return(c);
}

struct xtable_t { wchar_t c_old, c_new; };

R_INLINE static int xtable_comp(const void *a, const void *b)
{
    return (static_cast<const xtable_t *>(a))->c_old - (static_cast<const xtable_t *>(b))->c_old;
}

R_INLINE static int xtable_key_comp(const void *a, const void *b)
{
    return *(static_cast<const wchar_t *>(a)) - (static_cast<const xtable_t *>(b))->c_old;
}

#define SWAP(_a, _b, _TYPE)                                    \
{                                                              \
    _TYPE _t;                                                  \
    _t    = *(_a);                                             \
    *(_a) = *(_b);                                             \
    *(_b) = _t;                                                \
}

#define ISORT(_base,_num,_TYPE,_comp)                          \
{                                                              \
/* insert sort */                                              \
/* require stable data */                                      \
    int _i, _j ;                                               \
    for ( _i = 1 ; _i < _num ; _i++ )                          \
	for ( _j = _i; _j > 0 &&                               \
		      (*_comp)(_base+_j-1, _base+_j)>0; _j--)  \
	   SWAP(_base+_j-1, _base+_j, _TYPE);                  \
}

#define COMPRESS(_base,_num,_TYPE,_comp)                       \
{                                                              \
/* supress even c_old. last use */                             \
    int _i,_j ;                                                \
    for ( _i = 0 ; _i < (*(_num)) - 1 ; _i++ ){                \
	int rc = (*_comp)(_base+_i, _base+_i+1);               \
	if (rc == 0){                                          \
	   for ( _j = _i, _i-- ; _j < (*(_num)) - 1; _j++ )     \
		*((_base)+_j) = *((_base)+_j+1);               \
	    (*(_num))--;                                       \
	}                                                      \
    }                                                          \
}

#define BSEARCH(_rc,_key,_base,_nmemb,_TYPE,_comp)             \
{                                                              \
    std::size_t l, u, idx;                                          \
    _TYPE *p;                                                  \
    int comp;                                                  \
    l = 0;                                                     \
    u = _nmemb;                                                \
    _rc = nullptr;                                                \
    while (l < u)                                              \
    {                                                          \
	idx = (l + u) / 2;                                     \
	p =  (_base) + idx;                                    \
	comp = (*_comp)(_key, p);                              \
	if (comp < 0)                                          \
	    u = idx;                                           \
	else if (comp > 0)                                     \
	    l = idx + 1;                                       \
	else{                                                  \
	  _rc = p;                                             \
	  break;                                               \
	}                                                      \
    }                                                          \
}

HIDDEN SEXP do_chartr(/*const*/ Expression* call, const BuiltInFunction* op, RObject* old_, RObject* new_, RObject* x_)
{
    SEXP old, _new, x, y;
    R_xlen_t i, n;
    char *cbuf;
    SEXP el;
    cetype_t ienc;
    Rboolean use_UTF8 = FALSE;
    const void *vmax;

    old = old_;
    _new = new_;
    x = x_;
    n = XLENGTH(x);
    if (!Rf_isString(old) || LENGTH(old) < 1 || STRING_ELT(old, 0) == R_NaString)
	Rf_error(_("invalid '%s' argument"), "old");
    if (LENGTH(old) > 1)
	Rf_warning(_("argument '%s' has length > 1 and only the first element will be used"), "old");
    if (!Rf_isString(_new) || LENGTH(_new) < 1 || STRING_ELT(_new, 0) == R_NaString)
	Rf_error(_("invalid '%s' argument"), "new");
    if (LENGTH(_new) > 1)
	Rf_warning(_("argument '%s' has length > 1 and only the first element will be used"), "new");
    if (!Rf_isString(x)) Rf_error("invalid '%s' argument", "x");

    /* utf8towcs is really to UCS-4/2 */
#if defined(Win32) || defined(__STDC_ISO_10646__) || defined(__APPLE__)  || defined(__FreeBSD__)
    for (i = 0; i < n; i++)
	if (Rf_getCharCE(STRING_ELT(x, i)) == CE_UTF8) use_UTF8 = TRUE;

    if (Rf_getCharCE(STRING_ELT(old, 0)) == CE_UTF8) use_UTF8 = TRUE;
    if (Rf_getCharCE(STRING_ELT(_new, 0)) == CE_UTF8) use_UTF8 = TRUE;

    if (mbcslocale || use_UTF8 == TRUE)
#else
    if (mbcslocale)
#endif
    {
	int j, nb, nc;
	xtable_t *xtable, *tbl;
	int xtable_cnt;
	struct wtr_spec *trs_cnt, **trs_cnt_ptr;
	wchar_t c_old, c_new, *wc;
	const char *xi, *s;
	struct wtr_spec *trs_old, **trs_old_ptr;
	struct wtr_spec *trs_new, **trs_new_ptr;

	/* Initialize the old and new wtr_spec lists. */
	trs_old = Calloc(1, struct wtr_spec);
	trs_old->type = WTR_INIT;
	trs_old->next = nullptr;
	trs_new = Calloc(1, struct wtr_spec);
	trs_new->type = WTR_INIT;
	trs_new->next = nullptr;
	/* Build the old and new wtr_spec lists. */
	if (use_UTF8 && Rf_getCharCE(STRING_ELT(old, 0)) == CE_UTF8) {
	    s = R_CHAR(STRING_ELT(old, 0));
	    nc = int(Rf_utf8towcs(nullptr, s, 0));
	    if (nc < 0) Rf_error(_("invalid UTF-8 string 'old'"));
	    wc = static_cast<wchar_t *>(R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff));
	    Rf_utf8towcs(wc, s, nc + 1);
	} else {
	    s = Rf_translateChar(STRING_ELT(old, 0));
	    nc = int(mbstowcs(nullptr, s, 0));
	    if (nc < 0) Rf_error(_("invalid multibyte string 'old'"));
	    wc = static_cast<wchar_t *>(R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff));
	    mbstowcs(wc, s, nc + 1);
	}
	wtr_build_spec(wc, trs_old);
	trs_cnt = Calloc(1, struct wtr_spec);
	trs_cnt->type = WTR_INIT;
	trs_cnt->next = nullptr;
	wtr_build_spec(wc, trs_cnt); /* use count only */

	if (use_UTF8 && Rf_getCharCE(STRING_ELT(_new, 0)) == CE_UTF8) {
	    s = R_CHAR(STRING_ELT(_new, 0));
	    nc = int(Rf_utf8towcs(nullptr, s, 0));
	    if (nc < 0) Rf_error(_("invalid UTF-8 string 'new'"));
	    wc = static_cast<wchar_t *>(R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff));
	    Rf_utf8towcs(wc, s, nc + 1);
	} else {
	    s = Rf_translateChar(STRING_ELT(_new, 0));
	    nc = int(mbstowcs(nullptr, s, 0));
	    if (nc < 0) Rf_error(_("invalid multibyte string 'new'"));
	    wc = static_cast<wchar_t *>(R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff));
	    mbstowcs(wc, s, nc + 1);
	}
	wtr_build_spec(wc, trs_new);

	/* Initialize the pointers for walking through the old and new
	   wtr_spec lists and retrieving the next chars from the lists.
	*/

	trs_cnt_ptr = Calloc(1, struct wtr_spec *);
	*trs_cnt_ptr = trs_cnt->next;
	for (xtable_cnt = 0 ; wtr_get_next_char_from_spec(trs_cnt_ptr);
	      xtable_cnt++) ;
	wtr_free_spec(trs_cnt);
	Free(trs_cnt_ptr);
	xtable = static_cast<xtable_t *>(RHO_alloc(xtable_cnt+1, sizeof(xtable_t)));

	trs_old_ptr = Calloc(1, struct wtr_spec *);
	*trs_old_ptr = trs_old->next;
	trs_new_ptr = Calloc(1, struct wtr_spec *);
	*trs_new_ptr = trs_new->next;
	for (i = 0; ; i++) {
	    c_old = wtr_get_next_char_from_spec(trs_old_ptr);
	    c_new = wtr_get_next_char_from_spec(trs_new_ptr);
	    if (c_old == '\0')
		break;
	    else if (c_new == '\0')
		Rf_error(_("'old' is longer than 'new'"));
	    else {
		xtable[i].c_old = c_old;
		xtable[i].c_new = c_new;
	    }
	}

	/* Free the memory occupied by the wtr_spec lists. */
	wtr_free_spec(trs_old);
	wtr_free_spec(trs_new);
	Free(trs_old_ptr); Free(trs_new_ptr);

	ISORT(xtable, xtable_cnt, xtable_t , xtable_comp);
	COMPRESS(xtable, &xtable_cnt, xtable_t, xtable_comp);

	PROTECT(y = Rf_allocVector(STRSXP, n));
	vmax = vmaxget();
	for (i = 0; i < n; i++) {
	    el = STRING_ELT(x,i);
	    if (el == R_NaString)
		SET_STRING_ELT(y, i, R_NaString);
	    else {
		ienc = Rf_getCharCE(el);
		if (use_UTF8 && ienc == CE_UTF8) {
		    xi = R_CHAR(el);
		    nc = int(Rf_utf8towcs(nullptr, xi, 0));
		} else {
		    xi = Rf_translateChar(el);
		    nc = int(mbstowcs(nullptr, xi, 0));
		    ienc = CE_NATIVE;
		}
		if (nc < 0)
		    Rf_error(_("invalid input multibyte string %d"), i+1);
                wc = static_cast<wchar_t *>(R_AllocStringBuffer((nc+1)*sizeof(wchar_t),
								 &cbuff));
		if (ienc == CE_UTF8) Rf_utf8towcs(wc, xi, nc + 1);
		else mbstowcs(wc, xi, nc + 1);
		for (j = 0; j < nc; j++){
		    BSEARCH(tbl,&wc[j], xtable, xtable_cnt,
			    xtable_t, xtable_key_comp);
		    if (tbl) wc[j] = tbl->c_new;
		}
		if (ienc == CE_UTF8) {
		    nb = int(Rf_wcstoutf8(nullptr, wc, INT_MAX));
		    cbuf = CallocCharBuf(nb);
		    Rf_wcstoutf8(cbuf, wc, nb);
		    SET_STRING_ELT(y, i, Rf_mkCharCE(cbuf, CE_UTF8));
		} else {
		    nb = int(wcstombs(nullptr, wc, 0));
		    cbuf = CallocCharBuf(nb);
		    wcstombs(cbuf, wc, nb + 1);
		    SET_STRING_ELT(y, i, Rf_markKnown(cbuf, el));
		}
		Free(cbuf);
	    }
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    } else {
	unsigned char xtable[UCHAR_MAX + 1], *p, c_old, c_new;
	struct tr_spec *trs_old, **trs_old_ptr;
	struct tr_spec *trs_new, **trs_new_ptr;

	for (unsigned int ii = 0; ii <= UCHAR_MAX; ii++)
	    xtable[ii] = static_cast<unsigned char>(ii);

	/* Initialize the old and new tr_spec lists. */
	trs_old = Calloc(1, struct tr_spec);
	trs_old->type = TR_INIT;
	trs_old->next = nullptr;
	trs_new = Calloc(1, struct tr_spec);
	trs_new->type = TR_INIT;
	trs_new->next = nullptr;
	/* Build the old and new tr_spec lists. */
	tr_build_spec(Rf_translateChar(STRING_ELT(old, 0)), trs_old);
	tr_build_spec(Rf_translateChar(STRING_ELT(_new, 0)), trs_new);
	/* Initialize the pointers for walking through the old and new
	   tr_spec lists and retrieving the next chars from the lists.
	*/
	trs_old_ptr = Calloc(1, struct tr_spec *);
	*trs_old_ptr = trs_old->next;
	trs_new_ptr = Calloc(1, struct tr_spec *);
	*trs_new_ptr = trs_new->next;
	for (;;) {
	    c_old = tr_get_next_char_from_spec(trs_old_ptr);
	    c_new = tr_get_next_char_from_spec(trs_new_ptr);
	    if (c_old == '\0')
		break;
	    else if (c_new == '\0')
		Rf_error(_("'old' is longer than 'new'"));
	    else
		xtable[c_old] = c_new;
	}
	/* Free the memory occupied by the tr_spec lists. */
	tr_free_spec(trs_old);
	tr_free_spec(trs_new);
	Free(trs_old_ptr); Free(trs_new_ptr);

	n = LENGTH(x);
	PROTECT(y = Rf_allocVector(STRSXP, n));
	vmax = vmaxget();
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(x,i) == R_NaString)
		SET_STRING_ELT(y, i, R_NaString);
	    else {
		const char *xi = Rf_translateChar(STRING_ELT(x, i));
		cbuf = CallocCharBuf(strlen(xi));
		strcpy(cbuf, xi);
		for (p = reinterpret_cast<unsigned char *>(cbuf); *p != '\0'; p++)
		    *p = xtable[*p];
		SET_STRING_ELT(y, i, Rf_markKnown(cbuf, STRING_ELT(x, i)));
		Free(cbuf);
	    }
	}
	vmaxset(vmax);
    }

    SHALLOW_DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}

HIDDEN SEXP do_strtrim(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* width_)
{
    SEXP s, x, width;
    R_xlen_t i, len;
    int nw, w, nc;
    const char *This;
    char *buf;
    const char *p; char *q;
    int w0, wsum, k, nb;
    wchar_t wc;
    mbstate_t mb_st;
    const void *vmax;

    /* as.character happens at R level now */
    if (!Rf_isString(x = x_))
	Rf_error(_("strtrim() requires a character vector"));
    len = XLENGTH(x);
    PROTECT(s = Rf_allocVector(STRSXP, len));
    if(len > 0) {
	PROTECT(width = Rf_coerceVector(width_, INTSXP));
	nw = LENGTH(width);
	if (!nw || (nw < len && len % nw))
	    Rf_error(_("invalid '%s' argument"), "width");
	for (i = 0; i < nw; i++)
	    if (INTEGER(width)[i] == R_NaInt ||
		INTEGER(width)[i] < 0)
		Rf_error(_("invalid '%s' argument"), "width");
	vmax = vmaxget();
	for (i = 0; i < len; i++) {
	    if (STRING_ELT(x, i) == R_NaString) {
		SET_STRING_ELT(s, i, STRING_ELT(x, i));
		continue;
	    }
	    w = INTEGER(width)[i % nw];
	    This = Rf_translateChar(STRING_ELT(x, i));
	    nc = (int) strlen(This);
	    buf = static_cast<char *>(R_AllocStringBuffer(nc, &cbuff));
	    wsum = 0;
	    mbs_init(&mb_st);
	    for (p = This, w0 = 0, q = buf; *p ;) {
		nb =  (int) Rf_mbrtowc(&wc, p, MB_CUR_MAX, &mb_st);
		w0 = Ri18n_wcwidth(Rwchar_t(wc));
		if (w0 < 0) { p += nb; continue; } /* skip non-printable chars */
		wsum += w0;
		if (wsum <= w) {
		    for (k = 0; k < nb; k++) *q++ = *p++;
		} else break;
	    }
	    *q = '\0';
	    SET_STRING_ELT(s, i, Rf_markKnown(buf, STRING_ELT(x, i)));
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
	UNPROTECT(1);
    }
    SHALLOW_DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return s;
}

static int strtoi(SEXP s, int base)
{
    long int res;
    char *endp;

    /* strtol might return extreme values on error */
    errno = 0;

    if(s == R_NaString) return(R_NaInt);
    res = strtol(R_CHAR(s), &endp, base); /* ASCII */
    if(errno || *endp != '\0') res = R_NaInt;
    if(res > INT_MAX || res < INT_MIN) res = R_NaInt;
    return int(res);
}

HIDDEN SEXP do_strtoi(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* base_)
{
    SEXP ans, x, b;
    R_xlen_t i, n;
    int base;

    x = x_;
    b = base_;

    if(!Rf_isInteger(b) || (LENGTH(b) < 1))
	Rf_error(_("invalid '%s' argument"), "base");
    base = INTEGER(b)[0];
    if((base != 0) && ((base < 2) || (base > 36)))
	Rf_error(_("invalid '%s' argument"), "base");

    PROTECT(ans = Rf_allocVector(INTSXP, n = LENGTH(x)));
    for(i = 0; i < n; i++)
	INTEGER(ans)[i] = strtoi(STRING_ELT(x, i), base);
    UNPROTECT(1);

    return ans;
}

/* creates a new STRSXP which is a suffix of string, starting
   with given index; the result is returned unprotected  */

HIDDEN SEXP Rf_stringSuffix(SEXP string, int fromIndex) {

    int origLen = LENGTH(string);
    int newLen = origLen - fromIndex;

    SEXP res = PROTECT(Rf_allocVector(STRSXP, newLen));
    int i;
    for(i = 0; i < newLen; i++) {
	SET_STRING_ELT(res, i, STRING_ELT(string, fromIndex++));
    }

    UNPROTECT(1); /* res */
    return res;
}

HIDDEN SEXP do_strrep(Expression* call, const BuiltInFunction* op, RObject* x, RObject* n)
{
    SEXP d, s, el;
    R_xlen_t is, ix, in, ns, nx, nn;
    const char *xi;
    int j, ni, nc;
    const char *cbuf;
    char *buf;
    const void *vmax;

    nx = XLENGTH(x);
    nn = XLENGTH(n);
    if((nx == 0) || (nn == 0))
	return Rf_allocVector(STRSXP, 0);

    ns = std::max(nx, nn);

    PROTECT(s = Rf_allocVector(STRSXP, ns));
    vmax = vmaxget();
    is = ix = in = 0;
    for(; is < ns; is++) {
	el = STRING_ELT(x, ix);
	ni = INTEGER(n)[in];
	if((el == R_NaString) || (ni == R_NaInt)) {
	    SET_STRING_ELT(s, is, R_NaString);
	} else {
	    if(ni < 0)
		Rf_error(_("invalid '%s' value"), "times");
	    xi = R_CHAR(el);
	    nc = (int) strlen(xi);

	    /* check for feasible result length; use double to protect
	       against integer overflow */
	    double len = ((double) nc) * ni;
	    if (len > INT_MAX)
		Rf_error("R character strings are limited to 2^31-1 bytes");

	    cbuf = buf = CallocCharBuf(nc * ni);
	    for(j = 0; j < ni; j++) {
		strcpy(buf, xi);
		buf += nc;
	    }
	    SET_STRING_ELT(s, is, Rf_mkCharCE(cbuf, Rf_getCharCE(el)));
	    Free(cbuf);
	    vmaxset(vmax);
	}
	ix = (++ix == nx) ? 0 : ix;
	in = (++in == nn) ? 0 : in;
    }
    /* Copy names if not recycled. */
    if((ns == nx) &&
       (d = Rf_getAttrib(x, Symbols::NamesSymbol)) != nullptr)
	Rf_setAttrib(s, Symbols::NamesSymbol, d);
    UNPROTECT(1);
    return s;
}
