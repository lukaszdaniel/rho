/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2017   The R Core Team
 *  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
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

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h> /* for putenv */
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Fileio.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/Riconv.h>
#include <Rinterface.h>
#include <errno.h>
#include <rlocale.h>
#include <vector>
#include "rho/Symbol.hpp"

using namespace std;
using namespace rho;

/*
  See ../unix/system.txt for a description of some of these functions.
  Formally part of ../unix/sys-common.cpp.
 */

/*
 * FILESYSTEM INTERACTION
 */

/*
 * This call provides a simple interface to the "stat" system call.
 */

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

static int isDir(const char *path);

#ifdef HAVE_AQUA
int (*ptr_CocoaSystem)(const char*);
#endif

#ifdef Win32
bool R_FileExists(const char *path)
{
    struct _stati64 sb;
    return _stati64(R_ExpandFileName(path), &sb) == 0;
}

double attribute_hidden R_FileMtime(const char *path)
{
    struct _stati64 sb;
    if (_stati64(R_ExpandFileName(path), &sb) != 0)
	Rf_error(_("cannot determine file modification time of '%s'"), path);
    return sb.st_mtime;
}
#else
bool R_FileExists(const char *path)
{
    struct stat sb;
    return (stat(R_ExpandFileName(path), &sb) == 0);
}

double attribute_hidden R_FileMtime(const char *path)
{
    struct stat sb;
    if (stat(R_ExpandFileName(path), &sb) != 0)
	Rf_error(_("cannot determine file modification time of '%s'"), path);
    return double(sb.st_mtime);
}
#endif

    /*
     *  Unix file names which begin with "." are invisible.
     */

bool attribute_hidden R_HiddenFile(const char* name)
{
    if (name && name[0] != '.')
		return false;
    return true;
}

/* The MSVC runtime has a global to determine whether an unspecified
   file open is in text or binary mode.  We force explicit text mode
   here to avoid depending on that global, which may have been changed
   by user code (most likely in embedded applications of R).
*/

#ifdef Win32

static char * fixmode(const char *mode)
{
    /* Rconnection can have a mode of 4 chars plus a ccs= setting plus a null; we might
     * add one char if neither b nor t is specified. */
    static char fixedmode[20];
    fixedmode[19] = '\0';
    strncpy(fixedmode, mode, 19);
    if (!strpbrk(fixedmode, "bt")) {
	strcat(fixedmode, "t");
    }
    return fixedmode;
}

static wchar_t * wcfixmode(const wchar_t *mode)
{
    static wchar_t wcfixedmode[20];
    wcfixedmode[19] = L'\0';
    wcsncpy(wcfixedmode, mode, 19);
    if (!wcspbrk(wcfixedmode, L"bt")) {
	wcscat(wcfixedmode, L"t");
    }
    return wcfixedmode;
}

#else
#define fixmode(mode) (mode)
#define wcfixmode(mode) (mode)
#endif

FILE *R_fopen(const char *filename, const char *mode)
{
    return(filename ? fopen(filename, fixmode(mode)) : nullptr );
}

/* The point of this function is to allow file names in foreign
   character sets.  On Unix-alikes in a UTF-8 locale all that is
   needed is to convert file names to UTF-8, since they will be stored
   in UTF-8.  For other locales, it seems that there is no way to specify
   a file name in UTF-8.

   On NT-based versions of Windows, file names are stored in 'Unicode'
   (UCS-2), and _wfopen is provided to access them by UCS-2 names.
*/

#ifdef Win32

#define BSIZE 100000
wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand)
{
    static wchar_t filename[BSIZE+1];
    void *obj;
    const char *from = "", *inbuf;
    char *outbuf;
    size_t inb, outb, res;

    if(!strlen(R_CHAR(fn))) {
	wcscpy(filename, L"");
	return filename;
    }
    if(IS_LATIN1(fn))
#ifdef HAVE_ICONV_CP1252
	from = "CP1252";
#else
	from = "latin1";
#endif
    if(IS_UTF8(fn)) from = "UTF-8";
    if(IS_BYTES(fn)) Rf_error(_("encoding of a filename cannot be 'bytes'"));
    obj = Riconv_open("UCS-2LE", from);
    if(obj == (void *)(-1))
	Rf_error(_("unsupported conversion from '%s' in codepage %d"),
	      from, localeCP);

    if(expand) inbuf = R_ExpandFileName(R_CHAR(fn)); else inbuf = R_CHAR(fn);

    inb = strlen(inbuf)+1; outb = 2*BSIZE;
    outbuf = (char *) filename;
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    Riconv_close(obj);
    if(inb > 0) Rf_error(_("file name conversion problem -- name too long?"));
    if(res == -1) Rf_error(_("file name conversion problem"));

    return filename;
}

FILE *R_wfopen(const wchar_t *filename, const wchar_t *mode)
{
    return filename ? _wfopen(filename, wcfixmode(mode)) : NULL;
}


FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand)
{
    wchar_t wmode[10];

    if(fn == NA_STRING) return NULL;
    mbstowcs(wmode, fixmode(mode), 10);
    return _wfopen(filenameToWchar(fn, expand), wmode);
}
#else
FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand)
{
    const void *vmax = vmaxget();
    const char *filename = Rf_translateChar(fn), *res;
    if(fn == NA_STRING || !filename) return nullptr;
    if(expand) res = R_ExpandFileName(filename);
    else res = filename;
    vmaxset(vmax);
    return fopen(res, mode);
}
#endif

/*
 *  SYSTEM INFORMATION
 */

	  /* The location of the R system files */

char *R_HomeDir(void)
{
    return getenv("R_HOME");
}

/* This is a primitive (with no arguments) */
SEXP attribute_hidden do_interactive(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op)
{
    return Rf_ScalarLogical( (R_Interactive) ? 1 : 0 );
}

SEXP attribute_hidden do_tempdir(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* check_)
{
    Rboolean check = Rboolean(Rf_asLogical(check_));
    if(check && !isDir(R_TempDir)) {
	R_TempDir = nullptr;
	R_reInitTempDir(/* die_on_fail = */ FALSE);
    }
    return Rf_mkString(R_TempDir);
}


SEXP attribute_hidden do_tempfile(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* pattern_, rho::RObject* tmpdir_, rho::RObject* fileext_)
{
    SEXP  ans, pattern, fileext, tempdir;
    const char *tn, *td, *te;
    char *tm;
    int i, n1, n2, n3, slen;

    pattern = pattern_; n1 = Rf_length(pattern);
    tempdir = tmpdir_; n2 = Rf_length(tempdir);
    fileext = fileext_; n3 = Rf_length(fileext);
    if (!Rf_isString(pattern))
	Rf_error(_("invalid filename pattern"));
    if (!Rf_isString(tempdir))
	Rf_error(_("invalid '%s' value"), "tempdir");
    if (!Rf_isString(fileext))
	Rf_error(_("invalid file extension"));
    if (n1 < 1)
	Rf_error(_("no 'pattern'"));
    if (n2 < 1)
	Rf_error(_("no 'tempdir'"));
    if (n3 < 1)
	Rf_error(_("no 'fileext'"));
    slen = std::max(n1, n2);
    slen = std::max(n3, slen);
    PROTECT(ans = Rf_allocVector(STRSXP, slen));
    for(i = 0; i < slen; i++) {
	tn = Rf_translateChar( STRING_ELT( pattern , i%n1 ) );
	td = Rf_translateChar( STRING_ELT( tempdir , i%n2 ) );
	te = Rf_translateChar( STRING_ELT( fileext , i%n3 ) );
	/* try to get a new file name */
	tm = R_tmpnam2(tn, td, te);
	SET_STRING_ELT(ans, i, Rf_mkChar(tm));
	if(tm) free(tm);
    }
    UNPROTECT(1);
    return (ans);
}

FILE *R_popen(const char *command, const char *type)
{
    FILE *fp;
#ifdef OLD__APPLE__
    /* Luke recommends this to fix PR#1140 */
    /* As of 2016-01-06 on El Capitan this may no longer be needed -- LT */
    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGPROF);
    sigprocmask(SIG_BLOCK, &ss,  NULL);
    fp = popen(command, type);
    sigprocmask(SIG_UNBLOCK, &ss, NULL);
#else
    fp = popen(command, type);
#endif
    return fp;
}

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

int R_system(const char *command)
{
    int res;
#ifdef __APPLE__
# ifdef OLD__APPLE__
    /* Luke recommends this to fix PR#1140 */
    /* As of 2016-01-06 on El Capitan this may no longer be needed -- LT */
    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGPROF);
    sigprocmask(SIG_BLOCK, &ss,  NULL);
# endif
#ifdef HAVE_AQUA
    if(ptr_CocoaSystem) res = ptr_CocoaSystem(command); else
#endif
    res = system(command);
# ifdef OLD__APPLE__
    sigprocmask(SIG_UNBLOCK, &ss, NULL);
# endif
#else // not APPLE
    res = system(command);
#endif
#ifdef HAVE_SYS_WAIT_H
    if (WIFEXITED(res)) res = WEXITSTATUS(res);
#else
    /* assume that this is shifted if a multiple of 256 */
    if ((res % 256) == 0) res = res/256;
#endif
    if (res == -1) {
	/* this means that system() failed badly - it didn't
	   even get to try to run the shell */
	Rf_warning(_("system call failed: %s"), strerror(errno));
	/* R system() is documented to return 127 on failure, and a lot of
	   code relies on that - it will misinterpret -1 as success */
	res = 127;
    }
    return res;
}

#if defined(__APPLE__)
# include <crt_externs.h>
# define environ (*_NSGetEnviron())
#else
extern char ** environ;
#endif

#ifdef Win32
/* _wenviron is declared in stdlib.h */
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h> /* _wgetenv etc */
#endif

SEXP attribute_hidden do_getenv(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* symbol_, rho::RObject* default_value_)
{
    int i, j;
    SEXP ans;

    if (!Rf_isString(symbol_))
	Rf_error(_("wrong type for argument"));

    if (!Rf_isString(default_value_) || LENGTH(default_value_) != 1)
	Rf_error(_("wrong type for argument"));

    i = LENGTH(symbol_);
    if (i == 0) {
#ifdef Win32
	int n = 0, N;
	wchar_t **w;
	for (i = 0, w = _wenviron; *w != NULL; i++, w++)
	    n = max(n, wcslen(*w));
	N = 4*n+1;
	vector<char> bufv(N);
	char* buf = &bufv[0];
	PROTECT(ans = Rf_allocVector(STRSXP, i));
	for (i = 0, w = _wenviron; *w != NULL; i++, w++) {
	    Rf_wcstoutf8(buf, *w, sizeof(buf));
	    SET_STRING_ELT(ans, i, Rf_mkCharCE(buf, CE_UTF8));
	}
#else
	char **e;
	for (i = 0, e = environ; *e != nullptr; i++, e++);
	PROTECT(ans = Rf_allocVector(STRSXP, i));
	for (i = 0, e = environ; *e != nullptr; i++, e++)
	    SET_STRING_ELT(ans, i, Rf_mkChar(*e));
#endif
    } else {
	PROTECT(ans = Rf_allocVector(STRSXP, i));
	for (j = 0; j < i; j++) {
#ifdef Win32
	    const wchar_t *wnm = Rf_wtransChar(STRING_ELT(CAR(args), j));
	    wchar_t *w = _wgetenv(wnm);
	    if (w == NULL)
		SET_STRING_ELT(ans, j, STRING_ELT(CADR(args), 0));
	    else {
		int n = wcslen(w), N = 4*n+1; /* UTF-16 maps to <= 4 UTF-8 */
		vector<char> bufv(N);
		char* buf = &bufv[0];
		Rf_wcstoutf8(buf, w, sizeof(buf));
		SET_STRING_ELT(ans, j, Rf_mkCharCE(buf, CE_UTF8));
	    }
#else
	    char *s = getenv(Rf_translateChar(STRING_ELT(symbol_, j)));
	    if (s == nullptr)
		SET_STRING_ELT(ans, j, STRING_ELT(default_value_, 0));
	    else {
		SEXP tmp;
		if(known_to_be_latin1) tmp = Rf_mkCharCE(s, CE_LATIN1);
		else if(known_to_be_utf8) tmp = Rf_mkCharCE(s, CE_UTF8);
		else tmp = Rf_mkChar(s);
		SET_STRING_ELT(ans, j, tmp);
	    }
#endif
	}
    }
    UNPROTECT(1);
    return (ans);
}

#ifdef Win32
static int Rwputenv(const wchar_t *nm, const wchar_t *val)
{
    wchar_t *buf;
    buf = (wchar_t *) malloc((wcslen(nm) + wcslen(val) + 2) * sizeof(wchar_t));
    if(!buf) return 1;
    /* previously wsprintfW, which had a limit of 1024 chars */
    wcscpy(buf, nm); wcscat(buf, L"="); wcscat(buf, val);
    if(_wputenv(buf)) return 1;
    /* no free here: storage remains in use */
    return 0;
}
#elif !defined(HAVE_SETENV) && defined(HAVE_PUTENV)
static int Rputenv(const char *nm, const char *val)
{
    char *buf;
    buf = (char *) malloc((strlen(nm) + strlen(val) + 2) * sizeof(char));
    if(!buf) return 1;
    sprintf(buf, "%s=%s", nm, val);
    if(putenv(buf)) return 1;
    /* no free here: storage remains in use */
    return 0;
}
#endif


SEXP attribute_hidden do_setenv(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* name_, rho::RObject* value_)
{
#if defined(HAVE_PUTENV) || defined(HAVE_SETENV)
    int i, n;
    SEXP ans, nm, vars;

    if (!Rf_isString(nm = name_))
	Rf_error(_("wrong type for argument"));
    if (!Rf_isString(vars = value_))
	Rf_error(_("wrong type for argument"));
    if(LENGTH(nm) != LENGTH(vars))
	Rf_error(_("wrong length for argument"));

    n = LENGTH(vars);
    PROTECT(ans = Rf_allocVector(LGLSXP, n));
#ifdef HAVE_SETENV
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = setenv(Rf_translateChar(STRING_ELT(nm, i)),
				 Rf_translateChar(STRING_ELT(vars, i)),
				 1) == 0;
#elif defined(Win32)
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = Rwputenv(Rf_wtransChar(STRING_ELT(nm, i)),
				   Rf_wtransChar(STRING_ELT(vars, i))) == 0;
#else
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = Rputenv(Rf_translateChar(STRING_ELT(nm, i)),
				  Rf_translateChar(STRING_ELT(vars, i))) == 0;
#endif
    UNPROTECT(1);
    return ans;
#else
    Rf_error(_("'Sys.setenv' is not available on this system"));
    return R_NilValue; /* -Wall */
#endif
}

SEXP attribute_hidden do_unsetenv(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_)
{
    int i, n;
    SEXP ans, vars;

    if (!Rf_isString(vars = x_))
	Rf_error(_("wrong type for argument"));
    n = LENGTH(vars);

#if defined(HAVE_UNSETENV) || defined(HAVE_PUTENV_UNSET) || defined(HAVE_PUTENV_UNSET2)
#ifdef HAVE_UNSETENV
    for (i = 0; i < n; i++) unsetenv(Rf_translateChar(STRING_ELT(vars, i)));
#elif defined(HAVE_PUTENV_UNSET)
    for (i = 0; i < n; i++) {
	char buf[1000];
	snprintf(buf, 1000, "%s",  Rf_translateChar(STRING_ELT(vars, i)));
	putenv(buf);
    }
#elif defined(HAVE_PUTENV_UNSET2)
# ifdef Win32
    for (i = 0; i < n; i++) {
	const wchar_t *w = Rf_wtransChar(STRING_ELT(vars, i));
	vector<wchar_t> bufv(2*wcslen(w));
	wchar_t* buf = &bufv[0];
	wcscpy(buf, w);
	wcscat(buf, L"=");
	_wputenv(buf);
    }
# else
    for (i = 0; i < n; i++) {
	char buf[1000];
	snprintf(buf, 1000, "%s=", Rf_translateChar(STRING_ELT(vars, i)));
	putenv(buf);
    }
# endif
#endif

#elif defined(HAVE_PUTENV) || defined(HAVE_SETENV)
    Rf_warning(_("this system cannot unset environment variables: setting to \"\""));
    n = LENGTH(vars);
    for (i = 0; i < n; i++) {
#ifdef HAVE_SETENV
	setenv(Rf_translateChar(STRING_ELT(vars, i)), "", 1);
#else
	Rputenv(Rf_translateChar(STRING_ELT(vars, i)), "");
#endif
    }

#else
    Rf_warning(_("'Sys.unsetenv' is not available on this system"));
#endif

    PROTECT(ans = Rf_allocVector(LGLSXP, n));
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = !getenv(Rf_translateChar(STRING_ELT(vars, i)));
    UNPROTECT(1);
    return ans;
}

#include <iconv.h>

#ifdef HAVE_ICONVLIST
static unsigned int cnt;

static int
count_one (unsigned int namescount, const char * const *names, void *data)
{
    cnt += namescount;
    return 0;
}

static int
write_one (unsigned int namescount, const char * const *names, void *data)
{
  unsigned int i;
  SEXP ans = (SEXP) data;

  for (i = 0; i < namescount; i++)
      SET_STRING_ELT(ans, cnt++, Rf_mkChar(names[i]));
  return 0;
}
#endif

#include "RBufferUtils.h"

/* iconv(x, from, to, sub, mark) */
SEXP attribute_hidden do_iconv(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* x_, rho::RObject* from_, rho::RObject* to_, rho::RObject* sub_, rho::RObject* mark_, rho::RObject* toRaw_)
{
    SEXP ans, x = x_, si;
    void * obj;
    const char *inbuf;
    char *outbuf;
    const char *sub;
    size_t inb, outb, res;
    R_StringBuffer cbuff = {nullptr, 0, MAXELTSIZE};
    Rboolean isRawlist = FALSE;

    if(Rf_isNull(x)) {  /* list locales */
#ifdef HAVE_ICONVLIST
	cnt = 0;
	iconvlist(count_one, NULL);
	PROTECT(ans = Rf_allocVector(STRSXP, cnt));
	cnt = 0;
	iconvlist(write_one, (void *)ans);
#else
	PROTECT(ans = nullptr);
#endif
    } else {
	int mark, toRaw;
	const char *from, *to;
	Rboolean isLatin1 = FALSE, isUTF8 = FALSE;

	if(!Rf_isString(from_) || Rf_length(from_) != 1)
	    Rf_error(_("invalid '%s' argument"), "from");
	from = R_CHAR(STRING_ELT(from_, 0)); /* ASCII */
	if(!Rf_isString(to_) || Rf_length(to_) != 1)
	    Rf_error(_("invalid '%s' argument"), "to");
	to = R_CHAR(STRING_ELT(to_, 0));
	if(!Rf_isString(sub_) || Rf_length(sub_) != 1)
	    Rf_error(_("invalid '%s' argument"), "sub");
	if(STRING_ELT(sub_, 0) == NA_STRING) sub = nullptr;
	else sub = Rf_translateChar(STRING_ELT(sub_, 0));
	mark = Rf_asLogical(mark_);
	if(mark == NA_LOGICAL)
	    Rf_error(_("invalid '%s' argument"), "mark");
	toRaw = Rf_asLogical(toRaw_);
	if(toRaw == NA_LOGICAL)
	    Rf_error(_("invalid '%s' argument"), "toRaw");
	/* some iconv's allow "UTF8", but libiconv does not */
	if(streql(from, "UTF8") || streql(from, "utf8") ) from = "UTF-8";
	if(streql(to, "UTF8") || streql(to, "utf8") ) to = "UTF-8";
	/* Should we do something about marked CHARSXPs in 'from = ""'? */
	if(streql(to, "UTF-8")) isUTF8 = TRUE;
	if(streql(to, "latin1") || streql(to, "ISO_8859-1")
	    || streql(to, "CP1252")) isLatin1 = TRUE;
	if(streql(to, "") && known_to_be_latin1) isLatin1 = TRUE;
	if(streql(to, "") && known_to_be_utf8) isUTF8 = TRUE;
	obj = Riconv_open(to, from);
	if(obj == iconv_t((-1)))
#ifdef Win32
	    Rf_error(_("unsupported conversion from '%s' to '%s' in codepage %d"),
		  from, to, localeCP);
#else
	    Rf_error(_("unsupported conversion from '%s' to '%s'"), from, to);
#endif
	isRawlist = Rboolean((TYPEOF(x) == VECSXP));
	if(isRawlist) {
	    if(toRaw)
		PROTECT(ans = Rf_duplicate(x));
	    else {
		PROTECT(ans = Rf_allocVector(STRSXP, LENGTH(x)));
		SHALLOW_DUPLICATE_ATTRIB(ans, x);
	    }
	} else {
	    if(TYPEOF(x) != STRSXP) {
		Riconv_close(obj);
		Rf_error(_("'x' must be a character vector"));
	    }
	    if(toRaw) {
		PROTECT(ans = Rf_allocVector(VECSXP, LENGTH(x)));
		SHALLOW_DUPLICATE_ATTRIB(ans, x);
	    } else
		PROTECT(ans = Rf_duplicate(x));
	}
	R_AllocStringBuffer(0, &cbuff);  /* 0 -> default */
	for(R_xlen_t i = 0; i < XLENGTH(x); i++) {
	    if (isRawlist) {
		si = VECTOR_ELT(x, i);
		if (TYPEOF(si) == NILSXP) {
		    if (!toRaw) SET_STRING_ELT(ans, i, NA_STRING);
		    continue;
		} else if (TYPEOF(si) != RAWSXP) {
		    Riconv_close(obj);
		    Rf_error(_("'x' must be a character vector or a list of NULL or raw vectors"));
		}
	    } else {
		si = STRING_ELT(x, i);
		if (si == NA_STRING) {
		    if(!toRaw) SET_STRING_ELT(ans, i, NA_STRING);
		    continue;
		}
	    }
	top_of_loop:
	    inbuf = isRawlist ? reinterpret_cast<const char *>(RAW(si)) : R_CHAR(si); 
	    inb = LENGTH(si);
	    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
	    /* First initialize output */
	    Riconv (obj, nullptr, nullptr, &outbuf, &outb);
	next_char:
	    /* Then convert input  */
	    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
	    *outbuf = '\0';
	    /* other possible error conditions are incomplete
	       and invalid multibyte chars */
	    if(res == size_t(-1) && errno == E2BIG) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    } else if(res == size_t(-1) && sub && 
		      (errno == EILSEQ || errno == EINVAL)) {
		/* it seems this gets thrown for non-convertible input too */
		if(streql(sub, "byte")) {
		    if(outb < 5) {
			R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
			goto top_of_loop;
		    }
		    snprintf(outbuf, 5, "<%02x>", static_cast<unsigned char>(*inbuf));
		    outbuf += 4; outb -= 4;
		} else {
		    size_t j;
		    if(outb < strlen(sub)) {
			R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
			goto top_of_loop;
		    }
		    memcpy(outbuf, sub, j = strlen(sub));
		    outbuf += j;
		    outb -= j;
		}
		inbuf++; inb--;
		goto next_char;
	    }

	    if(toRaw) {
		if(res != size_t(-1) && inb == 0) {
		    size_t nout = cbuff.bufsize - 1 - outb;
		    SEXP el = Rf_allocVector(RAWSXP, nout);
		    memcpy(RAW(el), cbuff.data, nout);
		    SET_VECTOR_ELT(ans, i, el);
		} /* otherwise is already NULL */
	    } else {
		if(res != size_t(-1) && inb == 0) {
		    cetype_t ienc = CE_NATIVE;

		    size_t nout = cbuff.bufsize - 1 - outb;
		    if(mark) {
			if(isLatin1) ienc = CE_LATIN1;
			else if(isUTF8) ienc = CE_UTF8;
		    }
		    SET_STRING_ELT(ans, i,
				   Rf_mkCharLenCE(cbuff.data, int(nout), ienc));
		} else SET_STRING_ELT(ans, i, NA_STRING);
	    }
	}
	Riconv_close(obj);
	R_FreeStringBuffer(&cbuff);
    }
    UNPROTECT(1);
    return ans;
}

cetype_t Rf_getCharCE(SEXP x)
{
    if(TYPEOF(x) != CHARSXP)
	Rf_error(_("'%s' must be called on a CHARSXP"), "getCharCE");
    if(IS_UTF8(x)) return CE_UTF8;
    else if(IS_LATIN1(x)) return CE_LATIN1;
    else if(IS_BYTES(x)) return CE_BYTES;
    else return CE_NATIVE;
}


void * Riconv_open (const char* tocode, const char* fromcode)
{
#if defined Win32 || __APPLE__
// These two support "utf8"
# ifdef Win32
    const char *cp = "ASCII";
#  ifndef SUPPORT_UTF8_WIN32 /* Always, at present */
    char to[20] = "";
    if (localeCP > 0) {snprintf(to, 20, "CP%d", localeCP); cp = to;}
#  endif
# else /* __APPLE__ */
    const char *cp = "UTF-8";
    if (latin1locale) cp = "ISO-8859-1";
    else if (!utf8locale) cp = locale2charset(NULL);
# endif
    if (!*tocode && !*fromcode) return iconv_open(cp, cp);
    if(!*tocode)  return iconv_open(cp, fromcode);
    else if(!*fromcode) return iconv_open(tocode, cp);
    else return iconv_open(tocode, fromcode);
#else
// "utf8" is not valid but people keep on using it
    const char *to = tocode, *from = fromcode;
    if(strcasecmp(tocode, "utf8") == 0) to = "UTF-8";
    if(strcasecmp(fromcode, "utf8") == 0) from = "UTF-8";
    return iconv_open(to, from);
#endif
}

/* Should be defined in config.h, but prior to 2.13.0 was only checked
   if the NLS was enabled  */
#ifndef ICONV_CONST
# define ICONV_CONST
#endif

size_t Riconv (void *cd, const char **inbuf, size_t *inbytesleft,
	       char **outbuf, size_t *outbytesleft)
{
    /* here libiconv has const char **, glibc has char ** for inbuf */
    return iconv(static_cast<iconv_t>(cd), const_cast<ICONV_CONST char **>(inbuf), inbytesleft,
		 outbuf, outbytesleft);
}

int Riconv_close (void *cd)
{
    return iconv_close(iconv_t( cd));
}

enum nttype_t {
    NT_NONE        = 0, /* no translation to native encoding is needed */
    NT_FROM_UTF8   = 1, /* need to translate from UTF8 */
    NT_FROM_LATIN1 = 2, /* need to translate from latin1 */
};

/* Decides whether translation to native encoding is needed. */
static R_INLINE nttype_t needsTranslation(rho::RObject* x) {

    if (IS_ASCII(x)) return NT_NONE;
    if (IS_UTF8(x)) {
	if (utf8locale || x == NA_STRING) return NT_NONE;
	return NT_FROM_UTF8;
    }
    if (IS_LATIN1(x)) {
	if (x == NA_STRING || latin1locale) return NT_NONE;
	return NT_FROM_LATIN1;
    }
    if (IS_BYTES(x))
	Rf_error(_("translating strings with \"bytes\" encoding is not allowed"));
    return NT_NONE;
}

static void *latin1_obj = nullptr, *utf8_obj=nullptr, *ucsmb_obj=nullptr,
    *ucsutf8_obj=nullptr;

/* Translates string in "ans" to native encoding returning it as string
   buffer "cbuff" */
static void translateToNative(const char *ans, R_StringBuffer *cbuff,
			      nttype_t ttype) {

    if (ttype == NT_NONE)
	Rf_error(_("internal error: no translation needed"));

    void * obj;
    const char *inbuf, *from;
    char *outbuf;
    size_t inb, outb;
    int res;

    if(ttype == NT_FROM_LATIN1) {
	if(!latin1_obj) {
#ifdef HAVE_ICONV_CP1252
	    from = "CP1252";
#else
	    from = "latin1";
#endif
	    obj = Riconv_open("", from);
	    /* should never happen */
	    if(obj == (void *)(-1))
#ifdef Win32
		Rf_error(_("unsupported conversion from '%s' in codepage %d"),
		      from, localeCP);
#else
		Rf_error(_("unsupported conversion from '%s' to '%s'"),
		      from, "");
#endif
	    latin1_obj = obj;
	}
	obj = latin1_obj;
    } else {
	if(!utf8_obj) {
	    obj = Riconv_open("", "UTF-8");
	    /* should never happen */
	    if(obj == (void *)(-1))
#ifdef Win32
		Rf_error(_("unsupported conversion from '%s' in codepage %d"),
		      "UTF-8", localeCP);
#else
		Rf_error(_("unsupported conversion from '%s' to '%s'"),
		      "UTF-8", "");
#endif
	    utf8_obj = obj;
	}
	obj = utf8_obj;
    }

    R_AllocStringBuffer(0, cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff->data; outb = cbuff->bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = int(Riconv(obj, &inbuf , &inb, &outbuf, &outb));
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	goto top_of_loop;
    } else if(res == -1 && (errno == EILSEQ || errno == EINVAL)) {
	if(outb < 13) {
	    R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	    goto top_of_loop;
	}
	if (ttype == NT_FROM_UTF8) {
	    /* if starting in UTF-8, use \uxxxx */
	    /* This must be the first byte */
	    size_t clen;
	    wchar_t wc;
	    Rwchar_t ucs;
	    clen = Rf_utf8toucs(&wc, inbuf);
	    if(clen > 0 && inb >= clen) {
	    	if (IS_HIGH_SURROGATE(wc))
	    	    ucs = utf8toucs32(wc, inbuf);
	    	else
	    	    ucs = (Rwchar_t) wc;
		inbuf += clen; inb -= clen;
		if(ucs < 65536) {
		// gcc 7 objects to this with unsigned int
		    snprintf(outbuf, 9, "<U+%04X>", (unsigned short) ucs);
		    outbuf += 8; outb -= 8;
		} else {
		    // Rwchar_t is usually unsigned int, but wchar_t need not be
		    snprintf(outbuf, 13, "<U+%08X>", (unsigned int) ucs);
		    outbuf += 12; outb -= 12;
		}
	    } else {
		snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
		outbuf += 4; outb -= 4;
		inbuf++; inb--;
	    }
	} else {
	    snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	    outbuf += 4; outb -= 4;
	    inbuf++; inb--;
	}
	goto next_char;
    }
    *outbuf = '\0';
}


/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const char *Rf_translateChar(SEXP x)
{
    if(TYPEOF(x) != CHARSXP)
	Rf_error(_("'%s' must be called on a CHARSXP, but got '%s'"),
	      "translateChar", Rf_type2char(TYPEOF(x)));
    nttype_t t = needsTranslation(x);
    const char *ans = R_CHAR(x);
    assert(ans != nullptr);
    if (t == NT_NONE) return ans;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    translateToNative(ans, &cbuff, t);

    size_t res = strlen(cbuff.data) + 1;
    char *p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    assert(p != nullptr);
    return p;
}

/* (Install)s (Tr)anslated (Char)acter String */
SEXP Rf_installTrChar(SEXP x)
{
    if(TYPEOF(x) != CHARSXP)
	Rf_error(_("'%s' must be called on a CHARSXP, but got '%s'"),
	      "installTrChar", Rf_type2char(TYPEOF(x)));
    nttype_t t = needsTranslation(x);
    if (t == NT_NONE) return Rf_installNoTrChar(x);

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    translateToNative(R_CHAR(x), &cbuff, t);

    SEXP Sans = Rf_install(cbuff.data);
    R_FreeStringBuffer(&cbuff);
    return Sans;
}

/* now translates, now same as installTrChar */
SEXP Rf_installChar(SEXP x)
{
    return Rf_installTrChar(x);
}

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const char *Rf_translateChar0(SEXP x)
{
    if(TYPEOF(x) != CHARSXP)
	Rf_error(_("'%s' must be called on a CHARSXP"), "translateChar0");
    if(IS_BYTES(x)) return R_CHAR(x);
    return Rf_translateChar(x);
}

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const char *Rf_translateCharUTF8(SEXP x)
{
    void *obj;
    const char *inbuf, *ans = R_CHAR(x);
    char *outbuf, *p, *from = (char *) "";
    size_t inb, outb, res;
    R_StringBuffer cbuff = {nullptr, 0, MAXELTSIZE};

    if(TYPEOF(x) != CHARSXP)
	Rf_error(_("'%s' must be called on a CHARSXP, but got '%s'"),
	      "translateCharUTF8", Rf_type2char(TYPEOF(x)));
    if(x == NA_STRING) return ans;
    if(IS_UTF8(x)) return ans;
    if(IS_ASCII(x)) return ans;
    if(IS_BYTES(x))
	Rf_error(_("translating strings with \"bytes\" encoding is not allowed"));

    if (IS_LATIN1(x))
#ifdef HAVE_ICONV_CP1252
	from = (char *) "CP1252";
#else
	from = (char *) "latin1";
#endif
    obj = Riconv_open("UTF-8", from);
    if(obj == reinterpret_cast<void *>((-1))) 
#ifdef Win32
	Rf_error(_("unsupported conversion from '%s' in codepage %d"),
	      from, localeCP);
#else
	Rf_error(_("unsupported conversion from '%s' to '%s'"),
	      from, "UTF-8");
#endif
    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, nullptr, nullptr, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == size_t(-1) && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == size_t(-1) && (errno == EILSEQ || errno == EINVAL)) {
	if(outb < 5) {
	    R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	    goto top_of_loop;
	}
	snprintf(outbuf, 5, "<%02x>", static_cast<unsigned char>(*inbuf));
	outbuf += 4; outb -= 4;
	inbuf++; inb--;
	goto next_char;
    }
    *outbuf = '\0';
    Riconv_close(obj);
    res = strlen(cbuff.data) + 1;
    p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}


#ifdef Win32
static const char TO_WCHAR[] = "UCS-2LE";
#else
# ifdef WORDS_BIGENDIAN
static const char TO_WCHAR[] = "UCS-4BE";
# else
static const char TO_WCHAR[] = "UCS-4LE";
# endif
#endif

static void *latin1_wobj = nullptr, *utf8_wobj=nullptr;

/* Translate from current encoding to wchar_t = UCS-2/4
   NB: that wchar_t is UCS-4 is an assumption, but not easy to avoid.
*/

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
attribute_hidden /* but not hidden on Windows, where it was used in tcltk.cpp */
const wchar_t *Rf_wtransChar(SEXP x)
{
    void * obj;
    const char *inbuf, *ans = R_CHAR(x), *from;
    char *outbuf;
    wchar_t *p;
    size_t inb, outb, res, top;
    Rboolean knownEnc = FALSE;
    R_StringBuffer cbuff = {nullptr, 0, MAXELTSIZE};

    if(TYPEOF(x) != CHARSXP)
	Rf_error(_("'%s' must be called on a CHARSXP"), "wtransChar");

    if(IS_BYTES(x))
	Rf_error(_("translating strings with \"bytes\" encoding is not allowed"));

    if(IS_LATIN1(x)) {
	if(!latin1_wobj) {
#ifdef HAVE_ICONV_CP1252
	    from = "CP1252";
#else
	    from = "latin1";
#endif
	    obj = Riconv_open(TO_WCHAR, from);
	    if(obj == reinterpret_cast<void *>((-1)))
		Rf_error(_("unsupported conversion from '%s' to '%s'"),
		      from, TO_WCHAR);
	    latin1_wobj = obj;
	} else
	    obj = latin1_wobj;
	knownEnc = TRUE;
    } else if(IS_UTF8(x)) {
	if(!utf8_wobj) {
	    obj = Riconv_open(TO_WCHAR, "UTF-8");
	    if(obj == reinterpret_cast<void *>((-1))) 
		Rf_error(_("unsupported conversion from '%s' to '%s'"),
		      "UTF-8", TO_WCHAR);
	    utf8_wobj = obj;
	} else
	    obj = utf8_wobj;
	knownEnc = TRUE;
    } else {
	obj = Riconv_open(TO_WCHAR, "");
	if(obj == reinterpret_cast<void *>((-1)))
#ifdef Win32
	    Rf_error(_("unsupported conversion to '%s' from codepage %d"),
		  TO_WCHAR, localeCP);
#else
	    Rf_error(_("unsupported conversion from '%s' to '%s'"), "", TO_WCHAR);
#endif
    }

    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff.data; top = outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, nullptr, nullptr, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(int(res) == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(int(res) == -1 && (errno == EILSEQ || errno == EINVAL)) {
	if(outb < 5) {
	    R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	    goto top_of_loop;
	}
	snprintf(outbuf, 5, "<%02x>", static_cast<unsigned char>(*inbuf));
	outbuf += 4; outb -= 4;
	inbuf++; inb--;
	goto next_char;
	/* if(!knownEnc) Riconv_close(obj);
	   Rf_error(_("invalid input in wtransChar")); */
    }
    if(!knownEnc) Riconv_close(obj);
    res = (top - outb);
    /* terminator is 2 or 4 null bytes */
    p = reinterpret_cast<wchar_t *>(R_alloc(res+4, 1));
    memset(p, 0, res+4);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}


#include <R_ext/GraphicsEngine.h>
/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst)
{
    void * obj;
    const char *inbuf;
    char *outbuf, *p;
    size_t inb, outb, res, top;
    const char *tocode = nullptr, *fromcode = nullptr;
#ifdef Win32
    char buf[20];
#endif
    R_StringBuffer cbuff = {nullptr, 0, MAXELTSIZE};

    /* We can only encode from Symbol to UTF-8 */
    if(ce_in == ce_out || ce_out == CE_SYMBOL ||
       ce_in == CE_ANY || ce_out == CE_ANY) return x;
    if(ce_in == CE_SYMBOL) {
	if(ce_out == CE_UTF8) {
	    size_t nc = 3*strlen(x)+1; /* all in BMP */
	    p = R_alloc(nc, 1);
	    Rf_AdobeSymbol2utf8(p, x, int(nc));
	    return p;
	} else return x;
    }
    if(utf8locale && ce_in == CE_NATIVE && ce_out == CE_UTF8) return x;
    if(utf8locale && ce_out == CE_NATIVE && ce_in == CE_UTF8) return x;
    if(latin1locale && ce_in == CE_NATIVE && ce_out == CE_LATIN1) return x;
    if(latin1locale && ce_out == CE_NATIVE && ce_in == CE_LATIN1) return x;

    if(Rf_strIsASCII(x)) return x;

    switch(ce_in) {
#ifdef Win32
    case CE_NATIVE:
	{
	    /* Looks like CP1252 is treated as Latin-1 by iconv */
	    snprintf(buf, 20, "CP%d", localeCP);
	    fromcode = buf;
	    break;
	}
    case CE_LATIN1: fromcode = "CP1252"; break;
#else
    case CE_NATIVE: fromcode = ""; break;
    case CE_LATIN1: fromcode = "latin1"; break; /* FIXME: allow CP1252? */
#endif
    case CE_UTF8:   fromcode = "UTF-8"; break;
    default: return x;
    }

    switch(ce_out) {
 #ifdef Win32
    case CE_NATIVE:
	{
	    /* avoid possible misidentification of CP1250 as LATIN-2 */
	    snprintf(buf, 20, "CP%d", localeCP);
	    tocode = buf;
	    break;
	}
#else
    case CE_NATIVE: tocode = ""; break;
#endif
    case CE_LATIN1: tocode = "latin1"; break;
    case CE_UTF8:   tocode = "UTF-8"; break;
    default: return x;
    }

    obj = Riconv_open(tocode, fromcode);
    if(obj == reinterpret_cast<void *>((-1))) return x;
    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = x; inb = strlen(inbuf);
    outbuf = cbuff.data; top = outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, nullptr, nullptr, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == size_t(-1) && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == size_t(-1) && (errno == EILSEQ || errno == EINVAL)) {
	switch(subst) {
	case 1: /* substitute hex */
	    if(outb < 5) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    snprintf(outbuf, 5, "<%02x>", static_cast<unsigned char>(*inbuf));
	    outbuf += 4; outb -= 4;
	    inbuf++; inb--;
	    goto next_char;
	    break;
	case 2: /* substitute . */
	    if(outb < 1) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    *outbuf++ = '.'; inbuf++; outb--; inb--;
	    goto next_char;
	    break;
	case 3: /* substitute ? */
	    if(outb < 1) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    *outbuf++ = '?'; inbuf++; outb--; inb--;
	    goto next_char;
	    break;
	default: /* skip byte */
	    inbuf++; inb--;
	    goto next_char;
	}
    }
    Riconv_close(obj);
    *outbuf = '\0';
    res = (top-outb)+1; /* strlen(cbuff.data) + 1; */
    p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}

#ifdef Win32
/* A version avoiding R_alloc for use in the Rgui editor */
void reEnc2(const char *x, char *y, int ny,
	    cetype_t ce_in, cetype_t ce_out, int subst)
{
    void * obj;
    const char *inbuf;
    char *outbuf;
    size_t inb, outb, res, top;
    char *tocode = NULL, *fromcode = NULL;
    char buf[20];
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    strncpy(y, x, ny);

    if(ce_in == ce_out || ce_in == CE_ANY || ce_out == CE_ANY) return;
    if(utf8locale && ce_in == CE_NATIVE && ce_out == CE_UTF8) return;
    if(utf8locale && ce_out == CE_NATIVE && ce_in == CE_UTF8) return;
    if(latin1locale && ce_in == CE_NATIVE && ce_out == CE_LATIN1) return;
    if(latin1locale && ce_out == CE_NATIVE && ce_in == CE_LATIN1) return;

    if(Rf_strIsASCII(x)) return;

    switch(ce_in) {
    case CE_NATIVE:
	{
	    /* Looks like CP1252 is treated as Latin-1 by iconv */
	    snprintf(buf, 20, "CP%d", localeCP);
	    fromcode = buf;
	    break;
	}
    case CE_LATIN1: fromcode = "CP1252"; break;
    case CE_UTF8:   fromcode = "UTF-8"; break;
    default: return;
    }

    switch(ce_out) {
    case CE_NATIVE:
	{
	    /* avoid possible misidentification of CP1250 as LATIN-2 */
	    snprintf(buf, 20, "CP%d", localeCP);
	    tocode = buf;
	    break;
	}
    case CE_LATIN1: tocode = "latin1"; break;
    case CE_UTF8:   tocode = "UTF-8"; break;
    default: return;
    }

    obj = Riconv_open(tocode, fromcode);
    if(obj == (void *)(-1)) return;
    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = x; inb = strlen(inbuf);
    outbuf = cbuff.data; top = outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == -1 && (errno == EILSEQ || errno == EINVAL)) {
	switch(subst) {
	case 1: /* substitute hex */
	    if(outb < 5) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	    outbuf += 4; outb -= 4;
	    inbuf++; inb--;
	    goto next_char;
	    break;
	case 2: /* substitute . */
	    if(outb < 1) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    *outbuf++ = '.'; inbuf++; outb--; inb--;
	    goto next_char;
	    break;
	case 3: /* substitute ? */
	    if(outb < 1) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    *outbuf++ = '?'; inbuf++; outb--; inb--;
	    goto next_char;
	    break;
	default: /* skip byte */
	    inbuf++; inb--;
	    goto next_char;
	}
    }
    Riconv_close(obj);
    *outbuf = '\0';
    res = (top-outb)+1; /* strlen(cbuff.data) + 1; */
    if (res > ny) Rf_error("converted string too long for buffer");
    memcpy(y, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
}
#endif

#ifdef Win32
/* A version avoiding R_alloc for use in the Rgui editor */
void reEnc2(const char *x, char *y, int ny,
	    cetype_t ce_in, cetype_t ce_out, int subst)
{
    void * obj;
    const char *inbuf;
    char *outbuf;
    size_t inb, outb, res, top;
    char *tocode = NULL, *fromcode = NULL;
    char buf[20];
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    strncpy(y, x, ny);
    y[ny - 1] = '\0';

    if(ce_in == ce_out || ce_in == CE_ANY || ce_out == CE_ANY) return;
    if(utf8locale && ce_in == CE_NATIVE && ce_out == CE_UTF8) return;
    if(utf8locale && ce_out == CE_NATIVE && ce_in == CE_UTF8) return;
    if(latin1locale && ce_in == CE_NATIVE && ce_out == CE_LATIN1) return;
    if(latin1locale && ce_out == CE_NATIVE && ce_in == CE_LATIN1) return;

    if(Rf_strIsASCII(x)) return;

    switch(ce_in) {
    case CE_NATIVE:
	{
	    /* Looks like CP1252 is treated as Latin-1 by iconv */
	    snprintf(buf, 20, "CP%d", localeCP);
	    fromcode = buf;
	    break;
	}
    case CE_LATIN1: fromcode = "CP1252"; break;
    case CE_UTF8:   fromcode = "UTF-8"; break;
    default: return;
    }

    switch(ce_out) {
    case CE_NATIVE:
	{
	    /* avoid possible misidentification of CP1250 as LATIN-2 */
	    snprintf(buf, 20, "CP%d", localeCP);
	    tocode = buf;
	    break;
	}
    case CE_LATIN1: tocode = "latin1"; break;
    case CE_UTF8:   tocode = "UTF-8"; break;
    default: return;
    }

    obj = Riconv_open(tocode, fromcode);
    if(obj == (void *)(-1)) return;
    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = x; inb = strlen(inbuf);
    outbuf = cbuff.data; top = outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == -1 && (errno == EILSEQ || errno == EINVAL)) {
	switch(subst) {
	case 1: /* substitute hex */
	    if(outb < 5) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	    outbuf += 4; outb -= 4;
	    inbuf++; inb--;
	    goto next_char;
	    break;
	case 2: /* substitute . */
	    if(outb < 1) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    *outbuf++ = '.'; inbuf++; outb--; inb--;
	    goto next_char;
	    break;
	case 3: /* substitute ? */
	    if(outb < 1) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    *outbuf++ = '?'; inbuf++; outb--; inb--;
	    goto next_char;
	    break;
	default: /* skip byte */
	    inbuf++; inb--;
	    goto next_char;
	}
    }
    Riconv_close(obj);
    *outbuf = '\0';
    res = (top-outb)+1; /* strlen(cbuff.data) + 1; */
    if (res > ny) Rf_error("converted string too long for buffer");
    memcpy(y, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
}
#endif

void attribute_hidden
invalidate_cached_recodings(void)
{
    if (latin1_obj) {
	Riconv_close(latin1_obj);
	latin1_obj = nullptr;
    }
    if (utf8_obj) {
	Riconv_close(utf8_obj);
	utf8_obj = nullptr;
    }
    if (ucsmb_obj) {
	Riconv_close(ucsmb_obj);
	ucsmb_obj = nullptr;
    }
#ifdef Win32
    if (latin1_wobj) {
	Riconv_close(latin1_wobj);
	latin1_wobj = nullptr;
    }
    if (utf8_wobj) {
	Riconv_close(utf8_wobj);
	utf8_wobj = nullptr;
    }
#endif
}


/* in C11 these could use char32_t */
#ifdef WORDS_BIGENDIAN
static const char UNICODE[] = "UCS-4BE";
#else
static const char UNICODE[] = "UCS-4LE";
#endif

/* used in gram.cpp and devX11.cpp */
size_t Rf_ucstomb(char *s, const unsigned int wc)
{
    vector<char> bufv(MB_CUR_MAX+1);
    char* buf = &bufv[0];
    void    *cd = nullptr ;
    unsigned int  wcs[2];
    const char *inbuf = reinterpret_cast<const char *>(wcs);
    size_t   inbytesleft = sizeof(unsigned int); /* better be 4 */
    char    *outbuf = buf;
    size_t   outbytesleft = sizeof(buf);
    size_t   status;

    if(wc == 0) {*s = '\0'; return 1;}

    memset(buf, 0, bufv.size());
    memset(wcs, 0, sizeof(wcs));
    wcs[0] = wc;

    if(ucsmb_obj == nullptr) {
	if(reinterpret_cast<void *>((-1)) == (cd = Riconv_open("", UNICODE))) {
#ifndef  Win32
	    char tocode[128];
	    /* locale set fuzzy case */
	    strncpy(tocode, locale2charset(nullptr), sizeof(tocode));
	    tocode[sizeof(tocode) - 1] = '\0';
	    if(reinterpret_cast<void *>((-1)) == (cd = Riconv_open(tocode, UNICODE)))
		return size_t((-1));
#else
	    return size_t((-1));
#endif
	}
	ucsmb_obj = cd;
    }

    status = Riconv(ucsmb_obj, &inbuf, &inbytesleft, &outbuf, &outbytesleft);

    if (status == size_t(-1)) {
	switch(errno){
	case EINVAL:
	    return size_t(-2);
	case EILSEQ:
	    return size_t(-1);
	case E2BIG:
	    break;
	default:
	    errno = EILSEQ;
	    return size_t(-1);
	}
    }
    buf[MB_CUR_MAX] = '\0'; /* safety measure */
    strcpy(s, buf);
    return strlen(buf);
}

/* used in plot.cpp for non-UTF-8 MBCS */
size_t attribute_hidden
Rf_mbtoucs(unsigned int *wc, const char *s, size_t n)
{
    unsigned int  wcs[2];
    char     buf[16];
    void    *cd;
    const char *inbuf = s;
    size_t   inbytesleft = strlen(s);
    char    *outbuf = reinterpret_cast<char *>(wcs);
    size_t   outbytesleft = sizeof(buf);
    size_t   status;

    if(s[0] == 0) {*wc = 0; return 1;}

    if(reinterpret_cast<void *>((-1)) == (cd = Riconv_open(UNICODE, ""))) return size_t((-1));
    status = Riconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);

    if (status == size_t(-1)) {
	switch(errno){
	case EINVAL:
	    Riconv_close(cd);
	    return size_t(-2);
	case EILSEQ:
	    Riconv_close(cd);
	    return size_t(-1);
	case E2BIG:
	    break;
	default:
	    Riconv_close(cd);
	    errno = EILSEQ;
	    return size_t(-1);
	}
    }
    Riconv_close(cd);
    *wc = wcs[0];
    return 1;
}

/* made available for use in graphics devices */
size_t Rf_ucstoutf8(char *s, const unsigned int wc)
{
    char     buf[16];
    void    *cd = nullptr ;
    unsigned int  wcs[2];
    const char *inbuf = reinterpret_cast<const char *>(wcs);
    size_t   inbytesleft = sizeof(unsigned int); /* better be 4 */
    char    *outbuf = buf;
    size_t   outbytesleft = sizeof(buf);
    size_t   status;

    if(wc == 0) {*s = '\0'; return 1;}

    memset(buf, 0, sizeof(buf));
    wcs[0] = wc; wcs[1] = 0;

    if(ucsutf8_obj == nullptr) {
	if(reinterpret_cast<void *>((-1)) == (cd = Riconv_open("UTF-8", UNICODE))) {
	    Rf_error(_("unsupported conversion from '%s' to '%s'"),
		  UNICODE, "UTF-8");
	    return size_t((-1));
	}
	ucsutf8_obj = cd;
    }

    status = Riconv(ucsutf8_obj, &inbuf, &inbytesleft, &outbuf, &outbytesleft);

    if (status == size_t(-1)) {
	switch(errno){
	case E2BIG:
	    break;
	default:
	    Rf_error(_("invalid Unicode point %u"), wc);
	    return (size_t) -1; // Not reached
	}
    }
    *outbuf = '\0';
    strcpy(s, buf);
    return strlen(buf);
}

/* moved from src/unix/sys-unix.cpp and src/gnuwin32/extra.c */

#ifdef HAVE_STAT
# ifdef HAVE_ACCESS
#  ifdef HAVE_UNISTD_H
#   include <unistd.h>
#  endif
# endif

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h> /* For GetShortPathName */
#endif

#if !defined(S_IFDIR) && defined(__S_IFDIR)
# define S_IFDIR __S_IFDIR
#endif

static int isDir(const char *path)
{
#ifdef Win32
    struct _stati64 sb;
#else
    struct stat sb;
#endif
    int isdir = 0;
    if(!path) return 0;
#ifdef Win32
    if(_stati64(path, &sb) == 0) {
#else
    if(stat(path, &sb) == 0) {
#endif
	isdir = (sb.st_mode & S_IFDIR) > 0; /* is a directory */
#ifdef HAVE_ACCESS
	/* We want to know if the directory is writable by this user,
	   which mode does not tell us */
	isdir &= (access(path, W_OK) == 0);
#endif
    }
    return isdir;
}
#else
static int isDir(const char *path)
{
    return 1;
}
#endif /* HAVE_STAT */

#if !HAVE_DECL_MKDTEMP
extern char * mkdtemp (char *template);
#endif

#ifdef Win32
# include <ctype.h>
#endif

void R_reInitTempDir(int die_on_fail)
{
    char *tmp, tmp1[PATH_MAX+11], *p;
    const char* tm;
#ifdef Win32
    char tmp2[PATH_MAX];
    int hasspace = 0;
#endif

#define ERROR_MAYBE_DIE(MSG_)			\
    if(die_on_fail)				\
	R_Suicide(MSG_);			\
    else					\
	Rf_errorcall(R_NilValue, MSG_)

    if(R_TempDir) return; /* someone else set it */
    tmp = nullptr; /* getenv("R_SESSION_TMPDIR");   no longer set in R.sh */
    if (!tmp) {
	tm = getenv("TMPDIR");
	if (!isDir(tm)) {
	    tm = getenv("TMP");
	    if (!isDir(tm)) {
		tm = getenv("TEMP");
		if (!isDir(tm))
#ifdef Win32
		    tm = getenv("R_USER"); /* this one will succeed */
#else
		    tm = "/tmp";
#endif
	    }
	}
#ifdef Win32
	/* make sure no spaces in path */
	for (p = tm; *p; p++)
	    if (isspace(*p)) { hasspace = 1; break; }
	if (hasspace) {
	    GetShortPathName(tm, tmp2, MAX_PATH);
	    tm = tmp2;
	}
	snprintf(tmp1, PATH_MAX+11, "%s\\RtmpXXXXXX", tm);
#else
	snprintf(tmp1, PATH_MAX+11, "%s/RtmpXXXXXX", tm);
#endif
	tmp = mkdtemp(tmp1);
	if(!tmp) {
	    ERROR_MAYBE_DIE(_("cannot create 'R_TempDir'"));
	}
#ifndef Win32
# ifdef HAVE_SETENV
	if(setenv("R_SESSION_TMPDIR", tmp, 1))
	    Rf_errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
# elif defined(HAVE_PUTENV)
	{
	    size_t len = strlen(tmp) + 20;
	    char * buf = (char *) malloc((len) * sizeof(char));
	    if(buf) {
		snprintf(buf, len, "R_SESSION_TMPDIR=%s", tmp);
		if(putenv(buf))
		    Rf_errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
		/* no free here: storage remains in use */
	    } else
		Rf_errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
	}
# endif
#endif
    }

    size_t len = strlen(tmp) + 1;
    p = static_cast<char *>(malloc(len));
    if(!p)
	ERROR_MAYBE_DIE(_("cannot allocate 'R_TempDir'"));
    else {
	R_TempDir = p;
	strcpy(R_TempDir, tmp);
	Sys_TempDir = R_TempDir;
    }
}

void attribute_hidden Rf_InitTempDir() {
    R_reInitTempDir(/* die_on_fail = */ TRUE);
}

char * R_tmpnam(const char * prefix, const char * tempdir)
{
    return R_tmpnam2(prefix, tempdir, "");
}

/* NB for use with multicore: parent and all children share the same
   session directory and run in parallel.
   So as from 2.14.1, we make sure getpid() is part of the process.
*/
char * R_tmpnam2(const char *prefix, const char *tempdir, const char *fileext)
{
    char tm[PATH_MAX], *res;
    unsigned int n, done = 0, pid = getpid();
#ifdef Win32
    char filesep[] = "\\";
#else
    char filesep[] = "/";
#endif

    if(!prefix) prefix = "";	/* NULL */
    if(!fileext) fileext = "";  /*  "   */

#if RAND_MAX > 16777215
#define RAND_WIDTH 8
#else
#define RAND_WIDTH 12
#endif

    if(strlen(tempdir) + 1 + strlen(prefix) + RAND_WIDTH + strlen(fileext) >= PATH_MAX)
	Rf_error(_("temporary name too long"));

    for (n = 0; n < 100; n++) {
	/* try a random number at the end.  Need at least 6 hex digits */
#if RAND_MAX > 16777215
	snprintf(tm, PATH_MAX, "%s%s%s%x%x%s", tempdir, filesep, prefix, pid, rand(), fileext);
#else
	snprintf(tm, PATH_MAX, "%s%s%s%x%x%x%s", tempdir, filesep, prefix, pid, rand(), rand(), fileext);
#endif
	if(!R_FileExists(tm)) {
	    done = 1;
	    break;
	}
    }
    if(!done)
	Rf_error(_("cannot find unused tempfile name"));
    res = static_cast<char *>(malloc((strlen(tm)+1) * sizeof(char)));
    if(!res)
	Rf_error(_("allocation failed in R_tmpnam2"));
    strcpy(res, tm);
    return res;
}

SEXP attribute_hidden do_proctime(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op)
{
    SEXP ans, nm;

    PROTECT(ans = Rf_allocVector(REALSXP, 5));
    PROTECT(nm = Rf_allocVector(STRSXP, 5));
    R_getProcTime(REAL(ans));
    SET_STRING_ELT(nm, 0, Rf_mkChar("user.self"));
    SET_STRING_ELT(nm, 1, Rf_mkChar("sys.self"));
    SET_STRING_ELT(nm, 2, Rf_mkChar("elapsed"));
    SET_STRING_ELT(nm, 3, Rf_mkChar("user.child"));
    SET_STRING_ELT(nm, 4, Rf_mkChar("sys.child"));
    Rf_setAttrib(ans, Symbols::NamesSymbol, nm);
    Rf_setAttrib(ans, Symbols::ClassSymbol, Rf_mkString("proc_time"));
    UNPROTECT(2);
    return ans;
}

void attribute_hidden resetTimeLimits()
{
    double data[5];
    R_getProcTime(data);

    elapsedLimit = (elapsedLimitValue > 0) ? data[2] + elapsedLimitValue : -1.0;
    if (elapsedLimit2 > 0.0 &&
	(elapsedLimit <= 0.0 || elapsedLimit2 < elapsedLimit))
	elapsedLimit = elapsedLimit2;

#ifdef Win32
    cpuLimit = (cpuLimitValue > 0) ? data[0] + data[1] + cpuLimitValue : -1.0;
#else
    cpuLimit = (cpuLimitValue > 0) ? data[0] + data[1] + data[3] + data[4] + cpuLimitValue : -1.0;
#endif
    if (cpuLimit2 > 0.0 && (cpuLimit <= 0.0 || cpuLimit2 < cpuLimit))
	cpuLimit = cpuLimit2;
}

SEXP attribute_hidden
do_setTimeLimit(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* cpu_, rho::RObject* elapsed_, rho::RObject* transient_)
{
    double cpu, elapsed, old_cpu = cpuLimitValue,
	old_elapsed = elapsedLimitValue;
    int transient;

    cpu = Rf_asReal(cpu_);
    elapsed = Rf_asReal(elapsed_);
    transient = Rf_asLogical(transient_);

    if (R_FINITE(cpu) && cpu > 0) cpuLimitValue = cpu; else cpuLimitValue = -1;

    if (R_FINITE(elapsed) && elapsed > 0) elapsedLimitValue = elapsed;
    else elapsedLimitValue = -1;

    resetTimeLimits();

    if (transient == TRUE) {
	cpuLimitValue = old_cpu;
	elapsedLimitValue = old_elapsed;
    }

    return R_NilValue;
}

SEXP attribute_hidden
do_setSessionTimeLimit(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* cpu_, rho::RObject* elapsed_)
{
    double cpu, elapsed, data[5];

    cpu = Rf_asReal(cpu_);
    elapsed = Rf_asReal(elapsed_);
    R_getProcTime(data);

    if (R_FINITE(cpu) && cpu > 0)
#ifdef Win32
	cpuLimit2 = cpu + data[0] + data[1];
#else
	cpuLimit2 = cpu + data[0] + data[1] + data[3] + data[4];
#endif
    else cpuLimit2 = -1;

    if (R_FINITE(elapsed) && elapsed > 0) elapsedLimit2 = elapsed + data[2];
    else elapsedLimit2 = -1;

    return R_NilValue;
}

/* moved from character.cpp in 2.10.0: configure requires this */

#ifdef HAVE_GLOB_H
# include <glob.h>
#endif
#ifdef Win32
# include <dos_wglob.h>
# define globfree dos_wglobfree
# define glob_t wglob_t
#else
# ifndef GLOB_QUOTE
#  define GLOB_QUOTE 0
# endif
#endif
SEXP attribute_hidden do_glob(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* paths_, rho::RObject* dirmark_)
{
    SEXP x, ans;
    R_xlen_t i, n;
    int res, dirmark, initialized=FALSE;
    glob_t globbuf;
#ifdef Win32
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
#endif

    if (!Rf_isString(x = paths_))
	Rf_error(_("invalid '%s' argument"), "paths");
    if (!XLENGTH(x)) return Rf_allocVector(STRSXP, 0);
    dirmark = Rf_asLogical(dirmark_);
    if (dirmark == NA_LOGICAL)
	Rf_error(_("invalid '%s' argument"), "dirmark");
#ifndef GLOB_MARK
    if (dirmark)
	Rf_error(_("'dirmark = TRUE' is not supported on this platform"));
#endif

    for (i = 0; i < XLENGTH(x); i++) {
	SEXP el = STRING_ELT(x, i);
	if (el == NA_STRING) continue;
#ifdef Win32
	res = dos_wglob(filenameToWchar(el, FALSE),
			(dirmark ? GLOB_MARK : 0) |
			GLOB_QUOTE | (initialized ? GLOB_APPEND : 0),
			NULL, &globbuf);
	if (res == GLOB_NOSPACE)
	    Rf_error(_("internal out-of-memory condition"));
#else
	res = glob(Rf_translateChar(el),
# ifdef GLOB_MARK
		   (dirmark ? GLOB_MARK : 0) |
# endif
		   GLOB_QUOTE | (initialized ? GLOB_APPEND : 0),
		   nullptr, &globbuf);
# ifdef GLOB_ABORTED
	if (res == GLOB_ABORTED)
	    Rf_warning(_("read error on '%s'"), Rf_translateChar(el));
# endif
# ifdef GLOB_NOSPACE
	if (res == GLOB_NOSPACE)
	    Rf_error(_("internal out-of-memory condition"));
# endif
#endif
	initialized = TRUE;
    }
    n = initialized ? globbuf.gl_pathc : 0;
    PROTECT(ans = Rf_allocVector(STRSXP, n));
    for (i = 0; i < n; i++)
#ifdef Win32
    {
	wchar_t *w = globbuf.gl_pathv[i];
	char *buf;
	int nb = Rf_wcstoutf8(NULL, w, INT_MAX);
	buf = R_AllocStringBuffer(nb, &cbuff);
	Rf_wcstoutf8(buf, w, nb);
	SET_STRING_ELT(ans, i, Rf_mkCharCE(buf, CE_UTF8));
    }
#else
	SET_STRING_ELT(ans, i, Rf_mkChar(globbuf.gl_pathv[i]));
#endif
    UNPROTECT(1);
#ifdef Win32
    R_FreeStringBufferL(&cbuff);
#endif
    if (initialized) globfree(&globbuf);
    return ans;
}
