/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001-2015   The R Core Team
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

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Fileio.h>
#include <Fileio.h>
#include <IOStuff.h>
#include <Parse.h>
#include <Rconnections.h>
#include <IOStuff.h> // for R_ConsoleIob;

HIDDEN SEXP getParseContext(void)
{
    int i, last = PARSE_CONTEXT_SIZE;
    char context[PARSE_CONTEXT_SIZE+1];

    SEXP ans = nullptr, ans2;
    int nn, nread;
    char c;

    context[last] = '\0';
    for (i=R_ParseContextLast; last>0 ; i += PARSE_CONTEXT_SIZE - 1) {
	i = i % PARSE_CONTEXT_SIZE;
	context[--last] = R_ParseContext[i];
	if (!context[last]) {
	    last++;
	    break;
	}
    }

    nn = 16; /* initially allocate space for 16 lines */
    PROTECT(ans = Rf_allocVector(STRSXP, nn));
    c = context[last];
    nread = 0;
    while(c) {
	nread++;
	if(nread >= nn) {
	    ans2 = Rf_allocVector(STRSXP, 2*nn);
	    for(i = 0; i < nn; i++)
		SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
	    nn *= 2;
	    UNPROTECT(1); /* old ans */
	    PROTECT(ans = ans2);
	}
	i = last;
	while((c = context[i++])) {
	    if(c == '\n') break;
	}
	context[i-1] = '\0';
	SET_STRING_ELT(ans, nread-1, Rf_mkChar(context + last));
	last = i;
    }
    /* get rid of empty line after last newline */
    if (nread && !Rf_length(STRING_ELT(ans, nread-1))) {
	nread--;
	R_ParseContextLine--;
    }
    PROTECT(ans2 = Rf_allocVector(STRSXP, nread));
    for(i = 0; i < nread; i++)
	SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
    UNPROTECT(2);
    return ans2;
}

static void getParseFilename(char* buffer, size_t buflen)
{
    buffer[0] = '\0';
    if (R_ParseErrorFile) {
	if (Rf_isEnvironment(R_ParseErrorFile)) {
	    SEXP filename;
	    PROTECT(filename = Rf_findVar(Rf_install("filename"), R_ParseErrorFile));
	    if (Rf_isString(filename) && Rf_length(filename)) {
		strncpy(buffer, R_CHAR(STRING_ELT(filename, 0)), buflen - 1);
		buffer[buflen - 1] = '\0';
	    }
	    UNPROTECT(1);
	} else if (Rf_isString(R_ParseErrorFile) && Rf_length(R_ParseErrorFile)) {
	    strncpy(buffer, R_CHAR(STRING_ELT(R_ParseErrorFile, 0)), buflen - 1);
	    buffer[buflen - 1] = '\0';
	}
    }
}

static SEXP tabExpand(SEXP strings)
{
    int i;
    char buffer[200], *b;
    const char *input;
    SEXP result;
    PROTECT(strings);
    PROTECT(result = Rf_allocVector(STRSXP, Rf_length(strings)));
    for (i = 0; i < Rf_length(strings); i++) {
	input = R_CHAR(STRING_ELT(strings, i));
	for (b = buffer; *input && (b-buffer < 192); input++) {
	    if (*input == '\t') do {
		*b++ = ' ';
	    } while (((b-buffer) & 7) != 0);
	    else *b++ = *input;
	}
	*b = '\0';
	SET_STRING_ELT(result, i, Rf_mkCharCE(buffer, Rf_getCharCE(STRING_ELT(strings, i))));
    }
    UNPROTECT(2);
    return result;
}

NORET void parseError(SEXP call, int linenum)
{
    SEXP context;
    int len, width;
    char filename[128], buffer[10];
    PROTECT(context = tabExpand(getParseContext()));
    len = Rf_length(context);
    if (linenum) {
	getParseFilename(filename, sizeof(filename)-2);
	if (strlen(filename)) strcpy(filename + strlen(filename), ":");

	switch (len) {
	case 0:
	    Rf_error("%s%d:%d: %s",
		  filename, linenum, R_ParseErrorCol, R_ParseErrorMsg);
	    break;
	case 1: // replaces use of %n
	    width = snprintf(buffer, 10, "%d: ", R_ParseContextLine);
	    Rf_error("%s%d:%d: %s\n%d: %s\n%*s",
		  filename, linenum, R_ParseErrorCol, R_ParseErrorMsg,
		  R_ParseContextLine, R_CHAR(STRING_ELT(context, 0)),
		  width+R_ParseErrorCol+1, "^");
	    break;
	default:
	    width = snprintf(buffer, 10, "%d:", R_ParseContextLine);
	    Rf_error("%s%d:%d: %s\n%d: %s\n%d: %s\n%*s",
		  filename, linenum, R_ParseErrorCol, R_ParseErrorMsg,
		  R_ParseContextLine-1, R_CHAR(STRING_ELT(context, len-2)),
		  R_ParseContextLine, R_CHAR(STRING_ELT(context, len-1)),
		  width+R_ParseErrorCol+1, "^");
	    break;
	}
    } else {
	switch (len) {
	case 0:
	    Rf_error("%s", R_ParseErrorMsg);
	    break;
	case 1:
	    Rf_error("%s in \"%s\"",
		  R_ParseErrorMsg, R_CHAR(STRING_ELT(context, 0)));
	    break;
	default:
	    Rf_error("%s in:\n\"%s\n%s\"",
		  R_ParseErrorMsg, R_CHAR(STRING_ELT(context, len-2)),
		  R_CHAR(STRING_ELT(context, len-1)));
	    break;
	}
    }
    UNPROTECT(1);
}

/* "do_parse" - the user interface input/output to files.

 The internal R_Parse.. functions are defined in ./gram.y (-> gram.cpp)

 .Internal( parse(file, n, text, prompt, srcfile, encoding) )
 If there is text then that is read and the other arguments are ignored.
*/
HIDDEN SEXP do_parse(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op, rho::RObject* file_, rho::RObject* n_, rho::RObject* text_, rho::RObject* prompt_, rho::RObject* srcfile_, rho::RObject* encoding_)
{
    SEXP text, prompt, s, source;
    Rconnection con;
    Rboolean wasopen, old_latin1 = known_to_be_latin1,
	old_utf8 = known_to_be_utf8, allKnown = TRUE;
    int ifile, num, i;
    const char *encoding;
    ParseStatus status;

    if(!Rf_inherits(file_, "connection"))
	Rf_error(_("'file' must be a character string or connection"));
    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';

    ifile = Rf_asInteger(file_);

    con = getConnection(ifile);
    wasopen = con->isopen;
    num = Rf_asInteger(n_);
    if (num == 0)
	return Rf_allocVector(EXPRSXP, 0);

    PROTECT(text = Rf_coerceVector(text_, STRSXP));
    if(Rf_length(text_) && !Rf_length(text))
	Rf_error(_("coercion of 'text' to character was unsuccessful"));
    prompt = prompt_;
    source = srcfile_;
    if(!Rf_isString(encoding_) || Rf_length(encoding_) != 1)
	Rf_error(_("invalid '%s' value"), "encoding");
    encoding = R_CHAR(STRING_ELT(encoding_, 0)); /* ASCII */
    known_to_be_latin1 = known_to_be_utf8 = FALSE;
    /* allow 'encoding' to override declaration on 'text'. */
    if(streql(encoding, "latin1")) {
	known_to_be_latin1 = TRUE;
	allKnown = FALSE;
    } else if(streql(encoding, "UTF-8"))  {
	known_to_be_utf8 = TRUE;
	allKnown = FALSE;
    } else if(!streql(encoding, "unknown") && !streql(encoding, "native.enc"))
	Rf_warning(_("argument '%s = \"%s\"' will be ignored"), "encoding", encoding);

    if (prompt == nullptr)
	PROTECT(prompt);
    else
	PROTECT(prompt = Rf_coerceVector(prompt, STRSXP));

    if (Rf_length(text) > 0) {
	/* If 'text' has known encoding then we can be sure it will be
	   correctly re-encoded to the current encoding by
	   translateChar in the parser and so could mark the result in
	   a Latin-1 or UTF-8 locale.

	   A small complication is that different elements could have
	   different encodings, but all that matters is that all
	   non-ASCII elements have known encoding.
	*/
	for(i = 0; i < Rf_length(text); i++)
	    if(!ENC_KNOWN(STRING_ELT(text, i)) &&
	       !IS_ASCII(STRING_ELT(text, i))) {
		allKnown = FALSE;
		break;
	    }
	if(allKnown) {
	    known_to_be_latin1 = old_latin1;
	    known_to_be_utf8 = old_utf8;
	}
	if (num == R_NaInt) num = -1;
	s = R_ParseVector(text, num, &status, source);
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else if (ifile >= 3) {/* file != "" */
	if (num == R_NaInt) num = -1;
	try {
	    if(!wasopen && !con->open(con))
		Rf_error(_("cannot open the connection"));
	    if(!con->canread) Rf_error(_("cannot read from this connection"));
	    s = R_ParseConn(con, num, &status, source);
	    if(!wasopen) con->close(con);
	} catch (...) {
	    if (!wasopen && con->isopen)
		con->close(con);
	    throw;
	}
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else {
	if (num == R_NaInt) num = 1;
	s = R_ParseBuffer(&R_ConsoleIob, num, &status, prompt, source);
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    UNPROTECT(2);
    known_to_be_latin1 = old_latin1;
    known_to_be_utf8 = old_utf8;
    return s;
}
