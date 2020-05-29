/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2017   The R Core Team.
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

#include <Rconnections.h>
#include <Rdynpriv.h>
#include <R_ext/R-ftp-http.h>
#include <Rmodules/Rinternet.h>
#include "basedecl.h"

static R_InternetRoutines routines, *ptr = &routines;


/*
SEXP Rdownload(SEXP args);
Rconnection R_newurl(char *description, char *mode);
Rconnection R_newsock(char *host, int port, int server, char *mode, int timeout);


Next 6 are for use by libxml, only

void *R_HTTPOpen(const char *url);
int   R_HTTPRead(void *ctx, char *dest, int len);
void  R_HTTPClose(void *ctx);

void *R_FTPOpen(const char *url);
int   R_FTPRead(void *ctx, char *dest, int len);
void  R_FTPClose(void *ctx);

int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)

int R_HTTPDCreate(const char *ip, int port);
void R_HTTPDStop(void);
 */

static int initialized = 0;

R_InternetRoutines *
R_setInternetRoutines(R_InternetRoutines *routines)
{
    R_InternetRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return(tmp);
}

static void internet_Init(void)
{
    int res;
    res = R_moduleCdynload("internet", 1, 1);
    initialized = -1;
    if(!res) return;
    if(!ptr->download)
	Rf_error(_("internet routines cannot be accessed in module"));
    initialized = 1;
    return;
}

SEXP Rdownload(SEXP args)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->download)(args);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

HIDDEN Rconnection 
R_newurl(const char *description, const char * const mode, int type)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newurl)(description, mode, type);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

HIDDEN Rconnection
R_newsock(const char *host, int port, int server, const char * const mode,
	  int timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newsock)(host, port, server, mode, timeout);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

void *R_HTTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPOpen)(url, nullptr, 0);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

int   R_HTTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPRead)(ctx, dest, len);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_HTTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPClose)(ctx);
    else
	Rf_error(_("internet routines cannot be loaded"));
}

void *R_FTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPOpen)(url);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

int   R_FTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPRead)(ctx, dest, len);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_FTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->FTPClose)(ctx);
    else
	Rf_error(_("internet routines cannot be loaded"));
}

int extR_HTTPDCreate(const char *ip, int port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPDCreate)(ip, port);
    else
	Rf_error(_("internet routines cannot be loaded"));
    return -1;
}

void extR_HTTPDStop(void)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPDStop)();
    else
	Rf_error(_("internet routines cannot be loaded"));
}

SEXP Rsockconnect(SEXP sport, SEXP shost)
{
    if (Rf_length(sport) != 1) Rf_error("invalid 'socket' argument");
    int port = Rf_asInteger(sport);
    char *host[1];
    host[0] = const_cast<char *>(Rf_translateChar(STRING_ELT(shost, 0)));
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockconnect)(&port, host);
    else
	Rf_error(_("socket routines cannot be loaded"));
    return Rf_ScalarInteger(port); // The socket number
}

SEXP Rsockread(SEXP ssock, SEXP smaxlen)
{
    if (Rf_length(ssock) != 1) Rf_error("invalid 'socket' argument");
    int sock = Rf_asInteger(ssock), maxlen = Rf_asInteger(smaxlen);
    char buf[maxlen+1], *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockread)(&sock, abuf, &maxlen);
    else
	Rf_error(_("socket routines cannot be loaded"));
    if (maxlen < 0) // presumably -1, error from recv
	Rf_error("Error reading data in Rsockread");
    return Rf_ScalarString(Rf_mkCharLen(buf, maxlen));
}

SEXP Rsockclose(SEXP ssock)
{
    if (Rf_length(ssock) != 1) Rf_error("invalid 'socket' argument");
    int sock = Rf_asInteger(ssock);
    if (sock <= 0) Rf_error(_("attempt to close invalid socket"));
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockclose)(&sock);
    else
	Rf_error(_("socket routines cannot be loaded"));
    return Rf_ScalarLogical(sock);
}

SEXP Rsockopen(SEXP sport)
{
    if (Rf_length(sport) != 1) Rf_error("invalid 'port' argument");
    int port = Rf_asInteger(sport);
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockopen)(&port);
    else
	Rf_error(_("socket routines cannot be loaded"));
    return Rf_ScalarInteger(port); // The socket number
}

SEXP Rsocklisten(SEXP ssock)
{
    if (Rf_length(ssock) != 1) Rf_error("invalid 'socket' argument");
    int sock = Rf_asInteger(ssock), len = 256;
    char buf[257], *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->socklisten)(&sock, abuf, &len);
    else
	Rf_error(_("socket routines cannot be loaded"));
    SEXP ans = PROTECT(Rf_ScalarInteger(sock)); // The socket being listened on
    SEXP host = PROTECT(Rf_ScalarString(Rf_mkChar(buf)));
    Rf_setAttrib(ans, Rf_install("host"), host);
    UNPROTECT(2);
    return ans;
}

SEXP Rsockwrite(SEXP ssock, SEXP sstring)
{
    if (Rf_length(ssock) != 1) Rf_error("invalid 'socket' argument");
    int sock = Rf_asInteger(ssock), start = 0, end, len;
    char *buf = const_cast<char *>(Rf_translateChar(STRING_ELT(sstring, 0))), *abuf[1];
    end = len = int(strlen(buf));
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockwrite)(&sock, abuf, &start, &end, &len);
    else
	Rf_error(_("socket routines cannot be loaded"));
    return Rf_ScalarInteger(len);
}


HIDDEN
int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->sockselect)(nsock, insockfd, ready, write, timeout);
    else {
	Rf_error(_("socket routines cannot be loaded"));
	return 0;
    }
}

HIDDEN SEXP do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlVersion)(call, op, args, rho);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

HIDDEN SEXP do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlGetHeaders)(call, op, args, rho);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

HIDDEN SEXP do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlDownload)(call, op, args, rho);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return nullptr;
    }
}

HIDDEN Rconnection
R_newCurlUrl(const char *description, const char * const mode, int type)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newcurlurl)(description, mode, type);
    else {
	Rf_error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
    return (Rconnection)0; /* -Wall */
}
