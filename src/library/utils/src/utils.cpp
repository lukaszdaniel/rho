/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2015   The R Core Team.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>
#include <R.h>
#include <Rinternals.h>

#include "utils.h"
#include "localization.h"

/* from src/main/eval.cpp */
SEXP do_Rprof(SEXP args);

SEXP Rprof(SEXP args)
{
    return do_Rprof(CDR(args));
}

/* from src/main/memory.cpp */
SEXP do_Rprofmem(SEXP args);

SEXP Rprofmem(SEXP args)
{
    return do_Rprofmem(CDR(args));
}

/* from src/main/dounzip.cpp */
SEXP Runzip(SEXP args);

SEXP unzip(SEXP args)
{
    return Runzip(CDR(args));
}




#include <lzma.h>

SEXP crc64(SEXP in)
{
    uint64_t crc = 0;
    char ans[17];
    if (!Rf_isString(in)) Rf_error("input must be a character string");
    const char *str = R_CHAR(STRING_ELT(in, 0));

    /* Seems this is really 64-bit only on 64-bit platforms */
    crc = lzma_crc64((uint8_t *)str, strlen(str), crc);
    snprintf(ans, 17, "%lx", (long unsigned int) crc);
    return Rf_mkString(ans);
}

// As from 3.3.0 this means on Unix.
#if defined(HAVE_ARPA_INET_H)
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

SEXP nsl(SEXP hostname)
{
    SEXP ans = nullptr;
    const char *name; char ip[] = "xxx.xxx.xxx.xxx";
    struct hostent *hp;

    if (!Rf_isString(hostname) || Rf_length(hostname) != 1)
	Rf_error(_("'hostname' must be a character vector of length 1"));
    name = Rf_translateChar(STRING_ELT(hostname, 0));

    hp = gethostbyname(name);

    if (hp == NULL) {		/* cannot resolve the address */
	Rf_warning(_("nsl() was unable to resolve host '%s'"), name);
    } else {
	if (hp->h_addrtype == AF_INET) {
	    struct in_addr in;
	    memcpy(&in.s_addr, *(hp->h_addr_list), sizeof (in.s_addr));
	    strcpy(ip, inet_ntoa(in));
	} else {
	    Rf_warning(_("unknown format returned by 'gethostbyname'"));
	}
	ans = Rf_mkString(ip);
    }
    return ans;
}
#else
SEXP nsl(SEXP hostname)
{
    Rf_warning(_("nsl() is not supported on this platform"));
    return nullptr;
}
#endif

