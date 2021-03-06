/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2013  The R Core Team
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
# include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>
#include <Rversion.h>
#include <rho/Symbol.hpp>

using namespace rho;

HIDDEN void Rf_PrintGreeting(void)
{
    char buf[500];

    Rprintf("\n");
    PrintVersion_part_1(buf, 500);
    Rprintf("%s\n", buf);
    Rprintf("Rho is not part of the R project, so please do not report bugs\nvia r-bugs or the R website - instead refer to the author.\n\n");
    Rprintf(_("R is free software and comes with ABSOLUTELY NO WARRANTY.\n\
You are welcome to redistribute it under certain conditions.\n\
Type 'license()' or 'licence()' for distribution details.\n\n"));
    Rprintf(_("R is a collaborative project with many contributors.\n\
Type 'contributors()' for more information and\n\
'citation()' on how to cite R or R packages in publications.\n\n"));
    Rprintf(_("Type 'demo()' for some demos, 'help()' for on-line help, or\n\
'help.start()' for an HTML browser interface to help.\n\
Type 'q()' to quit R.\n\n"));
}

HIDDEN SEXP do_version(/*const*/ rho::Expression* call, const rho::BuiltInFunction* op)
{
    SEXP value, names;
    char buf[128];

    PROTECT(value = Rf_allocVector(VECSXP,15));
    PROTECT(names = Rf_allocVector(STRSXP,15));

    SET_STRING_ELT(names, 0, Rf_mkChar("platform"));
    SET_VECTOR_ELT(value, 0, Rf_mkString(R_PLATFORM));
    SET_STRING_ELT(names, 1, Rf_mkChar("arch"));
    SET_VECTOR_ELT(value, 1, Rf_mkString(R_CPU));
    SET_STRING_ELT(names, 2, Rf_mkChar("os"));
    SET_VECTOR_ELT(value, 2, Rf_mkString(R_OS));

    snprintf(buf, 128, "%s, %s", R_CPU, R_OS);
    SET_STRING_ELT(names, 3, Rf_mkChar("system"));
    SET_VECTOR_ELT(value, 3, Rf_mkString(buf));

    SET_STRING_ELT(names, 4, Rf_mkChar("status"));
    SET_VECTOR_ELT(value, 4, Rf_mkString(R_STATUS));
    SET_STRING_ELT(names, 5, Rf_mkChar("major"));
    SET_VECTOR_ELT(value, 5, Rf_mkString(R_MAJOR));
    SET_STRING_ELT(names, 6, Rf_mkChar("minor"));
    SET_VECTOR_ELT(value, 6, Rf_mkString(R_MINOR));
    SET_STRING_ELT(names, 7, Rf_mkChar("year"));
    SET_VECTOR_ELT(value, 7, Rf_mkString(R_YEAR));
    SET_STRING_ELT(names, 8, Rf_mkChar("month"));
    SET_VECTOR_ELT(value, 8, Rf_mkString(R_MONTH));
    SET_STRING_ELT(names, 9, Rf_mkChar("day"));
    SET_VECTOR_ELT(value, 9, Rf_mkString(R_DAY));
    SET_STRING_ELT(names, 10, Rf_mkChar("svn rev"));

    snprintf(buf, 128, "%d", R_SVN_BASEREVISION);
    SET_VECTOR_ELT(value, 10, Rf_mkString(buf));
    SET_STRING_ELT(names, 11, Rf_mkChar("git rev"));

    snprintf(buf, 128, "%s", R_GIT_REVISION);
    SET_VECTOR_ELT(value, 11, Rf_mkString(buf));
    SET_STRING_ELT(names, 12, Rf_mkChar("language"));
    SET_VECTOR_ELT(value, 12, Rf_mkString("R"));

    Rf_PrintRhoVersionString(buf, 128);
    SET_STRING_ELT(names, 13, Rf_mkChar("version.string"));
    SET_VECTOR_ELT(value, 13, Rf_mkString(buf));
    SET_STRING_ELT(names, 14, Rf_mkChar("nickname"));
    SET_VECTOR_ELT(value, 14, Rf_mkString(R_NICK));

    Rf_setAttrib(value, Symbols::NamesSymbol, names);
    UNPROTECT(2);
    return value;
}

HIDDEN void Rf_PrintVersion(char *s, size_t len)
{
    PrintVersion_part_1(s, len);

    strcat(s, "\n"
	   "R is free software and comes with ABSOLUTELY NO WARRANTY.\n"
	   "You are welcome to redistribute it under the terms of the\n"
	   "GNU General Public License versions 2 or 3.\n"
	   "For more information about these matters see\n"
	   "http://www.gnu.org/licenses/.\n");
}

HIDDEN void Rf_PrintVersionString(char *s, size_t len)
{
    if(R_SVN_BASEREVISION <= 0) {// 'svn info' failed in ../../Makefile.in
	snprintf(s, len, "R version %s.%s %s (%s-%s-%s)",
		R_MAJOR, R_MINOR, R_STATUS, R_BASEYEAR, R_BASEMONTH, R_BASEDAY);
    } else if(strlen(R_STATUS) == 0) {
	snprintf(s, len, "R version %s.%s (%s-%s-%s)",
		R_MAJOR, R_MINOR, R_BASEYEAR, R_BASEMONTH, R_BASEDAY);
    } else if(streql(R_STATUS, "Under development (unstable)")) {
	snprintf(s, len, "R %s (%s-%s-%s r%d)",
		R_STATUS, R_BASEYEAR, R_BASEMONTH, R_BASEDAY, R_SVN_BASEREVISION);
    } else {
	snprintf(s, len, "R version %s.%s %s (%s-%s-%s r%d)",
		R_MAJOR, R_MINOR, R_STATUS, R_BASEYEAR, R_BASEMONTH, R_BASEDAY,
		R_SVN_BASEREVISION);
    }
}

HIDDEN void Rf_PrintRhoVersionString(char *s, size_t len)
{
    std::string git_revision = R_GIT_REVISION;
    if (git_revision == "unknown" || git_revision == "") {
        // 'git log' failed in ../../Makefile.in
	snprintf(s, len, "Rho version %s.%s %s (%s-%s-%s)",
		R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY);
    } else if(strlen(R_STATUS) == 0) {
	snprintf(s, len, "Rho version %s.%s (%s-%s-%s)",
		R_MAJOR, R_MINOR, R_YEAR, R_MONTH, R_DAY);
    } else if(streql(R_STATUS, "Under development (unstable)")) {
	snprintf(s, len, "Rho %s (%s-%s-%s r%s)",
		R_STATUS, R_YEAR, R_MONTH, R_DAY, R_GIT_REVISION);
    } else {
	snprintf(s, len, "Rho version %s.%s %s (%s-%s-%s r%s)",
		R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY,
		R_GIT_REVISION);
    }
}

HIDDEN void PrintVersion_part_1(char *s, size_t len)
{
#define SPRINTF_2(_FMT, _OBJ) snprintf(tmp, 128, _FMT, _OBJ); strcat(s, tmp)
    char tmp[128];
    Rf_PrintRhoVersionString(s, len);
    strcat(s, " -- \"R(C) -> Rho(C++)\"\n");
    strcat(s, "Rho Copyright (C) 2008-2014 Andrew R. Runnalls\n\n");

    Rf_PrintVersionString(tmp, len);
    if(strlen(R_NICK) != 0) {
	char nick[128];
	snprintf(nick, 128, " -- \"%s\"", R_NICK);
	strcat(tmp, nick);
    }
    strcat(s, tmp);
    SPRINTF_2("\nCopyright (C) %s The R Foundation for Statistical Computing\n", R_YEAR);
/*  strcat(s, "ISBN 3-900051-07-0\n");  */
    SPRINTF_2("Platform: %s", R_PLATFORM);
    if(strlen(R_ARCH)) { SPRINTF_2("/%s", R_ARCH); }
    SPRINTF_2(" (%d-bit)\n", 8*int(sizeof(void *)));
}

HIDDEN SEXP do_internalsID(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return Rf_mkString(R_INTERNALS_UUID);
}