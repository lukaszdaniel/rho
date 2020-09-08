/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dialogs.cpp
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
 *  Copyright (C) 2005--2017  The R Core Team
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
#include <config.h>
#endif

#include <Defn.h>
#include "graphapp/ga.h"
#include <windows.h>
#undef ERROR
#include <R_ext/RS.h> /* for Calloc */

#include <Localization.h>
#include "rui.h"

#include "Startup.h"

 struct winprogressbar {
    window wprog;
    progressbar pb;
    label lab;
    int width;
    double min, max, val;
};

static void pbarFinalizer(SEXP ptr)
{
    winprogressbar *pbar;

    if(TYPEOF(ptr) != EXTPTRSXP) return;
    pbar = R_ExternalPtrAddr(ptr);
    if(!pbar) return;
    hide(pbar->wprog);
    if(pbar-> lab) del(pbar->lab);
    del(pbar->pb);
    del(pbar->wprog);
    Free(pbar);
    R_ClearExternalPtr(ptr); /* not really needed */
}


/* winProgressBar(width, title, label, min, max, initial) */
SEXP winProgressBar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP tmp, ptr;
    int width, iv;
    double d;
    const char *title, *label;
    winprogressbar *pbar;
    Rboolean haveLabel;

    args = CDR(args);
    pbar = Calloc(1, winprogressbar);
    width = Rf_asInteger(CAR(args)); args = CDR(args);
    if(width == NA_INTEGER || width < 0) width = 200;
    tmp = CAR(args); args = CDR(args);
    if(!Rf_isString(tmp) || length(tmp) < 1 || STRING_ELT(tmp, 0) == NA_STRING)
	Rf_errorcall(call, "invalid '%s' argument", "title");
    title = Rf_translateChar(STRING_ELT(tmp, 0));
    tmp = CAR(args); args = CDR(args);
    if(!Rf_isString(tmp) || length(tmp) < 1 || STRING_ELT(tmp, 0) == NA_STRING)
	Rf_errorcall(call, "invalid '%s' argument", "Label");
    label = Rf_translateChar(STRING_ELT(tmp, 0));
    haveLabel = strlen(label) > 0;
    d = Rf_asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(d)) Rf_errorcall(call, "invalid '%s' argument", "min");
    pbar->min = d;
    d = Rf_asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(d)) Rf_errorcall(call, "invalid '%s' argument", "max");
    pbar->max = d;
    d = Rf_asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(d)) Rf_errorcall(call, "invalid '%s' argument", "initial");
    pbar->val = d;

    pbar->width = width;
    pbar->wprog = newwindow(title, rect(0, 0, width+40, haveLabel ? 100: 80),
			    Titlebar | Centered);
    setbackground(pbar->wprog, dialog_bg());
    if(haveLabel)
	pbar->lab = newlabel(label, rect(10, 15, width+20, 25), AlignCenter);
    pbar->pb = newprogressbar(rect(20, haveLabel ? 50 : 30, width, 20),
			      0, width, 1, 1);
    iv = pbar->width * (pbar->val - pbar->min)/(pbar->max - pbar->min);
    setprogressbar(pbar->pb, iv);
    show(pbar->wprog);
    ptr = R_MakeExternalPtr(pbar, Rf_install("winProgressBar"), nullptr);
    R_RegisterCFinalizerEx(ptr, pbarFinalizer, TRUE);

    return ptr;
}

SEXP closeWinProgressBar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    pbarFinalizer(CADR(args));
    return nullptr;
}

SEXP setWinProgressBar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = CDR(args);
    SEXP ptr = CAR(args);
    winprogressbar *pbar;
    double value;

    pbar = R_ExternalPtrAddr(ptr);
    if(!pbar)
	Rf_error("invalid progressbar -- has it been closed?");
    value = pbar->val;
    if(!Rf_isNull(CADR(args))) {
	int iv;
	double val = Rf_asReal(CADR(args));
	SEXP title = CADDR(args), label = CADDDR(args);
	if (R_FINITE(val) && val >= pbar->min && val <= pbar->max) {
	    iv = pbar->width * (val - pbar->min)/(pbar->max - pbar->min);
	    setprogressbar(pbar->pb, iv);
	    pbar->val = val;
	}
	if (!Rf_isNull(title)) {
	    SEXP ctxt;
	    if(!Rf_isString(title) || length(title) < 1)
		Rf_errorcall(call, "invalid '%s' argument", "title");
	    ctxt = STRING_ELT(title, 0);
	    if (ctxt != NA_STRING)
		settext(pbar->wprog, Rf_translateChar(ctxt));
	}
	if(pbar->lab && !Rf_isNull(label)) {
	    SEXP clab;
	    if(!Rf_isString(label) || length(label) < 1)
		Rf_errorcall(call, "invalid '%s' argument", "label");
	    clab = STRING_ELT(label, 0);
	    if (clab != NA_STRING)
		settext(pbar->lab, Rf_translateChar(clab));
	}
    }
    return Rf_ScalarReal(value);
}

SEXP winDialog(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP message;
    const char * type;
    int res=YES;

    args = CDR(args);
    type = Rf_translateChar(STRING_ELT(CAR(args), 0));
    message = CADR(args);
    if(!Rf_isString(message) || length(message) != 1 ||
       strlen(Rf_translateChar(STRING_ELT(message, 0))) > 999)
	Rf_error(_("invalid '%s' argument"), "message");
    if (streql(type, "ok")) {
	askok(Rf_translateChar(STRING_ELT(message, 0)));
	res = 10;
    } else if (streql(type, "okcancel")) {
	res = askokcancel(Rf_translateChar(STRING_ELT(message, 0)));
	if(res == YES) res = 2;
    } else if (streql(type, "yesno")) {
	res = askyesno(Rf_translateChar(STRING_ELT(message, 0)));
    } else if (streql(type, "yesnocancel")) {
	res = askyesnocancel(Rf_translateChar(STRING_ELT(message, 0)));
    } else
	Rf_errorcall(call, _("unknown type"));
    return Rf_ScalarInteger(res);
}

SEXP winDialogString(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  message, def;
    const char *string;

    args = CDR(args);
    message = CAR(args);
    if(!Rf_isString(message) || length(message) != 1 ||
       strlen(Rf_translateChar(STRING_ELT(message, 0))) > 255)
	Rf_error(_("invalid '%s' argument"), "message");
    def = CADR(args);
    if(!Rf_isString(def) || length(def) != 1)
	Rf_error(_("invalid '%s' argument"), "default");
    string = askstring(Rf_translateChar(STRING_ELT(message, 0)),
		       Rf_translateChar(STRING_ELT(def, 0)));
    if (string) return Rf_mkString(string);
    else return nullptr;
}

static char msgbuf[256];

SEXP winMenuNames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP menuNames;
    int i, nmenus;

    args = CDR(args);
    if (CharacterMode != RGui)
	Rf_errorcall(call, _("menu functions can only be used in the GUI"));

    nmenus = numwinmenus();

    PROTECT(menuNames = Rf_allocVector(STRSXP, nmenus));

    for (i = 0; i < nmenus; i++) {
	SET_STRING_ELT(menuNames, i, Rf_mkChar(getusermenuname(i)));
    }

    UNPROTECT(1);
    return(menuNames);
}

SEXP winMenuItems(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP mname, ans, ansnames;
    menuItems *items;
    char errmsg[50];
    int i;

    args = CDR(args);

    if (CharacterMode != RGui)
	Rf_errorcall(call, _("menu functions can only be used in the GUI"));

    mname = CAR(args);
    if (!Rf_isString(mname) || length(mname) != 1)
	Rf_error(_("invalid '%s' argument"), "menuname");

    items = wingetmenuitems(Rf_translateChar(STRING_ELT(mname,0)), errmsg);
    if (items->numItems == 0) {
	snprintf(msgbuf, 256, _("unable to retrieve items for %s (%s)"),
		 Rf_translateChar(STRING_ELT(mname,0)), errmsg);
	freemenuitems(items);
	Rf_errorcall(call, msgbuf);
    }

    PROTECT(ans = Rf_allocVector(STRSXP, items->numItems));
    PROTECT(ansnames = Rf_allocVector(STRSXP, items->numItems));
    for (i = 0; i < items->numItems; i++) {
	SET_STRING_ELT(ans, i, Rf_mkChar(items->mItems[i]->action));
	SET_STRING_ELT(ansnames, i, Rf_mkChar(items->mItems[i]->name));
    }

    Rf_setAttrib(ans, R_NamesSymbol, ansnames);

    freemenuitems(items);

    UNPROTECT(2);
    return(ans);
}


SEXP winMenuAdd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP smenu, sitem;
    int res;
    char errmsg[50];

    args = CDR(args);
    if (CharacterMode != RGui)
	Rf_errorcall(call, _("menu functions can only be used in the GUI"));
    smenu = CAR(args);
    if(!Rf_isString(smenu) || length(smenu) != 1)
	Rf_error(_("invalid '%s' argument"), "menuname");
    sitem = CADR(args);
    if (Rf_isNull(sitem)) { /* add a menu */
	res = winaddmenu (Rf_translateChar(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    snprintf(msgbuf, 256, _("unable to add menu (%s)"), errmsg);
	    Rf_errorcall(call, msgbuf);
	}

    } else { /* add an item */
	if(!Rf_isString(sitem) || length(sitem) != 1)
	    Rf_error(_("invalid '%s' argument"), "itemname");
	res = winaddmenuitem (Rf_translateChar(STRING_ELT(sitem, 0)),
			      Rf_translateChar(STRING_ELT(smenu, 0)),
			      Rf_translateChar(STRING_ELT(CADDR(args), 0)),
			      errmsg);
	if (res > 0) {
	    snprintf(msgbuf, 256, _("unable to add menu item (%s)"), errmsg);
	    Rf_errorcall(call, msgbuf);
	}
    }
    return (nullptr);
}

SEXP winMenuDel(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP smenu, sitem;
    int res;
    char errmsg[50];

    args = CDR(args);
    if (CharacterMode != RGui)
	Rf_errorcall(call, _("menu functions can only be used in the GUI"));
    smenu = CAR(args);
    if(!Rf_isString(smenu) || length(smenu) != 1)
	Rf_error(_("invalid '%s' argument"), "menuname");
    sitem = CADR(args);
    if (Rf_isNull(sitem)) { /* delete a menu */
	res = windelmenu (Rf_translateChar(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0)
	    Rf_errorcall(call, _("menu does not exist"));
    } else { /* delete an item */
	if(!Rf_isString(sitem) || length(sitem) != 1)
	    Rf_error(_("invalid '%s' argument"), "itemname");
	res = windelmenuitem (Rf_translateChar(STRING_ELT(sitem, 0)),
			      Rf_translateChar(STRING_ELT(smenu, 0)), errmsg);
	if (res > 0) {
	    snprintf(msgbuf, 256, _("unable to delete menu item (%s)"), errmsg);
	    Rf_errorcall(call, msgbuf);
	}
    }
    return (nullptr);
}
