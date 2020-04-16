/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2017 The R Core Team
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


/*
  This is an effort to merge the 3 different dynload.cpp files in the
  distribution from the unix/, macintosh/dll/ and gnuwin32/ directories.
  The aim is to consolidate these different implementations into
      i) a generic or platform-independent common core
     ii) platform-dependent routines that are registered
	 as function pointers.
  The reason for using function pointers rather than explicit
  linking of symbols is
     a) to avoid confusion in the linking
     b) to allow for easily overriding these in embedded applications
	in which a host application needs to control how R finds
	symbols. This may be necessary for security reasons.
 */

/*  Dynamic Loading Support
 *
 *  This module provides support for run-time loading of shared objects
 *  access to symbols within such objects via .C and .Fortran.  This is
 *  done under Unix with dlopen, dlclose and dlsym (the exception is
 *  hpux, where we use compatibility code provided by Luke Tierney).
 *  There are two cases:
 *
 *
 *  1. The dlopen interface is available.
 *
 *  In this case all symbol location in packages is done using the
 *  dlopen routines.  We maintain a list of currently loaded shared
 *  objects in an array called "LoadedDLL" with the number of currently
 *  loaded objects being "CountDLL".  To locate a symbol, we probe
 *  the loaded objects in order until the symbol is located.  If we
 *  do not find a symbol in the loaded objects, we search the
 *  executable itself.  This search is not very efficient, but this
 *  probably pales into insignificance when compared with the
 *  inefficiencies in the R interpreter.
 *
 *  Loading and unloading of shared objects is done via the routines
 *  AddDLL and DeleteDLL.  These routines maintain the list of
 *  currently loaded objects.  When an object is added, any existing
 *  reference to that object is deleted and then the object is
 *  inserted at the start of the search list.  This way, symbols in
 *  more recently loaded objects are found first.
 *
 *
 *  Accessing native routines in base (the R executable).
 *
 *  In this case, we use the registration mechanism and the DllInfo array
 *  in ../main/Rdynload.cpp to locate functions in the executable. We do this
 *  by straight linear search through the table.
 *  Note that the base routines registered are listed in
 *               ../main/registration.cpp
 *  and are registered during the initialization of the R engine.
 *  (This replaces the previous mechanism that built a table
 *  from ../appl/ROUTINES using Perl/sed).
 *
 *
 *  If speed is ever an issue in the lookup of registered symbols, we can
 *  store the registered routines in a hashtable or binary tree as they
 *  are being registered.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <Internal.h>

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <Rmath.h>
#include <Rdynpriv.h>
#include "basedecl.h"
#include <vector>
#include "rho/GCStackRoot.hpp"
#include "rho/WeakRef.hpp"

using namespace std;
using namespace rho;

#ifdef Unix
/* HP-UX 11.0 has dlfcn.h, but according to libtool as of Dec 2001
   this support is broken. So we force use of shlib even when dlfcn.h
   is available */
# ifdef __hpux
#  ifdef HAVE_DL_H
#   define HAVE_DYNAMIC_LOADING
#  endif
# else
#  ifdef HAVE_DLFCN_H
#   define HAVE_DYNAMIC_LOADING
#  endif
# endif /* __hpux */
# ifndef HAVE_NO_SYMBOL_UNDERSCORE
#  ifdef HAVE_ELF_H
#   define HAVE_NO_SYMBOL_UNDERSCORE
#  endif /* HAVE_ELF_H */
# endif /* HAVE_NO_SYMBOL_UNDERSCORE */
#endif

#ifdef Win32
# define HAVE_DYNAMIC_LOADING
#endif

#ifdef CACHE_DLL_SYM  /* Used on Windows */
#define MAX_CACHE	100
/* keep a record of symbols that have been found, about 70 bytes each */
R_CPFun CPFun[MAX_CACHE];
int nCPFun = 0;
#endif

static int MaxNumDLLs = 0; /* initialized in initLoadedDLL */

static int CountDLL = 0;

#include <R_ext/Rdynload.h>

/* Allocated in initLoadedDLL at R session start. Never free'd */
static DllInfo* LoadedDLL = NULL;

static int addDLL(char *dpath, const char *name, HINSTANCE handle);
static SEXP Rf_MakeDLLInfo(DllInfo *info);

static SEXP createRSymbolObject(SEXP sname, DL_FUNC f,
				R_RegisteredNativeSymbol *symbol,
				Rboolean withRegistrationInfo);

static DllInfo *R_RegisterDLL(HINSTANCE handle, const char *path);

attribute_hidden OSDynSymbol Rf_osDynSymbol;
attribute_hidden OSDynSymbol *R_osDynSymbol = &Rf_osDynSymbol;

void R_init_base(DllInfo *); /* In registration.cpp */

static void initLoadedDLL();

void attribute_hidden
InitDynload()
{
    initLoadedDLL();
    int which = addDLL(strdup("base"), "base", nullptr);
    DllInfo *dll = &LoadedDLL[which];
    R_init_base(dll);
    Rf_InitFunctionHashing();
}

/* Allocate LoadedDLL. Errors are reported via R_Suicide, because this is
   called too early during startup to use Rf_error(.) */
static void initLoadedDLL()
{
    if (CountDLL != 0 || LoadedDLL != NULL)
	R_Suicide("DLL table corruption detected"); /* not translated */

    /* Note that it is likely that dlopen will use up at least one file
       descriptor for each DLL loaded (it may load further dynamically
       linked libraries), so we do not want to get close to the fd limit
       (which may be as low as 256). By default, the maximum number of DLLs
       that can be loaded is 100. When the fd limit is known, we allow
       increasing the maximum number of DLLs via environment variable up to
       60% of the limit on open files, but to no more than 1000.
    */
    int maxlimit;
    int fdlimit = R_GetFDLimit();
    if (fdlimit > 0) { /* fd limit known */
	maxlimit = (int) (0.6 * fdlimit);
	if (maxlimit > 1000) maxlimit = 1000;
	if (maxlimit < 100)
	    R_Suicide(_("the limit on the number of open files is too low"));
    } else
	maxlimit = 100;

    char *req = getenv("R_MAX_NUM_DLLS");
    if (req != NULL) {
	int reqlimit = atoi(req);
	if (reqlimit < 100)
	    R_Suicide(_("R_MAX_NUM_DLLS must be at least 100"));
	if (reqlimit > maxlimit) {
	    if (maxlimit == 1000)
		R_Suicide(_("MAX_NUM_DLLS cannot be bigger than 1000"));
	    
	    char msg[128];
	    snprintf(msg, 128,
	      _("MAX_NUM_DLLS bigger than %d may exhaust open files limit"),
	      maxlimit);
	    R_Suicide(msg);
	}
	MaxNumDLLs = reqlimit;
    } else
	MaxNumDLLs = 100;

    /* memory is set to zero */
    LoadedDLL = (DllInfo *) calloc(MaxNumDLLs, sizeof(DllInfo));
    if (LoadedDLL == NULL)
	R_Suicide(_("could not allocate space for DLL table"));
}

/* returns DllInfo used by the embedding application.
   the underlying "(embedding)" entry is created if not present */
DllInfo *R_getEmbeddingDllInfo()
{
    DllInfo *dll = R_getDllInfo("(embedding)");
    if (dll == nullptr) {
	int which = addDLL(strdup("(embedding)"), "(embedding)", nullptr);
	dll = &LoadedDLL[which];
	/* make sure we don't attempt dynamic lookup */
	R_useDynamicSymbols(dll, FALSE);
    }
    return dll;
}

Rboolean R_useDynamicSymbols(DllInfo *info, Rboolean value)
{
    Rboolean old;
    old = info->useDynamicLookup;
    info->useDynamicLookup = value;

    return old;
}

Rboolean R_forceSymbols(DllInfo *info, Rboolean value)
{
    Rboolean old;
    old = info->forceSymbols;
    info->forceSymbols = value;
    return old;
}

static void
R_addCRoutine(DllInfo *info, const R_CMethodDef * const croutine,
	      Rf_DotCSymbol *sym);
static void
R_addCallRoutine(DllInfo *info,
		 const R_CallMethodDef * const croutine,
		 Rf_DotCallSymbol *sym);
static void
R_addFortranRoutine(DllInfo *info,
		    const R_FortranMethodDef * const croutine,
		    Rf_DotFortranSymbol *sym);
static void
R_addExternalRoutine(DllInfo *info,
		     const R_ExternalMethodDef * const croutine,
		     Rf_DotExternalSymbol *sym);


/*
 Returns a reference to the DllInfo object associated with the shared object
 with the path name `path'. This ensures uniqueness rather than having the
 undesirable situation of two objects with the same name but in different
 directories.
 This is available so that it can be called from arbitrary C routines
 that need to call R_registerRoutines(). The initialization routine
 R_init_<object name> is passed the DllInfo reference as an argument.
 Other routines must explicitly request it using this routine.
 */
DllInfo *
R_getDllInfo(const char *path)
{
    int i;
    for(i = 0; i < CountDLL; i++) {
	if(streql(LoadedDLL[i].path, path)) return(&LoadedDLL[i]);
    }
    return nullptr;
}

/*
  Explicitly register the native routines for use in .Call(), .C() and
  .Fortran() functions. These registered values are used to resolve
  symbols in an object that makes a call to this routine, rather than
  the usual dynamic resolution done by dlsym() or the equivalent on
  the different platforms.
 */
int
R_registerRoutines(DllInfo *info, const R_CMethodDef * const croutines,
		   const R_CallMethodDef * const callRoutines,
		   const R_FortranMethodDef * const fortranRoutines,
		   const R_ExternalMethodDef * const externalRoutines)
{
    int i, num;

    if(info == nullptr)
	Rf_error(_("R_RegisterRoutines called with invalid DllInfo object."));

    /* Default is to look in registered and then dynamic (unless
       the is no handle such as in "base" or "embedded")
       Potentially change in the future to be only registered
       if there are any registered values.
    */
    info->useDynamicLookup = (info->handle) ? TRUE : FALSE;
    info->forceSymbols = FALSE;

    if(croutines) {
	for(num = 0; croutines[num].name != nullptr; num++) {;}
	info->CSymbols = static_cast<Rf_DotCSymbol*>(calloc(size_t(num),
							    sizeof(Rf_DotCSymbol)));
	info->numCSymbols = num;
	for(i = 0; i < num; i++) {
	    R_addCRoutine(info, croutines+i, info->CSymbols + i);
	}
    }

    if(fortranRoutines) {
	for(num = 0; fortranRoutines[num].name != nullptr; num++) {;}
	info->FortranSymbols =
	    static_cast<Rf_DotFortranSymbol*>(calloc(size_t(num),
						     sizeof(Rf_DotFortranSymbol)));
	info->numFortranSymbols = num;

	for(i = 0; i < num; i++)
	    R_addFortranRoutine(info, fortranRoutines+i,
				info->FortranSymbols + i);
    }

    if(callRoutines) {
	for(num = 0; callRoutines[num].name != nullptr; num++) {;}
	info->CallSymbols =
	    static_cast<Rf_DotCallSymbol*>(calloc(size_t(num), sizeof(Rf_DotCallSymbol)));
	info->numCallSymbols = num;
	for(i = 0; i < num; i++)
	    R_addCallRoutine(info, callRoutines+i, info->CallSymbols + i);
    }

    if(externalRoutines) {
	for(num = 0; externalRoutines[num].name != nullptr; num++) {;}
	info->ExternalSymbols =
	    static_cast<Rf_DotExternalSymbol*>(calloc(size_t(num),
						      sizeof(Rf_DotExternalSymbol)));
	info->numExternalSymbols = num;

	for(i = 0; i < num; i++)
	    R_addExternalRoutine(info, externalRoutines+i,
				 info->ExternalSymbols + i);
    }

    return(1);
}

static void
R_setPrimitiveArgTypes(const R_FortranMethodDef * const croutine,
		       Rf_DotFortranSymbol *sym)
{
    sym->types = static_cast<R_NativePrimitiveArgType *>
	(malloc(sizeof(R_NativePrimitiveArgType) * size_t(croutine->numArgs)));
    if(!sym->types)
	Rf_error("allocation failure in R_setPrimitiveArgTypes");
    if(sym->types)
	memcpy(sym->types, croutine->types,
	       sizeof(R_NativePrimitiveArgType) * size_t(croutine->numArgs));

}

static void
R_addFortranRoutine(DllInfo *info,
		    const R_FortranMethodDef * const croutine,
		    Rf_DotFortranSymbol *sym)
{
    sym->name = strdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
    if(croutine->types)
	R_setPrimitiveArgTypes(croutine, sym);
}

static void
R_addExternalRoutine(DllInfo *info,
		     const R_ExternalMethodDef * const croutine,
		     Rf_DotExternalSymbol *sym)
{
    sym->name = strdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
}

static void
R_addCRoutine(DllInfo *info, const R_CMethodDef * const croutine,
	      Rf_DotCSymbol *sym)
{
    sym->name = strdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
    if(croutine->types)
	R_setPrimitiveArgTypes(croutine, sym);
}

static void
R_addCallRoutine(DllInfo *info, const R_CallMethodDef * const croutine,
		 Rf_DotCallSymbol *sym)
{
    sym->name = strdup(croutine->name);
    sym->fun = croutine->fun;
    sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
}

static void
Rf_freeCSymbol(Rf_DotCSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeCallSymbol(Rf_DotCallSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeExternalSymbol(Rf_DotCallSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeFortranSymbol(Rf_DotFortranSymbol *sym)
{
    free(sym->name);
}

static void
Rf_freeDllInfo(DllInfo *info)
{
    int i;
    free(info->name);
    free(info->path);
    if(info->CSymbols) {
	for(i = 0; i < info->numCSymbols; i++)
	    Rf_freeCSymbol(info->CSymbols+i);
	free(info->CSymbols);
    }
    if(info->CallSymbols) {
	for(i = 0; i < info->numCallSymbols; i++)
	    Rf_freeCallSymbol(info->CallSymbols+i);
	free(info->CallSymbols);
    }
    if(info->ExternalSymbols) {
	for(i = 0; i < info->numExternalSymbols; i++)
	    Rf_freeExternalSymbol(info->ExternalSymbols+i);
	free(info->ExternalSymbols);
    }
    if(info->FortranSymbols) {
	for(i = 0; i < info->numFortranSymbols; i++)
	    Rf_freeFortranSymbol(info->FortranSymbols+i);
	free(info->FortranSymbols);
    }
}


typedef void (*DllInfoUnloadCall)(DllInfo *);
typedef DllInfoUnloadCall DllInfoInitCall;

static Rboolean
R_callDLLUnload(DllInfo *dllInfo)
{
    char buf[1024];
    DllInfoUnloadCall f;
    R_RegisteredNativeSymbol symbol;
    symbol.type = R_ANY_SYM;

    snprintf(buf, 1024, "R_unload_%s", dllInfo->name);
    f = reinterpret_cast<DllInfoUnloadCall>(R_dlsym(dllInfo, buf, &symbol));
    if(f) f(dllInfo);

    return(TRUE);
}

	/* Remove the specified DLL from the current DLL list */
	/* Returns 1 if the DLL was found and removed from */
	/* the list and returns 0 otherwise. */

static int DeleteDLL(const char *path)
{
    int   i, loc;

    for (i = 0; i < CountDLL; i++) {
	if (streql(path, LoadedDLL[i].path)) {
	    loc = i;
	    goto found;
	}
    }
    return 0;
found:
#ifdef CACHE_DLL_SYM
    if(R_osDynSymbol->deleteCachedSymbols)
	R_osDynSymbol->deleteCachedSymbols(&LoadedDLL[loc]);
#endif
    R_callDLLUnload(&LoadedDLL[loc]);
    R_osDynSymbol->closeLibrary(LoadedDLL[loc].handle);
    Rf_freeDllInfo(LoadedDLL+loc);
    /* FIXME: why not use memcpy here? */
    for(i = loc + 1 ; i < CountDLL ; i++) {
	LoadedDLL[i - 1].path = LoadedDLL[i].path;
	LoadedDLL[i - 1].name = LoadedDLL[i].name;
	LoadedDLL[i - 1].handle = LoadedDLL[i].handle;
	LoadedDLL[i - 1].useDynamicLookup = LoadedDLL[i].useDynamicLookup;
	LoadedDLL[i - 1].numCSymbols = LoadedDLL[i].numCSymbols;
	LoadedDLL[i - 1].numCallSymbols = LoadedDLL[i].numCallSymbols;
	LoadedDLL[i - 1].numFortranSymbols = LoadedDLL[i].numFortranSymbols;
	LoadedDLL[i - 1].numExternalSymbols = LoadedDLL[i].numExternalSymbols;
	LoadedDLL[i - 1].CSymbols = LoadedDLL[i].CSymbols;
	LoadedDLL[i - 1].CallSymbols = LoadedDLL[i].CallSymbols;
	LoadedDLL[i - 1].FortranSymbols = LoadedDLL[i].FortranSymbols;
	LoadedDLL[i - 1].ExternalSymbols = LoadedDLL[i].ExternalSymbols;
	LoadedDLL[i - 1].forceSymbols = LoadedDLL[i].forceSymbols;
    }
    CountDLL--;
    return 1;
}

attribute_hidden
DL_FUNC Rf_lookupCachedSymbol(const char *name, const char *pkg, int all)
{
#ifdef CACHE_DLL_SYM
    int i;
    for (i = 0; i < nCPFun; i++)
	if (streql(name, CPFun[i].name) &&
	    (all || streql(pkg, CPFun[i].pkg)))
	    return CPFun[i].func;
#endif

    return(nullptr);
}



#ifdef Win32
#define DLLerrBUFSIZE 4000
#else  /* Not Windows */
#define DLLerrBUFSIZE 1000
#endif

static char DLLerror[DLLerrBUFSIZE] = "";

/* the error message; length taken from ERRBUFSIZE in ./hpdlfcn.cpp  */

	/* Inserts the specified DLL at the head of the DLL list */
	/* Returns 1 if the DLL was successfully added */
	/* and returns 0 if the DLL table is full or */
	/* or if dlopen fails for some reason. */


static DllInfo* AddDLL(const char *path, int asLocal, int now,
		       const char *DLLsearchpath)
{
    HINSTANCE handle;
    DllInfo *info = nullptr;

    DeleteDLL(path);
    if(CountDLL == MaxNumDLLs) {
	strcpy(DLLerror, _("maximal number of DLLs reached..."));
	return nullptr;
    }

    handle = R_osDynSymbol->loadLibrary(path, asLocal, now, DLLsearchpath);

    if(handle == nullptr) {
	R_osDynSymbol->getError(DLLerror, DLLerrBUFSIZE);
	return nullptr;
    }

    info = R_RegisterDLL(handle, path);

    /* Now look for an initializing routine named R_init_<object name>.
       If it is present, we call it. It should take a reference to the
       DllInfo object currently being initialized.
    */
    if(info) {
	const char *nm = info->name;
	size_t len = strlen(nm) + 9;
	char tmp[len]; // R_init_ + underscore + null
	DllInfoInitCall f;
#ifdef HAVE_NO_SYMBOL_UNDERSCORE
	snprintf(tmp, len,  "%s%s","R_init_", info->name);
#else
	snprintf(tmp, len, "_%s%s","R_init_", info->name);
#endif
	f = DllInfoInitCall(R_osDynSymbol->dlsym(info, tmp));
	/* If that failed, might have used the package name with
	   . replaced by _ (as . it not valid in symbol names). */
	if(!f) {
	    /* This is potentially unsafe in MBCSs, as '.' might be
	       part of a character: but is not in UTF-8 */
	    for(char *p = tmp; *p; p++) if(*p == '.') *p = '_';
	    f = DllInfoInitCall(R_osDynSymbol->dlsym(info, tmp));
	}
	if(f) f(info);
    }

    return info;
}


static DllInfo *R_RegisterDLL(HINSTANCE handle, const char *path)
{
    char *dpath,  DLLname[PATH_MAX], *p;
    DllInfo *info;

    dpath = static_cast<char *>(malloc(strlen(path)+1));
    if(dpath == nullptr) {
	strcpy(DLLerror, _("could not allocate space for 'path'"));
	R_osDynSymbol->closeLibrary(handle);
	return nullptr;
    }
    strcpy(dpath, path);

    if(R_osDynSymbol->fixPath) R_osDynSymbol->fixPath(dpath);

    /* keep only basename from path */
    p = Rf_strrchr(dpath, FILESEP[0]);
    if(!p) p = dpath; else p++;
    if(strlen(p) < PATH_MAX) strcpy(DLLname, p);
    else Rf_error(_("DLLname '%s' is too long"), p);

    /* remove SHLIB_EXT if present */
    p = DLLname + strlen(DLLname) - strlen(SHLIB_EXT);
#ifdef Win32  /* case-insensitive file system */
    if(p > DLLname && stricmp(p, SHLIB_EXT) == 0) *p = '\0';
#else
    if(p > DLLname && streql(p, SHLIB_EXT)) *p = '\0';
#endif

    if (addDLL(dpath, DLLname, handle)) {
	info = &LoadedDLL[CountDLL-1];
	/* default is to use old-style dynamic lookup.  The object's
	   initialization routine can limit access by setting this to FALSE.
	*/
	info->useDynamicLookup = TRUE;
	info->forceSymbols = FALSE;
	return info;
    } else
	return NULL;
    return(info);
}

static int
addDLL(char *dpath, const char *DLLname, HINSTANCE handle)
{
    int ans = CountDLL;
    char *name = static_cast<char *>(malloc(strlen(DLLname)+1));
    if(name == nullptr) {
	strcpy(DLLerror, _("could not allocate space for 'name'"));
	if(handle)
	    R_osDynSymbol->closeLibrary(handle);
	free(dpath);
	return 0;
    }

    strcpy(name, DLLname);
    LoadedDLL[CountDLL].path = dpath;
    LoadedDLL[CountDLL].name = name;
    LoadedDLL[CountDLL].handle = handle;

    LoadedDLL[CountDLL].numCSymbols = 0;
    LoadedDLL[CountDLL].numCallSymbols = 0;
    LoadedDLL[CountDLL].numFortranSymbols = 0;
    LoadedDLL[CountDLL].numExternalSymbols = 0;
    LoadedDLL[CountDLL].CSymbols = nullptr;
    LoadedDLL[CountDLL].CallSymbols = nullptr;
    LoadedDLL[CountDLL].FortranSymbols = nullptr;
    LoadedDLL[CountDLL].ExternalSymbols = nullptr;
    CountDLL++;

    return(ans);
}

static Rf_DotCSymbol *
Rf_lookupRegisteredCSymbol(DllInfo *info, const char *name)
{
    for(int i = 0; i < info->numCSymbols; i++) {
	if(streql(name, info->CSymbols[i].name))
	    return(&(info->CSymbols[i]));
    }

    return nullptr;
}

static Rf_DotFortranSymbol *
Rf_lookupRegisteredFortranSymbol(DllInfo *info, const char *name)
{
    for(int i = 0; i < info->numFortranSymbols; i++) {
	if(streql(name, info->FortranSymbols[i].name))
	    return(&(info->FortranSymbols[i]));
    }

    return nullptr;
}

static Rf_DotCallSymbol *
Rf_lookupRegisteredCallSymbol(DllInfo *info, const char *name)
{
    for(int i = 0; i < info->numCallSymbols; i++) {
	if(streql(name, info->CallSymbols[i].name))
	    return(&(info->CallSymbols[i]));
    }
    return nullptr;
}

static Rf_DotExternalSymbol *
Rf_lookupRegisteredExternalSymbol(DllInfo *info, const char *name)
{
    for(int i = 0; i < info->numExternalSymbols; i++) {
	if(streql(name, info->ExternalSymbols[i].name))
	    return(&(info->ExternalSymbols[i]));
    }
    return nullptr;
}

static DL_FUNC
R_getDLLRegisteredSymbol(DllInfo *info, const char *name,
			 R_RegisteredNativeSymbol *symbol)
{
    NativeSymbolType purpose = R_ANY_SYM;

    if(symbol)
	purpose = symbol->type;

    if((purpose == R_ANY_SYM || purpose == R_C_SYM) &&
       info->numCSymbols > 0) {
	Rf_DotCSymbol *sym;
	sym = Rf_lookupRegisteredCSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_C_SYM;
		symbol->symbol.c = sym;
		symbol->dll = info;
	    }

	    return(static_cast<DL_FUNC>(sym->fun));
	}
    }

    if((purpose == R_ANY_SYM || purpose == R_CALL_SYM) &&
       info->numCallSymbols > 0) {
	Rf_DotCallSymbol *sym;
	sym = Rf_lookupRegisteredCallSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_CALL_SYM;
		symbol->symbol.call = sym;
		symbol->dll = info;
	    }
	    return(static_cast<DL_FUNC>(sym->fun));
	}
    }

    if((purpose == R_ANY_SYM || purpose == R_FORTRAN_SYM) &&
       info->numFortranSymbols > 0) {
	Rf_DotFortranSymbol *sym;
	sym = Rf_lookupRegisteredFortranSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_FORTRAN_SYM;
		symbol->symbol.fortran = sym;
		symbol->dll = info;
	    }
	    return(static_cast<DL_FUNC>(sym->fun));
	}
    }

    if((purpose == R_ANY_SYM || purpose == R_EXTERNAL_SYM) &&
       info->numExternalSymbols > 0) {
	Rf_DotExternalSymbol *sym;
	sym = Rf_lookupRegisteredExternalSymbol(info, name);
	if(sym) {
	    if(symbol) {
		symbol->type = R_EXTERNAL_SYM;
		symbol->symbol.external = sym;
		symbol->dll = info;
	    }
	    return(static_cast<DL_FUNC>(sym->fun));
	}
    }

    return nullptr;
}

DL_FUNC attribute_hidden
R_dlsym(DllInfo *info, const char *name,
	R_RegisteredNativeSymbol *symbol)
{
    size_t len = strlen(name) + 4;
    vector<char> bufv(len);  /* up to 3 additional underscores */
    char* buf = &bufv[0];

    DL_FUNC f;

    f = R_getDLLRegisteredSymbol(info, name, symbol);
    if(f) return(f);


    if(info->useDynamicLookup == FALSE) return(nullptr);

#ifdef HAVE_NO_SYMBOL_UNDERSCORE
    snprintf(buf, len, "%s", name);
#else
    snprintf(buf, len, "_%s", name);
#endif

#ifdef HAVE_F77_UNDERSCORE
    if(symbol && symbol->type == R_FORTRAN_SYM) {
	strcat(buf, "_");
# ifdef HAVE_F77_EXTRA_UNDERSCORE
	if(strchr(name, '_')) strcat(buf, "_");
# endif
    }
#endif

    f = static_cast<DL_FUNC>(R_osDynSymbol->dlsym(info, buf));
#ifdef HAVE_F77_UNDERSCORE
    if (!f && symbol && symbol->type == R_ANY_SYM) {
	strcat(buf, "_");
# ifdef HAVE_F77_EXTRA_UNDERSCORE
	if(strchr(name, '_')) strcat(buf, "_");
# endif
	f = static_cast<DL_FUNC>(R_osDynSymbol->dlsym(info, buf));
    }
#endif

    return f;
}

/* R_FindSymbol checks whether one of the objects that have been
   loaded contains the symbol name and returns a pointer to that
   symbol upon success.
 */

DL_FUNC R_FindSymbol(const char *name, const char *pkg,
		     R_RegisteredNativeSymbol *symbol)
{
    DL_FUNC fcnptr = nullptr;
    int i, all = (strlen(pkg) == 0), doit;

    if(R_osDynSymbol->lookupCachedSymbol)
	fcnptr = R_osDynSymbol->lookupCachedSymbol(name, pkg, all);

    if(fcnptr) return(fcnptr);

    /* The following is not legal ANSI C. */
    /* It is only meant to be used in systems supporting */
    /* the dlopen() interface, in which systems data and  */
    /* function pointers _are_ the same size and _can_   */
    /* be cast without loss of information.	     */

    for (i = CountDLL - 1; i >= 0; i--) {
	doit = all;
	if(!doit && streql(pkg, LoadedDLL[i].name)) doit = 2;
	if(doit && LoadedDLL[i].forceSymbols) doit = 0;
	if(doit) {
	    fcnptr = R_dlsym(&LoadedDLL[i], name, symbol); /* R_osDynSymbol->dlsym */
	    if (fcnptr != nullptr) {
		if(symbol)
		    symbol->dll = LoadedDLL+i;
#ifdef CACHE_DLL_SYM
		if(strlen(pkg) <= 20 && strlen(name) <= 40 && nCPFun < MAX_CACHE
		   && (!symbol || !symbol->symbol.c)) {
		    strcpy(CPFun[nCPFun].pkg, LoadedDLL[i].name);
		    strcpy(CPFun[nCPFun].name, name);
		    CPFun[nCPFun++].func = fcnptr;
		}
#endif
		return fcnptr;
	    }
	}
	if(doit > 1) return nullptr;  /* Only look in the first-matching DLL */
    }

    return nullptr;
}


static void GetFullDLLPath(SEXP call, char *buf, const char *const path)
{
    R_osDynSymbol->getFullDLLPath(call, buf, path);
}

	/* do_dynload implements the R-Interface for the */
	/* loading of shared objects */

/*
  Extended to support 2 additional arguments (3 in total).
  First argument is the name of the DLL.
  Second argument is a logical indicating whether we
  want the symbols to be kept in their own local symbol table
  or added to the global symbol table of the application.
  Third argument is a logical indicating whether the
  dynamic loading should relocate all routine symbols
  now and signal any errors immediately or lazily relocate
  the symbols as they are invoked. This is useful for
  developers so that they can ensure that all the symbols
  are available before they release, and allows users to
  call routines from "incomplete" DLLs.
 */

SEXP attribute_hidden do_dynload(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_, RObject* local_, RObject* now_, RObject* dots_)
{
    char buf[2 * PATH_MAX];
    DllInfo *info;

    if (!Rf_isString(x_) || LENGTH(x_) != 1)
	Rf_error(_("character argument expected"));
    GetFullDLLPath(call, buf, Rf_translateChar(STRING_ELT(x_, 0)));
    /* AddDLL does this DeleteDLL(buf); */
    info = AddDLL(buf, LOGICAL(local_)[0], LOGICAL(now_)[0],
		  Rf_translateChar(STRING_ELT(dots_, 0)));
    if(!info)
	Rf_error(_("unable to load shared object '%s':\n  %s"), buf, DLLerror);
    return(Rf_MakeDLLInfo(info));
}

SEXP attribute_hidden do_dynunload(/*const*/ Expression* call, const BuiltInFunction* op, RObject* x_)
{
    char buf[2 * PATH_MAX];

    if (!Rf_isString(x_) || LENGTH(x_) != 1)
	Rf_error(_("character argument expected"));
    GetFullDLLPath(call, buf, Rf_translateChar(STRING_ELT(x_, 0)));
    if(!DeleteDLL(buf))
	Rf_error(_("shared object '%s\' was not loaded"), buf);
    return R_NilValue;
}

int R_moduleCdynload(const char *module, int local, int now)
{
    char dllpath[PATH_MAX], *p = getenv("R_HOME");
    DllInfo *res;

    if(!p) return 0;
#ifdef R_ARCH
    snprintf(dllpath, PATH_MAX, "%s%smodules%s%s%s%s%s", p, FILESEP, FILESEP,
	     R_ARCH, FILESEP, module, SHLIB_EXT);
#else
    snprintf(dllpath, PATH_MAX, "%s%smodules%s%s%s", p, FILESEP, FILESEP,
	     module, SHLIB_EXT);
#endif
    res = AddDLL(dllpath, local, now, "");
    if(!res)
	Rf_warning(_("unable to load shared object '%s':\n  %s"),
		dllpath, DLLerror);
    return res != nullptr ? 1 : 0;
}

int R_cairoCdynload(int local, int now)
{
    char dllpath[PATH_MAX];
    const char *p = getenv("R_HOME"), *module = "cairo";
    DllInfo *res;

    if(!p) return 0;
#ifdef R_ARCH
    snprintf(dllpath, PATH_MAX, "%s/library/grDevices/libs/%s/%s%s",
	     p, R_ARCH, module, SHLIB_EXT);
#else
    snprintf(dllpath, PATH_MAX, "%s/library/grDevices/libs/%s%s",
	     p, module, SHLIB_EXT);
#endif
    res = AddDLL(dllpath, local, now, "");
    if(!res)
	Rf_warning(_("unable to load shared object '%s':\n  %s"),
		dllpath, DLLerror);
    return res != nullptr ? 1 : 0;
}

/**
  Creates an R object representing the value of the
  function pointer given by `f'. This object has class
  NativeSymbol and can be used to relay symbols from
  one DLL to another.
 */
static SEXP
Rf_MakeNativeSymbolRef(DL_FUNC f)
{
    SEXP ref, klass;

    PROTECT(ref = R_MakeExternalPtrFn(f, Rf_install("native symbol"),
				      nullptr));
    PROTECT(klass = Rf_mkString("NativeSymbol"));
    Rf_setAttrib(ref, R_ClassSymbol, klass);
    UNPROTECT(2);
    return(ref);
}

static void
freeRegisteredNativeSymbolCopy(SEXP ref)
{
   void *ptr;
   ptr = R_ExternalPtrAddr(ref);
   if (ptr)
       free(ptr);
}

static SEXP
Rf_MakeRegisteredNativeSymbol(R_RegisteredNativeSymbol *symbol)
{
    SEXP ref, klass;
    R_RegisteredNativeSymbol *copy;
    copy = static_cast<R_RegisteredNativeSymbol *>(malloc(1 * sizeof(R_RegisteredNativeSymbol)));
    if(!copy) {
	Rf_error(n_("cannot allocate memory for registered native symbol (%d byte)",
		       "cannot allocate memory for registered native symbol (%d bytes)",
		      (int) sizeof(R_RegisteredNativeSymbol)),
	      int(sizeof(R_RegisteredNativeSymbol)));
    }
    *copy = *symbol;

    PROTECT(ref = R_MakeExternalPtr(copy,
				    Rf_install("registered native symbol"),
				    nullptr));
    R_RegisterCFinalizer(ref, freeRegisteredNativeSymbolCopy);

    PROTECT(klass = Rf_mkString("RegisteredNativeSymbol"));
    Rf_setAttrib(ref, R_ClassSymbol, klass);

    UNPROTECT(2);
    return(ref);
}


static SEXP
Rf_makeDllObject(HINSTANCE inst)
{
    SEXP ans;

    PROTECT(ans = R_MakeExternalPtr(inst, Rf_install("DLLHandle"),
				    nullptr));
    Rf_setAttrib(ans, R_ClassSymbol, Rf_mkString("DLLHandle"));
    UNPROTECT(1);

    return(ans);
}

static SEXP
Rf_makeDllInfoReference(HINSTANCE inst)
{
    SEXP ans;

    PROTECT(ans = R_MakeExternalPtr(inst, Rf_install("DLLInfo"),
				    Rf_install("DLLInfo")));
    Rf_setAttrib(ans, R_ClassSymbol, Rf_mkString("DLLInfoReference"));
    UNPROTECT(1);

    return(ans);
}


/**
 Creates an R object representing the public DLL information stored in
 info. Currently this is only the short and the long, fully qualified
 name of the DLL and whether we only look for symbols that have been
 registered in this DLL or do we also use dynamic lookup.
 */
static SEXP
Rf_MakeDLLInfo(DllInfo *info)
{
    SEXP ref, elNames;
    size_t i, n;
    const char *const names[] = {"name", "path", "dynamicLookup",
				 "handle", "info"};

    n = sizeof(names)/sizeof(names[0]);

    PROTECT(ref = Rf_allocVector(VECSXP, n));
    SET_VECTOR_ELT(ref, 0, Rf_ScalarString(info->name ? Rf_mkChar(info->name)
					   : R_BlankString));

    SET_VECTOR_ELT(ref, 1, Rf_ScalarString(info->path ? Rf_mkChar(info->path)
					   : R_BlankString));

    SET_VECTOR_ELT(ref, 2, Rf_ScalarLogical(info->useDynamicLookup));

    SET_VECTOR_ELT(ref, 3, Rf_makeDllObject(info->handle));

    SET_VECTOR_ELT(ref, 4, Rf_makeDllInfoReference(HINSTANCE( info)));

    PROTECT(elNames = Rf_allocVector(STRSXP, n));
    for(i = 0; i < n; i++)
	SET_STRING_ELT(elNames, i, Rf_mkChar(names[i]));
    Rf_setAttrib(ref, R_NamesSymbol, elNames);

    Rf_setAttrib(ref, R_ClassSymbol, Rf_mkString("DLLInfo"));

    UNPROTECT(2);

    return(ref);
}

/*
  This is the routine associated with the getNativeSymbolInfo()
  function and it takes the name of a symbol and optionally an
  object identifier (package usually) in which to restrict the search
  for this symbol. It resolves the symbol and returns it to the caller
  giving the symbol address, the package information (i.e. name and
  fully qualified shared object name). If the symbol was explicitly
  registered (rather than dynamically resolved by R), then we pass
  back that information also, giving the number of arguments it
  expects and the interface by which it should be called.
  The returned object has class NativeSymbol. If the symbol was
  registered, we add a class identifying the interface type
  for which it is intended (i.e. .C(), .Call(), etc.)
 */
SEXP attribute_hidden
R_getSymbolInfo(SEXP sname, SEXP spackage, SEXP withRegistrationInfo)
{
    const void *vmax = vmaxget();
    const char *package, *name;
    R_RegisteredNativeSymbol symbol = {R_ANY_SYM, {nullptr}, nullptr};
    SEXP sym = R_NilValue;
    DL_FUNC f = nullptr;

    package = "";

    name = Rf_translateChar(STRING_ELT(sname, 0));

    if(Rf_length(spackage)) {
	if(TYPEOF(spackage) == STRSXP)
	    package = Rf_translateChar(STRING_ELT(spackage, 0));
	else if(TYPEOF(spackage) == EXTPTRSXP &&
		R_ExternalPtrTag(spackage) == Rf_install("DLLInfo")) {
	    f = R_dlsym(static_cast<DllInfo *>(R_ExternalPtrAddr(spackage)), name, &symbol);
	    package = nullptr;
	} else
	    Rf_error(_("must pass package name or DllInfo reference"));
    }

    if(package)
	f = R_FindSymbol(name, package, &symbol);

    if(f)
	sym = createRSymbolObject(sname, f, &symbol,
				  Rboolean(LOGICAL(withRegistrationInfo)[0]));

    vmaxset(vmax);
    return sym;
}

SEXP attribute_hidden
R_getDllTable()
{
    int i;
    SEXP ans;

 again:
    PROTECT(ans = Rf_allocVector(VECSXP, CountDLL));
    for(i = 0; i < CountDLL; i++) {
	SET_VECTOR_ELT(ans, i, Rf_MakeDLLInfo(&(LoadedDLL[i])));
    }
    Rf_setAttrib(ans, R_ClassSymbol, Rf_mkString("DLLInfoList"));
    UNPROTECT(1);

    /* There is a problem here: The allocations can cause gc, and gc
       may result in no longer referenced DLLs being unloaded.  So
       CountDLL can be reduced during this loop.  A simple work-around
       is to just try again until CountDLL at the end is the same as
       it was at the beginning.  LT */
    if (CountDLL != LENGTH(ans))
	goto again;

    return(ans);
}

static SEXP
createRSymbolObject(SEXP sname, DL_FUNC f, R_RegisteredNativeSymbol *symbol,
		    Rboolean withRegistrationInfo)
{
    SEXP tmp, klass, sym, names;
    int n = (symbol->type != R_ANY_SYM) ? 4 : 3;
    int numProtects = 0;

    PROTECT(sym = Rf_allocVector(VECSXP, n));    numProtects++;
    PROTECT(names = Rf_allocVector(STRSXP, n));    numProtects++;

    if(!sname || sname == R_NilValue) {
	PROTECT(sname = Rf_mkString(symbol->symbol.call->name));
	numProtects++;
    }

    SET_VECTOR_ELT(sym, 0, sname);
    SET_STRING_ELT(names, 0, Rf_mkChar("name"));

    SET_VECTOR_ELT(sym, 1,
		   withRegistrationInfo && symbol && symbol->symbol.c && symbol->dll
		   ? Rf_MakeRegisteredNativeSymbol(symbol)
		   : Rf_MakeNativeSymbolRef(f));
    SET_STRING_ELT(names, 1, Rf_mkChar("address"));
    if(symbol->dll)
	SET_VECTOR_ELT(sym, 2, Rf_MakeDLLInfo(symbol->dll));
    SET_STRING_ELT(names, 2, Rf_mkChar("dll"));


    PROTECT(klass = Rf_allocVector(STRSXP, (symbol->type != R_ANY_SYM ? 2 : 1)));
    numProtects++;
    SET_STRING_ELT(klass, LENGTH(klass) - 1, Rf_mkChar("NativeSymbolInfo"));

    if(n > 3) {
	/* Add the registration information:
	   the number of arguments and the classname.
	*/
	int nargs = -1;
	const char *className = "";
	switch(symbol->type) {
	case R_C_SYM:
	    nargs = symbol->symbol.c->numArgs;
	    className = "CRoutine";
	    break;
	case R_CALL_SYM:
	    nargs = symbol->symbol.call->numArgs;
	    className = "CallRoutine";
	    break;
	case R_FORTRAN_SYM:
	    nargs = symbol->symbol.fortran->numArgs;
	    className = "FortranRoutine";
	    break;
	case R_EXTERNAL_SYM:
	    nargs = symbol->symbol.external->numArgs;
	    className = "ExternalRoutine";
	    break;
	default:
	    /* Something unintended has happened if we get here. */
	    Rf_error(_("unimplemented type %d in createRSymbolObject"),
		  symbol->type);
	    break;
	}
	SET_VECTOR_ELT(sym, 3, tmp = Rf_ScalarInteger(nargs));
	SET_STRING_ELT(klass, 0, Rf_mkChar(className));
	SET_STRING_ELT(names, 3, Rf_mkChar("numParameters"));
    }

    Rf_setAttrib(sym, R_ClassSymbol, klass);
    Rf_setAttrib(sym, R_NamesSymbol, names);

    UNPROTECT(numProtects);
    return(sym);
}

static SEXP
R_getRoutineSymbols(NativeSymbolType type, DllInfo *info)
{
    SEXP ans;
    int i, num;
    R_RegisteredNativeSymbol  sym;
    DL_FUNC address = nullptr;

    sym.dll = info;
    sym.type =type;

    switch(type) {
    case R_CALL_SYM: num = info->numCallSymbols;
	break;
    case R_C_SYM: num = info->numCSymbols;
	break;
    case R_FORTRAN_SYM: num = info->numFortranSymbols;
	break;
    case R_EXTERNAL_SYM: num = info->numExternalSymbols;
	break;
    default:
	num = 0;
    }

    PROTECT(ans = Rf_allocVector(VECSXP, num));

    for(i = 0; i < num ; i++) {
	switch(type) {
	case R_CALL_SYM:
	    sym.symbol.call = &info->CallSymbols[i];
	    address = sym.symbol.call->fun;
	    break;
	case R_C_SYM:
	    sym.symbol.c = &info->CSymbols[i];
	    address = sym.symbol.c->fun;
	    break;
	case R_FORTRAN_SYM:
	    sym.symbol.fortran = &info->FortranSymbols[i];
	    address = sym.symbol.fortran->fun;
	    break;
	case R_EXTERNAL_SYM:
	    sym.symbol.external = &info->ExternalSymbols[i];
	    address = sym.symbol.external->fun;
	    break;
	default:
	    continue;
	}
	SET_VECTOR_ELT(ans, i, createRSymbolObject(nullptr,  address, &sym, TRUE));/* XXX */
    }

    Rf_setAttrib(ans, R_ClassSymbol, Rf_mkString("NativeRoutineList"));
    UNPROTECT(1);
    return(ans);
}


SEXP attribute_hidden
R_getRegisteredRoutines(SEXP dll)
{
    DllInfo *info;
    SEXP ans, snames;
    int i;
    const char * const names[] = {".C", ".Call", ".Fortran", ".External"};

    if(TYPEOF(dll) != EXTPTRSXP &&
       R_ExternalPtrTag(dll) != Rf_install("DLLInfo"))
	Rf_error(_("R_getRegisteredRoutines() expects a DllInfo reference"));

    info = static_cast<DllInfo *>(R_ExternalPtrAddr(dll));
    if(!info) Rf_error(_("NULL value passed for DllInfo"));


    PROTECT(ans = Rf_allocVector(VECSXP, 4));

    SET_VECTOR_ELT(ans, 0, R_getRoutineSymbols(R_C_SYM, info));
    SET_VECTOR_ELT(ans, 1, R_getRoutineSymbols(R_CALL_SYM, info));
    SET_VECTOR_ELT(ans, 2, R_getRoutineSymbols(R_FORTRAN_SYM, info));
    SET_VECTOR_ELT(ans, 3, R_getRoutineSymbols(R_EXTERNAL_SYM, info));

    PROTECT(snames = Rf_allocVector(STRSXP, 4));
    for(i = 0; i < 4; i++)
	SET_STRING_ELT(snames, i, Rf_mkChar(names[i]));
    Rf_setAttrib(ans, R_NamesSymbol, snames);
    UNPROTECT(2);
    return(ans);
}

SEXP attribute_hidden
do_getSymbolInfo(/*const*/ Expression* call, const BuiltInFunction* op, RObject* name_, RObject* package_, RObject* with_registration_info_)
{
    const char *package = "", *name;
    R_RegisteredNativeSymbol symbol = {R_ANY_SYM, {nullptr}, nullptr};
    SEXP sym = R_NilValue;
    DL_FUNC f = nullptr;

    SEXP sname = name_, spackage = package_, 
	withRegistrationInfo = with_registration_info_;

    if (!Rf_isString(sname) || LENGTH(sname) != 1)
	Rf_error(_("invalid '%s' argument"), "name");
    name = Rf_translateChar(STRING_ELT(sname, 0));
    if(Rf_length(spackage)) {
	if(TYPEOF(spackage) == STRSXP)
	    package = Rf_translateChar(STRING_ELT(spackage, 0));
	else if(TYPEOF(spackage) == EXTPTRSXP &&
		R_ExternalPtrTag(spackage) == Rf_install("DLLInfo")) {
	    f = R_dlsym(static_cast<DllInfo *>(R_ExternalPtrAddr(spackage)), name, &symbol);
	    package = nullptr;
	} else
	    Rf_error(_("must pass package name or DllInfo reference"));
    }
    if(package)
	f = R_FindSymbol(name, package, &symbol);
    if(f)
	sym = createRSymbolObject(sname, f, &symbol,
				  Rboolean(LOGICAL(withRegistrationInfo)[0]));
    return sym;
}

/* .Internal(getLoadedDLLs()) */
SEXP attribute_hidden
do_getDllTable(/*const*/ Expression* call, const BuiltInFunction* op)
{
    SEXP ans, nm;

 again:
    PROTECT(ans = Rf_allocVector(VECSXP, CountDLL));
    for(int i = 0; i < CountDLL; i++)
	SET_VECTOR_ELT(ans, i, Rf_MakeDLLInfo(&(LoadedDLL[i])));
    Rf_setAttrib(ans, R_ClassSymbol, Rf_mkString("DLLInfoList"));
    UNPROTECT(1);

    /* There is a problem here: The allocations can cause gc, and gc
       may result in no longer referenced DLLs being unloaded.  So
       CountDLL can be reduced during this loop.  A simple work-around
       is to just try again until CountDLL at the end is the same as
       it was at the beginning.  LT */
    if (CountDLL != LENGTH(ans)) goto again;

    PROTECT(ans);
    PROTECT(nm = Rf_allocVector(STRSXP, CountDLL));
    Rf_setAttrib(ans, R_NamesSymbol, nm);
    for(int i = 0; i < CountDLL; i++)
	SET_STRING_ELT(nm, i,
		       STRING_ELT(VECTOR_ELT(VECTOR_ELT(ans, i), 0), 0));
    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden
do_getRegisteredRoutines(/*const*/ Expression* call, const BuiltInFunction* op, RObject* info_)
{
    const char * const names[] = {".C", ".Call", ".Fortran", ".External"};

    SEXP dll = info_, ans, snames;

    if(TYPEOF(dll) != EXTPTRSXP &&
       R_ExternalPtrTag(dll) != Rf_install("DLLInfo"))
	Rf_error(_("R_getRegisteredRoutines() expects a DllInfo reference"));

    DllInfo *info = static_cast<DllInfo *>(R_ExternalPtrAddr(dll));
    if(!info) Rf_error(_("NULL value passed for DllInfo"));


    PROTECT(ans = Rf_allocVector(VECSXP, 4));

    SET_VECTOR_ELT(ans, 0, R_getRoutineSymbols(R_C_SYM, info));
    SET_VECTOR_ELT(ans, 1, R_getRoutineSymbols(R_CALL_SYM, info));
    SET_VECTOR_ELT(ans, 2, R_getRoutineSymbols(R_FORTRAN_SYM, info));
    SET_VECTOR_ELT(ans, 3, R_getRoutineSymbols(R_EXTERNAL_SYM, info));

    PROTECT(snames = Rf_allocVector(STRSXP, 4));
    for(int i = 0; i < 4; i++)
	SET_STRING_ELT(snames, i, Rf_mkChar(names[i]));
    Rf_setAttrib(ans, R_NamesSymbol, snames);
    UNPROTECT(2);
    return(ans);
}



/* Experimental interface for exporting and importing functions and
   data from one package for use from C code in a package.  The
   registration part probably ought to be integrated with the other
   registrations.  The naming of these routines may be less than
   ideal. */

static GCRoot<> CEntryTable = nullptr;

static SEXP get_package_CEntry_table(const char *package)
{
    SEXP penv, pname;

    GCStackRoot<> zero(Rf_ScalarInteger(0));
    if (CEntryTable == nullptr) {
	CEntryTable = R_NewHashedEnv(R_NilValue, zero);
	R_PreserveObject(CEntryTable);
    }
    pname = Rf_install(package);
    penv = Rf_findVarInFrame(CEntryTable, pname);
    if (penv == R_UnboundValue) {
	penv = R_NewHashedEnv(R_NilValue, zero);
	Rf_defineVar(pname, penv, CEntryTable);
    }
    return penv;
}


void R_RegisterCCallable(const char *package, const char *name, DL_FUNC fptr)
{
    SEXP penv = get_package_CEntry_table(package);
    PROTECT(penv);
    SEXP eptr = R_MakeExternalPtrFn(fptr, R_NilValue, R_NilValue);
    PROTECT(eptr);
    Rf_defineVar(Rf_install(name), eptr, penv);
    UNPROTECT(2);
}

DL_FUNC R_GetCCallable(const char *package, const char *name)
{
    SEXP penv = get_package_CEntry_table(package);
    PROTECT(penv);
    SEXP eptr = Rf_findVarInFrame(penv, Rf_install(name));
    UNPROTECT(1);
    if (eptr == R_UnboundValue)
	Rf_error(_("function '%s' not provided by package '%s'"), name, package);
    else if (TYPEOF(eptr) != EXTPTRSXP)
	Rf_error(_("table entry must be an external pointer"));
    return R_ExternalPtrAddrFn(eptr);
}
