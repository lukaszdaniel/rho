/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef STATS_STUBS_H
#define STATS_STUBS_H 1

#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#ifdef __cplusplus
extern "C" {
#endif

HIDDEN void S_Rf_divset(int alg, int iv[], int liv, int lv, double v[])
{
    static void(*fun)(int,int[],int,int,double[]) = NULL;
    if (fun == NULL)
	fun = (void(*)(int,int[],int,int,double[]))
	    R_GetCCallable("stats", "Rf_divset");
    fun(alg, iv, liv, lv, v);
}

HIDDEN void S_nlminb_iterate(double b[], double d[], double fx, double g[], double h[],
		 int iv[], int liv, int lv, int n, double v[], double x[])
{
    static void(*fun)(double[],double[],double,double[],double[],
		      int[],int,int,int,double[],double[]) = NULL;
    if (fun == NULL)
	fun = (void(*)(double[],double[],double,double[],double[],
			  int[],int,int,int,double[],double[]))
	    R_GetCCallable("stats", "nlminb_iterate");
    fun(b, d, fx, g, h, iv, liv, lv, n, v, x);
}

HIDDEN void S_nlsb_iterate(double b[], double d[], double dr[], int iv[], int liv,
	       int lv, int n, int nd, int p, double r[], double rd[],
	       double v[], double x[])
{
    static void(*fun)(double[],double[],double[],int[],int,int,
		      int,int,int,double[],double[],double[],
		      double[]) = NULL;
    if (fun == NULL)
	fun = (void(*)(double[],double[],double[],int[],int,
		       int, int,int,int,double[],
		       double[],double[],double[]))
	    R_GetCCallable("stats", "nlsb_iterate");
    fun(b, d, dr, iv, liv, lv, n, nd, p, r, rd, v, x);
}

HIDDEN void S_rcont2(int nrow[], int ncol[], int nrowt[], int ncolt[], 
         int ntotal[], double fact[], int jwork[], int matrix[])
{
    static void(*fun)(int[], int[], int[], int[], int[], double[], 
                      int[], int[]) = NULL;
    if (fun == NULL)
	fun = (void(*)(int[], int[], int[], int[], int[], double[], 
                       int[], int[]))
	    R_GetCCallable("stats", "rcont2");
    fun(nrow, ncol, nrowt, ncolt, ntotal, fact, jwork, matrix);
}
#ifdef __cplusplus
} //extern "C"
#endif

#endif /* STATS_STUBS_H */
