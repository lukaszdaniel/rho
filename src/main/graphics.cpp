/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2014  The R Core Team
 *  Copyright (C) 2002--2011  The R Foundation
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


 *  This is an extensive reworking by Paul Murrell of an original
 *  quick hack by Ross Ihaka designed to give a superset of the
 *  functionality in the AT&T Bell Laboratories GRZ library.
 */

#define R_NO_REMAP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Localization.h>
#include <float.h> /* for DBL_EPSILON etc */
#include <Graphics.h>
// --> R_ext/GraphicsEngine.h + Rgraphics.h
#include <Rmath.h>		/* eg. fmax2() */

static void GLPretty(double *ul, double *uh, int *n);

// used in GScale(), but also grDevices/src/axis_scales.cpp :
// (usr, log, n_inp) |--> (axp, n_out) :
void GAxisPars(double *min, double *max, int *n, Rboolean log, int axis)
{
#define EPS_FAC_2 100
    bool swap = (*min > *max);
    double t_, min_o, max_o;

    if(swap) { /* Feature: in R, something like  xlim = c(100,0)  just works */
	std::swap(*min, *max);
    }
    /* save only for the extreme case (EPS_FAC_2): */
    min_o = *min; max_o = *max;

    if(log) {
	/* Avoid infinities */
	if(*max > 308) *max = 308;
	if(*min < -307) *min = -307;
	*min = std::pow(10.0, *min);
	*max = std::pow(10.0, *max);
	GLPretty(min, max, n);
    }
    else GEPretty(min, max, n);

    double tmp2 = EPS_FAC_2 * DBL_EPSILON;/* << prevent overflow in product below */
    if(std::abs(*max - *min) < (t_ = std::max(std::abs(*max), std::abs(*min)))* tmp2) {
	/* Treat this case somewhat similar to the (min ~= max) case above */
	/* Too much accuracy here just shows machine differences */
	Rf_warning(_("relative range of values =%4.0f * EPS, is small (axis %d)")
		/*"to compute accurately"*/,
		std::abs(*max - *min) / (t_*DBL_EPSILON), axis);

	/* No pretty()ing anymore */
	*min = min_o;
	*max = max_o;
	double eps = .005 * std::abs(*max - *min);/* .005: not to go to DBL_MIN/MAX */
	*min += eps;
	*max -= eps;
	if(log) {
	    *min = std::pow(10.0, *min);
	    *max = std::pow(10.0, *max);
	}
	*n = 1;
    }
    if(swap) {
		std::swap(*min, *max);
    }
}

#define LPR_SMALL  2
#define LPR_MEDIUM 3

static void GLPretty(double* ul, double* uh, int* n)
{
    /* Generate pretty tick values --	LOGARITHMIC scale
     * __ ul < uh __
     * This only does a very simple setup.
     * The real work happens when the axis is drawn. */
    int p1, p2;
    double dl = *ul, dh = *uh;
    p1 = int(std::ceil(std::log10(dl)));
    p2 = int(std::floor(std::log10(dh)));
    if (p2 <= p1 && dh / dl > 10.0) {
	p1 = int(std::ceil(std::log10(dl) - 0.5));
	p2 = int(std::floor(std::log10(dh) + 0.5));
    }

    if (p2 <= p1) { /* floor(log10(uh)) <= ceil(log10(ul))
		     * <==>	 log10(uh) - log10(ul) < 2
		     * <==>		uh / ul	       < 100 */
	/* Very small range : Use tickmarks from a LINEAR scale
	 *		      Splus uses n = 9 here, but that is dumb */
	GPretty(ul, uh, n);
	*n = -*n;
    } else { /* extra tickmarks --> CreateAtVector() in ./plot.cpp */
	/* round to nice "1e<N>" */
	*ul = std::pow(10.0, p1);
	*uh = std::pow(10.0, p2);
	if (p2 - p1 <= LPR_SMALL)
	    *n = 3; /* Small range :	Use 1,2,5,10 times 10^k tickmarks */
	else if (p2 - p1 <= LPR_MEDIUM)
	    *n = 2; /* Medium range :	Use 1,5 times 10^k tickmarks */
	else
	    *n = 1; /* Large range :	Use 10^k tickmarks
		     *			But decimate, when there are too many*/
    }
}

void GPretty(double *lo, double *up, int *ndiv)
{
    GEPretty(lo, up, ndiv);
}
