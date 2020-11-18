/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file BinaryFunction.cpp
 *
 * @brief Implementation of VectorOps::BinaryFunction and related functions.
 */

#include <rho/BinaryFunction.hpp>
#include <rho/PairList.hpp>
#include <rho/Symbol.hpp>

using namespace rho;
using namespace Symbols;
using namespace VectorOps;

// Implementation of operandsConformable() is in logic.cpp (for the
// time being).

void GeneralBinaryAttributeCopier::apply(VectorBase* vout,
					 const VectorBase* vl,
					 const VectorBase* vr)
{
    // Handle layout attributes:
    {
	RObject* dims = vl ? vl->getAttribute(DimSymbol) : nullptr;
	RObject* dimnames = nullptr;
	size_t nx = vl ? vl->size() : 0;
	size_t ny = vr ? vr->size() : 0;

	if (dims && (ny != 0 || nx == 0)) {
	    dimnames = vl->getAttribute(DimNamesSymbol);
	} else if (vr && (nx != 0 || ny == 0)) {
	    dims = vr->getAttribute(DimSymbol);
	} else
	    dims = nullptr;
	if (vr && !dimnames)
	    dimnames = vr->getAttribute(DimNamesSymbol);
	if (dims) {
	    Rf_setAttrib(vout, DimSymbol, dims);
	    // vout->setAttribute(DimSymbol, dims);
	    if (dimnames)
		vout->setAttribute(DimNamesSymbol, dimnames);
	} else {
	    // Neither operand is an array.  Get the names from the longer
	    // attribute (if present), prefering the first if the lengths are
	    // the same.
	    RObject* vl_names = vl ? vl->getAttribute(NamesSymbol) : nullptr;
	    if (vl_names && vout->size() == nx) {
		vout->setAttribute(NamesSymbol, vl_names);
	    } else if (vr) {
		RObject* vr_names = vr->getAttribute(NamesSymbol);
		if (vr_names && vout->size() == ny)
		    vout->setAttribute(NamesSymbol, vr_names);
	    }
	}
    }
    // Handle attributes related to time series:
    {
	RObject* tsp = vl? vl->getAttribute(TspSymbol): nullptr;
	RObject* klass = nullptr;
	if (tsp)
	    klass = vl->getAttribute(ClassSymbol);
	if (!tsp && vr) {
	   tsp = vr->getAttribute(TspSymbol); 
	   if (tsp)
	       klass = vr->getAttribute(ClassSymbol);
	}
	if (tsp)
	    vout->setAttribute(TspSymbol, tsp);
	if (klass)
	    vout->setAttribute(ClassSymbol, klass);
    }
}
