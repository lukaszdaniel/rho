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

/** @file VectorBase.cpp
 *
 * @brief Implementation of class VectorBase and related functions.
 */

#include <rho/VectorBase.hpp>

#include <rho/IntVector.hpp>
#include <rho/ListVector.hpp>
#include <rho/PairList.hpp>
#include <rho/StringVector.hpp>
#include <rho/Symbol.hpp>
#include <rho/errors.hpp>
#include <Localization.h>

using namespace rho;
using namespace Symbols;

namespace rho
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &STDVEC_LENGTHptr = STDVEC_LENGTH;
        const auto &SET_TRUELENGTHptr = SET_TRUELENGTH;
        const auto &STDVEC_TRUELENGTHptr = STDVEC_TRUELENGTH;
    } // namespace ForceNonInline
} // namespace rho

const ListVector *VectorBase::dimensionNames() const
{
    return static_cast<const ListVector *>(getAttribute(DimNamesSymbol));
}

const StringVector *VectorBase::dimensionNames(unsigned int d) const
{
    const ListVector *lv = dimensionNames();
    if (!lv || d > lv->size())
        return nullptr;
    return static_cast<const StringVector *>((*lv)[d - 1].get());
}

const IntVector *VectorBase::dimensions() const
{
    return static_cast<const IntVector *>(getAttribute(DimSymbol));
}

const StringVector *VectorBase::names() const
{
    return static_cast<const StringVector *>(getAttribute(NamesSymbol));
}

PairList *VectorBase::resizeAttributes(const PairList *attributes,
                                       std::size_t new_size)
{
    GCStackRoot<PairList> ans(PairList::cons(nullptr)); // dummy first link
    PairList *op = ans;
    for (const PairList *ip = attributes; ip; ip = ip->tail())
    {
        const RObject *tag = ip->tag();
        RObject *value = ip->car();

        if (tag == NamesSymbol)
        {
            StringVector *names = SEXP_downcast<StringVector *>(value);
            size_t old_size = names->size();
            names = VectorBase::resize(names, new_size);
            // resize() pads with NA, but we want blank strings instead.
            for (size_t i = old_size; i < new_size; i++)
                (*names)[i] = String::blank();
            value = names;
        }
        if (tag != DimSymbol && tag != DimNamesSymbol)
        {
            op->setTail(PairList::cons(value, nullptr, tag));
            op = op->tail();
        }
    }
    return ans->tail();
}

// TODO(kmillar): Ensure that names(dims(x)) and names(dimnames(x)) always match
//   when dims(x) and dimnames(x) are both defined.
void VectorBase::setDimensionNames(ListVector *names)
{
    setAttribute(DimNamesSymbol, names);
}

void VectorBase::setDimensionNames(unsigned int d, StringVector *names)
{
    size_t ndims = dimensions()->size();
    if (d == 0 || d > ndims)
        Rf_error(_("Attempt to associate dimnames with a non-existent dimension"));
    ListVector *lv = SEXP_downcast<ListVector *>(getAttribute(DimNamesSymbol));
    if (!lv)
    {
        lv = ListVector::create(ndims);
        setAttribute(DimNamesSymbol, lv);
    }
    (*lv)[d - 1] = names;
}

void VectorBase::setDimensions(IntVector *dims)
{
    setAttribute(DimSymbol, dims);
}

void VectorBase::setNames(StringVector *names)
{
    setAttribute(NamesSymbol, names);
}

void VectorBase::decreaseSizeInPlace(size_type)
{
    Rf_error(_("this object cannot be resized"));
}

// The error messages here match those used by CR (as of 3.0.2),
// not including the malformed unit abbreviations.
void VectorBase::tooBig(std::size_t bytes)
{
    double dsize = double(bytes) / 1024.0;
    if (dsize > 1024.0 * 1024.0)
        Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f GB"),
                     dsize / 1024.0 / 1024.0);
    if (dsize > 1024.0)
        Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f MB"),
                     dsize / 1024.0);
    Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f KB"), dsize);
}

// Rf_allocVector is still in memory.cpp (for the time being).

Rboolean Rf_isVector(SEXP s)
{
    switch (TYPEOF(s))
    {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

    case VECSXP:
    case EXPRSXP:
        return TRUE;
    case CXXSXP:
        return Rboolean(dynamic_cast<const VectorBase *>(s) != nullptr);
    default:
        return FALSE;
    }
}

void SETLENGTH(SEXP x, R_xlen_t v)
{
    VectorBase *vb = dynamic_cast<VectorBase *>(x);
    if (!vb)
        Rf_error("SETLENGTH invoked for a non-vector.");
    vb->decreaseSizeInPlace(VectorBase::size_type(v));
}
