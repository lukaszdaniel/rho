/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file RObject.cpp
 *
 * Class RObject and associated C interface functions.
 */

#define R_NO_REMAP

#include <rho/RObject.hpp>

#include <cstdlib>
#include <iostream>
#include <Localization.h>
#include <R_ext/Error.h>
#include <Rinternals.h>
#include <rho/Expression.hpp>
#include <rho/GCStackRoot.hpp>
#include <rho/LogicalVector.hpp>
#include <rho/PairList.hpp>
#include <rho/Symbol.hpp>

using namespace std;
using namespace rho;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace rho
{
    namespace ForceNonInline
    {
        const auto &DUPLICATE_ATTRIBptr = DUPLICATE_ATTRIB;
        const auto &SHALLOW_DUPLICATE_ATTRIBptr = SHALLOW_DUPLICATE_ATTRIB;
        const auto &isNullptr = Rf_isNull;
        const auto &isObjectptr = Rf_isObject;
        const auto &IS_S4_OBJECTptr = IS_S4_OBJECT;
        const auto &NAMEDptr = NAMED;
        const auto &OBJECTptr = OBJECT;
        const auto &SET_NAMEDptr = SET_NAMED;
        const auto &ENSURE_NAMEDMAXptr = ENSURE_NAMEDMAX;
        const auto &ENSURE_NAMEDptr = ENSURE_NAMED;
        const auto &SETTER_CLEAR_NAMEDptr = SETTER_CLEAR_NAMED;
        const auto &RAISE_NAMEDptr = RAISE_NAMED;
        const auto &SET_S4_OBJECTptr = SET_S4_OBJECT;
        const auto &TYPEOFptr = TYPEOF;
        const auto &UNSET_S4_OBJECTptr = UNSET_S4_OBJECT;
        // const auto &attributesPtr = rho::RObject::attributes;
        auto attributesPtr = &RObject::attributes;
        const auto &LEVELSptr = LEVELS;
        const auto &SETLEVELSptr = SETLEVELS;
        const auto &ALTREPptr = ALTREP;
        const auto &SETALTREPptr = SETALTREP;
    } // namespace ForceNonInline
} // namespace rho

namespace
{
    // Used in {,un}packGPBits():
    const unsigned int S4_OBJECT_MASK = 1 << 4;
} // namespace

const unsigned char RObject::s_sexptype_mask;
const unsigned char RObject::s_S4_mask;
const unsigned char RObject::s_class_mask;

RObject::RObject(const RObject &pattern)
    : m_type(pattern.m_type), m_named(0), m_altrep(0),
      m_memory_traced(pattern.m_memory_traced), m_missing(pattern.m_missing),
      m_active_binding(pattern.m_active_binding),
      m_binding_locked(pattern.m_binding_locked)
{
    m_attrib = clone(pattern.m_attrib.get());
    maybeTraceMemory(&pattern);
}

void RObject::clearAttributes()
{
    if (m_attrib)
    {
        m_attrib = nullptr;
        // Beware promotion to int by ~:
        m_type &= static_cast<signed char>(~s_class_mask);
    }
}

void RObject::copyAttributes(const RObject *source, Duplicate deep)
{
    if (!source)
    {
        clearAttributes();
        setS4Object(false);
        return;
    }
    const PairList *attributes = source->attributes();
    if (attributes)
    {
        attributes = deep == Duplicate::DEEP
                         ? attributes->clone()
                         : (PairList *)Rf_shallow_duplicate(const_cast<PairList *>(attributes));
    }
    setAttributes(attributes);
    setS4Object(source->isS4Object());
}

RObject *RObject::evaluate(Environment *env)
{
    ENSURE_NAMEDMAX(this);
    return this;
}

RObject *RObject::getAttribute(const Symbol *name) const
{
    for (PairList *node = m_attrib; node; node = node->tail())
        if (node->tag() == name)
            return node->car();
    return nullptr;
}

unsigned int RObject::packGPBits() const
{
    unsigned int ans = 0;
    if (isS4Object())
        ans |= S4_OBJECT_MASK;
    return ans;
}

// This follows CR in adding new attributes at the end of the list,
// though it would be easier to add them at the beginning.
void RObject::setAttribute(const Symbol *name, RObject *value)
{
    if (!name)
        Rf_error(_("attributes must be named"));
    // Update 'has class' bit if necessary:
    if (name == Symbols::ClassSymbol)
    {
        if (value == nullptr)
            m_type &= static_cast<signed char>(~s_class_mask);
        else
            m_type |= static_cast<signed char>(s_class_mask);
    }
    // Find attribute:
    PairList *prev = nullptr;
    PairList *node = m_attrib;
    while (node && node->tag() != name)
    {
        prev = node;
        node = node->tail();
    }
    if (node)
    { // Attribute already present
        // Update existing attribute:
        if (value)
            node->setCar(value);
        // Delete existing attribute:
        else if (prev)
            prev->setTail(node->tail());
        else
            m_attrib = node->tail();
    }
    else if (value)
    {
        // Create new node:
        PairList *newnode = PairList::cons(value, nullptr, name);
        if (prev)
            prev->setTail(newnode);
        else
        { // No preexisting attributes at all:
            m_attrib = newnode;
        }
    }
}

// This has complexity O(n^2) where n is the number of attributes, but
// we assume n is very small.
void RObject::setAttributes(const PairList *new_attributes)
{
    clearAttributes();
    while (new_attributes)
    {
        const Symbol *name = SEXP_downcast<const Symbol *>(new_attributes->tag());
        setAttribute(name, new_attributes->car());
        new_attributes = new_attributes->tail();
    }
}

void RObject::setS4Object(bool on)
{
    // Check suppressed (temporarily I hope) during upgrade to R 2.8.1:
    // if (!on && sexptype() == S4SXP)
    //      Rf_error("S4 object (S4SXP) cannot cease to be an S4 object.");
    if (on)
        m_type |= s_S4_mask;
    else
        m_type &= ~s_S4_mask;
}

// The implementation of RObject::traceMemory() is in debug.cpp

const char *RObject::typeName() const
{
    return Rf_type2char(sexptype());
}

void RObject::unpackGPBits(unsigned int gpbits)
{
    // Be careful with precedence!
    setS4Object((gpbits & S4_OBJECT_MASK) != 0);
}

void RObject::visitReferents(const_visitor *v) const
{
    if (m_attrib)
        (*v)(m_attrib);
}

// ***** C interface *****

SEXP ATTRIB(SEXP x)
{
    return x ? const_cast<PairList *>(x->attributes()) : nullptr;
}

void DUPLICATE_ATTRIB(SEXP to, SEXP from)
{
    to->copyAttributes(from, RObject::Duplicate::DEEP);
}

void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from)
{
    to->copyAttributes(from, RObject::Duplicate::SHALLOW);
}

void SET_ATTRIB(SEXP x, SEXP v)
{
    GCStackRoot<PairList> pl(SEXP_downcast<PairList *>(v));
    x->setAttributes(pl);
}

void SET_OBJECT(SEXP, int)
{
    // This is a no-op in rho.  The object bit is set based on the class
    // attribute.
}

void maybeTraceMemory1(SEXP dest, SEXP src)
{
#ifdef R_MEMORY_PROFILING
    dest->maybeTraceMemory(src);
#endif
}

void maybeTraceMemory2(SEXP dest, SEXP src1, SEXP src2)
{
#ifdef R_MEMORY_PROFILING
    dest->maybeTraceMemory(src1, src2);
#endif
}

/*
 * Evil lurks here.
 *
 * To support SET_TYPEOF (which is unbelievably nasty, but unfortunately
 * required by some important packages), we need to turn objects of one type
 * into another, in-place.
 *
 * The way that this is done is to store the contents of the object, including
 * GC related state, into local variables, explicitly destroy the object
 * and then use placement new to create an object of the new type in its place.
 * Finally, the values are restored.
 *
 * This scheme is somewhat brittle -- changes to the way that objects are
 * represented may require rewriting parts of this code.
 */
void RObject::Transmute(RObject *source,
                        std::function<RObject *(void *)> constructor)
{
    // Store RObject properties.
    unsigned char named = source->m_named;
    unsigned char altrep = source->m_altrep;
    bool isS4 = source->isS4Object();
#ifdef R_MEMORY_PROFILING
    bool memory_traced = source->memoryTraced();
#endif
    unsigned missing = source->m_missing;
    bool active_binding = source->m_active_binding;
    bool binding_locked = source->m_binding_locked;
    const PairList *attributes = source->attributes();
    auto gc_data = source->storeInternalData();

    // Destroy the object and create the new type in it's place.
    void *location = source;
    source->~RObject();
    RObject *dest = constructor(location);

    // Restore the RObject properties.
    dest->restoreInternalData(gc_data);
    dest->setAttributes(attributes);

    dest->m_named = named;
    dest->m_altrep = altrep;
    dest->setS4Object(isS4);
#ifdef R_MEMORY_PROFILING
    dest->setMemoryTracing(memory_traced);
#endif
    dest->m_missing = missing;
    dest->m_active_binding = active_binding;
    dest->m_binding_locked = binding_locked;
}

namespace
{

    /* This code is complicated by the fact that PairList and CachingExpression
 * are two different sizes.
 * In order to make in-place conversion possible, we have created a
 * PaddedPairList object the same size as a CachingExpression, and a
 * (less efficient) Expression object the same size as a PairList.
 * Conversions then go between objects of the same size.
 */
    class PaddedPairList : public PairList
    {
    public:
        PaddedPairList() {}

    protected:
        virtual ~PaddedPairList() {}

        void *m_unused_padding_1;
    };

} // anonymous namespace

void RObject::TransmuteConsCell(ConsCell *object, SEXPTYPE dest_type)
{
    static_assert(sizeof(CachingExpression) == sizeof(PaddedPairList),
                  "Expected PaddedPairList and CachingExpression to be the same size");
    static_assert(sizeof(PairList) == sizeof(Expression),
                  "Expected PairList and Expression to be the same size");

    // Store the fields from the object.
    RObject *car = object->car();
    PairList *tail = object->tail();
    const RObject *tag = object->tag();

    // Transmute the object.
    std::function<ConsCell *(void *)> constructor;
    if (dest_type == LANGSXP)
    {
        constructor = dynamic_cast<PaddedPairList *>(object)
            ? [](void *p) -> ConsCell * { return new (p) CachingExpression; }
        : [](void *p) -> ConsCell * { return new (p) Expression; };
    }
    else
    {
        constructor = dynamic_cast<CachingExpression *>(object)
            ? [](void *p) -> ConsCell * { return new (p) PaddedPairList; }
        : [](void *p) -> ConsCell * { return new (p) PairList; };
    }

    RObject::Transmute(object, constructor);

    // Restore the values.
    object->setCar(car);
    object->setTail(tail);
    object->setTag(tag);
}

void RObject::TransmuteLogicalToInt(RObject *x)
{
    LogicalVector *object = SEXP_downcast<LogicalVector *>(x);

    size_t length = object->size();
    size_t truelength = XTRUELENGTH(object);

    // Store any data values that fall within the memory range of the
    // object.
    static const int STORAGE_SIZE = 4;
    Logical storage[STORAGE_SIZE];

    Logical *data_start = object->begin();
    Logical *data_end = object->end();
    Logical *object_end = reinterpret_cast<Logical *>(object + 1);
    bool data_start_is_in_object = (void *)object <= data_start && data_start <= object_end;

    Logical *embedded_data_end = std::min(data_end, object_end);
    ptrdiff_t stored_length = embedded_data_end - data_start;

    // Sanity checking.
    assert(data_start_is_in_object);
    assert(stored_length <= STORAGE_SIZE);
    if (!data_start_is_in_object || stored_length > STORAGE_SIZE)
    {
        Rf_error("Unexpected LogicalVector layout in SET_TYPEOF");
    }

    std::copy(data_start, embedded_data_end, storage);

    // Replace the original LogicalVector an IntVector in the same memory
    // location.
    RObject::Transmute(object,
                       [=](void *p) { return new (p) IntVector(length); });

    // Restore the truelength and stored values.
    SET_TRUELENGTH(object, truelength);
    std::copy(storage, storage + stored_length, data_start);
}

void SET_TYPEOF(SEXP x, SEXPTYPE dest_type)
{
    SEXPTYPE source_type = x->sexptype();
    if (source_type == dest_type)
        return;

    switch (dest_type)
    {
    case LANGSXP:
    case LISTSXP:
        if (source_type == LANGSXP || source_type == LISTSXP)
        {
            RObject::TransmuteConsCell(SEXP_downcast<ConsCell *>(x), dest_type);
            return;
        }
    case INTSXP:
        if (source_type == LGLSXP)
        {
            RObject::TransmuteLogicalToInt(SEXP_downcast<LogicalVector *>(x));
            return;
        }
    default:
        break;
    }

    Rf_error(
        "Calling SET_TYPEOF to convert from type %s to type %s is not supported in rho",
        Rf_type2char(source_type),
        Rf_type2char(dest_type));
}
