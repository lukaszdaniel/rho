/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/** @file ConsCell.hpp
 * @brief Class rho::ConsCell and associated C interface.
 *
 * To facilitate inlining of various ConsCell member functions, this
 * file also includes the definition of class rho::PairList.
 *
 * This file includes C functions for examining and setting the CAR
 * and TAG of a rho::ConsCell; functions for examining and setting
 * the CDR, and other operations accessing the tail of the list, are
 * to be found in PairList.hpp.
 */

#ifndef CONSCELL_HPP
#define CONSCELL_HPP

#include <stdexcept>
#include <rho/RObject.hpp>
#include <rho/GCManager.hpp>
#include <rho/GCRoot.hpp>
#include <rho/SEXP_downcast.hpp>

namespace rho
{
    class PairList;

    /** @brief Element of a singly linked list.
     *
     * Element of a LISP-like singly-linked list, containing pointers
     * to a 'car' object (this is LISP terminology, and has nothing to
     * do with automobiles) and to a 'tag' object, as well as a
     * pointer to the next element of the list, which must be of the
     * derived type PairList.  (Any of these pointers may be null.)
     *
     * When the object is copied, the copy thus created will contain
     * copies of the 'car' and of subsequent elements of the list.
     * However, the tag is not copied: the copy object will simply
     * contain a pointer to the tag of the original object.  Despite
     * this, the tag is considered to be part of the object.
     *
     * @note This class is used as a base class to implement CR's
     * LISTSXP, LANGSXP, DOTSXP and (for the time being) BCODESXP.
     * Because what these SEXPTYPEs have in common is implementation
     * rather than meaning in the application domain, canons of
     * object-oriented design would argue against their publicly
     * inheriting from a common base class.  Without doing this,
     * however, it would have been difficult efficiently to implement
     * functions such as CAR(), which are ubiquitous in the CR code.
     *
     * @note The semantics of this class are somewhat inconsistent.
     * When a ConsCell is copied, the copy constructor tries to copy
     * the 'car', implying that the car is considered part of the
     * object.  But the const member function car() passes back a
     * non-const pointer to the car.  See the discussion in the
     * documentation of class RObject regarding the handling of const
     * pointers.
     */
    class ConsCell : public RObject
    {
    public:
        /** @brief iterator for iterating over a HeterogeneousList.
         */
        template <typename ValueType = ConsCell>
        class iterator_tmpl
            : public std::iterator<std::forward_iterator_tag, ValueType>
        {
        public:
            /** @brief Constructor.
             *
             * @param cc Pointer, possibly null, to the ConsCell to be
             *           designated by the iterator.
             */
            explicit iterator_tmpl(ValueType *cc = nullptr) : m_cc(cc) {}

            ValueType &operator*() const { return *m_cc; }

            ValueType *operator->() const { return m_cc; }

            iterator_tmpl operator++()
            {
                advance();
                return *this;
            }

            iterator_tmpl operator++(int)
            {
                iterator_tmpl ans = *this;
                advance();
                return ans;
            }

            bool operator==(iterator_tmpl other) const
            {
                return m_cc == other.m_cc;
            }
            bool operator!=(iterator_tmpl other) const
            {
                return m_cc != other.m_cc;
            }

        private:
            ValueType *m_cc;

            void advance();
        };

        /** @brief const_iterator for iterating over a ConsCell list.
         */
        template <typename ValueType = ConsCell>
        class const_iterator_tmpl
            : public std::iterator<std::forward_iterator_tag, const ValueType>
        {
        public:
            /** @brief Constructor.
             *
             * @param cc Pointer, possibly null, to the ConsCell to be
             *           designated by the const_iterator.
             */
            explicit const_iterator_tmpl(const ValueType *cc = nullptr) : m_cc(cc)
            {
            }

            const ValueType &operator*() const { return *m_cc; }

            const ValueType *operator->() const { return m_cc; }

            const_iterator_tmpl operator++()
            {
                advance();
                return *this;
            }

            const_iterator_tmpl operator++(int)
            {
                const_iterator_tmpl ans = *this;
                advance();
                return ans;
            }

            bool operator==(const_iterator_tmpl other) const
            {
                return m_cc == other.m_cc;
            }
            bool operator!=(const_iterator_tmpl other) const
            {
                return m_cc != other.m_cc;
            }

        private:
            const ValueType *m_cc;

            void advance();
        };

        typedef iterator_tmpl<ConsCell> iterator;
        typedef const_iterator_tmpl<ConsCell> const_iterator;

        iterator begin() { return iterator(this); }

        const_iterator begin() const { return const_iterator(this); }

        /**
         * @return a const pointer to the 'car' of this ConsCell
         * element.
         */
        RObject *car() const { return m_car; }

        /** @brief Convert a ConsCell to a (possibly) different
         * ConsCell type.
         *
         * @tparam T A (non-abstract) class derived from ConsCell.
         *
         * @param cc Pointer to a ConsCell (possibly null).  The
         *          effect of the method on \a cc is undefined;
         *          consequently \a cc should not be used subsequently to
         *          the method call.
         *
         * @return Pointer to the converted object, or a null pointer
         * if \a cc is null.  If \a cc is already of the desired type,
         * the method simply returns \a cc.
         */
        template <class T>
        static T *convert(ConsCell *cc)
        {
            if (!cc)
                return 0;
            if (T *ccc = dynamic_cast<T *>(cc))
                return ccc;
            T *ans = new T(cc->car(), cc->tail(), cc->tag());
            ans->setAttributes(cc->attributes());
            return ans;
        }

        iterator end() { return iterator(); }

        const_iterator end() const { return const_iterator(); }

        /** @brief Set the 'car' value.
         *
         * @param cr Pointer to the new car object (or a null
         *           pointer).
         */
        void setCar(RObject *cr) { m_car = cr; }

        /** @brief Set the 'tag' value.
         *
         * @param tg Pointer to the new tag object (or a null
         *           pointer).
         */
        void setTag(const RObject *tg) { m_tag = tg; }

        /** @brief Set the 'tail' value.
         *
         * @param tl Pointer to the new tail list (or a null
         *           pointer).
         */
        void setTail(PairList *tl);
        // Implemented inline in rho/PairList.h

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName() { return "(pairlist type)"; }

        /**
         * @return a pointer to the 'tag' of this ConsCell.
         */
        const RObject *tag() const { return m_tag; }

        /**
         * @return a const pointer to the 'tail' of this ConsCell
         * element.
         */
        const PairList *tail() const;

        /**
         * @return a pointer to the 'tail' of this ConsCell.
         */
        PairList *tail();

        // Virtual function of GCNode:
        void visitReferents(const_visitor *v) const override;

    protected:
        /**
         * @param st The required ::SEXPTYPE of the ConsCell.  Must
         *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP (not
         *           normally checked).
         *
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit ConsCell(SEXPTYPE st, RObject *cr = nullptr,
                          PairList *tl = nullptr, const RObject *tg = nullptr);

        /** @brief Copy constructor.
         *
         * @param pattern ConsCell to be copied.  Beware that if this
         *          ConsCell or any of its successors have unclonable
         *          'car' objects, they will be shared between \a
         *          pattern and the created object.
         */
        ConsCell(const ConsCell &pattern);

        /** @brief Tailless copy constructor.
         *
         * Copies the node without copying its tail.  Used in
         * implementing the PairList copy constructor proper.
         *
         * @param pattern ConsCell to be copied.  Beware that if this
         *          ConsCell or any of its successors have unclonable
         *          'car' objects, they will be shared between \a
         *          pattern and the created object.
         *
         * @param dummy This parameter is used simply to provide the
         *          constructor with a distinct signature.  Its value
         *          is ignored.
         */
        ConsCell(const ConsCell &pattern, int dummy);

        /**
         * Declared protected to ensure that ConsCell objects are
         * allocated only using 'new'.
         */
        ~ConsCell() {}

        // Virtual function of GCNode:
        void detachReferents() override;

    private:
        friend class PairList;

        GCEdge<> m_car;
        GCEdge<PairList> m_tail;
        GCEdge<const RObject> m_tag;

        // Not implemented yet.  Declared to prevent
        // compiler-generated version:
        ConsCell &operator=(const ConsCell &);

        // Check that st is a legal SEXPTYPE for a ConsCell:
        static void checkST(SEXPTYPE st);

        friend void ::SET_TYPEOF(SEXP, SEXPTYPE);
    };

    /** @brief <tt>cc ? cc->car() : 0</tt>
     *
     * @param cc Pointer to the ConsCell whose 'car' object is
     * required.
     *
     * @return a null pointer if \a cc is itself null, otherwise a
     * pointer (possibly null) to the 'car' object of \a cc.
     *
     * @deprecated This is a utility function used to implement CADR(SEXP) and
     * kindred functions in the C interface.  Its use for other
     * purposes is deprecated.
     */
    inline RObject *car0(ConsCell *cc) { return cc ? cc->car() : nullptr; }

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void ccdump(std::ostream &os, const ConsCell &cc, size_t margin = 0);

    /** @brief Number of elements in list.
     *
     * @param start Pointer to a ConsCell, possibly null.
     *
     * @return zero if \a start is a null pointer, otherwise the
     * number of elements in the list starting at the ConsCell
     * pointed to by \a start.
     */
    inline size_t listLength(const ConsCell *start)
    {
        return (start ? size_t(std::distance(start->begin(), start->end())) : 0);
    }

    /** @brief <tt>cc ? cc->tail() : 0</tt>
     *
     * @param cc Pointer to the ConsCell whose tail pointer is
     * required.
     *
     * @return a null pointer if \a cc is itself null, otherwise a
     * the tail pointer (possibly null) of \a cc.
     *
     * @deprecated This is a utility function used to implement CADR(SEXP) and
     * kindred functions in the C interface.  Its use for other
     * purposes is deprecated.
     */
    inline PairList *tail0(ConsCell *cc) { return cc ? cc->tail() : nullptr; }

    /** @brief Singly linked list of pairs.
     *
     * LISP-like singly-linked list, each element containing pointers to a
     * 'car' object (this is LISP terminology, and has nothing to do
     * with automobiles) and to a 'tag' object, as well as a pointer to
     * the next element of the list.  (Any of these pointers may be
     * null.)  A PairList object is considered to 'own' its car, its
     * tag, and all its successors.
     */
    class PairList : public ConsCell
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit PairList(RObject *cr = nullptr, PairList *tl = nullptr,
                          const RObject *tg = nullptr)
            : ConsCell(LISTSXP, cr, tl, tg)
        {
        }

        /** @brief Copy constructor.
         *
         * @param pattern PairList to be copied.
         */
        PairList(const PairList &pattern);

        /** @brief Create a PairList element on the free store.
         *
         * Unlike the constructor (and contrary to rho conventions
         * generally) this function protects its arguments from the
         * garbage collector.
         *
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tag Pointer to the tag of the element to be constructed.
         *
         * @return Pointer to newly created PairList element.
         */
        static PairList *cons(
            RObject *cr, PairList *tl = nullptr, const RObject *tag = nullptr)
        {
            GCManager::GCInhibitor no_gc;
            // We inhibit garbage collection here to avoid (a) the need
            // to protect the arguments from GC, and (b) the
            // possibility of reentrant calls to this function (from
            // object destructors).  However, calling code should not
            // rely on the fact that no GC will occur, because the
            // implementation may change in the future.
            return new PairList(cr, tl, tag);
        }

        /** @brief Create a PairList of a specified length.
         *
         * This constructor creates a chain of PairList nodes with a
         * specified number of elements.  On creation, each element
         * has null 'car' and 'tag'.
         *
         * @param sz Number of elements required in the list.  If
         *           zero, the function returns a null pointer.
         */
        static PairList *make(size_t sz);

        /** @brief Create a PairList from values in an array.
         *
         * This constructor creates a chain of PairList nodes.  The nodes
         * contain the vars args[0]...args[size - 1].
         */
        static PairList *make(int size, RObject *const *args);

        typedef iterator_tmpl<PairList> iterator;
        typedef const_iterator_tmpl<PairList> const_iterator;

        iterator begin() { return iterator(this); }
        const_iterator begin() const { return const_iterator(this); }
        iterator end() { return iterator(); }
        const_iterator end() const { return const_iterator(); }

        /** @brief Copy the tags from one PairList to another.
         */
        void copyTagsFrom(const PairList *listWithTags);

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName() { return "pairlist"; }

        // Virtual functions of RObject:
        PairList *clone() const override;
        unsigned int packGPBits() const override;
        const char *typeName() const override;
        void unpackGPBits(unsigned int gpbits) override;

    protected:
        // Declared protected to ensure that PairList objects are
        // allocated only using 'new':
        virtual ~PairList() HOT_FUNCTION;

        PairList(SEXPTYPE type, RObject *cr, PairList *tail, const RObject *tag)
            : ConsCell(type, cr, tail, tag)
        {
        }

    private:
        // Tailless copy constructor.  Copies the node without copying
        // its tail.  Used in implementing the copy constructor
        // proper.  The second parameter is simply to provide a
        // distinct signature, and its value is ignored.
        PairList(const PairList &pattern, int) : ConsCell(pattern, 0) {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated version:
        PairList &operator=(const PairList &);
    };

    template <typename ValueType>
    inline void ConsCell::iterator_tmpl<ValueType>::advance()
    {
        m_cc = m_cc->tail();
    }

    template <typename ValueType>
    inline void ConsCell::const_iterator_tmpl<ValueType>::advance()
    {
        m_cc = m_cc->tail();
    }

    inline ConsCell::ConsCell(
        SEXPTYPE st, RObject *cr, PairList *tl, const RObject *tg)
        : RObject(st)
    {
        m_car = cr;
        m_tail = tl;
        m_tag = tg;
        // checkST(st);
    }

    inline ConsCell::ConsCell(const ConsCell &pattern) : RObject(pattern)
    {
        m_car = clone(pattern.m_car.get());
        m_tail = clone(pattern.tail());
        m_tag = pattern.tag();
    }

    inline ConsCell::ConsCell(const ConsCell &pattern, int) : RObject(pattern)
    {
        m_car = clone(pattern.m_car.get());
        m_tail = nullptr;
        m_tag = pattern.tag();
    }

    inline void ConsCell::setTail(PairList *tl) { m_tail = tl; }

    inline const PairList *ConsCell::tail() const { return m_tail; }

    inline PairList *ConsCell::tail() { return m_tail; }

    inline const rho::PairList *rho::RObject::attributes() const
    {
        return m_attrib;
    }

} // namespace rho

extern "C"
{

    /** @brief Get car of rho::ConsCell.
     *
     * @param e Pointer to a rho::ConsCell (checked), or a null pointer.
     * @return Pointer to the value of the list car, or 0 if \a e is
     * a null pointer.
     */
    inline SEXP CAR(SEXP e)
    {
        using namespace rho;
        return car0(SEXP_downcast<ConsCell *>(e));
    }

    /**
     * @brief Equivalent to CAR(CAR(e)).
     */
    inline SEXP CAAR(SEXP e) { return CAR(CAR(e)); }

    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    inline int MISSING(SEXP x)
    {
        using namespace rho;
        return SEXP_downcast<ConsCell *>(x)->m_missing;
    }

    /**
     * @brief Set the 'car' value of a rho::ConsCell.
     * @param x Pointer to a rho::ConsCell (checked).
     * @param y Pointer a rho::RObject representing the new value of the
     *          list car.
     *
     * @returns \a y.
     */
    SEXP SETCAR(SEXP x, SEXP y);

    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    inline void SET_MISSING(SEXP x, int v)
    {
        using namespace rho;
        // The RHS is a kludge to avoid a -Wconversion warning:
        SEXP_downcast<ConsCell *>(x)->m_missing = static_cast<unsigned int>(v & 3);
    }

    /** @brief Get tag of rho::ConsCell.
     *
     * @param e Pointer to a rho::ConsCell (checked), or a null pointer.
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
    inline SEXP TAG(SEXP e)
    {
        using namespace rho;
        if (!e)
            return nullptr;
        ConsCell &cc = *SEXP_downcast<ConsCell *>(e);
        return const_cast<RObject *>(cc.tag());
    }

    /**
     * @brief Set the tag of a rho::ConsCell.
     *
     * @param x Pointer to a rho::ConsCell (checked).
     * @param y Pointer a rho::RObject representing the new tag of
     *          the rho::ConsCell.
     */
    void SET_TAG(SEXP x, SEXP y);

    /** @brief Create an object of a type derived from rho::ConsCell.
     *
     * The object is created with null car, tag and tail pointers.
     *
     * @param t The ::SEXPTYPE of the required object. Must be one of
     *          LISTSXP, LANGSXP, DOTSXP or BCODESXP (not checked).
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocSExp(SEXPTYPE t);
}

#endif /* CONSCELL_HPP */
