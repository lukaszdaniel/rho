/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  http://www.r-project.org/Licenses/
 */

/** @file BuiltInFunction.cpp
 *
 * Implementation of class CXXR::BuiltInFunction and associated
 * C interface.
 */

#include "CXXR/BuiltInFunction.h"

#include "RCNTXT.h"
#include "CXXR/DotInternal.h"
#include "CXXR/Environment.h"
#include "CXXR/Expression.h"
#include "CXXR/Evaluator.h"
#include "CXXR/GCStackRoot.h"
#include "CXXR/RAllocStack.h"
#include "CXXR/Symbol.h"
#include "CXXR/errors.h"
#include "R_ext/Print.h"

using namespace std;
using namespace CXXR;

namespace CXXR {
    namespace ForceNonInline {
	const char* (*PRIMNAMEp)(SEXP x) = PRIMNAME;
	int (*PRIMOFFSETp)(SEXP x) = PRIMOFFSET;
	unsigned int (*PRIMVALp)(SEXP x) = PRIMVAL;
    }
}

BuiltInFunction::TableEntry* BuiltInFunction::s_function_table = 0;

RObject* BuiltInFunction::apply(Expression* call, PairList* args,
				Environment* env)
{
    size_t pps_size = GCStackRootBase::ppsSize();
    size_t ralloc_size = RAllocStack::size();
    Evaluator::enableResultPrinting(m_result_printing_mode != FORCE_OFF);
    GCStackRoot<> ans;
    if (sexptype() == SPECIALSXP)
	ans = m_function(call, this, args, env);
    else {
	pair<unsigned int, PairList*> pr = Evaluator::mapEvaluate(args, env);
	if (pr.first != 0)
	    missingArgumentError(this, args, pr.first);
	GCStackRoot<> evaluated_args(pr.second);
	if (Evaluator::profiling() || kind() == PP_FOREIGN) {
	    RCNTXT cntxt;
	    Rf_begincontext(&cntxt, CTXT_BUILTIN, call, Environment::base(),
			    Environment::base(), 0, 0);
	    ans = m_function(call, this, evaluated_args, env);
	    Rf_endcontext(&cntxt);
	} else {
	    ans = m_function(call, this, evaluated_args, env);
	}
    }
    if (m_result_printing_mode != SOFT_ON)
	Evaluator::enableResultPrinting(m_result_printing_mode != FORCE_OFF);
    if (pps_size != GCStackRootBase::ppsSize())
	REprintf("Warning: stack imbalance in '%s', %d then %d\n",
		 name(), pps_size, GCStackRootBase::ppsSize());
    RAllocStack::restoreSize(ralloc_size);
    return ans;
}

void BuiltInFunction::checkNumArgs(PairList* args, Expression* call) const
{
    if (arity() >= 0) {
	size_t nargs = ConsCell::listLength(args);
	if (int(nargs) != arity()) {
	    if (viaDotInternal())
		Rf_error(_("%d arguments passed to .Internal(%s)"
			   " which requires %d"), nargs, name(), arity());
	    else
		Rf_errorcall(call,
			     _("%d arguments passed to '%s' which requires %d"),
			     nargs, name(), arity());
	}
    }
}

int BuiltInFunction::indexInTable(const char* name)
{
    for (int i = 0; s_function_table[i].name; ++i)
	if (strcmp(name, s_function_table[i].name) == 0)
	    return i;
    return -1;
}

// BuiltInFunction::initialize() is in names.cpp

// BuiltInFunction::missingArgumentError() is in eval.cpp (for the
// time being).

const char* BuiltInFunction::typeName() const
{
    return sexptype() == SPECIALSXP ? "special" : "builtin";
}
