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

/** @file LoopBailout.cpp
 *
 * @brief Implementation of class LoopBailout.
 */

#include <rho/LoopBailout.hpp>

#include <rho/Environment.hpp>
#include <rho/LoopException.hpp>

using namespace rho;

void LoopBailout::detachReferents()
{
    m_environment.detach();
    Bailout::detachReferents();
}

void LoopBailout::throwException()
{
    throw LoopException(m_environment, m_next);
}

void LoopBailout::visitReferents(const_visitor *v) const
{
    Bailout::visitReferents(v);
    if (m_environment)
        (*v)(m_environment);
}
