/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_EXPORT_HPP
#define ORES_QT_EXPORT_HPP

#include <boost/config.hpp>

/**
 * @brief Symbol visibility macro for the ores.qt.api shared library.
 *
 * When building ores.qt.api itself (ORES_QT_API_LIBRARY is defined by the
 * CMakeLists.txt of ores.qt.api), this expands to BOOST_SYMBOL_EXPORT
 * (__declspec(dllexport) on MSVC, __attribute__((visibility("default"))) on
 * GCC/Clang). For all consumers it expands to BOOST_SYMBOL_IMPORT
 * (__declspec(dllimport) on MSVC, the same visibility attribute on GCC/Clang).
 */
#ifdef ORES_QT_API_LIBRARY
#  define ORES_QT_API BOOST_SYMBOL_EXPORT
#else
#  define ORES_QT_API BOOST_SYMBOL_IMPORT
#endif

#endif
