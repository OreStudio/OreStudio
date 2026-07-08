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
#ifndef ORES_QT_HEADLESS_EXPORT_HPP
#define ORES_QT_HEADLESS_EXPORT_HPP

#include <boost/config.hpp>

/**
 * @brief Symbol visibility macro for the ores.qt.headless shared library.
 *
 * A distinct macro name from ores.qt.api's own ORES_QT_API — deliberately,
 * since ores.qt.api consumes this component's headers directly, so both
 * export.hpp headers can end up included in the same translation unit; a
 * shared macro name would silently clash (last #define wins) instead of
 * failing loudly.
 *
 * When building ores.qt.headless itself (ORES_QT_HEADLESS_LIBRARY is
 * defined by the CMakeLists.txt of ores.qt.headless), this expands to
 * BOOST_SYMBOL_EXPORT (__declspec(dllexport) on MSVC,
 * __attribute__((visibility("default"))) on GCC/Clang). For all consumers
 * it expands to BOOST_SYMBOL_IMPORT (__declspec(dllimport) on MSVC, the
 * same visibility attribute on GCC/Clang).
 *
 * A TU compiled directly into another target (e.g. a test executable that
 * links this component's source in for headless testing rather than via
 * the shared library) must also define ORES_QT_HEADLESS_LIBRARY — the same
 * definitions this file guards, just linked into a different binary — or
 * MSVC flags "dllimport on a definition" (C4273) as inconsistent linkage.
 */
#ifdef ORES_QT_HEADLESS_LIBRARY
#    define ORES_QT_HEADLESS_API BOOST_SYMBOL_EXPORT
#else
#    define ORES_QT_HEADLESS_API BOOST_SYMBOL_IMPORT
#endif

#endif
