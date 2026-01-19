/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_PLATFORM_ATTRIBUTES_HPP
#define ORES_PLATFORM_ATTRIBUTES_HPP

/**
 * @brief Cross-platform attribute macros.
 *
 * Provides portable macros for C++ attributes that have different spellings
 * across compilers or may not be supported on older compilers.
 */

/**
 * @brief Portable [[no_unique_address]] attribute.
 *
 * This attribute allows empty class members to not occupy space when the
 * compiler can prove it's safe. MSVC uses [[msvc::no_unique_address]] while
 * GCC/Clang use [[no_unique_address]]. Falls back to empty on older compilers.
 */
#if __has_cpp_attribute(msvc::no_unique_address)
#define ORES_NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#elif __has_cpp_attribute(no_unique_address)
#define ORES_NO_UNIQUE_ADDRESS [[no_unique_address]]
#else
#define ORES_NO_UNIQUE_ADDRESS
#endif

#endif
