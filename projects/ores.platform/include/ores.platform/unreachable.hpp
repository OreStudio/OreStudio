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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_PLATFORM_UNREACHABLE_HPP
#define ORES_PLATFORM_UNREACHABLE_HPP

namespace ores::platform {

/**
 * @brief Marks a code path as unreachable.
 *
 * This function indicates to the compiler that the code path should never
 * be executed. If it is reached at runtime, the behavior is undefined.
 *
 * Use this after exhaustive switch statements on enums to satisfy the
 * compiler while allowing it to warn if a new enum value is added.
 *
 * Uses compiler-specific intrinsics:
 * - MSVC: __assume(false)
 * - GCC/Clang: __builtin_unreachable()
 */
[[noreturn]] inline void unreachable() {
#if defined(_MSC_VER)
    __assume(false);
#else
    __builtin_unreachable();
#endif
}

}

#endif
