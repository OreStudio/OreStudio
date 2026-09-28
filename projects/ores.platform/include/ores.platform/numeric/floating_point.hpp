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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_PLATFORM_NUMERIC_FLOATING_POINT_HPP
#define ORES_PLATFORM_NUMERIC_FLOATING_POINT_HPP

#include "ores.platform/export.hpp"
#include <optional>
#include <string_view>

namespace ores::platform::numeric {

/**
 * @brief Parses a decimal floating-point value from @p text.
 *
 * The portable counterpart of the floating-point overload of
 * @c std::from_chars: the whole of @p text must be one number, and the value
 * is parsed independently of the program's locale.
 *
 * It exists because libc++ does not implement that overload: on Apple
 * platforms its floating-point @c from_chars is a deleted function, so a call
 * compiles under libstdc++ and MSVC and stops the macOS build with "call to
 * deleted function 'from_chars'". The integer overloads, and the
 * floating-point @c to_chars the estate formats with, are implemented there.
 * Boost's lexical cast is used instead, which parses in the classic locale and
 * demands the whole input, which is the contract the standard overload has.
 *
 * @param text The whole number, and nothing else. Leading or trailing
 * whitespace is not part of a number, so it is rejected rather than skipped.
 * @return The value, or @c std::nullopt when @p text is empty, is not a
 * number, holds anything after the number, or names a value outside the
 * range of a @c double.
 */
[[nodiscard]] ORES_PLATFORM_EXPORT std::optional<double> parse_double(std::string_view text);

}

#endif
