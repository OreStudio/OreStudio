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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.platform/numeric/floating_point.hpp"
#include <cerrno>
#include <cmath>
#include <cstdlib>
#include <locale.h>
#include <string>

#if defined(__APPLE__) && __has_include(<xlocale.h>)
#    include <xlocale.h>
#endif

namespace {

/**
 * @brief Whether the whole of @p text is one decimal number.
 *
 * The grammar is checked here rather than left to the conversion, so what the
 * parser accepts does not depend on the program's locale, and the C library's
 * extensions -- hexadecimal input, "inf", "nan" -- are not numbers here.
 */
bool is_decimal_number(std::string_view text) {
    std::size_t i = 0;
    if (i < text.size() && (text[i] == '+' || text[i] == '-'))
        ++i;
    bool mantissa_digits = false;
    while (i < text.size() && text[i] >= '0' && text[i] <= '9') {
        mantissa_digits = true;
        ++i;
    }
    if (i < text.size() && text[i] == '.') {
        ++i;
        while (i < text.size() && text[i] >= '0' && text[i] <= '9') {
            mantissa_digits = true;
            ++i;
        }
    }
    if (!mantissa_digits)
        return false;
    if (i < text.size() && (text[i] == 'e' || text[i] == 'E')) {
        ++i;
        if (i < text.size() && (text[i] == '+' || text[i] == '-'))
            ++i;
        bool exponent_digits = false;
        while (i < text.size() && text[i] >= '0' && text[i] <= '9') {
            exponent_digits = true;
            ++i;
        }
        if (!exponent_digits)
            return false;
    }
    return i == text.size();
}

/**
 * @brief Converts in the C locale, whatever the program's locale is.
 *
 * The locale object is made once and kept: it is a process-lifetime constant,
 * and this conversion is on the path of every number an ORE document carries.
 */
double strtod_in_c_locale(const char* text) {
#if defined(_WIN32)
    static const _locale_t c_locale = _create_locale(LC_NUMERIC, "C");
    return _strtod_l(text, nullptr, c_locale);
#else
    static const locale_t c_locale = newlocale(LC_NUMERIC_MASK, "C", nullptr);
    return strtod_l(text, nullptr, c_locale);
#endif
}

}

namespace ores::platform::numeric {

std::optional<double> parse_double(std::string_view text) {
    if (!is_decimal_number(text))
        return std::nullopt;

    const std::string copy(text);
    errno = 0;
    const double value = strtod_in_c_locale(copy.c_str());

    // A value the type cannot hold. A subnormal is representable -- 5e-324 is
    // the smallest denormal a double has -- and the conversion reports it with
    // the same ERANGE it reports an underflow with, so only a result that
    // rounds to zero or to infinity is refused here. libc++ reports the
    // subnormal as a stream failure, which is why this converts through the C
    // library rather than through a stream.
    if (std::isinf(value))
        return std::nullopt;
    if (errno == ERANGE && value == 0.0)
        return std::nullopt;
    return value;
}

}
