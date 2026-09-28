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
#include "ores.platform/numeric/floating_point.hpp"
#include <boost/lexical_cast.hpp>
#include <exception>
#include <string>

namespace {

bool has_edge_whitespace(std::string_view text) {
    constexpr std::string_view whitespace(" \t\n\v\f\r");
    return whitespace.find(text.front()) != std::string_view::npos ||
           whitespace.find(text.back()) != std::string_view::npos;
}

}

namespace ores::platform::numeric {

std::optional<double> parse_double(std::string_view text) {
    if (text.empty() || has_edge_whitespace(text))
        return std::nullopt;
    try {
        // lexical_cast's float parser reads the whole input, in the classic
        // locale, which is the same contract the deleted-from-libc++
        // floating-point from_chars overload has.
        return boost::lexical_cast<double>(std::string(text));
    } catch (const boost::bad_lexical_cast&) {
        return std::nullopt;
    } catch (const std::exception&) {
        // A value outside the range of a double may surface as a range error
        // rather than a bad cast; both mean the text is not a number here.
        return std::nullopt;
    }
}

}
