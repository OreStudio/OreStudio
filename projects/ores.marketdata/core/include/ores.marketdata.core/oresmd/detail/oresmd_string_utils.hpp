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
#ifndef ORES_MARKETDATA_CORE_ORESMD_DETAIL_ORESMD_STRING_UTILS_HPP
#define ORES_MARKETDATA_CORE_ORESMD_DETAIL_ORESMD_STRING_UTILS_HPP

#include <algorithm>
#include <string>
#include <string_view>

namespace ores::marketdata::core::detail {

/**
 * @brief Case-folding helpers shared by oresmd_parser and oresmd_projections, so the two
 * don't drift if ASCII-vs-locale handling ever needs to change.
 */
inline std::string to_upper(std::string_view s) {
    std::string r(s);
    std::ranges::transform(r, r.begin(), [](unsigned char c) { return std::toupper(c); });
    return r;
}

inline std::string to_lower(std::string_view s) {
    std::string r(s);
    std::ranges::transform(r, r.begin(), [](unsigned char c) { return std::tolower(c); });
    return r;
}

/**
 * @brief Whether @p s is shaped like an ISO 4217 code: three alphabetic characters.
 *
 * Both grammars place a currency in a fixed segment, so both have to answer this.
 * The check is on shape alone -- which currencies exist is ores.refdata's question,
 * not this library's.
 */
inline bool is_currency_code(std::string_view s) {
    return s.size() == 3 &&
           std::ranges::all_of(s, [](unsigned char c) { return std::isalpha(c); });
}

}

#endif
