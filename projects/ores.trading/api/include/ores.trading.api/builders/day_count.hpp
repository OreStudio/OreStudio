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
#ifndef ORES_TRADING_API_BUILDERS_DAY_COUNT_HPP
#define ORES_TRADING_API_BUILDERS_DAY_COUNT_HPP

#include <array>
#include <optional>
#include <string_view>
#include <utility>

namespace ores::trading::builders {

/**
 * @brief A convention's day count, in the canonical form the conventions hold, and the code a
 * trade leg states for it.
 *
 * The conventions hold the canonical form, such as ACT/365.FIXED. A trade leg's day count is a
 * code of the day count fraction types, such as A365F, which are ORE's DayCounter enumeration.
 * Until both carry one form, a builder translates at this boundary.
 */
inline constexpr std::array<std::pair<std::string_view, std::string_view>, 12> trade_day_counts{{
    {"ACT/360", "A360"},
    {"ACT/365.FIXED", "A365F"},
    {"A365", "A365"},
    {"ACT/ACT.ISDA", "ActAct(ISDA)"},
    {"ACT/ACT.ISMA", "ActAct(ISMA)"},
    {"ACT/ACT.AFB", "ActAct(AFB)"},
    {"30/360", "30/360"},
    {"30E/360", "30E/360"},
    {"30E/360.ISDA", "30E/360(ISDA)"},
    {"BUS/252", "Business252"},
    {"1/1", "1/1"},
    {"Simple", "Simple"},
}};

/// The code a trade leg states for the day count @p stated, or none when the trade vocabulary
/// has no code for it. A value that is already a trade code is that code.
[[nodiscard]] constexpr std::optional<std::string_view>
trade_day_count_of(std::string_view stated) {
    for (const auto& [canonical, code] : trade_day_counts)
        if (stated == canonical || stated == code)
            return code;
    return std::nullopt;
}

}

#endif
