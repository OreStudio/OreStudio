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
#ifndef ORES_TRADING_API_BUILDERS_CADENCE_HPP
#define ORES_TRADING_API_BUILDERS_CADENCE_HPP

#include <algorithm>
#include <array>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

namespace ores::trading::builders {

/**
 * @brief How often a leg pays: the period of its schedule, and the frequency that names it.
 *
 * The frequency is a code of the payment frequency vocabulary, such as Annual or Lunarmonth,
 * and it is empty for a period that vocabulary does not name, such as 91D.
 */
struct cadence final {
    std::string_view frequency;
    std::string tenor;
};

/// A frequency of the vocabulary and the period it names.
inline constexpr std::array<std::pair<std::string_view, std::string_view>, 8> named_cadences{{
    {"Annual", "1Y"},
    {"Semiannual", "6M"},
    {"Quarterly", "3M"},
    {"Bimonthly", "2M"},
    {"Monthly", "1M"},
    {"Lunarmonth", "28D"},
    {"Weekly", "1W"},
    {"Daily", "1D"},
}};

/// Periods that name the period of a frequency in another unit.
inline constexpr std::array<std::pair<std::string_view, std::string_view>, 2> same_periods{{
    {"12M", "1Y"},
    {"7D", "1W"},
}};

/**
 * @brief The cadence a convention states, as a frequency such as Annual or as a period such as
 * 6M, 12M or 91D, or none when it states neither.
 */
[[nodiscard]] inline std::optional<cadence> cadence_of(std::string_view stated) {
    for (const auto& [from, to] : same_periods)
        if (stated == from)
            stated = to;
    for (const auto& [frequency, tenor] : named_cadences)
        if (stated == frequency || stated == tenor)
            return cadence{frequency, std::string(tenor)};
    const auto unit = stated.empty() ? '\0' : stated.back();
    const auto count = stated.substr(0, stated.empty() ? 0 : stated.size() - 1);
    if ((unit != 'D' && unit != 'W' && unit != 'M' && unit != 'Y') || count.empty() ||
        count.front() == '0' ||
        !std::ranges::all_of(count, [](char c) { return c >= '0' && c <= '9'; }))
        return std::nullopt;
    return cadence{{}, std::string(stated)};
}

}

#endif
