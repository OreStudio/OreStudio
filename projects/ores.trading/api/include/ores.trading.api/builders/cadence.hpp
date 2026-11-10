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

#include <array>
#include <optional>
#include <string_view>

namespace ores::trading::builders {

/// How often a leg pays, as a convention states it and as a schedule's tenor.
struct cadence final {
    std::string_view frequency;
    std::string_view tenor;
};

/// A convention states a frequency and an index name a tenor; a schedule takes only a tenor.
inline constexpr std::array cadences{cadence{"Annual", "1Y"},
                                     cadence{"Semiannual", "6M"},
                                     cadence{"Quarterly", "3M"},
                                     cadence{"Bimonthly", "2M"},
                                     cadence{"Monthly", "1M"}};

[[nodiscard]] constexpr std::optional<cadence> cadence_of(std::string_view stated) {
    for (const auto& known : cadences)
        if (stated == known.frequency || stated == known.tenor)
            return known;
    return std::nullopt;
}

}

#endif
