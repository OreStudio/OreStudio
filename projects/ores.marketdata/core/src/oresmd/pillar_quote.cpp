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
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include <format>
#include <stdexcept>

namespace ores::marketdata::core {

namespace {

// A tenor a curve template names its pillar by: a count of days, weeks, months or years,
// such as 1D, 3M or 2Y. A date is not one, and neither is a meeting code such as 1F.
bool named_as_a_period(const std::string& code) {
    if (code.size() < 2 || code.size() > 4)
        return false;
    if (code.front() < '1' || code.front() > '9')
        return false;
    for (std::size_t i = 1; i + 1 < code.size(); ++i)
        if (code[i] < '0' || code[i] > '9')
            return false;
    const auto unit = code.back();
    return unit == 'D' || unit == 'W' || unit == 'M' || unit == 'Y';
}

}

std::string pillar_term_text(const std::string& start_tenor_code,
                             const std::string& end_tenor_code,
                             std::chrono::year_month_day start_date,
                             std::chrono::year_month_day end_date) {
    if (start_tenor_code == "SPOT") {
        if (!end_tenor_code.empty())
            return end_tenor_code;
        const auto days = std::chrono::sys_days{end_date} - std::chrono::sys_days{start_date};
        if (days.count() <= 0)
            throw std::invalid_argument(std::format(
                "pillar quote: a spot pillar must end after spot, but {:%Y-%m-%d} is not after "
                "{:%Y-%m-%d}",
                std::chrono::sys_days{end_date},
                std::chrono::sys_days{start_date}));
        return std::format("{}D", days.count());
    }
    if (named_as_a_period(start_tenor_code) && named_as_a_period(end_tenor_code))
        return end_tenor_code;
    return std::format("{:%Y%m%d}", std::chrono::sys_days{end_date});
}

}
