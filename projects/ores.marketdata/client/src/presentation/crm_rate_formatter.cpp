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
#include "ores.marketdata.client/presentation/crm_rate_formatter.hpp"
#include <cmath>
#include <iomanip>
#include <sstream>

namespace ores::marketdata::client::presentation {

namespace {

/// Matches the pre-formatter Qt default of QString::number(rate, 'f', 5).
constexpr int default_decimal_places = 5;

}

std::string crm_rate_formatter::format_rate(double rate, int decimal_places) {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(decimal_places) << rate;
    return oss.str();
}

crm_rate_display crm_rate_formatter::format(
    const ores::marketdata::messaging::crm_rate_item& item,
    std::optional<int> decimal_places) {
    crm_rate_display result;
    result.rate_text =
        format_rate(item.rate, decimal_places.value_or(default_decimal_places));

    if (item.status == "stale") {
        result.tooltip_text = "Stale as of " + item.as_of;
    } else if (item.status == "unavailable") {
        result.tooltip_text = "Unavailable";
    } else {
        result.tooltip_text = item.inverted ?
            "Computed inverse (1/rate); fresh as of " + item.as_of :
            "Fresh as of " + item.as_of;
    }

    result.change_text = "—";
    if (item.delta_pct.has_value() && std::abs(*item.delta_pct) > 1e-9) {
        const auto pct = *item.delta_pct;
        std::ostringstream oss;
        oss << (pct >= 0 ? "▲ +" : "▼ ") << std::fixed << std::setprecision(3)
            << pct << "%";
        result.change_text = oss.str();
    }

    return result;
}

}
