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
#include "ores.analytics.quant/service/rate_inverter.hpp"
#include <cmath>

namespace ores::analytics::quant::service {

using domain::crm_rate_view;
using domain::derived_rate;
using domain::rate_status;

rate_inverter::rate_lookup rate_inverter::make_lookup(const std::vector<derived_rate>& rates) {
    rate_lookup lookup;
    for (const auto& r : rates)
        lookup[{r.base_code, r.quote_code}] = r;
    return lookup;
}

crm_rate_view rate_inverter::resolve(const std::string& base_code,
                                     const std::string& quote_code,
                                     const rate_lookup& lookup,
                                     bool allow_invert) {
    if (const auto it = lookup.find({base_code, quote_code}); it != lookup.end()) {
        const auto& r = it->second;
        return crm_rate_view{.base_code = base_code,
                             .quote_code = quote_code,
                             .rate = r.rate,
                             .status = r.status,
                             .as_of = r.as_of,
                             .inverted = false,
                             .delta_pct = std::nullopt};
    }

    if (allow_invert) {
        if (const auto it = lookup.find({quote_code, base_code}); it != lookup.end()) {
            const auto& r = it->second;
            const bool can_divide =
                r.status != rate_status::unavailable && std::isfinite(r.rate) && r.rate != 0.0;
            return crm_rate_view{.base_code = base_code,
                                 .quote_code = quote_code,
                                 .rate = can_divide ? 1.0 / r.rate : 0.0,
                                 .status = can_divide ? r.status : rate_status::unavailable,
                                 .as_of = r.as_of,
                                 .inverted = true,
                                 .delta_pct = std::nullopt};
        }
    }

    return crm_rate_view{.base_code = base_code,
                         .quote_code = quote_code,
                         .rate = 0.0,
                         .status = rate_status::unavailable,
                         .as_of = std::chrono::system_clock::time_point{},
                         .inverted = false,
                         .delta_pct = std::nullopt};
}

} // namespace ores::analytics::quant::service
