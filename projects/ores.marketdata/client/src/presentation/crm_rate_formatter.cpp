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
#include <charconv>
#include <cmath>

namespace ores::marketdata::client::presentation {

namespace {

/// Allocation-free fixed-precision double->string, used instead of
/// ostringstream/iomanip -- this runs once per CRM cell per reload (up to
/// hundreds per call), and stream construction/imbue is a real cost at
/// that scale that to_chars avoids entirely. Only used here for change_text
/// (a plain percentage, not a currency-pair rate) -- rate_text itself comes
/// from currency_pair_rate_formatter, which has its own copy of this same
/// helper for the same reason.
std::string to_fixed_string(double value, int precision) {
    char buf[64];
    const auto res =
        std::to_chars(buf, buf + sizeof(buf), value, std::chars_format::fixed, precision);
    return std::string(buf, res.ptr);
}

}

std::vector<crm_rate_display>
crm_rate_formatter::format(const std::vector<crm_rate_format_request>& requests) {
    std::vector<crm_rate_display> results;
    results.reserve(requests.size());

    for (const auto& request : requests) {
        const auto& item = *request.item;
        crm_rate_display display;
        display.rate_text =
            refdata::client::presentation::currency_pair_rate_formatter::format_rate(
                item.rate, request.convention, request.convention_reversed);

        if (item.status == "stale") {
            display.tooltip_text = "Stale - " + item.as_of;
        } else if (item.status == "disconnected") {
            display.tooltip_text = "Disconnected - " + item.as_of;
        } else if (item.status == "unavailable") {
            display.tooltip_text = "Unavailable";
        } else {
            display.tooltip_text = item.reciprocal ?
                                       "Computed reciprocal (1/rate); Live - " + item.as_of :
                                       "Live - " + item.as_of;
        }

        display.change_text = "-";
        if (item.delta_pct.has_value() && std::abs(*item.delta_pct) > 1e-9) {
            const auto pct = *item.delta_pct;
            display.change_text = (pct >= 0 ? "+" : "") + to_fixed_string(pct, 3) + "%";
        }

        results.push_back(std::move(display));
    }

    return results;
}

}
