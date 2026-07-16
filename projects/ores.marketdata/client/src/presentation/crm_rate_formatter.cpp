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

/// Matches the pre-formatter Qt default of QString::number(rate, 'f', 5).
constexpr int default_decimal_places = 5;

/// Allocation-free fixed-precision double->string, used instead of
/// ostringstream/iomanip -- this runs once per CRM cell per reload (up to
/// hundreds per call), and stream construction/imbue is a real cost at
/// that scale that to_chars avoids entirely.
std::string to_fixed_string(double value, int precision) {
    char buf[64];
    const auto res =
        std::to_chars(buf, buf + sizeof(buf), value, std::chars_format::fixed, precision);
    return std::string(buf, res.ptr);
}

/// floor(log10(x)) for x > 0 -- the base-10 order of magnitude (e.g. 82.99
/// -> 1, 0.012 -> -2). Nudges by a small epsilon before flooring: log10
/// isn't guaranteed correctly-rounded on every libm, so an x that's an
/// exact (or near-exact) power of ten can evaluate fractionally below the
/// integer (e.g. 1.9999999999998 for log10(100.0)), which floor() would
/// then round down one too many.
int order_of_magnitude(double x) {
    return static_cast<int>(std::floor(std::log10(x) + 1e-9));
}

/// Derives decimal_places for the reciprocal of a rate whose *own*
/// direction is described by convention.decimal_places, preserving
/// significant figures across the inversion instead of reusing
/// decimal_places verbatim (which is only valid for the convention's own
/// direction/magnitude). rate is the reciprocal value being rendered, so
/// 1/rate recovers the direct-direction magnitude the convention assumes.
int inverted_decimal_places(
    double rate, const ores::refdata::domain::currency_pair_convention& convention) {
    if (rate <= 0.0)
        return default_decimal_places;

    const int direct_order = order_of_magnitude(1.0 / rate);
    const int significant_figures = direct_order + 1 + convention.decimal_places;
    const int inverted_order = order_of_magnitude(rate);
    return std::max(0, significant_figures - 1 - inverted_order);
}

}

std::string crm_rate_formatter::format_rate(double rate,
    const std::optional<ores::refdata::domain::currency_pair_convention>& convention,
    bool convention_reversed) {
    if (!convention)
        return to_fixed_string(rate, default_decimal_places);

    if (convention_reversed)
        return to_fixed_string(rate, inverted_decimal_places(rate, *convention));

    // Snap to the pair's minimum tick (tick_size is in pips; pip_factor
    // converts pips to an absolute rate move) before rendering, rather
    // than just truncating decimal_places -- a rate whose last digit
    // doesn't fall on a real tick is not a value the pair can actually
    // quote.
    const double absolute_tick = convention->tick_size * convention->pip_factor;
    double snapped = rate;
    if (absolute_tick > 0.0)
        snapped = std::round(rate / absolute_tick) * absolute_tick;

    return to_fixed_string(snapped, convention->decimal_places);
}

std::vector<crm_rate_display>
crm_rate_formatter::format(const std::vector<crm_rate_format_request>& requests) {
    std::vector<crm_rate_display> results;
    results.reserve(requests.size());

    for (const auto& request : requests) {
        const auto& item = *request.item;
        crm_rate_display display;
        display.rate_text =
            format_rate(item.rate, request.convention, request.convention_reversed);

        if (item.status == "stale") {
            display.tooltip_text = "Stale as of " + item.as_of;
        } else if (item.status == "unavailable") {
            display.tooltip_text = "Unavailable";
        } else {
            display.tooltip_text = item.inverted ?
                "Computed inverse (1/rate); fresh as of " + item.as_of :
                "Fresh as of " + item.as_of;
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
