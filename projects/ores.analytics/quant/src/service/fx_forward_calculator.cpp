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
#include "ores.analytics.quant/service/fx_forward_calculator.hpp"
#include <cmath>
#include <exception>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

using date = std::chrono::year_month_day;

[[nodiscard]] bool is_positive(double value) noexcept {
    return value > 0.0 && std::isfinite(value);
}

struct pair_discounts final {
    double base = 0.0;
    double quote = 0.0;
};

[[nodiscard]] std::expected<double, std::string>
discount_at(const std::vector<bootstrapped_point>& curve,
            const std::string& currency,
            date d,
            const std::string& label,
            interpolation_method_code interpolation) {
    double discount = 0.0;
    try {
        discount = curve_bootstrap_engine::interpolate_discount_factor(curve, d, interpolation);
    } catch (const std::exception& e) {
        return std::unexpected("The " + currency + " curve cannot state " + label + ": " +
                               e.what());
    }
    if (!is_positive(discount))
        return std::unexpected("The " + currency + " curve holds no positive discount factor at " +
                               label + ".");
    return discount;
}

[[nodiscard]] std::expected<pair_discounts, std::string>
discounts_at(const fx_forward_request& request, date d, const std::string& label) {
    const auto base =
        discount_at(request.base_curve, request.base_currency, d, label, request.interpolation);
    if (!base)
        return std::unexpected(base.error());
    const auto quote =
        discount_at(request.quote_curve, request.quote_currency, d, label, request.interpolation);
    if (!quote)
        return std::unexpected(quote.error());
    return pair_discounts{.base = *base, .quote = *quote};
}

}

std::expected<std::vector<fx_forward_point>, std::string>
calculate_fx_forwards(const fx_forward_request& request) {
    const auto pair = request.base_currency + "/" + request.quote_currency;
    if (!is_positive(request.spot))
        return std::unexpected("The spot of " + pair + " is not positive and finite.");
    if (!is_positive(request.pip_factor))
        return std::unexpected("The pip factor of " + pair + " is not positive and finite.");

    const auto at_spot = discounts_at(request, request.spot_date, "the spot date");
    if (!at_spot)
        return std::unexpected(at_spot.error());

    const auto forward = [&](const pair_discounts& d) {
        return request.spot * (d.base / at_spot->base) / (d.quote / at_spot->quote);
    };

    std::vector<fx_forward_point> points;
    points.reserve(request.tenors.size());
    for (const auto& tenor : request.tenors) {
        const auto far_leg = discounts_at(request, tenor.value_date, tenor.tenor_id);
        if (!far_leg)
            return std::unexpected(far_leg.error());

        const auto near_leg =
            tenor.near_date ?
                discounts_at(request, *tenor.near_date, tenor.tenor_id + " near leg") :
                at_spot;
        if (!near_leg)
            return std::unexpected(near_leg.error());

        const double forward_rate = forward(*far_leg);
        points.push_back(fx_forward_point{
            .tenor_id = tenor.tenor_id,
            .value_date = tenor.value_date,
            .near_date = tenor.near_date.value_or(request.spot_date),
            .base_discount = far_leg->base,
            .quote_discount = far_leg->quote,
            .forward_rate = forward_rate,
            .forward_points = (forward_rate - request.spot) / request.pip_factor,
            .swap_points = (forward_rate - forward(*near_leg)) / request.pip_factor,
        });
    }
    return points;
}

}
