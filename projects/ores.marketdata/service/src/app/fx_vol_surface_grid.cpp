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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.service/app/fx_vol_surface_grid.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.refdata.core/mapping/fx_option_convention_mapping.hpp"
#include <algorithm>
#include <map>

namespace ores::marketdata::service::app {

std::expected<surface_grid, std::string> surface_grid_of(std::vector<surface_grid_point> points,
                                                         const std::vector<std::string>& strike_order,
                                                         const std::string& expiry,
                                                         const std::string& strike) {
    namespace qd = ores::analytics::quant::domain;
    std::map<std::string, qd::date_period> periods;
    for (const auto& p : points) {
        if (periods.contains(p.expiry))
            continue;
        const auto period = ores::refdata::mapping::parse_tenor_text(p.expiry);
        if (!period)
            return std::unexpected("the surface holds the expiry " + p.expiry + ": " + period.error());
        periods.emplace(p.expiry, *period);
    }
    const auto rank_of_strike = [&](const std::string& s) {
        const auto at = std::ranges::find(strike_order, s);
        return at == strike_order.end() ? strike_order.size() : static_cast<std::size_t>(at - strike_order.begin());
    };
    std::ranges::sort(points, [&](const auto& a, const auto& b) {
        if (a.expiry != b.expiry)
            return qd::compare_periods(periods.at(a.expiry), periods.at(b.expiry)) < 0;
        if (rank_of_strike(a.strike) != rank_of_strike(b.strike))
            return rank_of_strike(a.strike) < rank_of_strike(b.strike);
        return a.strike < b.strike;
    });

    if (!expiry.empty() && !std::ranges::any_of(points, [&](const auto& p) { return p.expiry == expiry; }))
        return std::unexpected("the surface has no expiry " + expiry);
    if (!strike.empty() && !std::ranges::any_of(points, [&](const auto& p) { return p.strike == strike; }))
        return std::unexpected("the surface has no strike " + strike);

    surface_grid out;
    for (auto& p : points) {
        if ((!expiry.empty() && p.expiry != expiry) || (!strike.empty() && p.strike != strike))
            continue;
        if (std::ranges::find(out.expiries, p.expiry) == out.expiries.end())
            out.expiries.push_back(p.expiry);
        if (std::ranges::find(out.strikes, p.strike) == out.strikes.end())
            out.strikes.push_back(p.strike);
        out.points.push_back(std::move(p));
    }
    std::ranges::sort(out.strikes, {}, rank_of_strike);
    return out;
}

}
