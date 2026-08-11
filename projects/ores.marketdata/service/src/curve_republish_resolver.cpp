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
#include "curve_republish_resolver.hpp"
#include "ores.refdata.api/domain/tenor_resolution.hpp"
#include <algorithm>
#include <stdexcept>

namespace ores::marketdata::service {

namespace {

using ores::analytics::quant::service::bootstrap_pillar;
using ores::analytics::quant::service::parse_bootstrap_curve_role_code;
using ores::refdata::domain::resolve_end_date;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention_resolution;

const tenor& find_tenor(const curve_republish_refdata_context& ctx, const std::string& code) {
    auto it = ctx.tenors_by_code.find(code);
    if (it == ctx.tenors_by_code.end())
        throw std::invalid_argument("curve_republish_resolver: unknown tenor code '" + code + "'");
    return it->second;
}

const tenor_convention_resolution& find_resolution(const curve_republish_refdata_context& ctx,
                                                   const std::string& tenor_code) {
    auto it = ctx.resolutions_by_tenor.find(tenor_code);
    if (it == ctx.resolutions_by_tenor.end())
        throw std::invalid_argument("curve_republish_resolver: tenor '" + tenor_code +
                                    "' has no resolution row under convention '" +
                                    ctx.convention.code + "'");
    return it->second;
}

double find_observed_rate(const std::unordered_map<std::string, double>& raw_rates_by_point_id,
                          const std::string& point_id) {
    auto it = raw_rates_by_point_id.find(point_id);
    if (it == raw_rates_by_point_id.end())
        throw std::invalid_argument("curve_republish_resolver: no observed rate for pillar '" +
                                    point_id + "' in the raw grid");
    return it->second;
}

} // namespace

std::chrono::year_month_day resolve_tenor_date(const curve_republish_refdata_context& ctx,
                                               const std::string& code) {
    if (code == "SPOT")
        return ctx.horizon;
    return resolve_end_date(find_tenor(ctx, code),
                            ctx.convention,
                            find_resolution(ctx, code),
                            ctx.horizon,
                            ctx.horizon,
                            ctx.schedule_dates);
}

std::vector<bootstrap_pillar> resolve_bootstrap_pillars(
    const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
    const curve_republish_refdata_context& ctx,
    const std::unordered_map<std::string, double>& raw_rates_by_point_id) {
    auto sorted_pillars = pillars;
    std::sort(sorted_pillars.begin(), sorted_pillars.end(), [](const auto& a, const auto& b) {
        return a.sequence_index < b.sequence_index;
    });

    std::vector<bootstrap_pillar> out;
    out.reserve(sorted_pillars.size());

    std::vector<std::chrono::year_month_day> prior_end_dates;
    for (const auto& p : sorted_pillars) {
        bootstrap_pillar bp;
        bp.point_id = p.end_tenor_code;
        bp.curve_role = parse_bootstrap_curve_role_code(p.curve_role_code);
        bp.observed_rate = find_observed_rate(raw_rates_by_point_id, p.end_tenor_code);
        bp.start_date = resolve_tenor_date(ctx, p.start_tenor_code);
        bp.end_date = resolve_tenor_date(ctx, p.end_tenor_code);

        if (bp.curve_role == ores::analytics::quant::service::bootstrap_curve_role_code::SWAP) {
            bp.fixed_leg_dates = prior_end_dates;
            bp.fixed_leg_dates.push_back(bp.end_date);
        }

        prior_end_dates.push_back(bp.end_date);
        out.push_back(std::move(bp));
    }

    return out;
}

}
