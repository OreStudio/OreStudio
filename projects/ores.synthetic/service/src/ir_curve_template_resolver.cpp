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
#include "ir_curve_template_resolver.hpp"
#include "ores.refdata.api/domain/tenor_resolution.hpp"
#include <algorithm>
#include <stdexcept>

namespace ores::synthetic::service {

namespace {

using ores::refdata::domain::resolve_end_date;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention_resolution;

std::size_t days_between(std::chrono::year_month_day a, std::chrono::year_month_day b) {
    return static_cast<std::size_t>((std::chrono::sys_days(b) - std::chrono::sys_days(a)).count());
}

double year_fraction(std::chrono::year_month_day a, std::chrono::year_month_day b) {
    return static_cast<double>(days_between(a, b)) / 365.0;
}

const tenor& find_tenor(const ir_curve_refdata_context& ctx, const std::string& code) {
    auto it = ctx.tenors_by_code.find(code);
    if (it == ctx.tenors_by_code.end())
        throw std::invalid_argument("ir_curve_template_resolver: unknown tenor code '" + code +
                                    "'");
    return it->second;
}

const tenor_convention_resolution& find_resolution(const ir_curve_refdata_context& ctx,
                                                    const std::string& tenor_code) {
    auto it = ctx.resolutions_by_tenor.find(tenor_code);
    if (it == ctx.resolutions_by_tenor.end())
        throw std::invalid_argument("ir_curve_template_resolver: tenor '" + tenor_code +
                                    "' has no resolution row under convention '" +
                                    ctx.convention.code + "'");
    return it->second;
}

std::chrono::year_month_day resolve_tenor_code(const ir_curve_refdata_context& ctx,
                                               const std::string& code) {
    if (code == "SPOT")
        return ctx.spot;
    return resolve_end_date(
        find_tenor(ctx, code), ctx.convention, find_resolution(ctx, code), ctx.horizon, ctx.spot);
}

// Builds the fixed-leg schedule for a Swap entry by stepping the payment frequency's own
// period_unit/period_multiplier from spot, via an in-memory (not catalog-persisted) tenor +
// resolution pair — legitimate reuse of resolve_end_date()'s pure ANCHOR_OFFSET arithmetic, since
// only convention.measured_from/resolution_algorithm (both real, fetched rows) drive the anchor;
// the ad hoc tenor/resolution below carry no catalog identity of their own. Steps stop once they
// reach or pass the entry's own resolved maturity; the final step is clamped to that maturity
// exactly, so an irregular stub period is folded into the last accrual rather than silently
// dropped or overshooting past the entry's own end_tenor_code.
std::vector<ir_curve_schedule_step>
build_swap_schedule(const ir_curve_refdata_context& ctx,
                    const ores::refdata::domain::payment_frequency& freq,
                    std::chrono::year_month_day maturity) {
    std::vector<ir_curve_schedule_step> steps;

    if (freq.period_unit == "NONE" || !freq.period_multiplier || *freq.period_multiplier <= 0) {
        // "Once" (or a malformed row): a single bullet payment at maturity.
        steps.push_back({days_between(ctx.horizon, maturity), year_fraction(ctx.spot, maturity)});
        return steps;
    }

    tenor step_tenor;
    step_tenor.kind = "PERIOD";
    step_tenor.unit = freq.period_unit;

    tenor_convention_resolution step_resolution;
    step_resolution.convention_code = ctx.convention.code;

    auto previous = ctx.spot;
    int multiple = 1;
    for (;;) {
        step_tenor.multiplier = multiple * (*freq.period_multiplier);
        auto date = resolve_end_date(step_tenor, ctx.convention, step_resolution, ctx.horizon, ctx.spot);
        if (date >= maturity) {
            steps.push_back({days_between(ctx.horizon, maturity), year_fraction(previous, maturity)});
            break;
        }
        steps.push_back({days_between(ctx.horizon, date), year_fraction(previous, date)});
        previous = date;
        ++multiple;
    }
    return steps;
}

} // namespace

std::vector<ir_curve_resolved_entry>
resolve(const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
       const ir_curve_refdata_context& ctx,
       const std::string& fixed_leg_payment_frequency_code) {
    std::vector<ir_curve_resolved_entry> out;
    out.reserve(entries.size());

    for (const auto& e : entries) {
        const auto start_date = resolve_tenor_code(ctx, e.start_tenor_code);
        const auto end_date = resolve_tenor_code(ctx, e.end_tenor_code);

        auto ic_it = ctx.instrument_codes_by_code.find(e.instrument_code);
        if (ic_it == ctx.instrument_codes_by_code.end())
            throw std::invalid_argument("ir_curve_template_resolver: unknown instrument code '" +
                                        e.instrument_code + "'");

        ir_curve_resolved_entry re;
        re.sequence_index = e.sequence_index;
        re.point_id = e.end_tenor_code;
        re.curve_role = ic_it->second.curve_role;
        re.ticks_ahead_start = days_between(ctx.horizon, start_date);
        re.ticks_ahead_end = days_between(ctx.horizon, end_date);
        re.year_fraction = year_fraction(start_date, end_date);

        if (re.curve_role == "SWAP") {
            auto pf_it = ctx.payment_frequencies_by_code.find(fixed_leg_payment_frequency_code);
            if (pf_it == ctx.payment_frequencies_by_code.end())
                throw std::invalid_argument("ir_curve_template_resolver: unknown payment "
                                            "frequency code '" +
                                            fixed_leg_payment_frequency_code + "'");
            re.fixed_leg_schedule = build_swap_schedule(ctx, pf_it->second, end_date);
        }

        out.push_back(std::move(re));
    }

    std::sort(out.begin(), out.end(), [](const auto& a, const auto& b) {
        return a.sequence_index < b.sequence_index;
    });
    return out;
}

}
