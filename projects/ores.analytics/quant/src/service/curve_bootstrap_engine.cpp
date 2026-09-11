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
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <cmath>
#include <magic_enum/magic_enum.hpp>
#include <stdexcept>

namespace ores::analytics::quant::service {

namespace {

bool is_supported_interpolation_method(interpolation_method_code method) {
    return method == interpolation_method_code::LOG_LINEAR_DISCOUNT ||
           method == interpolation_method_code::FLAT_FORWARD_THEN_LOG_LINEAR;
}

// ACT/365-fixed time metric for interpolation, deliberately independent of
// the per-instrument day_count_convention: interpolation needs a single,
// consistent notion of "how far apart" two dates are regardless of which
// accrual convention a given pillar's own rate uses.
double interpolation_time_metric(std::chrono::year_month_day a, std::chrono::year_month_day b) {
    return static_cast<double>((std::chrono::sys_days(b) - std::chrono::sys_days(a)).count()) /
           365.0;
}

}

bootstrap_curve_role_code parse_bootstrap_curve_role_code(const std::string& code) {
    const auto parsed = magic_enum::enum_cast<bootstrap_curve_role_code>(code);
    if (!parsed)
        throw std::invalid_argument("parse_bootstrap_curve_role_code: unsupported curve_role '" +
                                    code + "' -- expected DEPOSIT, FRA, or SWAP");
    return *parsed;
}

interpolation_method_code parse_interpolation_method_code(const std::string& code) {
    const auto parsed = magic_enum::enum_cast<interpolation_method_code>(code);
    if (!parsed)
        throw std::invalid_argument(
            "parse_interpolation_method_code: unsupported interpolation_method '" + code +
            "' -- expected LOG_LINEAR_DISCOUNT, FLAT_FORWARD_THEN_LOG_LINEAR, or CUBIC_SPLINE");
    return *parsed;
}

double curve_bootstrap_engine::interpolate_discount_factor(
    const std::vector<bootstrapped_point>& points,
    std::chrono::year_month_day query_date,
    interpolation_method_code interpolation_method) {
    if (points.empty())
        throw std::invalid_argument("curve_bootstrap_engine: no points to interpolate from");
    if (!is_supported_interpolation_method(interpolation_method))
        throw std::invalid_argument(
            "curve_bootstrap_engine: unsupported interpolation_method '" +
            std::string(magic_enum::enum_name(interpolation_method)) +
            "' -- only LOG_LINEAR_DISCOUNT and FLAT_FORWARD_THEN_LOG_LINEAR are implemented");

    const auto qd = std::chrono::sys_days(query_date);
    if (qd < std::chrono::sys_days(points.front().date) ||
        qd > std::chrono::sys_days(points.back().date))
        throw std::out_of_range("curve_bootstrap_engine: query_date is outside the known "
                                "[first, last] point range -- cannot extrapolate");

    // Exact match (including the common case of querying the last-known point).
    for (const auto& p : points) {
        if (std::chrono::sys_days(p.date) == qd)
            return p.discount_factor;
    }

    // Bracket: the two known points either side of query_date. points is
    // ascending by date, so the first point whose date exceeds query_date
    // is the upper bracket.
    std::size_t hi_idx = 0;
    while (hi_idx < points.size() && std::chrono::sys_days(points[hi_idx].date) < qd)
        ++hi_idx;
    const auto& lo = points[hi_idx - 1];
    const auto& hi = points[hi_idx];

    // Both LOG_LINEAR_DISCOUNT and FLAT_FORWARD_THEN_LOG_LINEAR interpolate
    // the log of the discount factor linearly between two bootstrapped
    // points -- for a query strictly between two already-solved pillars,
    // flat-forward (constant instantaneous forward over the segment) and
    // log-linear-on-discount-factor are the same formula. The two labels
    // only diverge once a non-local method (e.g. a spline) exists for the
    // long end; see the class docs.
    const double t_total = interpolation_time_metric(lo.date, hi.date);
    const double t_query = interpolation_time_metric(lo.date, query_date);
    const double ln_lo = std::log(lo.discount_factor);
    const double ln_hi = std::log(hi.discount_factor);
    const double frac = t_total > 0.0 ? t_query / t_total : 0.0;
    return std::exp(ln_lo + frac * (ln_hi - ln_lo));
}

std::vector<bootstrapped_point>
curve_bootstrap_engine::bootstrap(std::chrono::year_month_day value_date,
                                  const std::vector<bootstrap_pillar>& pillars,
                                  day_count_convention_code day_count_convention,
                                  interpolation_method_code interpolation_method,
                                  const std::vector<bootstrapped_point>* discount_curve) {
    if (pillars.empty())
        throw std::invalid_argument("curve_bootstrap_engine: pillars must not be empty");
    if (!is_supported_interpolation_method(interpolation_method))
        throw std::invalid_argument(
            "curve_bootstrap_engine: unsupported interpolation_method '" +
            std::string(magic_enum::enum_name(interpolation_method)) +
            "' -- only LOG_LINEAR_DISCOUNT and FLAT_FORWARD_THEN_LOG_LINEAR are implemented");

    // Build-order check: when an external curve is supplied (bootstrapping
    // anything other than a self-contained FUNDING curve), it must already
    // cover every date this bootstrap run will need -- a stale or
    // incomplete external curve fails cleanly here rather than the caller
    // silently reading past its known range later.
    if (discount_curve) {
        if (discount_curve->empty())
            throw discount_curve_required_error(
                "curve_bootstrap_engine: discount_curve was supplied but is empty");
        auto covers = [&](std::chrono::year_month_day d) {
            const auto sd = std::chrono::sys_days(d);
            return sd >= std::chrono::sys_days(discount_curve->front().date) &&
                   sd <= std::chrono::sys_days(discount_curve->back().date);
        };
        for (const auto& p : pillars) {
            if (!covers(p.end_date) ||
                (p.curve_role != bootstrap_curve_role_code::DEPOSIT && !covers(p.start_date)))
                throw discount_curve_required_error(
                    "curve_bootstrap_engine: discount_curve does not cover pillar '" + p.point_id +
                    "' -- its required generation is stale or missing");
            for (const auto& d : p.fixed_leg_dates) {
                if (!covers(d))
                    throw discount_curve_required_error(
                        "curve_bootstrap_engine: discount_curve does not cover a fixed-leg date "
                        "for pillar '" +
                        p.point_id + "' -- its required generation is stale or missing");
            }
        }
    }

    std::vector<bootstrapped_point> curve_points;
    curve_points.push_back({"", value_date, 1.0});

    auto get_df = [&](std::chrono::year_month_day d) -> double {
        if (std::chrono::sys_days(d) == std::chrono::sys_days(curve_points.back().date))
            return curve_points.back().discount_factor;
        if (std::chrono::sys_days(d) > std::chrono::sys_days(curve_points.back().date))
            throw std::invalid_argument(
                "curve_bootstrap_engine: pillar depends on a date beyond what has been "
                "bootstrapped so far -- pillars must be supplied in strictly increasing "
                "maturity order");
        return interpolate_discount_factor(curve_points, d, interpolation_method);
    };

    std::vector<bootstrapped_point> result;
    result.reserve(pillars.size());

    for (const auto& p : pillars) {
        if (std::chrono::sys_days(p.end_date) <= std::chrono::sys_days(curve_points.back().date))
            throw std::invalid_argument("curve_bootstrap_engine: pillar '" + p.point_id +
                                        "' does not mature strictly after the previous pillar");

        double df_end;
        switch (p.curve_role) {
            case bootstrap_curve_role_code::DEPOSIT: {
                // curve_instrument_pricer::deposit_rate(df, yf) is documented as
                // P(0, tau) -- always anchored at the valuation date, df_start ==
                // 1.0 -- so a DEPOSIT pillar's start_date must be value_date
                // itself; anything else would silently discount from the wrong
                // base rather than failing loudly like every other check here.
                if (std::chrono::sys_days(p.start_date) != std::chrono::sys_days(value_date))
                    throw std::invalid_argument("curve_bootstrap_engine: DEPOSIT pillar '" +
                                                p.point_id +
                                                "' must have start_date == value_date");
                const double yf = day_count_calculator::year_fraction(
                    p.start_date, p.end_date, day_count_convention);
                // Inverts curve_instrument_pricer::deposit_rate(df, yf) = (1/df - 1)/yf.
                df_end = 1.0 / (1.0 + p.observed_rate * yf);
                break;
            }
            case bootstrap_curve_role_code::FRA: {
                const double yf = day_count_calculator::year_fraction(
                    p.start_date, p.end_date, day_count_convention);
                const double df_start = get_df(p.start_date);
                df_end = df_start / (1.0 + p.observed_rate * yf);
                break;
            }
            case bootstrap_curve_role_code::SWAP: {
                if (p.fixed_leg_dates.empty() || std::chrono::sys_days(p.fixed_leg_dates.back()) !=
                                                     std::chrono::sys_days(p.end_date))
                    throw std::invalid_argument(
                        "curve_bootstrap_engine: SWAP pillar '" + p.point_id +
                        "' must supply a non-empty fixed_leg_dates ending at its own end_date");

                const double df_start = get_df(p.start_date);
                double partial_annuity = 0.0;
                auto previous = p.start_date;
                for (std::size_t i = 0; i + 1 < p.fixed_leg_dates.size(); ++i) {
                    const auto& d = p.fixed_leg_dates[i];
                    // Every intermediate fixed-leg date (all but the pillar's own
                    // maturity) must already be resolvable from previously-
                    // bootstrapped points. A date strictly beyond the current
                    // frontier would need its discount factor derived from this
                    // very pillar's own not-yet-solved df_end -- a genuine
                    // circular dependency (df_end appears on both sides of the
                    // equation via a fractional power under log-linear
                    // interpolation) that only resolves via iterative
                    // root-finding. Not implemented: this engine requires a
                    // SWAP's fixed-leg schedule to align with the curve's own
                    // tenor grid (a common simplifying convention, not a
                    // silent gap), and raises a specific, distinct error rather
                    // than a confusing generic one when it doesn't.
                    if (std::chrono::sys_days(d) > std::chrono::sys_days(curve_points.back().date))
                        throw std::invalid_argument(
                            "curve_bootstrap_engine: SWAP pillar '" + p.point_id +
                            "' has a fixed-leg date that falls strictly between the last "
                            "bootstrapped point and this pillar's own (still unknown) maturity -- "
                            "intermediate fixed-leg dates must align with the curve's own tenor "
                            "grid; resolving an unaligned intermediate date requires iterative "
                            "root-finding, not yet implemented");
                    const double accrual =
                        day_count_calculator::year_fraction(previous, d, day_count_convention);
                    partial_annuity += accrual * get_df(d);
                    previous = d;
                }
                const double last_accrual =
                    day_count_calculator::year_fraction(previous, p.end_date, day_count_convention);

                // Closed-form solve of swap_par_rate's identity for the one
                // unknown (df_end appears in both the numerator and the final
                // annuity term):
                //   S = (df_start - df_end) / (partial_annuity + last_accrual*df_end)
                //   => df_end = (df_start - S*partial_annuity) / (1 + S*last_accrual)
                df_end = (df_start - p.observed_rate * partial_annuity) /
                         (1.0 + p.observed_rate * last_accrual);
                break;
            }
            default:
                throw std::invalid_argument(
                    "curve_bootstrap_engine: unrecognized bootstrap_curve_role_code");
        }

        curve_points.push_back({p.point_id, p.end_date, df_end});
        result.push_back({p.point_id, p.end_date, df_end});
    }

    return result;
}

}
