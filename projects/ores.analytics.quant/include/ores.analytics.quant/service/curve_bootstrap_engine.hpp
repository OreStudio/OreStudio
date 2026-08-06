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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_CURVE_BOOTSTRAP_ENGINE_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_CURVE_BOOTSTRAP_ENGINE_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <chrono>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief The instrument classifications this engine can bootstrap -- a
 * strongly typed subset of ores.refdata.curve_role's DEPOSIT/FRA/SWAP
 * (its sentinel NONE is not a valid bootstrap pillar role and has no
 * enumerator here). Enumerator names match their refdata code string
 * exactly so parse_bootstrap_curve_role_code() can delegate to
 * magic_enum::enum_cast.
 */
enum class bootstrap_curve_role_code { DEPOSIT, FRA, SWAP };

/**
 * @brief Parses a curve_role.code string into its strongly typed
 * enumerator.
 *
 * @throws std::invalid_argument if @p code is not one of DEPOSIT, FRA,
 * SWAP.
 */
ORES_ANALYTICS_QUANT_EXPORT bootstrap_curve_role_code
parse_bootstrap_curve_role_code(const std::string& code);

/**
 * @brief The interpolation methods this engine implements -- a strongly
 * typed subset of ores.refdata's interpolation_method.code.
 *
 * LOG_LINEAR_DISCOUNT and FLAT_FORWARD_THEN_LOG_LINEAR are mathematically
 * identical for pillar-to-pillar interpolation (see curve_bootstrap_engine's
 * own docs); CUBIC_SPLINE is recognised but not implemented and is
 * rejected at the point of use, not at parse time, so callers can
 * distinguish "unrecognised code" from "recognised but not yet supported".
 * Enumerator names match their refdata code string exactly so
 * parse_interpolation_method_code() can delegate to magic_enum::enum_cast.
 */
enum class interpolation_method_code {
    LOG_LINEAR_DISCOUNT,
    FLAT_FORWARD_THEN_LOG_LINEAR,
    CUBIC_SPLINE
};

/**
 * @brief Parses an interpolation_method.code string into its strongly
 * typed enumerator.
 *
 * @throws std::invalid_argument if @p code is not one of
 * LOG_LINEAR_DISCOUNT, FLAT_FORWARD_THEN_LOG_LINEAR, CUBIC_SPLINE.
 */
ORES_ANALYTICS_QUANT_EXPORT interpolation_method_code
parse_interpolation_method_code(const std::string& code);

/**
 * @brief One pillar of the raw instrument grid a bootstrap config's pillar
 * list resolves to concrete dates -- the input to curve_bootstrap_engine.
 *
 * Tenor-code resolution (turning e.g. "SPOT"/"1Y" into calendar dates) is
 * deliberately not this engine's concern, matching how curve_instrument_pricer
 * itself only takes discount factors/year fractions, never tenor codes --
 * that resolution is a refdata/synthetic-side responsibility (see
 * ores.synthetic's ir_curve_template_resolver for the equivalent on the
 * generation side).
 */
struct ORES_ANALYTICS_QUANT_EXPORT bootstrap_pillar {
    /// Echoed back on the corresponding bootstrapped_point; the pillar's own
    /// end_tenor_code.
    std::string point_id;
    bootstrap_curve_role_code curve_role;
    /// The observed market rate this pillar must reprice to.
    double observed_rate = 0.0;
    /// Start of the instrument's accrual period (DEPOSIT/FRA); ignored for
    /// SWAP, which uses fixed_leg_dates instead.
    std::chrono::year_month_day start_date{};
    /// Maturity date; the date the solved discount factor is anchored to.
    std::chrono::year_month_day end_date{};
    /// SWAP only: every fixed-leg payment date, ascending, the last entry
    /// equal to end_date. Empty for DEPOSIT/FRA. Every date but the last
    /// must already be resolvable from a previously-bootstrapped point
    /// (i.e. coincide with the curve's own tenor grid) -- an intermediate
    /// date falling strictly between the last bootstrapped pillar and this
    /// pillar's own maturity creates a circular dependency (its discount
    /// factor would itself depend on this pillar's still-unsolved df_end)
    /// that only resolves via iterative root-finding, not implemented
    /// here; bootstrap() raises a clear error rather than attempting it.
    std::vector<std::chrono::year_month_day> fixed_leg_dates;
};

/// One bootstrapped point: the discount factor that reprices its pillar.
struct ORES_ANALYTICS_QUANT_EXPORT bootstrapped_point {
    std::string point_id;
    std::chrono::year_month_day date{};
    double discount_factor = 0.0;
};

/**
 * @brief A PROJECTION config's pillar needed a point beyond what the
 * supplied external (FUNDING) discount curve covers, or no external curve
 * was supplied at all for a PROJECTION bootstrap.
 *
 * Distinct from std::invalid_argument (a caller programming error) --
 * this is the "fails/defers cleanly" signal the Funding-before-Projection
 * build order calls for: the caller (the future official-curve-republish
 * task) is expected to catch this and defer the bootstrap run, not treat
 * it as a bug.
 */
class ORES_ANALYTICS_QUANT_EXPORT discount_curve_required_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

/**
 * @brief Inverts curve_instrument_pricer's forward (discount-factor-in,
 * rate-out) functions: given observed pillar rates, solves tenor-by-tenor
 * for the discount factors that reprice every pillar.
 *
 * Single-curve, self-discounting: every pillar's own developing solution is
 * used both to solve for and to discount subsequent pillars, matching
 * exactly what ores.synthetic's ir_curve_template_resolver already
 * publishes on the generation side (one process, one discount_factor()
 * concept -- see that component's own "self_discounting" role default).
 * Genuine dual-curve discounting -- using a FUNDING curve's own discount
 * factors to discount a PROJECTION curve's cashflows while the PROJECTION
 * curve's own solved values represent forward-rate projections instead --
 * needs new pricer formulas beyond curve_instrument_pricer's current
 * signatures (which take one homogeneous discount-factor concept used
 * identically on both legs) and is explicitly out of scope here; it
 * mirrors the "Dual-curve (discount + projection) short-rate model" task
 * already tracked separately on the generation side. What this engine
 * *does* implement for a PROJECTION config is the run-time build-order
 * check: an optional external discount curve, required to already cover
 * every pillar's dates when bootstrapping anything other than a
 * self-contained (FUNDING) curve.
 */
class ORES_ANALYTICS_QUANT_EXPORT curve_bootstrap_engine {
public:
    /**
     * @brief Bootstraps @p pillars (in tenor order, short-end first) into
     * one discount factor per pillar.
     *
     * @param value_date The curve's own valuation/spot date; anchors
     *        discount_factor(value_date) = 1.0.
     * @param pillars Pillars in strictly increasing end_date order.
     * @param day_count_convention Applied uniformly to every pillar.
     * @param interpolation_method log_linear_discount and
     *        flat_forward_then_log_linear are mathematically identical
     *        for pillar-to-pillar interpolation (see class docs);
     *        cubic_spline is not yet implemented and throws.
     * @param discount_curve Required (must be non-null and cover every
     *        pillar's dates) when bootstrapping anything that isn't a
     *        self-contained curve -- pass nullptr only for a FUNDING
     *        config. Currently unused for actual dual-curve discounting
     *        (see class docs); its sole role today is the presence/
     *        coverage check, raising discount_curve_required_error when
     *        that check fails.
     */
    static std::vector<bootstrapped_point>
    bootstrap(std::chrono::year_month_day value_date,
              const std::vector<bootstrap_pillar>& pillars,
              day_count_convention_code day_count_convention,
              interpolation_method_code interpolation_method,
              const std::vector<bootstrapped_point>* discount_curve = nullptr);

    /**
     * @brief Interpolates the discount factor at @p query_date from a
     * sorted (ascending, by date) set of already-known @p points, using
     * @p interpolation_method.
     *
     * Exposed independently so the interpolation behaviour itself is
     * directly testable, not only indirectly through a full bootstrap run.
     */
    static double interpolate_discount_factor(const std::vector<bootstrapped_point>& points,
                                              std::chrono::year_month_day query_date,
                                              interpolation_method_code interpolation_method);
};

}

#endif
