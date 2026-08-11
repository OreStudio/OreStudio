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
#ifndef ORES_MARKETDATA_SERVICE_CURVE_REPUBLISH_RESOLVER_HPP
#define ORES_MARKETDATA_SERVICE_CURVE_REPUBLISH_RESOLVER_HPP

#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Refdata inputs resolve_bootstrap_pillars() needs, read once per republish() call --
 * mirrors ores.synthetic::service::ir_curve_refdata_context, the equivalent context on the
 * generation side.
 */
struct curve_republish_refdata_context final {
    std::unordered_map<std::string, ores::refdata::domain::tenor> tenors_by_code;
    ores::refdata::domain::tenor_convention convention;
    std::unordered_map<std::string, ores::refdata::domain::tenor_convention_resolution>
        resolutions_by_tenor;
    std::chrono::year_month_day horizon; // == value_date == spot: T+0, see resolve's doc.
    // The event-lookup schedule's date set (FOMC meeting dates), sorted
    // ascending -- the requirement of the SCHEDULE_STEP walk's
    // nth_on_or_after. Only SCHEDULE_STEP conventions consult it; the
    // standard ANCHOR_OFFSET conventions never do.
    std::optional<std::vector<std::chrono::year_month_day>> schedule_dates;
};

/**
 * @brief Resolves a single tenor code (or "SPOT") into a calendar date under @p ctx's convention
 * and horizon -- exposed independently of resolve_bootstrap_pillars() so a discount_curve's own
 * point_id tenor codes (read back from market_observations, which carries no date column) can be
 * resolved the same way when building the Funding-before-Projection build-order input.
 *
 * @throws std::invalid_argument if @p code doesn't resolve under @p ctx.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::chrono::year_month_day
resolve_tenor_date(const curve_republish_refdata_context& ctx, const std::string& code);

/**
 * @brief Resolves a bootstrap config's pillar list (tenor codes) plus the raw grid's observed
 * rates into curve_bootstrap_engine's own bootstrap_pillar list, sorted by sequence_index.
 *
 * A SWAP pillar's fixed_leg_dates needs no separate payment-frequency concept (unlike the
 * generation-side resolver, which builds a synthetic schedule for pricing a continuous process):
 * curve_bootstrap_engine only requires each intermediate fixed-leg date to already resolve at or
 * before the curve's currently-bootstrapped frontier, so every prior pillar's own resolved
 * end_date -- the config's own tenor grid, naturally ascending since pillars are walked in
 * sequence_index order -- already satisfies that; it *is* the fixed-leg schedule.
 *
 * @param pillars The bootstrap config's pillars, any order (sorted internally by sequence_index).
 * @param ctx Refdata context for tenor-code resolution.
 * @param raw_rates_by_point_id The raw grid's observed rate per pillar, keyed by the pillar's own
 * end_tenor_code (matching market_observation.point_id on the source series).
 * @throws std::invalid_argument if a pillar's tenor code doesn't resolve, its curve_role_code is
 * unrecognised, or the raw grid has no observed rate for its point_id.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::vector<ores::analytics::quant::service::bootstrap_pillar>
resolve_bootstrap_pillars(
    const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
    const curve_republish_refdata_context& ctx,
    const std::unordered_map<std::string, double>& raw_rates_by_point_id);

}

#endif
