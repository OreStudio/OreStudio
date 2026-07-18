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
#ifndef ORES_SYNTHETIC_SERVICE_IR_CURVE_TEMPLATE_RESOLVER_HPP
#define ORES_SYNTHETIC_SERVICE_IR_CURVE_TEMPLATE_RESOLVER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/instrument_code.hpp"
#include "ores.refdata.api/domain/payment_frequency.hpp"
#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include <chrono>
#include <cstddef>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief One fixed-leg payment date of a Swap entry's schedule, reduced to what
 * curve_instrument_pricer::swap_par_rate() needs.
 */
struct ir_curve_schedule_step final {
    std::size_t ticks_ahead = 0;
    double accrual_fraction = 0.0;
};

/**
 * @brief One ir_curve_template_entry, reduced to what a tick loop needs to derive its rate from a
 * IYieldCurveProcess::discount_factor() every tick, without re-touching the refdata catalog.
 *
 * Ticks are calendar days from the config's horizon (see resolve()'s doc for why): a Phase-1
 * simplification appropriate for a synthetic PoC curve, deliberately not the full business-day/
 * calendar convention machinery real trade-date generation needs.
 */
struct ir_curve_resolved_entry final {
    int sequence_index = 0;
    std::string point_id; // end_tenor_code
    std::string curve_role; // DEPOSIT, FRA, or SWAP
    std::size_t ticks_ahead_start = 0;
    std::size_t ticks_ahead_end = 0;
    double year_fraction = 0.0; // accrual from start to end (Deposit/FRA)
    std::vector<ir_curve_schedule_step> fixed_leg_schedule; // Swap only
};

/**
 * @brief Refdata inputs resolve() needs, gathered once at feed construction (never touched again
 * from the tick loop) — the same "resolve once, tick many" shape fx_spot_feed's own construction
 * (process, subject) already follows.
 */
struct ir_curve_refdata_context final {
    std::map<std::string, ores::refdata::domain::tenor> tenors_by_code;
    std::map<std::string, ores::refdata::domain::instrument_code> instrument_codes_by_code;
    std::map<std::string, ores::refdata::domain::payment_frequency> payment_frequencies_by_code;
    ores::refdata::domain::tenor_convention convention;
    std::map<std::string, ores::refdata::domain::tenor_convention_resolution> resolutions_by_tenor;
    std::chrono::year_month_day horizon;
    std::chrono::year_month_day spot; // == horizon: see resolve()'s doc
};

/**
 * @brief Resolves a Curve Template's raw (tenor code, instrument code) entries into the tick
 * counts, year fractions, and (for Swap entries) fixed-leg schedules curve_instrument_pricer
 * needs, sorted by sequence_index.
 *
 * Design decisions, deliberately simplified for this Phase-1 synthetic PoC (see the parent
 * task's Analysis section for the broader rationale of keeping this feed self-contained):
 *
 * - *1 tick == 1 calendar day.* IYieldCurveProcess::discount_factor()'s ticks_ahead is
 *   process-internal simulation time with no fixed calendar unit; ticks_per_hour governs
 *   real-time publish cadence only, not the model's own step size. A day-per-tick convention
 *   is the simplest mapping that lets every tenor (1M, 2Y, ...) resolve to a whole number of
 *   ticks without inventing a second calibration parameter no config field carries.
 * - *spot == horizon (T+0).* ir_curve_template_entry's own doc establishes SPOT as "a genuine
 *   zero-duration PERIOD/DAY tenor ... resolving directly to the horizon's spot date" — a
 *   currency-specific spot lag (e.g. T+2) is FX settlement machinery this synthetic IR curve
 *   generator does not model.
 * - *Actual/365 fixed* day-count for year fractions — no day-count-convention catalog lookup;
 *   consistent with the "no business-day calendar" simplification above.
 * - *Swap fixed-leg schedule* is built by repeatedly stepping the payment frequency's own
 *   period_unit/period_multiplier from spot via the same ANCHOR_OFFSET arithmetic
 *   resolve_end_date() uses for tenors (this is exactly the reuse payment_frequency's own doc
 *   comment calls for), clamping the final step to the entry's own resolved maturity date so an
 *   irregular final stub is folded into the last accrual period rather than silently dropped.
 */
std::vector<ir_curve_resolved_entry>
resolve(const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
       const ir_curve_refdata_context& ctx,
       const std::string& fixed_leg_payment_frequency_code);

/**
 * @brief Builds the refdata inputs resolve() needs by reading the tenor/instrument_code/
 * payment_frequency/tenor_convention_resolution catalogs once. Shared by auto-start (once at
 * service startup) and the on-demand start control-plane (once per start request) -- cheap
 * enough to rebuild per call given the catalog sizes involved (tens of rows each).
 *
 * @return An empty optional if the RATES_SPOT_FORWARD tenor convention is not found (a
 * misconfigured environment, not a per-request error) -- callers should treat that as fatal to
 * every curve feed, not just the one being started.
 */
std::optional<ir_curve_refdata_context> build_ir_curve_refdata_context(ores::database::context ctx);

}

#endif
