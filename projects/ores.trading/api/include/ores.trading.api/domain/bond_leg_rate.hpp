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
#ifndef ORES_TRADING_API_DOMAIN_BOND_LEG_RATE_HPP
#define ORES_TRADING_API_DOMAIN_BOND_LEG_RATE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The rate terms of a bond leg, one arm of the schema's leg-data choice per row.
 *
 * One row per bond leg, carrying the arm of the rate group the document
 * chose and the members that arm states.
 *
 * The ORE schema states the group as a choice of eighteen alternatives.
 * A bond leg carries a coupon, and the corpus states three of them:
 * fixed, floating and formula-based. The other fifteen are a recorded
 * boundary, and a document that states one of them has no row here. The
 * rate_kind column records which arm the document engaged.
 *
 * The three arms share members. All three lead with an index and all
 * three state an arrears flag, and the floating and formula arms both
 * state a fixing-days count. Those three members have one column each
 * and the arm decides how a reader takes them, so a fixed leg's row
 * leaves them unset.
 *
 * Each arm also has members the others lack, and they sit here as
 * nullable columns rather than in a table per arm. The floating arm's
 * sixteen members and the formula arm's fixing calendar are what remain
 * after the shared three and the lists below are taken out.
 *
 * The three lists the floating arm states, its spreads, caps, floors and
 * gearings, are not here: each is a numbered amount and lives in
 * bond_leg_amount under its own role. Its two schedules live in
 * instrument_schedule, under the fixing_schedule and
 * reset_schedule roles.
 *
 * The two stub interpolation blocks are folded into columns here. Each
 * is a pair of indices with an optional rounding, and the schema states
 * the block at most once at each end of the leg.
 */
struct bond_leg_rate final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument this rate block belongs to.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Which leg list of the instrument the leg holding this rate block belongs to: bond,
     * trs_funding, repo or ascot_swap.
     */
    std::string leg_role;

    /**
     * @brief Ordinal of the leg within its list, counting from one.
     */
    int leg_number;

    /**
     * @brief Which arm of the schema's rate group the document engaged: fixed, floating or
     * formula_based.
     */
    std::string rate_kind;

    /**
     * @brief Index the arm's coupons accrue on.
     *
     * The floating and formula arms both require this member and the fixed arm states none, so the
     * column is nullable and a fixed row leaves it unset.
     */
    std::optional<std::string> index;

    /**
     * @brief True when the arm's coupons accrue in arrears.
     */
    std::optional<bool> is_in_arrears;

    /**
     * @brief Number of days before a coupon period ends that its index fixes.
     *
     * The floating arm states this member optionally and the formula arm requires it. One column
     * serves both, and the arm decides whether a reader treats a value as required.
     */
    std::optional<std::int64_t> fixing_days;

    /**
     * @brief Calendar the formula arm's fixings are taken against.
     *
     * Only the formula arm states this member.
     */
    std::optional<std::string> fixing_calendar;

    /**
     * @brief Tenor of the floating arm's last recent period.
     */
    std::optional<std::string> last_recent_period;

    /**
     * @brief Calendar the last recent period is measured against.
     */
    std::optional<std::string> last_recent_period_calendar;

    /**
     * @brief Tenor the floating arm looks back over when it fixes.
     */
    std::optional<std::string> lookback;

    /**
     * @brief Number of days before a period ends after which the floating arm stops fixing.
     */
    std::optional<std::int64_t> rate_cutoff;

    /**
     * @brief True when the floating arm averages its fixings over the period.
     */
    std::optional<bool> is_averaged;

    /**
     * @brief True when the floating arm divides a period into sub-periods.
     */
    std::optional<bool> has_sub_periods;

    /**
     * @brief True when the floating arm applies its spread to the fixings.
     */
    std::optional<bool> include_spread;

    /**
     * @brief True when the floating arm's cross-currency leg does not reset.
     */
    std::optional<bool> is_not_resetting_xccy;

    /**
     * @brief True when the floating arm's caps and floors are taken without the underlying.
     */
    std::optional<bool> naked_option;

    /**
     * @brief True when the floating arm's caps and floors apply to the local index rather than to
     * the compounded rate.
     */
    std::optional<bool> local_cap_floor;

    /**
     * @brief True when the floating arm prices its stubs from the original curve.
     */
    std::optional<bool> stub_use_original_curve;

    /**
     * @brief True when the floating arm shifts its observations by the lookback.
     */
    std::optional<bool> observation_shift;

    /**
     * @brief Short index the floating arm's front stub interpolates between.
     */
    std::optional<std::string> front_stub_short_index;

    /**
     * @brief Long index the floating arm's front stub interpolates between.
     */
    std::optional<std::string> front_stub_long_index;

    /**
     * @brief Rounding the front stub's interpolated rate is rounded by.
     */
    std::optional<std::string> front_stub_rounding_type;

    /**
     * @brief Decimal places the front stub's rounding keeps.
     */
    std::optional<std::int64_t> front_stub_rounding_precision;

    /**
     * @brief Short index the floating arm's back stub interpolates between.
     */
    std::optional<std::string> back_stub_short_index;

    /**
     * @brief Long index the floating arm's back stub interpolates between.
     */
    std::optional<std::string> back_stub_long_index;

    /**
     * @brief Rounding the back stub's interpolated rate is rounded by.
     */
    std::optional<std::string> back_stub_rounding_type;

    /**
     * @brief Decimal places the back stub's rounding keeps.
     */
    std::optional<std::int64_t> back_stub_rounding_precision;

    /**
     * @brief Username of the person who last modified this bond leg rate.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for bond_leg_rate, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_leg_rate&) {
    return "ores.trading.bond_leg_rate";
}

}

#endif
