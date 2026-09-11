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
#ifndef ORES_TRADING_API_DOMAIN_BOND_LEG_HPP
#define ORES_TRADING_API_DOMAIN_BOND_LEG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One leg of a bond instrument, keyed to the instrument and the leg's ordinal.
 *
 * One row per leg an instrument states, keyed to the instrument, the list
 * the leg belongs to and the leg's ordinal within that list.
 *
 * The ORE schema declares the bond's leg list unbounded and a bond states
 * one leg per coupon. The total return swap, the repo and the ascot each
 * state at most one leg of their own, and all four are the same leg
 * shape. One table holds them and leg_role says which list the row
 * belongs to.
 *
 * The row carries the leg's payment terms, its day counters, its
 * settlement block and the two flags the schema states on the leg itself.
 * Everything else the leg states lives in a table of its own: the
 * schedules in instrument_schedule, the amortizations, the named
 * amounts and the rate group below. Those tables key on leg_role as
 * well, so a schedule or an amount reaches the leg that stated it.
 *
 * The leg's currency and day counter also reach the issue row, because
 * the issue is where a reader looks for the coupon terms. The row here
 * is the document's own statement and wins on export, so a leg whose
 * terms differ from the issue's still round trips.
 *
 * Every member the schema declares optional is nullable here and an
 * std::optional in C++, so a member the document states and the row
 * cannot hold stays distinguishable from one the document omits.
 */
struct bond_leg final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument this leg belongs to.
     *
     * The instrument row carries the trade, the workspace and the party. The leg rows are
     * family-owned and ride the instrument's scope, so no workspace column rides them.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Which leg list of the instrument this row belongs to: bond for a leg of the bond
     * itself, or trs_funding, repo or ascot_swap for the single leg of a total return swap, a repo
     * or an ascot.
     *
     * A document states the bond's legs as a list and each of the other three as at most one leg.
     * The four are the same leg shape, so one table holds them and the role says which member of
     * the document a reader rebuilds. Without the role a leg numbered one would be ambiguous
     * between them.
     */
    std::string leg_role;

    /**
     * @brief Ordinal of the leg within its list, counting from one.
     *
     * The bond's leg list is unbounded and the other three roles carry one leg, which is always
     * number one.
     */
    int leg_number;

    /**
     * @brief True when the leg's payer is the trade's counterparty rather than the party.
     */
    std::optional<bool> payer;

    /**
     * @brief The leg's type as the document spells it, such as Fixed or Floating.
     *
     * The type is the document's own member and drives which arm of the rate group a document
     * states. It is kept as text, not decoded, so export re-emits the spelling.
     */
    std::optional<std::string> leg_type;

    /**
     * @brief ISO 4217 currency code of the leg.
     */
    std::optional<std::string> currency;

    /**
     * @brief Business day convention the leg's payments are adjusted by.
     */
    std::optional<std::string> payment_convention;

    /**
     * @brief Number of business days between a payment's accrual end and its pay date.
     *
     * The column is text rather than a number because the container holds the document's text and
     * export re-emits it unchanged.
     */
    std::optional<std::string> payment_lag;

    /**
     * @brief Calendar the leg's payment dates are adjusted against.
     */
    std::optional<std::string> payment_calendar;

    /**
     * @brief Day count convention of the leg.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief Day count convention the leg applies to its final coupon period.
     */
    std::optional<std::string> last_period_day_counter;

    /**
     * @brief Number of business days between a notional exchange and its pay date.
     */
    std::optional<std::int64_t> notional_payment_lag;

    /**
     * @brief True when the leg's notional exchanges fall on the schedule's dates without
     * adjustment.
     */
    std::optional<bool> strict_notional_dates;

    /**
     * @brief True when the leg takes its indexings from the asset leg rather than stating them.
     */
    std::optional<bool> indexings_from_asset_leg;

    /**
     * @brief FX index the leg's settlement block fixes against.
     *
     * The block is present when this column is set. The schema requires the index on a settlement
     * block, so a null column and an absent block are the same statement, and one column carries
     * both.
     */
    std::optional<std::string> settlement_fx_index;

    /**
     * @brief Date the settlement block's FX index fixes on (ISO 8601 date string).
     */
    std::optional<std::string> settlement_fixing_date;

    /**
     * @brief Username of the person who last modified this bond leg.
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
 * @brief Dispatch-key identifier for bond_leg, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_leg&) {
    return "ores.trading.bond_leg";
}

}

#endif
