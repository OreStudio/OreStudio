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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FORWARD_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FORWARD_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The forward block a document states on a forward bond, keyed to the instrument.
 *
 * One row per forward bond instrument, keyed to the instrument.
 *
 * A forward bond states a bond block, a settlement block, an optional
 * premium block and a long-in-forward flag. The bond block is the issue
 * and the nine bond tables hold it; this table holds the other three,
 * which no row holds.
 *
 * Each optional member is a nullable column. An empty string is not a
 * null, so a member the document stated empty stays distinct from one it
 * omitted, and a block is present exactly when its required member is not
 * null.
 */
struct bond_forward final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the forward bond instrument this row extends.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Flag saying the holder is long the forward.
     *
     * The schema declares the member required, so an unengaged value means the container came from
     * a row set rather than from a document.
     */
    std::optional<std::string> long_in_forward;

    /**
     * @brief Maturity date of the forward.
     *
     * The column is text because the schema states the member as a string, not as a date. The
     * corpus spells it both ways, 20251220 and 2025-07-16, and a date column would return the first
     * as the second and break the round trip on those documents.
     *
     * The settlement block is present exactly when this member is not null, because the schema
     * declares it the block's one required member.
     */
    std::optional<std::string> forward_maturity_date;

    /**
     * @brief Date the forward settles on.
     *
     * The column is text for the same reason as the maturity date beside it.
     */
    std::optional<std::string> forward_settlement_date;

    /**
     * @brief Settlement type the forward pays under.
     */
    std::optional<std::string> settlement;

    /**
     * @brief Amount the forward settles for.
     */
    std::optional<double> amount;

    /**
     * @brief Rate the forward's value is locked at.
     */
    std::optional<double> lock_rate;

    /**
     * @brief Sensitivity the document states alongside the lock rate.
     */
    std::optional<double> dv01;

    /**
     * @brief Day counter the lock rate accrues under.
     */
    std::optional<std::string> lock_rate_day_counter;

    /**
     * @brief Flag saying the settlement is dirty.
     *
     * The column is text because the schema spells the member as one of its own bool type's
     * thirteen spellings, and the document's own spelling is what export re-emits.
     */
    std::optional<std::string> settlement_dirty;

    /**
     * @brief Amount of the forward's premium.
     *
     * The column is text because the schema states the amount as text. The premium block is present
     * exactly when this member and the pay date are both not null, because the schema declares the
     * two required together.
     */
    std::optional<std::string> premium_amount;

    /**
     * @brief Date the forward's premium pays on.
     */
    std::optional<std::string> premium_date;

    /**
     * @brief Username of the person who last modified this bond forward.
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
 * @brief Dispatch-key identifier for bond_forward, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_forward&) {
    return "ores.trading.bond_forward";
}

}

#endif
