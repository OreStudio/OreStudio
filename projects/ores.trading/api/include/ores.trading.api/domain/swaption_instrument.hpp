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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_SWAPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_SWAPTION_INSTRUMENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Swaption (option on an interest rate swap) instrument.
 *
 * Represents a swaption — an option granting the right to enter into an
 * interest rate swap at a future date. Exercise type may be European,
 * Bermudan, or American.
 *
 * This row is the family's fact table, not its identity. The header,
 * ores.trading.rate_instruments, holds the trade type code, the party, the
 * instrument's start and maturity dates and its description; this table holds
 * only the product's own fields, and joins the header by trade_id.
 */
struct swaption_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this instrument belongs to, and the instrument's own key.
     *
     * The trade id identifies both the trade and its instrument, so the instrument carries no
     * identity of its own: this column is the key, and the trade it names gives the instrument its
     * scope.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief Option expiry date.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::chrono::year_month_day expiry_date;

    /**
     * @brief Exercise type: European, Bermudan, or American.
     *
     * Determines when the option may be exercised.
     *
     * Soft FK to ores_trading_exercise_types_tbl: the values are the closed ORE exerciseStyle set
     * (European, Bermudan, American). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string exercise_type;

    /**
     * @brief Settlement type: Cash or Physical.
     *
     * Determines how the swaption is settled upon exercise.
     *
     * Soft FK to ores_trading_settlement_types_tbl: the values are the closed ORE settlementType
     * set (Physical, Cash). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string settlement_type;

    /**
     * @brief Position direction: Long or Short.
     *
     * Soft FK to ores_trading_long_short_types_tbl: the values are the closed ORE longShort set
     * (Long, Short), which the SQL schema already states as a check. PR 4 tightens the soft
     * reference into a real foreign key.
     *
     * Indicates whether the party holds or writes the option.
     */
    std::string long_short;

    /**
     * @brief Username of the person who last modified this swaption instrument.
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
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const swaption_instrument&, const swaption_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for swaption_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const swaption_instrument&) {
    return "ores.trading.swaption_instrument";
}

}

#endif
