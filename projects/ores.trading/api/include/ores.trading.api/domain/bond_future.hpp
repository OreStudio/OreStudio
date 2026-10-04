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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FUTURE_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FUTURE_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Per-trade bond future facts: one row per future instrument, keyed by trade_id.
 *
 * One row per bond future trade, keyed by the trade. The row carries the
 * whole of bondFutureData, the only type the ORE schema states for the
 * product (instruments.xsd:428):
 * ContractName, ContractNotional and LongShort are required and
 * ApplyConversionFactor and UseFuturePrice are optional booleans.
 *
 * v17 moved the contract's own terms out of the trade. Currency,
 * ContractMonth, DeliverableGrade, LastTradingDate,
 * LastDeliveryDate, Settlement, DirtyQuotation, RootDate,
 * ExpiryBasis, SettlementBasis, ExpiryLag and SettlementLag are
 * elements of bondFutureReferenceDatum (referencedata.xsd:132),
 * and DeliveryBasket is that datum's element too. FairPrice is not in
 * the schema at all. The table carries none of the thirteen, because ORE
 * states them on the datum and not on the trade; they are recorded as
 * dropped rather than kept in a place ORE does not state them (decision
 * D25).
 *
 * The datum reaches only the ReferenceData document root, and the
 * generated ORE bindings cover the Portfolio root, so no trade document
 * can state one of these terms and no mapper can read one. The shared entity
 * that will hold them is keyed by the contract name, the way
 * ores.trading.bond_issue is keyed by the security id; it lands when the
 * reference-data document becomes importable, and the drop is recorded on
 * the bond pilot until then.
 */
struct bond_future final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade the bond future fact row belongs to.
     *
     * The trade row carries the party; the fact row only carries the future terms.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Name of the futures contract (the bond the contract is written on).
     */
    std::string contract_name;

    /**
     * @brief Notional of one contract.
     */
    ores::utility::decimal::decimal contract_notional;

    /**
     * @brief Long or short position in the contract.
     *
     * Soft FK to ores_trading_long_short_types_tbl: the values are the closed ORE longShort set
     * (Long, Short), which the SQL schema already states as a check. PR 4 tightens the soft
     * reference into a real foreign key.
     */
    std::string long_short;

    /**
     * @brief True when the delivery is converted by the contract's conversion factor.
     *
     * Optional in the schema, so the column is nullable: null is an absent element and a value is a
     * stated one, which keeps a stated false different from an absent element (decision D24).
     */
    std::optional<bool> apply_conversion_factor;

    /**
     * @brief True when the future's own price is used rather than the bond's.
     *
     * Optional in the schema, so the column is nullable, as for apply_conversion_factor.
     */
    std::optional<bool> use_future_price;

    /**
     * @brief Username of the person who last modified this bond future.
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
    friend bool operator==(const bond_future&, const bond_future&) = default;
};

/**
 * @brief Dispatch-key identifier for bond_future, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_future&) {
    return "ores.trading.bond_future";
}

}

#endif
