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
#ifndef ORES_TRADING_API_DOMAIN_PAYOFF_TYPE_HPP
#define ORES_TRADING_API_DOMAIN_PAYOFF_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Option payoff type from the ORE optionData PayoffType.
 *
 * Reference data table defining the closed payoff set ORE states on
 * optionData.PayoffType and optionData.PayoffType2
 * (external/ore/xsd/instruments.xsd, lines 337 and 338).
 *
 * ORE declares those elements as bare xs:string with no enum, so there is no
 * XSD enumeration to read. The set is the one the ORE example corpus actually
 * uses: every PayoffType element across external/ore/examples/ is one of
 * Accumulator, Asian, AverageStrike, Decumulator, TargetExact, TargetFull or
 * Vanilla, and nothing else. The list is the corpus's, not the model's own; the
 * model previously stated "Accumulator, Decumulator, or TaRF" and never wrote it.
 *
 * TargetFull and TargetExact are two of these seven, not a separate ORE set:
 * equity_accumulator_instrument.target_type holds one of them, and the ORE
 * tarfData.TargetType element is a scripted free-style number that no corpus
 * document states.
 */
struct payoff_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique payoff type code.
     *
     * Examples: 'Accumulator', 'Decumulator', 'TargetFull'.
     */
    std::string code;

    /**
     * @brief Detailed description of the payoff type.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this payoff type.
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
    friend bool operator==(const payoff_type&, const payoff_type&) = default;
};

/**
 * @brief Dispatch-key identifier for payoff_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const payoff_type&) {
    return "ores.trading.payoff_type";
}

}

#endif
