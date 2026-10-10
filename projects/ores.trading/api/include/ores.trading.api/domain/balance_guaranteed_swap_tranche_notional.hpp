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
#ifndef ORES_TRADING_API_DOMAIN_BALANCE_GUARANTEED_SWAP_TRANCHE_NOTIONAL_HPP
#define ORES_TRADING_API_DOMAIN_BALANCE_GUARANTEED_SWAP_TRANCHE_NOTIONAL_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One dated notional of a balance guaranteed swap tranche, keyed to the trade, the tranche
 * and the notional's ordinal.
 *
 * One row per notional a tranche states, keyed to the instrument, the
 * tranche and the notional's ordinal within that tranche.
 *
 * A tranche's notional amortises, so the document states a list of notionals,
 * each dated from the day it applies. The first notional states no date and
 * applies from the start of the swap. The export writes the rows back in the
 * order the document stated them.
 */
struct balance_guaranteed_swap_tranche_notional final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade whose balance guaranteed swap states this tranche notional.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Ordinal of the tranche this notional belongs to, matching the tranche row's own.
     */
    int tranche_number;

    /**
     * @brief Ordinal of this notional within the tranche, counting from one, in the order the
     * document states it.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The day this notional starts to apply.
     *
     * Null when the document states the notional without a start date.
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief The tranche's notional from start_date onwards.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief Username of the person who last modified this balance guaranteed swap tranche
     * notional.
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
    friend bool operator==(const balance_guaranteed_swap_tranche_notional&,
                           const balance_guaranteed_swap_tranche_notional&) = default;
};

/**
 * @brief Dispatch-key identifier for balance_guaranteed_swap_tranche_notional, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const balance_guaranteed_swap_tranche_notional&) {
    return "ores.trading.balance_guaranteed_swap_tranche_notional";
}

}

#endif
