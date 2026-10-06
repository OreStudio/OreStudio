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
#ifndef ORES_TRADING_API_DOMAIN_CALLABLE_SWAP_CALL_DATE_HPP
#define ORES_TRADING_API_DOMAIN_CALLABLE_SWAP_CALL_DATE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One call date of a callable swap instrument's exercise schedule, keyed to the trade and
 * the date's ordinal.
 *
 * One row per date the callable swap's exercise schedule names, keyed to
 * the instrument and the date's ordinal within the schedule.
 *
 * The ORE schema states the callable swap's exercise dates as an unbounded
 * list of ISO 8601 dates on the option block. The list order is the
 * document's order and the ordinal preserves it, so export re-emits the
 * dates as the document held them.
 *
 * A date list states no rule, no calendar and no convention: the document
 * writes out every date it means. The columns here are therefore the
 * date's own ordinal and the date itself, and nothing else. A malformed
 * date is refused by the column's type rather than by a convention the
 * readers must share, and a repeated date stays two rows because the
 * ordinal, not the date, is part of the key.
 *
 * The trade row carries the party. The call date rows are family-owned and ride the trade's scope.
 */
struct callable_swap_call_date final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade whose callable swap schedule states this date.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Ordinal of this date within the schedule's list, counting from one.
     *
     * The schema declares the list unbounded and a document's order is the order it stated, so the
     * ordinal is what identifies a row and preserves that order.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The date on which the call may be exercised.
     *
     * The ORE document states it as an ISO 8601 date string; the column's type refuses any other
     * spelling.
     */
    std::chrono::year_month_day call_date;

    /**
     * @brief Username of the person who last modified this callable swap call date.
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
    friend bool operator==(const callable_swap_call_date&,
                           const callable_swap_call_date&) = default;
};

/**
 * @brief Dispatch-key identifier for callable_swap_call_date, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const callable_swap_call_date&) {
    return "ores.trading.callable_swap_call_date";
}

}

#endif
