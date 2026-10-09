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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_EXERCISE_PRICE_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_EXERCISE_PRICE_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One exercise price an option block states, keyed to the trade and the entry's ordinal.
 *
 * One row per exercise price an option block states, keyed to the
 * instrument and the entry's ordinal.
 *
 * The schema states the exercise price list as a string, and the option
 * block's fourth keyed child holds the decoded pairs: the exercise date
 * and the price stated for it. The ordinal preserves the document's order.
 *
 * Both members are required within an entry, so neither column here can be
 * null.
 */
struct instrument_option_exercise_price final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade whose option block stated this exercise price.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Ordinal of this exercise price within the option block's list.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief Date the exercise price applies on.
     *
     * The schema states the date as text and the boundary parses it, so the column is a date.
     */
    std::chrono::year_month_day exercise_date;

    /**
     * @brief Price stated for the exercise date.
     */
    ores::utility::decimal::decimal price;

    /**
     * @brief Username of the person who last modified this instrument option exercise price.
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
    friend bool operator==(const instrument_option_exercise_price&,
                           const instrument_option_exercise_price&) = default;
};

/**
 * @brief Dispatch-key identifier for instrument_option_exercise_price, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_option_exercise_price&) {
    return "ores.trading.instrument_option_exercise_price";
}

}

#endif
