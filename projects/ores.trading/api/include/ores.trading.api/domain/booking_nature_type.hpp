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
#ifndef ORES_TRADING_API_DOMAIN_BOOKING_NATURE_TYPE_HPP
#define ORES_TRADING_API_DOMAIN_BOOKING_NATURE_TYPE_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Whether anything happened to cause a booking.
 *
 * Reference data table holding whether anything happened to cause a
 * booking: actual (the firm did a deal), test (the booking exercises
 * the system) and hypothetical (the booking answers a question).
 *
 * The set is closed, and the C++ enum domain::booking_nature holds the same
 * values. A code with no enum value cannot be read back, so the table is
 * immutable: it is seeded once and never changes at run time. Because it
 * is immutable, the trade anchor references it with a database foreign
 * key rather than a trigger check. The table has no tenant: the set is the
 * same for every tenant.
 */
struct booking_nature_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique booking nature code.
     *
     * Examples: 'actual', 'hypothetical'.
     */
    std::string code;

    /**
     * @brief What the code means.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this booking nature type.
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
    friend bool operator==(const booking_nature_type&, const booking_nature_type&) = default;
};

/**
 * @brief Dispatch-key identifier for booking_nature_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const booking_nature_type&) {
    return "ores.trading.booking_nature_type";
}

}

#endif
