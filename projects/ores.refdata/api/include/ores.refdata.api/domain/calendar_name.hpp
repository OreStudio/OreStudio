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
#ifndef ORES_REFDATA_API_DOMAIN_CALENDAR_NAME_HPP
#define ORES_REFDATA_API_DOMAIN_CALENDAR_NAME_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One calendar name that an ORE document may write.
 *
 * ORE names a calendar by text, and its schema fixes the names it accepts: 471
 * of them, such as TARGET, US settlement, weekends only and currency codes
 * that stand for a currency's calendar. The schema also accepts two open forms
 * no list can hold -- any four-letter exchange code such as XNYS, and any name
 * beginning CUSTOM_ -- and joins of several calendars, written as TARGET,UK
 * or JoinHolidays(TARGET, US settlement).
 *
 * A curve keeps the calendar expression its document wrote, so the export writes
 * it back unchanged. ores_refdata_validate_calendar_fn splits the expression
 * and checks each calendar in it against this table or against one of the two
 * open forms, so a calendar ORE would refuse is refused on insert. The refdata
 * calendar table models calendars the system materialises; this table is the
 * vocabulary of names an ORE document may write.
 */
struct calendar_name final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The calendar name exactly as an ORE document may write it: 'TARGET', 'US settlement',
     * 'weekends only', 'USD'.
     */
    std::string code;

    /**
     * @brief What the spelling means, in one line, for a reader who does not know ORE.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this calendar name.
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
    friend bool operator==(const calendar_name&, const calendar_name&) = default;
};

/**
 * @brief Dispatch-key identifier for calendar_name, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const calendar_name&) {
    return "ores.refdata.calendar_name";
}

}

#endif
