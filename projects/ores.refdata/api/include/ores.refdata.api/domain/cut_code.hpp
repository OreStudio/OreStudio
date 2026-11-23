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
#ifndef ORES_REFDATA_API_DOMAIN_CUT_CODE_HPP
#define ORES_REFDATA_API_DOMAIN_CUT_CODE_HPP

#include "ores.platform/time/time_of_day.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The name of an option expiry instant, with its time, zone and fixing source.
 *
 * Reference data table of the option expiry cuts ORE Studio supports. A cut code
 * is the name of the instant an option expires at; the cut time and its zone are
 * what the name resolves to. Each cut names the fixing source that fixes the
 * market at its instant, because a cut with no source cannot be offered.
 *
 * See [[id:3B39B2E2-6E05-4DF8-B68B-6ACA12F2FCE9][The option-expiry cut code]] and
 * [[id:5AF9CEEF-A878-443A-8C32-C6E9F6C26FC9][Fixing sources for option cuts and non-deliverable
 * currencies]].
 */
struct cut_code final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The cut's code.
     *
     * Examples: 'NYC1000', 'TKY1500', 'LON1600'.
     */
    std::string code;

    /**
     * @brief Human-readable name of the cut, such as 'New York 10:00'.
     */
    std::string name;

    /**
     * @brief What the cut is used for.
     */
    std::string description;

    /**
     * @brief The time of day the cut names, read on the clock of cut_time_zone.
     */
    ores::platform::time::time_of_day cut_time;

    /**
     * @brief The IANA name of the zone the cut time is read in.
     */
    std::string cut_time_zone;

    /**
     * @brief The fixing source that fixes the market at the cut.
     */
    std::string fixing_source_code;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this cut code.
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
    friend bool operator==(const cut_code&, const cut_code&) = default;
};

/**
 * @brief Dispatch-key identifier for cut_code, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cut_code&) {
    return "ores.refdata.cut_code";
}

}

#endif
