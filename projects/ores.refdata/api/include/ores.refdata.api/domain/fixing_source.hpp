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
#ifndef ORES_REFDATA_API_DOMAIN_FIXING_SOURCE_HPP
#define ORES_REFDATA_API_DOMAIN_FIXING_SOURCE_HPP

#include "ores.platform/time/time_of_day.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A source that fixes a rate at a stated time, with its administrator, calendar, rounding
 * and status.
 *
 * Reference data table of the fixing sources ORE Studio supports: the WM/R cut
 * sources, the central-bank FX reference rates, the settlement rate options of the
 * non-deliverable currencies and the interest rate indices. The code is the label
 * the market uses, which is also the SOURCE of an ORE FX index such as
 * FX-WMR1600LON-EUR-USD.
 *
 * The publication time is a time of day without a zone, and the zone is an IANA
 * name in its own column, so the instant a source fixes at is read on the
 * source's own clock. The calendar names the source's fixing days by the code of
 * a calendar. It is not checked against the calendar table, because the calendars
 * are published from their dataset after the reference data is seeded.
 *
 * See [[id:5AF9CEEF-A878-443A-8C32-C6E9F6C26FC9][Fixing sources for option cuts and non-deliverable
 * currencies]].
 */
struct fixing_source final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The source's label.
     *
     * Examples: 'WMR1600LON', 'ECB', 'KFTC18', 'SOFR'.
     */
    std::string code;

    /**
     * @brief Human-readable name of the source.
     */
    std::string name;

    /**
     * @brief What the source fixes and for whom.
     */
    std::string description;

    /**
     * @brief How the source arrives at its value: published, keyed or derived.
     */
    std::string kind_code;

    /**
     * @brief The asset class of the rates the source fixes, as an oresmd address names it: fx or
     * ir.
     */
    std::string asset_class;

    /**
     * @brief The body that administers the source, such as LSEG or the European Central Bank.
     */
    std::string administrator;

    /**
     * @brief The time of day the source fixes at, read on the clock of publication_time_zone.
     */
    ores::platform::time::time_of_day publication_time;

    /**
     * @brief The IANA name of the zone the publication time is read in.
     */
    std::string publication_time_zone;

    /**
     * @brief The code of the calendar whose business days the source fixes on.
     */
    std::string calendar_code;

    /**
     * @brief The number of decimal places the source rounds its value to.
     */
    int rounding_decimals = 0;

    /**
     * @brief Whether the source still fixes: active or ceased.
     */
    std::string status_code;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this fixing source.
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
    friend bool operator==(const fixing_source&, const fixing_source&) = default;
};

/**
 * @brief Dispatch-key identifier for fixing_source, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fixing_source&) {
    return "ores.refdata.fixing_source";
}

}

#endif
