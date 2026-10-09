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
#ifndef ORES_REFDATA_API_DOMAIN_IBOR_INDEX_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_IBOR_INDEX_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for an interbank offered rate (IBOR) index.
 *
 * Defines the fixing calendar, day count, settlement lag, and business day
 * convention for a term IBOR index such as EURIBOR or USD LIBOR.
 * Corresponds to the <IborIndex> element in ORE conventions.xml.
 */
struct ibor_index_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique index identifier.
     *
     * Examples: 'EUR-EURIBOR', 'USD-LIBOR'.
     */
    std::string id;

    /**
     * @brief Calendar used to determine valid fixing dates (e.g. 'TARGET').
     */
    std::string fixing_calendar;

    /**
     * @brief Day count fraction for accrual (canonical FpML, e.g. 'ACT/360').
     */
    std::string day_count_fraction;

    /**
     * @brief Number of business days from fixing to settlement.
     */
    int settlement_days = 0;

    /**
     * @brief Business day convention for maturity dates (canonical FpML).
     */
    std::string business_day_convention;

    /**
     * @brief Whether end-of-month convention applies.
     */
    bool end_of_month = false;

    /**
     * @brief The oresmd URI of the index this convention defines: the IBOR index itself, as a
     * fixing URI, for example 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR' for
     * 'EUR-EURIBOR'. Classification: fixing — the convention defines the index, so the address
     * names the fixing itself and not a series it references.
     *
     * The column holds the address as a value, so refdata depends on the oresmd format as a
     * contract only and never on the marketdata library or its tables. Nullable, because no
     * convention is required to state its address yet.
     */
    std::optional<std::string> oresmd_uri;

    /**
     * @brief Username of the person who last modified this IBOR index convention.
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
    friend bool operator==(const ibor_index_convention&, const ibor_index_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for ibor_index_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ibor_index_convention&) {
    return "ores.refdata.ibor_index_convention";
}

}

#endif
