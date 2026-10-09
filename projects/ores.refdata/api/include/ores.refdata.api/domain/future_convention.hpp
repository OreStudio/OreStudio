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
#ifndef ORES_REFDATA_API_DOMAIN_FUTURE_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_FUTURE_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for an interest rate or overnight index future.
 *
 * Describes how ORE builds the dates and settles the fixings of a future.
 * Corresponds to the <Future> element in ORE conventions.xml. The id field is the
 * natural key (ORE <Id> element).
 */
struct future_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique future identifier.
     *
     * Examples: 'EUR-3M-FUTURE', 'USD-SOFR-3M-FUTURE'.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Underlying index the future settles against.
     */
    std::string index;

    /**
     * @brief How the future's dates are generated, in ORE's own spelling (e.g. 'IMM',
     * 'FirstDayOfMonth', 'SecondThursday', 'IMMCAD'). ORE enumerates these, and the token is
     * carried verbatim so that an export writes back what an import read.
     */
    std::optional<std::string> date_generation_rule;

    /**
     * @brief How an overnight index future nets its fixings, in ORE's own spelling: 'Averaging' or
     * 'Compounding'.
     */
    std::optional<std::string> netting_type;

    /**
     * @brief Calendar the future's dates roll on (e.g. 'TARGET').
     */
    std::optional<std::string> calendar;

    /**
     * @brief Tenor of the overnight index the future tracks, when it tracks one that is not the
     * index's own.
     */
    std::optional<std::string> overnight_index_tenor;

    /**
     * @brief The oresmd URI of the market data this convention needs: the index the future
     * references, as a fixing URI. Classification: identifier — the index is named in full, so the
     * address pins one series.
     *
     * The column holds the address as a value, so refdata depends on the oresmd format as a
     * contract only and never on the marketdata library or its tables. Nullable, because no
     * convention is required to state its address yet.
     */
    std::optional<std::string> oresmd_uri;

    /**
     * @brief Username of the person who last modified this future convention.
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
    friend bool operator==(const future_convention&, const future_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for future_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const future_convention&) {
    return "ores.refdata.future_convention";
}

}

#endif
