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
#ifndef ORES_REFDATA_API_DOMAIN_AVERAGE_OIS_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_AVERAGE_OIS_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for an averaging overnight index swap.
 *
 * Describes how ORE averages the floating leg of an overnight index swap over a
 * tenor, and how the fixed leg pays against it. Corresponds to the <AverageOIS>
 * element in ORE conventions.xml. The id field is the natural key (ORE <Id>
 * element).
 */
struct average_ois_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique averaging OIS identifier.
     *
     * Examples: 'EUR-AVERAGE-OIS', 'USD-SOFR-AVERAGE-OIS'.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Business days from trade date to spot date.
     */
    int spot_lag = 0;

    /**
     * @brief Tenor the fixed leg averages over.
     */
    std::string fixed_tenor;

    /**
     * @brief Day count fraction of the fixed leg, as the canonical code the mapper stores.
     */
    std::string fixed_day_count_fraction;

    /**
     * @brief Fixed-leg payment calendar (e.g. 'TARGET').
     */
    std::optional<std::string> fixed_calendar;

    /**
     * @brief Business day convention of the fixed leg.
     */
    std::optional<std::string> fixed_convention;

    /**
     * @brief Business day convention of the fixed leg's payments.
     */
    std::optional<std::string> fixed_payment_convention;

    /**
     * @brief Payment frequency of the fixed leg.
     */
    std::optional<std::string> fixed_frequency;

    /**
     * @brief Overnight index the floating leg compounds.
     */
    std::string index;

    /**
     * @brief Tenor the floating leg resets on.
     */
    std::string on_tenor;

    /**
     * @brief Business days before the period end at which the compounded rate is fixed.
     */
    std::string rate_cutoff;

    /**
     * @brief Username of the person who last modified this averaging OIS convention.
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
    friend bool operator==(const average_ois_convention&, const average_ois_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for average_ois_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const average_ois_convention&) {
    return "ores.refdata.average_ois_convention";
}

}

#endif
