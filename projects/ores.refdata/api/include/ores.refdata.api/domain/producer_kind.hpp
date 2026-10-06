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
#ifndef ORES_REFDATA_API_DOMAIN_PRODUCER_KIND_HPP
#define ORES_REFDATA_API_DOMAIN_PRODUCER_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Classifies the producer behind a feed_binding or market_series as a real feed or a
 * generated one.
 *
 * Reference data table defining the valid feed_binding.producer_kind and
 * market_series.producer_kind values: whether the producer is a real feed
 * (VENDOR) or a generated one (SYNTHETIC). Real-versus-generated is a
 * property of the producer, not of the series: a generated producer publishes
 * its ticks exactly as a vendor does, so the axis this table carries is
 * orthogonal to derivation_kind, which answers published-versus-computed.
 * feed_binding carries the code for the producer it binds, and the series an
 * observation lands in is stamped with the binding's kind, so a reader of a
 * series can tell generated data from observed data without guessing from the
 * source string on its observations. Both values are seeded and the column
 * is FK-validated rather than free text, the same pattern its sibling
 * derivation_kind establishes. Managed by the system tenant, like other
 * refdata code tables.
 */
struct producer_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique producer kind code.
     *
     * Examples: 'VENDOR', 'SYNTHETIC'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the producer kind.
     */
    std::string name;

    /**
     * @brief Detailed description of the producer kind.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this producer kind.
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
    friend bool operator==(const producer_kind&, const producer_kind&) = default;
};

/**
 * @brief Dispatch-key identifier for producer_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const producer_kind&) {
    return "ores.refdata.producer_kind";
}

}

#endif
