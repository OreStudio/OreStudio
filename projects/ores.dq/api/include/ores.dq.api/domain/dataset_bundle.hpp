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
#ifndef ORES_DQ_API_DOMAIN_DATASET_BUNDLE_HPP
#define ORES_DQ_API_DOMAIN_DATASET_BUNDLE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief A named collection of datasets designed to work together.
 *
 * Installing a bundle gets the system into a ready state with a coherent set
 * of reference data. Bundles provide a way to group related datasets that
 * should be installed together.
 *
 * A bundle belongs to one level and one scope, and the two decide the tenant it
 * writes into: a platform or world bundle publishes into the system tenant once,
 * a tenant-wide bundle into a tenant, and a party bundle into the tenant that
 * holds the party. The table does not carry either as a column yet: the bundle's
 * documentation states them, and the knowledge page "Reference data classification
 * by publication level" holds the rule that assigns them.
 *
 * Examples:
 * - "system_core": the platform's own vocabulary, published at bring-up
 * - "world": reference data adopted from an outside standard, published at bring-up
 * - "tenant_defaults": the configuration a tenant starts with
 * - "crypto": the world catalogue plus the cryptocurrency slice
 */
struct dataset_bundle final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this bundle.
     *
     * This is the surrogate key for the bundle.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique code for stable referencing.
     *
     * Examples: "world", "tenant_defaults", "risk_management".
     */
    std::string code;

    /**
     * @brief Human-readable name for the bundle.
     */
    std::string name;

    /**
     * @brief Detailed description of the bundle's contents and purpose.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this dataset bundle.
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
    friend bool operator==(const dataset_bundle&, const dataset_bundle&) = default;
};

/**
 * @brief Dispatch-key identifier for dataset_bundle, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const dataset_bundle&) {
    return "ores.dq.dataset_bundle";
}

}

#endif
