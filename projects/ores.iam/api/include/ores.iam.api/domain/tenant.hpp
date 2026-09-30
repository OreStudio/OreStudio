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
#ifndef ORES_IAM_API_DOMAIN_TENANT_HPP
#define ORES_IAM_API_DOMAIN_TENANT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A tenant representing an isolated organisation or the system platform.
 *
 * Core entity for multi-tenancy support. Each tenant represents an isolated
 * organisation with its own users, roles, and data. The system tenant is a
 * special tenant used for shared reference data and system administration;
 * its id is the maximum UUID value (ffffffff-ffff-ffff-ffff-ffffffffffff).
 *
 * Tenants are identified by:
 * - id: UUID primary key, which is the row's own identity. The tenant_id column
 *   is the system tenant's, because the registry belongs to it and its check
 *   requires that of every row.
 * - code: Unique text code for stable referencing (e.g., 'system', 'acme')
 * - hostname: Unique hostname for tenant routing during login
 */
struct tenant final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this tenant.
     *
     * The system tenant has the maximum UUID value, ffffffff-ffff-ffff-ffff-ffffffffffff. A tenant
     * row's own identity is this column and never tenant_id: the registry is the system tenant's,
     * so the table's check requires every row, this one included, to carry the system tenant in
     * tenant_id. A predicate that asks tenant_id which tenant a row describes therefore answers
     * "the system tenant" for all of them.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique code for stable referencing. Its shape is the deployment's and not a form's: a
     * lowercase letter first, then lowercase letters, digits and underscores, at most fifty
     * characters. check_tenant_code states that to a caller as a sentence, and the table's check
     * holds every other writer to it.
     *
     * Examples: 'system', 'acme', 'demo'.
     */
    std::string code;

    /**
     * @brief Human-readable display name for the tenant.
     */
    std::string name;

    /**
     * @brief Tenant type classification (FK to tenant_types). The synthetic generator emits the
     * canonical automation type: the type/status insert validations reject any value with no active
     * tenant_type row, and the seed data (iam_tenant_types_populate.sql) seeds the four
     * system-tenant types, of which automation is the system-process default.
     */
    std::string type;

    /**
     * @brief Detailed description of the tenant.
     */
    std::string description;

    /**
     * @brief Unique hostname for tenant routing.
     */
    std::string hostname;

    /**
     * @brief Tenant lifecycle status (FK to tenant_statuses).
     */
    std::string status;

    /**
     * @brief Whether self-registrations land in this tenant when the address they arrive at names
     * no tenant. At most one live row may hold it, because two tenants flagged as the default is a
     * question with no answer.
     *
     * The flag belongs to the super administrator. The write policy on this table admits only the
     * system tenant, and every tenant row carries the system tenant, so only a super administrator
     * may write a tenant row at all. That is also why the default party and the default role are
     * not columns here: a tenant administrator could not set them.
     */
    bool is_registration_default = false;

    /**
     * @brief Username of the person who last modified this tenant.
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
    friend bool operator==(const tenant&, const tenant&) = default;
};

/**
 * @brief Dispatch-key identifier for tenant, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenant&) {
    return "ores.iam.tenant";
}

}

#endif
