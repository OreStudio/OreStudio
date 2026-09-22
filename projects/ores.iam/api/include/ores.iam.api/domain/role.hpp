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
#ifndef ORES_IAM_API_DOMAIN_ROLE_HPP
#define ORES_IAM_API_DOMAIN_ROLE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A named collection of permissions that can be assigned to accounts.
 *
 * A named collection of permissions that can be assigned to accounts. Roles
 * group related permissions for easier management: a "Trading" role might
 * include permissions to read and execute trades, while a "Support" role
 * might have read-only access to most resources.
 *
 * The table is bi-temporal and audited (see
 * projects/ores.sql/create/iam/iam_roles_create.sql): it carries
 * version, the four audit columns and the valid_from/valid_to pair
 * with the GIST exclusion and the delete rule, so the model takes the
 * ordinary audited shape and needs no shape flag.
 *
 * The model describes the table alone. The hand-written domain struct also
 * carried a std::vector<std::string> permission_codes that no column
 * backs -- it is denormalised from ores_iam_role_permissions_tbl by an
 * RBAC join. A joined shape is a message or a query result, never an entity
 * member, so the member is not modelled and the generated role.hpp
 * replaces it; the join itself stays in the hand-written authorization
 * layer.
 *
 * The entity's CRUD handler and sub-registrar are switched off below: the
 * hand-written role_handler already owns the iam.v1.roles.* subjects for
 * the authorization protocol, and the generated role_handler.hpp would
 * overwrite it. The generated role_protocol.hpp still declares the entity
 * CRUD messages; only the competing handler is suppressed.
 */
struct role final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for the role.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique name for the role within its tenant (for example "Trading", "Sales", "Admin").
     * It is the natural key, so the generated table adds the partial unique index on (tenant_id,
     * name) the hand-written table already had.
     */
    std::string name;

    /**
     * @brief Human-readable description of the role's purpose and scope.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this role.
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
    friend bool operator==(const role&, const role&) = default;
};

/**
 * @brief Dispatch-key identifier for role, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const role&) {
    return "ores.iam.role";
}

}

#endif
