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
#ifndef ORES_IAM_DOMAIN_ROLE_PERMISSION_HPP
#define ORES_IAM_DOMAIN_ROLE_PERMISSION_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief Grants a permission to an IAM role.
 *
 * Junction table granting permissions to IAM roles. A role holds several
 * permissions and a permission is held by several roles. A permission taken
 * away closes the row and deletes nothing, so the history of a role includes
 * the permissions it no longer holds.
 */
struct role_permission final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the IAM role.
     *
     * References ores_iam_roles_tbl.id (soft FK).
     */
    boost::uuids::uuid role_id;

    /**
     * @brief ID of the permission granted to the role.
     *
     * References ores_iam_permissions_tbl.id (soft FK).
     */
    boost::uuids::uuid permission_id;

    /**
     * @brief Username of the person who last modified this role permission.
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
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality, on the same terms as an entity's.
     */
    friend bool operator==(const role_permission&, const role_permission&) = default;
};

/**
 * @brief Dispatch-key identifier for role_permission, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const role_permission&) {
    return "ores.iam.role_permission";
}

}

#endif
