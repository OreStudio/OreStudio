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
#ifndef ORES_IAM_DOMAIN_ROLE_GRANT_REQUEST_ROLE_HPP
#define ORES_IAM_DOMAIN_ROLE_GRANT_REQUEST_ROLE_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief One role a role grant request asks for.
 *
 * The roles an iam.role_grant approval request asks for, one row each. A row is
 * identified by the request and the role. When the request is approved, IAM
 * grants each role the account does not already hold, and records that it
 * applied it.
 */
struct role_grant_request_role final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief The role grant request the role belongs to.
     *
     * References ores_iam_role_grant_requests_tbl.request_id (soft FK).
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The role asked for.
     *
     * References ores_iam_roles_tbl.id (soft FK).
     */
    boost::uuids::uuid role_id;

    /**
     * @brief When IAM applied the role after the request was approved, or null until it has.
     * Applying is done once: a role the account already held is marked applied without a grant, and
     * a role taken away after it was applied is not granted again, because the request is still
     * approved but no longer unapplied.
     */
    std::optional<std::chrono::system_clock::time_point> applied_at;

    /**
     * @brief Username of the person who last modified this role grant request role.
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
    friend bool operator==(const role_grant_request_role&,
                           const role_grant_request_role&) = default;
};

/**
 * @brief Dispatch-key identifier for role_grant_request_role, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const role_grant_request_role&) {
    return "ores.iam.role_grant_request_role";
}

}

#endif
