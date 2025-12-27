/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_SERVICE_AUTHORIZATION_SERVICE_HPP
#define ORES_IAM_SERVICE_AUTHORIZATION_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/permission.hpp"
#include "ores.iam/domain/account_role.hpp"
#include "ores.iam/repository/role_repository.hpp"
#include "ores.iam/repository/permission_repository.hpp"
#include "ores.iam/repository/account_role_repository.hpp"
#include "ores.iam/repository/role_permission_repository.hpp"
#include "ores.eventing/service/event_bus.hpp"

namespace ores::iam::service {

/**
 * @brief Service for managing role-based access control (RBAC).
 *
 * This service provides functionality for:
 * - Managing permissions (CRUD operations)
 * - Managing roles and their associated permissions
 * - Assigning and revoking roles from accounts
 * - Checking if an account has specific permissions
 * - Computing the effective permissions for an account
 *
 * Events are published when role assignments change, allowing other
 * components (such as session management) to react to permission changes.
 */
class authorization_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.authorization_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;
    using event_bus = ores::eventing::service::event_bus;

    /**
     * @brief Constructs an authorization_service with required repositories.
     *
     * @param ctx The database context.
     * @param event_bus Optional event bus for publishing permission change events.
     */
    explicit authorization_service(context ctx,
        event_bus* event_bus = nullptr);

    // ========================================================================
    // Permission Management
    // ========================================================================

    /**
     * @brief Lists all permissions in the system.
     */
    std::vector<domain::permission> list_permissions();

    /**
     * @brief Finds a permission by its code.
     */
    std::optional<domain::permission>
    find_permission_by_code(const std::string& code);

    /**
     * @brief Creates a new permission.
     *
     * @param code The permission code (e.g., "accounts:create")
     * @param description Human-readable description
     * @return The created permission
     */
    domain::permission create_permission(const std::string& code,
        const std::string& description);

    // ========================================================================
    // Role Management
    // ========================================================================

    /**
     * @brief Lists all roles in the system.
     */
    std::vector<domain::role> list_roles();

    /**
     * @brief Finds a role by its ID.
     */
    std::optional<domain::role> find_role(const boost::uuids::uuid& role_id);

    /**
     * @brief Finds a role by its name.
     */
    std::optional<domain::role> find_role_by_name(const std::string& name);

    /**
     * @brief Creates a new role with the specified permissions.
     *
     * @param name The role name
     * @param description Human-readable description
     * @param permission_codes List of permission codes to assign
     * @param recorded_by Username of the person creating the role
     * @return The created role
     */
    domain::role create_role(const std::string& name,
        const std::string& description,
        const std::vector<std::string>& permission_codes,
        const std::string& recorded_by);

    /**
     * @brief Gets the permission codes assigned to a role.
     */
    std::vector<std::string>
    get_role_permissions(const boost::uuids::uuid& role_id);

    // ========================================================================
    // Role Assignment
    // ========================================================================

    /**
     * @brief Assigns a role to an account.
     *
     * Publishes a role_assigned_event and permissions_changed_event if
     * an event bus is configured.
     *
     * @param account_id The account to receive the role
     * @param role_id The role to assign
     * @param assigned_by Username of the person making the assignment
     */
    void assign_role(const boost::uuids::uuid& account_id,
        const boost::uuids::uuid& role_id,
        const std::string& assigned_by);

    /**
     * @brief Revokes a role from an account.
     *
     * Publishes a role_revoked_event and permissions_changed_event if
     * an event bus is configured.
     *
     * @param account_id The account to remove the role from
     * @param role_id The role to revoke
     */
    void revoke_role(const boost::uuids::uuid& account_id,
        const boost::uuids::uuid& role_id);

    /**
     * @brief Gets all roles assigned to an account.
     */
    std::vector<domain::role>
    get_account_roles(const boost::uuids::uuid& account_id);

    // ========================================================================
    // Permission Checking
    // ========================================================================

    /**
     * @brief Computes the effective permissions for an account.
     *
     * This aggregates all permissions from all roles assigned to the account.
     *
     * @param account_id The account to query
     * @return List of permission codes the account has
     */
    std::vector<std::string>
    get_effective_permissions(const boost::uuids::uuid& account_id);

    /**
     * @brief Checks if an account has a specific permission.
     *
     * Supports the wildcard permission "*" which grants all permissions.
     *
     * @param account_id The account to check
     * @param permission_code The permission to check for
     * @return true if the account has the permission, false otherwise
     */
    bool has_permission(const boost::uuids::uuid& account_id,
        const std::string& permission_code);

    /**
     * @brief Checks if the given permissions list satisfies a permission check.
     *
     * Supports the wildcard permission "*" which grants all permissions.
     *
     * @param permissions The list of permission codes
     * @param required_permission The permission to check for
     * @return true if the permissions satisfy the requirement
     */
    static bool check_permission(const std::vector<std::string>& permissions,
        const std::string& required_permission);

private:
    /**
     * @brief Publishes a permissions_changed_event for the given account.
     */
    void publish_permissions_changed(const boost::uuids::uuid& account_id);

    repository::permission_repository permission_repo_;
    repository::role_repository role_repo_;
    repository::account_role_repository account_role_repo_;
    repository::role_permission_repository role_permission_repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
    event_bus* event_bus_;
};

}

#endif
