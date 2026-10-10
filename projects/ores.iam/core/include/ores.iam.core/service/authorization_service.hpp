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

#include "ores.database/domain/context.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.iam.api/domain/account_role.hpp"
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_role_repository.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.iam.core/repository/role_permission_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief One role an account holds, with its permissions and assignment tail.
 *
 * The permissions are attributed to the role that grants them rather than
 * flattened into one list, and the tail is the account-role junction row's own
 * record of who granted the role, when and why.
 */
struct account_access_entry {
    domain::role role;
    std::vector<std::string> permission_codes;
    std::string assigned_by;
    std::chrono::system_clock::time_point assigned_at;
    std::string change_reason_code;
    std::string change_commentary;
};

/**
 * @brief The answer to an access read: the outcome and the rows it names.
 *
 * A refusal is a value rather than an empty list: @c result.outcome is
 * @c denied with the permission that was required, so a caller can tell a
 * refusal from an account that holds nothing.
 */
struct account_access {
    ores::utility::domain::result result;
    std::vector<account_access_entry> roles;
};

/**
 * @brief The answer to a bundle write: the outcome and the bundle as stored.
 *
 * The codes are the role's own list after the write, so the caller reads back
 * what it wrote rather than assuming the two agree. A refusal is a value, as
 * it is for the access reads: @c result.outcome is @c denied with the
 * permission that was required, @c invalid with the code that was not a
 * permission, or @c missing with the role that does not exist.
 */
struct role_permissions {
    ores::utility::domain::result result;
    std::vector<std::string> permission_codes;
};

/**
 * @brief Which page of what an account's roles allow is asked for.
 *
 * An empty area means the first area the account holds something in, so a page
 * is always of one area. The search narrows the resources by name. The limit is
 * kept to a sensible range so a caller cannot ask for the whole catalogue.
 */
struct permission_query {
    std::string area;
    std::string search;
    int offset = 0;
    int limit = 15;
    /// Also the resources the roles do not grant, for the role editor to tick.
    bool include_unheld = false;
};

/**
 * @brief Which page of the tenant's roles is asked for.
 */
struct roles_query {
    /// One role, by id; the other fields are then ignored.
    std::string role_id;
    std::string search;
    std::string area;
    bool include_service = false;
    int offset = 0;
    int limit = 15;
};

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
class ORES_IAM_CORE_EXPORT authorization_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.authorization_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
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
    explicit authorization_service(context ctx, event_bus* event_bus = nullptr);

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
    std::optional<domain::permission> find_permission_by_code(const std::string& code);

    /**
     * @brief Creates a new permission.
     *
     * @param code The permission code (e.g., "accounts:create")
     * @param description Human-readable description
     * @return The created permission
     */
    domain::permission create_permission(const std::string& code, const std::string& description);

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
     * @param modified_by Username of the person creating the role
     * @return The created role
     */
    domain::role create_role(const std::string& name,
                             const std::string& description,
                             const std::vector<std::string>& permission_codes,
                             const std::string& modified_by);

    /**
     * @brief Gets the permission codes assigned to a role.
     */
    std::vector<std::string> get_role_permissions(const boost::uuids::uuid& role_id);

    /**
     * @brief Replaces the permissions a role bundles.
     *
     * The codes are the whole bundle the role should carry, not an increment:
     * a code the role bundles and this call omits is removed, a code both name
     * stays, and a code neither names is added. The check on
     * =iam::roles:update= runs before anything is read or written, the whole
     * bundle resolves before anything is written, and every row this call
     * creates carries the change reason and commentary it was given.
     *
     * @param caller_id The authenticated caller's account
     * @param role_id The role whose bundle is being replaced
     * @param permission_codes The complete bundle the role should carry
     * @param change_reason_code Why the bundle changed; the new record reason
     * when empty
     * @param change_commentary The sentence that goes with the reason
     * @return The outcome, and the bundle as stored
     */
    role_permissions replace_role_permissions(const boost::uuids::uuid& caller_id,
                                              const boost::uuids::uuid& role_id,
                                              const std::vector<std::string>& permission_codes,
                                              const std::string& change_reason_code,
                                              const std::string& change_commentary);

    // ========================================================================
    // Role Assignment
    // ========================================================================

    /**
     * @brief Assigns a role to an account.
     *
     * Publishes a role_assigned_event and account_permissions_changed_event if
     * an event bus is configured.
     *
     * @param account_id The account to receive the role
     * @param role_id The role to assign
     * @param assigned_by Username of the person making the assignment
     * @param change_commentary Optional commentary explaining the role assignment
     * @param change_reason_code Why the role is given; empty records
     * system.new_record, for callers acting on the platform's behalf
     */
    void assign_role(const boost::uuids::uuid& account_id,
                     const boost::uuids::uuid& role_id,
                     const std::string& assigned_by,
                     const std::string& change_commentary = "Role assigned to account",
                     const std::string& change_reason_code = "");

    /**
     * @brief Revokes a role from an account.
     *
     * Publishes a role_revoked_event and account_permissions_changed_event if
     * an event bus is configured.
     *
     * @param account_id The account to remove the role from
     * @param role_id The role to revoke
     */
    void revoke_role(const boost::uuids::uuid& account_id, const boost::uuids::uuid& role_id);

    /**
     * @brief Gets all roles assigned to an account.
     */
    std::vector<domain::role> get_account_roles(const boost::uuids::uuid& account_id);

    // ========================================================================
    // Composed Access Reads
    // ========================================================================

    /**
     * @brief Resolves the request's authenticated actor to their account.
     *
     * The request context carries the actor's username, and every permission
     * check and self read is keyed by account id. This is the one place the
     * two are joined, so no caller parses a username as a uuid.
     *
     * @return The account the context's actor names, or nothing when the
     * context's tenant holds no account with that username
     */
    std::optional<boost::uuids::uuid> caller_account() const;

    /**
     * @brief Reads an account's own access.
     *
     * The caller names no other account, so a session is the whole of the
     * requirement and no permission is checked.
     *
     * @param account_id The authenticated caller's own account
     * @return Each role the account holds, with its permissions and tail
     */
    account_access read_own_access(const boost::uuids::uuid& account_id);

    /**
     * @brief Reads another account's access, refusing a caller who may not.
     *
     * The database-backed roles:read check runs before the account is looked
     * up, so a caller without it learns nothing about the account. A refusal
     * answers @c denied with roles:read as its code.
     *
     * @param caller_id The authenticated caller's account
     * @param account_id The account whose access was asked for
     */
    account_access read_account_access(const boost::uuids::uuid& caller_id,
                                       const boost::uuids::uuid& account_id);

    /**
     * @brief Reads one page of what an account's own roles let it do.
     *
     * The caller names no other account, so a session is the whole of the
     * requirement and no permission is checked.
     */
    messaging::permission_page_response read_own_permissions(const boost::uuids::uuid& account_id,
                                                              const permission_query& query);

    /**
     * @brief Reads one page of what another account's roles let it do.
     *
     * Needs roles:read, as reading the account's roles does, and answers
     * @c denied with that code otherwise.
     */
    messaging::permission_page_response
    read_account_permissions(const boost::uuids::uuid& caller_id,
                             const boost::uuids::uuid& account_id,
                             const permission_query& query);

    /**
     * @brief Reads one page of the catalogue against what one role grants.
     *
     * Serves the role editor. Needs roles:read and answers @c denied with that
     * code otherwise.
     */
    messaging::permission_page_response read_role_permissions(const boost::uuids::uuid& caller_id,
                                                              const boost::uuids::uuid& role_id,
                                                              const permission_query& query);

    /**
     * @brief Reads one page of the tenant's roles, with each role's permissions
     * counted and not listed. Needs roles:read.
     */
    messaging::role_page_response read_roles_page(const boost::uuids::uuid& caller_id,
                                                  const roles_query& query);

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
    std::vector<std::string> get_effective_permissions(const boost::uuids::uuid& account_id);

    /**
     * @brief Checks if an account has a specific permission.
     *
     * Supports the wildcard permission "*" which grants all permissions.
     *
     * @param account_id The account to check
     * @param permission_code The permission to check for
     * @return true if the account has the permission, false otherwise
     */
    bool has_permission(const boost::uuids::uuid& account_id, std::string_view permission_code);

    /**
     * @brief Checks if the given permissions list satisfies a permission check.
     *
     * The same rule the token check uses: a code, the wildcard "*", or a
     * component wildcard such as "refdata::*". The list needs no order.
     *
     * @param permissions The list of permission codes
     * @param required_permission The permission to check for
     * @return true if the permissions satisfy the requirement
     */
    static bool check_permission(const std::vector<std::string>& permissions,
                                 std::string_view required_permission);

private:
    /**
     * @brief Pages the catalogue by resource against the permissions the roles
     * grant, wildcards included.
     */
    messaging::permission_page_response
    page_permissions(const std::vector<account_access_entry>& roles, const permission_query& query);

    /**
     * @brief Composes one account's roles, their permissions and their tails.
     *
     * The only place the joined shape is assembled; both access reads reach
     * the rows through it.
     */
    std::vector<account_access_entry> compose_account_access(const boost::uuids::uuid& account_id);

    /**
     * @brief Publishes an account_permissions_changed_event for the given account.
     */
    void publish_account_permissions_changed(const boost::uuids::uuid& account_id);

    context ctx_;
    repository::permission_repository permission_repo_;
    repository::role_repository role_repo_;
    repository::account_role_repository account_role_repo_;
    repository::role_permission_repository role_permission_repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
    event_bus* event_bus_;
};

}

#endif
