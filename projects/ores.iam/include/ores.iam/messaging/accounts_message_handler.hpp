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
#ifndef ORES_IAM_MESSAGING_ACCOUNTS_MESSAGE_HANDLER_HPP
#define ORES_IAM_MESSAGING_ACCOUNTS_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/account_setup_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/repository/session_repository.hpp"
#include "ores.iam/repository/tenant_repository.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.geo/service/geolocation_service.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"

namespace ores::iam::messaging {

/**
 * @brief Function type for fetching available bundles during bootstrap.
 *
 * Returns lightweight bundle info (code, name, description) without
 * requiring a dependency on ores.dq.
 */
using bundle_provider_fn = std::function<std::vector<bootstrap_bundle_info>()>;

/**
 * @brief Message handler for accounts subsystem messages.
 *
 * Processes messages in the accounts subsystem range (0x2000-0x2FFF).
 * Currently handles:
 * - save_account_request: Creates a new account
 * - get_accounts_request: Retrieves all accounts from the repository
 * - list_login_info_request: Retrieves all login info records
 * - login_request: Authenticates a user and updates login tracking
 * - logout_request: Logs out a user and marks them as offline
 * - lock_account_request: Locks an account preventing login
 * - unlock_account_request: Unlocks a locked account
 * - delete_account_request: Deletes an account (bitemporal soft delete)
 * - create_initial_admin_request: Creates initial admin (bootstrap mode only, localhost only)
 * - bootstrap_status_request: Checks if system is in bootstrap mode
 * - get_account_history_request: Retrieves all historical versions of an account
 * - reset_password_request: Sets password_reset_required flag to force password change
 * - change_password_request: Changes user's password and clears password_reset_required flag
 * - update_my_email_request: Allows user to update their own email address
 * - signup_request: Creates a new account via self-registration (when enabled)
 * - list_sessions_request: Lists session history for an account
 * - get_session_statistics_request: Gets computed session statistics
 * - get_active_sessions_request: Gets currently active sessions
 *
 * RBAC operations:
 * - list_roles_request: Lists all roles in the system
 * - list_permissions_request: Lists all permissions in the system
 * - get_role_request: Gets a specific role by ID or name
 * - assign_role_request: Assigns a role to an account
 * - revoke_role_request: Revokes a role from an account
 * - get_account_roles_request: Gets all roles assigned to an account
 * - get_account_permissions_request: Gets effective permissions for an account
 *
 * Tenant management operations:
 * - get_tenants_request: Lists all tenants
 * - save_tenant_request: Creates or updates a tenant
 * - delete_tenant_request: Deletes a tenant
 * - get_tenant_history_request: Gets tenant version history
 * - get_tenant_types_request: Lists tenant types
 * - save_tenant_type_request: Creates or updates a tenant type
 * - delete_tenant_type_request: Deletes a tenant type
 * - get_tenant_type_history_request: Gets tenant type version history
 * - get_tenant_statuses_request: Lists tenant statuses
 * - save_tenant_status_request: Creates or updates a tenant status
 * - delete_tenant_status_request: Deletes a tenant status
 * - get_tenant_status_history_request: Gets tenant status version history
 */
class accounts_message_handler final : public comms::messaging::message_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.iam.messaging.accounts_message_handler");
        return instance;
    }

public:
    /**
     * @brief Construct an accounts message handler.
     *
     * @param ctx Database context for repository access
     * @param system_flags Shared system flags service for flag access
     * @param sessions Shared auth session service for authentication
     * @param auth_service Shared authorization service for RBAC permission checks
     * @param geo_service Shared geolocation service for IP to location lookups
     * @param bundle_provider Optional callback to fetch available bundles for bootstrap
     */
    accounts_message_handler(database::context ctx,
        std::shared_ptr<variability::service::system_flags_service> system_flags,
        std::shared_ptr<comms::service::auth_session_service> sessions,
        std::shared_ptr<service::authorization_service> auth_service,
        std::shared_ptr<geo::service::geolocation_service> geo_service,
        bundle_provider_fn bundle_provider = nullptr);

    using handler_result = boost::asio::awaitable<
        std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>
    >;

    /**
     * @brief Handle an accounts subsystem message.
     *
     * @param type The message type (must be in range 0x2000-0x2FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    handler_result
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    /**
     * @brief Handle save_account_request message.
     *
     * Requires authentication. Only admin users can create accounts.
     */
    handler_result
    handle_save_account_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_accounts_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_get_accounts_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle list_login_info_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_list_login_info_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle login_request message.
     */
    handler_result
    handle_login_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle lock_account_request message.
     *
     * Uses session context to determine requester identity for authorization.
     */
    handler_result
    handle_lock_account_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle unlock_account_request message.
     *
     * Requires authentication. Only admin users can unlock accounts.
     */
    handler_result
    handle_unlock_account_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_account_request message.
     *
     * Requires authentication. Only admin users can delete accounts.
     */
    handler_result
    handle_delete_account_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle create_initial_admin_request message.
     *
     * Only available in bootstrap mode and from localhost.
     */
    handler_result
    handle_create_initial_admin_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle bootstrap_status_request message.
     *
     * Always available - returns current bootstrap mode status.
     */
    handler_result
    handle_bootstrap_status_request(std::span<const std::byte> payload);

    /**
     * @brief Handle logout_request message.
     *
     * Sets the user's online status to false and removes the session.
     */
    handler_result
    handle_logout_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_account_history_request message.
     *
     * Requires authentication. Returns all historical versions of an account.
     */
    handler_result
    handle_get_account_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle reset_password_request message.
     *
     * Requires authentication. Only admin users can reset passwords.
     * Sets the password_reset_required flag on the target account(s).
     */
    handler_result
    handle_reset_password_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle change_password_request message.
     *
     * Requires authentication. Allows users to change their own password.
     * Validates password strength and clears password_reset_required flag.
     */
    handler_result
    handle_change_password_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle update_my_email_request message.
     *
     * Requires authentication. Allows users to update their own email address.
     */
    handler_result
    handle_update_my_email_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle signup_request message.
     *
     * Does NOT require authentication. Allows unauthenticated users to
     * create their own accounts when the system.user_signups flag is enabled.
     */
    handler_result
    handle_signup_request(std::span<const std::byte> payload);

    // =========================================================================
    // RBAC Handlers
    // =========================================================================

    /**
     * @brief Handle list_roles_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_list_roles_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle list_permissions_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_list_permissions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_role_request message.
     *
     * Requires authentication. Returns a specific role by ID or name.
     */
    handler_result
    handle_get_role_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle assign_role_request message.
     *
     * Requires authentication and roles:assign permission.
     */
    handler_result
    handle_assign_role_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle revoke_role_request message.
     *
     * Requires authentication and roles:revoke permission.
     */
    handler_result
    handle_revoke_role_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_account_roles_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_get_account_roles_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_account_permissions_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_get_account_permissions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle suggest_role_commands_request message.
     *
     * Requires authentication. Generates shell commands to assign all roles
     * to a specified account. The account is identified by username and
     * either hostname or tenant_id.
     */
    handler_result
    handle_suggest_role_commands_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle assign_role_by_name_request message.
     *
     * Requires authentication and roles:assign permission. Resolves
     * principal (username@hostname) to account ID and role name to role ID,
     * then delegates to assign_role.
     */
    handler_result
    handle_assign_role_by_name_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle revoke_role_by_name_request message.
     *
     * Requires authentication and roles:revoke permission. Resolves
     * principal (username@hostname) to account ID and role name to role ID,
     * then delegates to revoke_role.
     */
    handler_result
    handle_revoke_role_by_name_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Resolved account and role IDs from a principal + role name.
     */
    struct resolved_role_target {
        boost::uuids::uuid account_id;
        boost::uuids::uuid role_id;
    };

    /**
     * @brief Resolve principal and role name to account and role IDs.
     *
     * Parses the principal, resolves tenant context from hostname,
     * looks up the account by username and the role by name.
     *
     * @param principal The principal string (username or username@hostname)
     * @param role_name The role name to resolve
     * @return Resolved IDs on success, or a serialized error response
     */
    using resolve_result = std::expected<
        resolved_role_target,
        std::vector<std::byte>
    >;
    resolve_result resolve_role_target(
        const std::string& principal, const std::string& role_name);

    /**
     * @brief Check if a remote address is localhost.
     *
     * @param remote_address The remote endpoint address
     * @return true if the address is localhost (127.0.0.1 or ::1), false otherwise
     */
    static bool is_localhost(const std::string& remote_address);

    /**
     * @brief Create a database context for the session's tenant.
     *
     * Creates a new context with the tenant ID from the session, ensuring
     * that database operations use the correct tenant isolation. Each
     * request should use its own context to avoid race conditions.
     *
     * @param session The session containing the tenant ID
     * @return A new context configured for the session's tenant
     */
    [[nodiscard]] database::context
    make_request_context(const comms::service::session_info& session) const;

    /**
     * @brief Result type for authorization checks.
     *
     * Contains the session info if authorized, or an error code if not.
     */
    using auth_check_result = std::expected<
        comms::service::session_info,
        ores::utility::serialization::error_code
    >;

    /**
     * @brief Check if a request is authorized with the required permission.
     *
     * Verifies the requester has an active session and the required permission.
     * Logs warnings for denied requests.
     *
     * @param remote_address The remote endpoint address
     * @param permission The required permission (e.g., "accounts:create")
     * @param operation_name Human-readable name for logging (e.g., "Create account")
     * @return The session if authorized, or error code if not
     */
    auth_check_result check_authorization(
        const std::string& remote_address,
        std::string_view permission,
        std::string_view operation_name);

    /**
     * @brief Get session for a request without permission check.
     *
     * Used for operations that require authentication but no specific permission.
     *
     * @param remote_address The remote endpoint address
     * @param operation_name Human-readable name for logging
     * @return The session if found, or error code if not
     */
    auth_check_result get_authenticated_session(
        const std::string& remote_address,
        std::string_view operation_name);

    /**
     * @brief Handle list_sessions_request message.
     *
     * Requires authentication. Admin can view any account's sessions,
     * regular users can only view their own.
     */
    handler_result
    handle_list_sessions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_session_statistics_request message.
     *
     * Requires authentication. Admin can view statistics for any account,
     * regular users can only view their own.
     */
    handler_result
    handle_get_session_statistics_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_active_sessions_request message.
     *
     * Requires authentication. Admin gets all active sessions,
     * regular users get only their own active sessions.
     */
    handler_result
    handle_get_active_sessions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Tenant Management Handlers
    // =========================================================================

    /**
     * @brief Handle get_tenants_request message.
     *
     * Requires authentication and tenants:read permission.
     */
    handler_result
    handle_get_tenants_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle save_tenant_request message.
     *
     * Requires authentication and tenants:write permission.
     */
    handler_result
    handle_save_tenant_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_tenant_request message.
     *
     * Requires authentication and tenants:delete permission.
     */
    handler_result
    handle_delete_tenant_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_tenant_history_request message.
     *
     * Requires authentication and tenants:read permission.
     */
    handler_result
    handle_get_tenant_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle provision_tenant_request message.
     *
     * Creates a new tenant and provisions it with base IAM data by calling
     * ores_iam_provision_tenant_fn. Requires authentication and tenants:write
     * permission.
     */
    handler_result
    handle_provision_tenant_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_tenant_types_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_get_tenant_types_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle save_tenant_type_request message.
     *
     * Requires authentication and system admin permission.
     */
    handler_result
    handle_save_tenant_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_tenant_type_request message.
     *
     * Requires authentication and system admin permission.
     */
    handler_result
    handle_delete_tenant_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_tenant_type_history_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_get_tenant_type_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_tenant_statuses_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_get_tenant_statuses_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle save_tenant_status_request message.
     *
     * Requires authentication and system admin permission.
     */
    handler_result
    handle_save_tenant_status_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_tenant_status_request message.
     *
     * Requires authentication and system admin permission.
     */
    handler_result
    handle_delete_tenant_status_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_tenant_status_history_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_get_tenant_status_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    service::account_service service_;
    database::context ctx_;
    std::shared_ptr<variability::service::system_flags_service> system_flags_;
    std::shared_ptr<comms::service::auth_session_service> sessions_;
    std::shared_ptr<service::authorization_service> auth_service_;
    service::account_setup_service setup_service_;
    repository::session_repository session_repo_;
    repository::tenant_repository tenant_repo_;
    std::shared_ptr<geo::service::geolocation_service> geo_service_;
    bundle_provider_fn bundle_provider_;
};

}

#endif
