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
#include "ores.accounts/service/authorization_service.hpp"

#include <algorithm>
#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include "ores.accounts/domain/permission.hpp"
#include "ores.accounts/domain/events/role_assigned_event.hpp"
#include "ores.accounts/domain/events/role_revoked_event.hpp"
#include "ores.accounts/domain/events/permissions_changed_event.hpp"

namespace ores::accounts::service {

using namespace ores::utility::log;

authorization_service::authorization_service(context ctx, event_bus* event_bus)
    : permission_repo_(ctx),
      role_repo_(ctx),
      account_role_repo_(ctx),
      role_permission_repo_(ctx),
      event_bus_(event_bus) {
    BOOST_LOG_SEV(lg(), info) << "Authorization service initialized.";
}

// ============================================================================
// Permission Management
// ============================================================================

std::vector<domain::permission> authorization_service::list_permissions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all permissions.";
    return permission_repo_.read_latest();
}

std::optional<domain::permission>
authorization_service::find_permission_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding permission by code: " << code;
    auto permissions = permission_repo_.read_latest_by_code(code);
    if (permissions.empty()) {
        return std::nullopt;
    }
    return permissions.front();
}

domain::permission authorization_service::create_permission(
    const std::string& code,
    const std::string& description) {
    BOOST_LOG_SEV(lg(), info) << "Creating permission: " << code;

    if (code.empty()) {
        throw std::invalid_argument("Permission code cannot be empty.");
    }

    // Check if permission already exists
    auto existing = find_permission_by_code(code);
    if (existing) {
        throw std::runtime_error("Permission with code '" + code +
            "' already exists.");
    }

    domain::permission perm;
    perm.id = uuid_generator_.generate();
    perm.code = code;
    perm.description = description;

    permission_repo_.write(perm);

    BOOST_LOG_SEV(lg(), info) << "Created permission: " << code
                              << " with ID: " << perm.id;
    return perm;
}

// ============================================================================
// Role Management
// ============================================================================

std::vector<domain::role> authorization_service::list_roles() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all roles.";
    auto roles = role_repo_.read_latest();

    // Batch fetch all role-permission mappings in a single query
    auto role_perm_map = role_permission_repo_.read_all_role_permission_codes();

    // Populate permission_codes for each role from the pre-fetched map
    for (auto& role : roles) {
        const auto role_id_str = boost::lexical_cast<std::string>(role.id);
        if (auto it = role_perm_map.find(role_id_str); it != role_perm_map.end()) {
            role.permission_codes = std::move(it->second);
        }
    }

    return roles;
}

std::optional<domain::role>
authorization_service::find_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding role by ID: " << role_id;
    auto roles = role_repo_.read_latest(role_id);
    if (roles.empty()) {
        return std::nullopt;
    }
    auto role = roles.front();
    role.permission_codes = get_role_permissions(role.id);
    return role;
}

std::optional<domain::role>
authorization_service::find_role_by_name(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Finding role by name: " << name;
    auto roles = role_repo_.read_latest_by_name(name);
    if (roles.empty()) {
        return std::nullopt;
    }
    auto role = roles.front();
    role.permission_codes = get_role_permissions(role.id);
    return role;
}

domain::role authorization_service::create_role(
    const std::string& name,
    const std::string& description,
    const std::vector<std::string>& permission_codes,
    const std::string& recorded_by) {
    BOOST_LOG_SEV(lg(), info) << "Creating role: " << name;

    if (name.empty()) {
        throw std::invalid_argument("Role name cannot be empty.");
    }

    // Check if role already exists
    auto existing = find_role_by_name(name);
    if (existing) {
        throw std::runtime_error("Role with name '" + name + "' already exists.");
    }

    // Validate all permissions exist before creating anything
    std::vector<domain::permission> resolved_perms;
    resolved_perms.reserve(permission_codes.size());
    for (const auto& code : permission_codes) {
        auto perm = find_permission_by_code(code);
        if (!perm) {
            throw std::runtime_error("Permission with code '" + code + "' not found.");
        }
        resolved_perms.push_back(*perm);
    }

    // Create the role
    domain::role role;
    role.id = uuid_generator_.generate();
    role.version = 0;
    role.name = name;
    role.description = description;
    role.recorded_by = recorded_by;
    role.permission_codes = permission_codes;

    role_repo_.write(role);

    // Create role-permission mappings
    for (const auto& perm : resolved_perms) {
        domain::role_permission rp;
        rp.role_id = role.id;
        rp.permission_id = perm.id;
        role_permission_repo_.write(rp);
    }

    BOOST_LOG_SEV(lg(), info) << "Created role: " << name
                              << " with ID: " << role.id
                              << " and " << permission_codes.size()
                              << " permissions.";
    return role;
}

std::vector<std::string>
authorization_service::get_role_permissions(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting permissions for role: " << role_id;

    auto role_perms = role_permission_repo_.read_latest_by_role(role_id);
    std::vector<std::string> codes;
    codes.reserve(role_perms.size());

    for (const auto& rp : role_perms) {
        auto perms = permission_repo_.read_latest(rp.permission_id);
        if (!perms.empty()) {
            codes.push_back(perms.front().code);
        }
    }

    return codes;
}

// ============================================================================
// Role Assignment
// ============================================================================

void authorization_service::assign_role(
    const boost::uuids::uuid& account_id,
    const boost::uuids::uuid& role_id,
    const std::string& assigned_by) {
    BOOST_LOG_SEV(lg(), info) << "Assigning role " << role_id
                              << " to account " << account_id;

    // Verify role exists
    auto role = find_role(role_id);
    if (!role) {
        throw std::runtime_error("Role not found: " +
            boost::lexical_cast<std::string>(role_id));
    }

    // Check if already assigned
    if (account_role_repo_.exists(account_id, role_id)) {
        BOOST_LOG_SEV(lg(), warn) << "Role " << role_id
                                  << " already assigned to account "
                                  << account_id;
        return;
    }

    // Create assignment
    domain::account_role ar;
    ar.account_id = account_id;
    ar.role_id = role_id;
    ar.assigned_by = assigned_by;

    account_role_repo_.write(ar);

    BOOST_LOG_SEV(lg(), info) << "Role " << role->name
                              << " assigned to account " << account_id;

    // Publish events
    if (event_bus_) {
        domain::events::role_assigned_event event;
        event.account_id = account_id;
        event.role_id = role_id;
        event.timestamp = std::chrono::system_clock::now();
        event_bus_->publish(event);

        publish_permissions_changed(account_id);
    }
}

void authorization_service::revoke_role(
    const boost::uuids::uuid& account_id,
    const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), info) << "Revoking role " << role_id
                              << " from account " << account_id;

    account_role_repo_.remove(account_id, role_id);

    BOOST_LOG_SEV(lg(), info) << "Role " << role_id
                              << " revoked from account " << account_id;

    // Publish events
    if (event_bus_) {
        domain::events::role_revoked_event event;
        event.account_id = account_id;
        event.role_id = role_id;
        event.timestamp = std::chrono::system_clock::now();
        event_bus_->publish(event);

        publish_permissions_changed(account_id);
    }
}

std::vector<domain::role>
authorization_service::get_account_roles(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting roles for account: " << account_id;

    // Use optimized single-query approach that fetches roles with permissions
    return account_role_repo_.read_roles_with_permissions(account_id);
}

// ============================================================================
// Permission Checking
// ============================================================================

std::vector<std::string>
authorization_service::get_effective_permissions(
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Computing effective permissions for account: "
                               << account_id;

    // Use optimized single-query approach with JOINs
    auto result = account_role_repo_.read_effective_permissions(account_id);

    BOOST_LOG_SEV(lg(), debug) << "Account " << account_id << " has "
                               << result.size() << " effective permissions.";
    return result;
}

bool authorization_service::has_permission(
    const boost::uuids::uuid& account_id,
    const std::string& permission_code) {
    auto permissions = get_effective_permissions(account_id);
    return check_permission(permissions, permission_code);
}

bool authorization_service::check_permission(
    const std::vector<std::string>& permissions,
    const std::string& required_permission) {
    // Precondition: permissions vector must be sorted (guaranteed by
    // get_effective_permissions which uses ORDER BY in the SQL query)

    // Wildcard grants all permissions
    if (std::binary_search(permissions.begin(), permissions.end(),
                           domain::permissions::all)) {
        return true;
    }

    // Check for exact match
    return std::binary_search(permissions.begin(), permissions.end(),
                              required_permission);
}

void authorization_service::publish_permissions_changed(
    const boost::uuids::uuid& account_id) {
    if (!event_bus_) {
        return;
    }

    auto permissions = get_effective_permissions(account_id);

    domain::events::permissions_changed_event event;
    event.account_id = account_id;
    event.permission_codes = std::move(permissions);
    event.timestamp = std::chrono::system_clock::now();

    event_bus_->publish(event);
}

}
