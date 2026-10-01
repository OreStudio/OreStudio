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
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/domain/permission_codes.hpp"
#include "ores.iam.api/eventing/account_permissions_changed_event.hpp"
#include "ores.iam.api/eventing/role_assigned_event.hpp"
#include "ores.iam.api/eventing/role_revoked_event.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <stdexcept>

namespace ores::iam::service {

using namespace ores::logging;
namespace reason = ores::dq::domain::change_reason_constants;

authorization_service::authorization_service(context ctx, event_bus* event_bus)
    : ctx_(ctx)
    , account_role_repo_(ctx)
    , role_permission_repo_(ctx)
    , event_bus_(event_bus) {
    BOOST_LOG_SEV(lg(), info) << "Authorization service initialized.";
}

// ============================================================================
// Permission Management
// ============================================================================

std::vector<domain::permission> authorization_service::list_permissions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all permissions.";
    return permission_repo_.read_latest(ctx_);
}

std::optional<domain::permission>
authorization_service::find_permission_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding permission by code: " << code;
    auto permissions = permission_repo_.read_latest_by_code(ctx_, code);
    if (permissions.empty()) {
        return std::nullopt;
    }
    return permissions.front();
}

domain::permission authorization_service::create_permission(const std::string& code,
                                                            const std::string& description) {
    BOOST_LOG_SEV(lg(), info) << "Creating permission: " << code;

    if (code.empty()) {
        throw std::invalid_argument("Permission code cannot be empty.");
    }

    // Check if permission already exists
    auto existing = find_permission_by_code(code);
    if (existing) {
        throw std::runtime_error("Permission with code '" + code + "' already exists.");
    }

    domain::permission perm;
    perm.id = uuid_generator_();
    perm.code = code;
    perm.description = description;

    permission_repo_.write(ctx_, perm);

    BOOST_LOG_SEV(lg(), info) << "Created permission: " << code << " with ID: " << perm.id;
    return perm;
}

// ============================================================================
// Role Management
// ============================================================================

std::vector<domain::role> authorization_service::list_roles() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all roles.";
    return role_repo_.read_latest(ctx_);
}

std::optional<domain::role> authorization_service::find_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding role by ID: " << role_id;
    auto roles = role_repo_.read_latest(ctx_, boost::lexical_cast<std::string>(role_id));
    if (roles.empty()) {
        return std::nullopt;
    }
    return roles.front();
}

std::optional<domain::role> authorization_service::find_role_by_name(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Finding role by name: " << name;
    auto roles = role_repo_.read_latest_by_name(ctx_, name);
    if (roles.empty()) {
        return std::nullopt;
    }
    return roles.front();
}

domain::role authorization_service::create_role(const std::string& name,
                                                const std::string& description,
                                                const std::vector<std::string>& permission_codes,
                                                const std::string& modified_by) {
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
    role.id = uuid_generator_();
    role.version = 0;
    role.name = name;
    role.description = description;
    role.modified_by = modified_by;

    role_repo_.write(ctx_, role);

    // Create role-permission mappings
    for (const auto& perm : resolved_perms) {
        domain::role_permission rp;
        rp.tenant_id = ctx_.tenant_id();
        rp.role_id = role.id;
        rp.permission_id = perm.id;
        rp.assigned_by = modified_by;
        rp.change_reason_code = std::string{reason::codes::new_record};
        rp.change_commentary = "Role created with its permission bundle";
        role_permission_repo_.write(rp);
    }

    BOOST_LOG_SEV(lg(), info) << "Created role: " << name << " with ID: " << role.id << " and "
                              << permission_codes.size() << " permissions.";
    return role;
}

std::vector<std::string>
authorization_service::get_role_permissions(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting permissions for role: " << role_id;

    auto role_perms = role_permission_repo_.read_latest_by_role(role_id);
    std::vector<std::string> codes;
    codes.reserve(role_perms.size());

    for (const auto& rp : role_perms) {
        auto perms = permission_repo_.read_latest(ctx_, boost::uuids::to_string(rp.permission_id));
        if (!perms.empty()) {
            codes.push_back(perms.front().code);
        }
    }

    /*
     * The order is the answer's, not the store's: the write answers with this
     * same list, and two calls that bundle the same codes must not disagree
     * about how to spell them.
     */
    std::sort(codes.begin(), codes.end());

    return codes;
}

role_permissions authorization_service::replace_role_permissions(
    const boost::uuids::uuid& caller_id,
    const boost::uuids::uuid& role_id,
    const std::vector<std::string>& permission_codes,
    const std::string& change_reason_code,
    const std::string& change_commentary) {
    using ores::utility::domain::outcome;

    role_permissions answer;

    /*
     * The check runs before the role is looked up, so a caller without the
     * permission learns nothing about whether the role exists.
     */
    if (!has_permission(caller_id, domain::permissions::roles_update)) {
        BOOST_LOG_SEV(lg(), warn) << "Bundle write for role " << role_id << " denied: caller "
                                  << caller_id << " lacks "
                                  << domain::permissions::roles_update;
        answer.result.outcome = outcome::denied;
        answer.result.code = domain::permissions::roles_update;
        answer.result.message = std::string("Permission denied: ") +
                                std::string(domain::permissions::roles_update) + " required";
        return answer;
    }

    if (!find_role(role_id)) {
        BOOST_LOG_SEV(lg(), warn) << "Bundle write for a role that does not exist: " << role_id;
        answer.result.outcome = outcome::missing;
        answer.result.code = boost::lexical_cast<std::string>(role_id);
        answer.result.message = "Role not found: " + boost::lexical_cast<std::string>(role_id);
        return answer;
    }

    /*
     * The whole bundle resolves before anything is written, so a code that is
     * not a permission refuses the write rather than landing the part of it
     * that was valid.
     */
    std::vector<domain::permission> desired;
    desired.reserve(permission_codes.size());
    for (const auto& code : permission_codes) {
        auto permission = find_permission_by_code(code);
        if (!permission) {
            BOOST_LOG_SEV(lg(), warn) << "Bundle write names a code that is not a permission: "
                                      << code;
            answer.result.outcome = outcome::invalid;
            answer.result.code = code;
            answer.result.message = "Unknown permission code: " + code;
            return answer;
        }
        desired.push_back(std::move(*permission));
    }

    std::sort(desired.begin(), desired.end(), [](const auto& lhs, const auto& rhs) {
        return lhs.id < rhs.id;
    });
    desired.erase(std::unique(desired.begin(),
                              desired.end(),
                              [](const auto& lhs, const auto& rhs) { return lhs.id == rhs.id; }),
                  desired.end());

    const auto current = role_permission_repo_.read_latest_by_role(role_id);

    const auto bundles_permission = [](const std::vector<domain::role_permission>& links,
                                       const boost::uuids::uuid& permission_id) {
        return std::any_of(links.begin(), links.end(), [&](const auto& link) {
            return link.permission_id == permission_id;
        });
    };

    const auto reason = change_reason_code.empty() ? std::string{reason::codes::new_record}
                                                   : change_reason_code;

    for (const auto& permission : desired) {
        if (bundles_permission(current, permission.id)) {
            continue;
        }
        domain::role_permission link;
        link.tenant_id = ctx_.tenant_id();
        link.role_id = role_id;
        link.permission_id = permission.id;
        link.assigned_by = ctx_.actor();
        link.change_reason_code = reason;
        link.change_commentary = change_commentary;
        role_permission_repo_.write(link);
    }

    for (const auto& link : current) {
        const auto wanted = std::any_of(desired.begin(), desired.end(), [&](const auto& permission) {
            return permission.id == link.permission_id;
        });
        if (!wanted) {
            /*
             * The delete rule closes the row and leaves its tail as the grant
             * wrote it, so the record of who added the permission survives the
             * removal. The same is true of an account's revoked role.
             */
            role_permission_repo_.remove(role_id, link.permission_id);
        }
    }

    answer.permission_codes = get_role_permissions(role_id);

    BOOST_LOG_SEV(lg(), info) << "Role " << role_id << " bundles "
                              << answer.permission_codes.size() << " permission(s).";
    return answer;
}

// ============================================================================
// Role Assignment
// ============================================================================
void authorization_service::assign_role(const boost::uuids::uuid& account_id,
                                        const boost::uuids::uuid& role_id,
                                        const std::string& assigned_by,
                                        const std::string& change_commentary) {
    BOOST_LOG_SEV(lg(), info) << "Assigning role " << role_id << " to account " << account_id;

    // Verify role exists
    auto role = find_role(role_id);
    if (!role) {
        throw std::runtime_error("Role not found: " + boost::lexical_cast<std::string>(role_id));
    }

    // Check if already assigned
    if (account_role_repo_.exists(account_id, role_id)) {
        BOOST_LOG_SEV(lg(), warn) << "Role " << role_id << " already assigned to account "
                                  << account_id;
        return;
    }

    // Create assignment
    domain::account_role ar;
    ar.account_id = account_id;
    ar.role_id = role_id;
    ar.assigned_by = assigned_by;
    ar.change_reason_code = std::string{reason::codes::new_record};
    ar.change_commentary = change_commentary;

    account_role_repo_.write(ar);

    BOOST_LOG_SEV(lg(), info) << "Role " << role->name << " assigned to account " << account_id;

    // Publish events
    if (event_bus_) {
        eventing::role_assigned_event event;
        event.account_id = account_id;
        event.role_id = role_id;
        event.timestamp = std::chrono::system_clock::now();
        event_bus_->publish(event);

        publish_account_permissions_changed(account_id);
    }
}

void authorization_service::revoke_role(const boost::uuids::uuid& account_id,
                                        const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), info) << "Revoking role " << role_id << " from account " << account_id;

    account_role_repo_.remove(account_id, role_id);

    BOOST_LOG_SEV(lg(), info) << "Role " << role_id << " revoked from account " << account_id;

    // Publish events
    if (event_bus_) {
        eventing::role_revoked_event event;
        event.account_id = account_id;
        event.role_id = role_id;
        event.timestamp = std::chrono::system_clock::now();
        event_bus_->publish(event);

        publish_account_permissions_changed(account_id);
    }
}

std::vector<domain::role>
authorization_service::get_account_roles(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting roles for account: " << account_id;

    // Use optimized single-query approach that fetches roles with permissions
    return account_role_repo_.read_roles_with_permissions(account_id);
}

// ============================================================================
// Composed Access Reads
// ============================================================================

std::optional<boost::uuids::uuid> authorization_service::caller_account() const {
    repository::account_repository accounts;

    const auto found = accounts.read_latest_by_username(ctx_, ctx_.actor());
    if (found.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "The actor '" << ctx_.actor()
                                  << "' names no account in tenant " << ctx_.tenant_id().to_string();
        return std::nullopt;
    }
    return found.front().id;
}

account_access authorization_service::read_own_access(const boost::uuids::uuid& account_id) {
    account_access answer;
    answer.roles = compose_account_access(account_id);
    return answer;
}

account_access authorization_service::read_account_access(
    const boost::uuids::uuid& caller_id, const boost::uuids::uuid& account_id) {
    account_access answer;
    if (!has_permission(caller_id, domain::permissions::roles_read)) {
        BOOST_LOG_SEV(lg(), warn) << "Access read for account " << account_id << " denied: caller "
                                  << caller_id << " lacks " << domain::permissions::roles_read;
        answer.result.outcome = ores::utility::domain::outcome::denied;
        answer.result.code = domain::permissions::roles_read;
        answer.result.message = std::string("Permission denied: ") +
                                std::string(domain::permissions::roles_read) + " required";
        return answer;
    }
    answer.roles = compose_account_access(account_id);
    return answer;
}

std::vector<account_access_entry>
authorization_service::compose_account_access(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Composing access for account: " << account_id;

    /*
     * The junction row carries the assignment tail and names the role; the
     * role and its permission codes are read per assignment. The joined shape
     * reaches a caller as one answer, so the round trips a screen would make
     * are made here instead.
     */
    const auto assignments = account_role_repo_.read_latest_by_account(account_id);

    std::vector<account_access_entry> result;
    result.reserve(assignments.size());

    for (const auto& assignment : assignments) {
        auto roles = role_repo_.read_latest(ctx_, boost::uuids::to_string(assignment.role_id));
        if (roles.empty()) {
            /*
             * A junction row whose role cannot be read is a dangling
             * reference, and the answer would carry fewer roles than the
             * account holds. The caller is told why rather than left to
             * notice the difference.
             */
            BOOST_LOG_SEV(lg(), warn)
                << "Account " << account_id << " holds a role that cannot be read: "
                << assignment.role_id;
            continue;
        }
        account_access_entry entry;
        entry.role = std::move(roles.front());
        entry.permission_codes = get_role_permissions(assignment.role_id);
        entry.assigned_by = assignment.assigned_by;
        entry.assigned_at = assignment.assigned_at;
        entry.change_reason_code = assignment.change_reason_code;
        entry.change_commentary = assignment.change_commentary;
        result.push_back(std::move(entry));
    }

    std::sort(result.begin(),
              result.end(),
              [](const account_access_entry& lhs, const account_access_entry& rhs) {
                  return lhs.role.name < rhs.role.name;
              });

    BOOST_LOG_SEV(lg(), debug) << "Account " << account_id << " holds " << result.size()
                               << " role(s).";
    return result;
}

// ============================================================================
// Permission Checking
// ============================================================================

std::vector<std::string>
authorization_service::get_effective_permissions(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Computing effective permissions for account: " << account_id;

    // Use optimized single-query approach with JOINs
    auto result = account_role_repo_.read_effective_permissions(account_id);

    BOOST_LOG_SEV(lg(), debug) << "Account " << account_id << " has " << result.size()
                               << " effective permissions.";
    return result;
}

bool authorization_service::has_permission(const boost::uuids::uuid& account_id,
                                           std::string_view permission_code) {
    auto permissions = get_effective_permissions(account_id);
    return check_permission(permissions, permission_code);
}

bool authorization_service::check_permission(const std::vector<std::string>& permissions,
                                             std::string_view required_permission) {
    // Precondition: permissions vector must be sorted (guaranteed by
    // get_effective_permissions which uses ORDER BY in the SQL query)

    // Wildcard grants all permissions
    if (std::binary_search(permissions.begin(), permissions.end(), domain::permissions::all)) {
        return true;
    }

    // Check for exact match
    return std::binary_search(permissions.begin(), permissions.end(), required_permission);
}

void authorization_service::publish_account_permissions_changed(
    const boost::uuids::uuid& account_id) {
    if (!event_bus_) {
        return;
    }

    auto permissions = get_effective_permissions(account_id);

    eventing::account_permissions_changed_event event;
    event.account_id = account_id;
    event.permission_codes = std::move(permissions);
    event.timestamp = std::chrono::system_clock::now();

    event_bus_->publish(event);
}

}
