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
#include "ores.iam.core/service/tenant_session_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.core/repository/auth_event_repository.hpp"
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <exception>

namespace ores::iam::service {

using namespace ores::logging;

namespace {

inline auto& lg() {
    static auto instance = make_logger("ores.iam.service.tenant_session_service");
    return instance;
}

constexpr std::string_view read_verb = ":read";

messaging::enter_tenant_response refused_entry(tenant_session_refusal refusal) {
    return {.success = false, .message = std::string(describe(refusal))};
}

}

std::string_view describe(tenant_session_refusal refusal) {
    switch (refusal) {
        case tenant_session_refusal::outside_system_administration:
            return "A tenant is entered from system administration.";
        case tenant_session_refusal::already_inside_a_tenant:
            return "The session is already inside a tenant; leave it first.";
        case tenant_session_refusal::not_permitted:
            return "Entering a tenant needs the permission iam::tenants:impersonate.";
        case tenant_session_refusal::unreadable_tenant_id:
            return "The tenant id could not be read.";
        case tenant_session_refusal::system_tenant:
            return "The system tenant is not entered; it is the deployment's own.";
        case tenant_session_refusal::unknown_tenant:
            return "No tenant has this id.";
        case tenant_session_refusal::no_system_party:
            return "The tenant has no system party yet, so there is nothing to act as.";
        case tenant_session_refusal::no_read_permissions:
            return "The caller holds no read permission to carry into the tenant.";
        case tenant_session_refusal::not_inside_a_tenant:
            return "The session is not inside a tenant.";
    }
    return "The request was refused.";
}

tenant_session_service::tenant_session_service(context ctx,
                                               security::jwt::jwt_authenticator signer,
                                               std::chrono::seconds lifetime)
    : ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , lifetime_(lifetime) {}

std::vector<std::string>
tenant_session_service::read_only(const std::vector<std::string>& granted,
                                  const std::vector<std::string>& catalogue) {
    std::vector<std::string> reads;
    for (const auto& code : catalogue) {
        if (code.ends_with(read_verb) && authorization_service::check_permission(granted, code))
            reads.push_back(code);
    }
    std::ranges::sort(reads);
    return reads;
}

messaging::enter_tenant_response
tenant_session_service::enter(const tenant_session_caller& caller,
                              const messaging::enter_tenant_request& request) {
    if (!caller.tenant_id.is_system())
        return refused_entry(tenant_session_refusal::outside_system_administration);
    if (caller.acting_from_tenant_id)
        return refused_entry(tenant_session_refusal::already_inside_a_tenant);
    if (!authorization_service::check_permission(caller.permissions, impersonate_permission))
        return refused_entry(tenant_session_refusal::not_permitted);

    const auto target = utility::uuid::tenant_id::from_string(request.tenant_id);
    if (!target)
        return refused_entry(tenant_session_refusal::unreadable_tenant_id);
    if (target->is_system())
        return refused_entry(tenant_session_refusal::system_tenant);

    const auto system_ctx = database::service::tenant_context::with_system_tenant(ctx_);
    const auto tenants = repository::read_active_tenant_by_id(system_ctx, target->to_uuid());
    if (tenants.empty())
        return refused_entry(tenant_session_refusal::unknown_tenant);
    const auto& tenant = tenants.front();

    std::vector<std::string> catalogue;
    for (const auto& permission : authorization_service(system_ctx).list_permissions())
        catalogue.push_back(permission.code);
    auto permissions = read_only(caller.permissions, catalogue);
    if (permissions.empty())
        return refused_entry(tenant_session_refusal::no_read_permissions);

    const auto target_id = target->to_string();
    const auto target_ctx = ctx_.with_tenant(*target, caller.username);
    using ores::database::repository::execute_parameterized_string_query;
    using ores::database::repository::execute_parameterized_multi_column_query;
    const auto system_party = execute_parameterized_multi_column_query(
        target_ctx,
        "SELECT id::text, full_name FROM ores_refdata_read_system_party_fn($1::uuid)",
        {target_id},
        lg(),
        "Reading the system party of the tenant entered");
    if (system_party.empty() || system_party.front().size() < 2 || !system_party.front()[0])
        return refused_entry(tenant_session_refusal::no_system_party);
    const auto party_id = *system_party.front()[0];
    const auto party_name = system_party.front()[1].value_or("");
    const auto visible = execute_parameterized_string_query(
        target_ctx,
        "SELECT unnest(ores_refdata_visible_party_ids_fn($1::uuid, $2::uuid))::text",
        {target_id, party_id},
        lg(),
        "Reading the parties the tenant's system party sees");

    const auto now = std::chrono::system_clock::now();
    security::jwt::jwt_claims claims;
    claims.subject = boost::uuids::to_string(caller.account_id);
    claims.username = caller.username;
    claims.tenant_id = target_id;
    claims.party_id = party_id;
    claims.visible_party_ids = visible;
    claims.roles = std::move(permissions);
    claims.session_id = caller.session_id;
    claims.session_start_time = now;
    claims.acting_from_tenant_id = caller.tenant_id.to_string();
    claims.issued_at = now;
    claims.expires_at = now + lifetime_;
    const auto token = signer_.create_token(claims);
    if (!token) {
        BOOST_LOG_SEV(lg(), error) << "Failed to sign the session for tenant " << target_id;
        return {.success = false, .message = "The session could not be issued."};
    }

    // The entry is recorded once the session exists and before it is handed
    // over, so the trail never names an entry that did not happen and no
    // session leaves without its record.
    repository::auth_event_repository(target_ctx)
        .record_tenant_entered(now,
                               target_id,
                               boost::uuids::to_string(caller.account_id),
                               caller.username,
                               caller.session_id,
                               party_id);

    BOOST_LOG_SEV(lg(), info) << caller.username << " entered tenant " << tenant.code;
    return {.success = true,
            .token = *token,
            .tenant_id = target_id,
            .tenant_code = tenant.code,
            .tenant_name = tenant.name,
            .party_id = party_id,
            .party_name = party_name,
            .access_lifetime_s = static_cast<int>(lifetime_.count())};
}

messaging::leave_tenant_response
tenant_session_service::leave(const tenant_session_caller& caller) {
    const auto from_system = caller.acting_from_tenant_id &&
                             utility::uuid::tenant_id::from_string(*caller.acting_from_tenant_id)
                                 .transform([](const auto& id) { return id.is_system(); })
                                 .value_or(false);
    if (!from_system) {
        return {.success = false,
                .message = std::string(describe(tenant_session_refusal::not_inside_a_tenant))};
    }
    const auto tenant_id = caller.tenant_id.to_string();
    repository::auth_event_repository(ctx_.with_tenant(caller.tenant_id, caller.username))
        .record_tenant_left(std::chrono::system_clock::now(),
                            tenant_id,
                            boost::uuids::to_string(caller.account_id),
                            caller.username,
                            caller.session_id,
                            caller.party_id.value_or(""));
    BOOST_LOG_SEV(lg(), info) << caller.username << " left tenant " << tenant_id;
    return {.success = true};
}

}
