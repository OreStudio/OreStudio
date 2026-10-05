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
#include "ores.iam.core/service/run_grant_operations_service.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/run_grant_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/authorization/grants.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <limits>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

using namespace ores::logging;
using ores::service::messaging::change_reasons::new_record;
using ores::service::messaging::change_reasons::update;
using ores::service::messaging::has_permission;
using ores::service::messaging::stamp;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.iam.service.run_grant_operations_service");
    return instance;
}

constexpr std::string_view revoke_permission = "iam::run_grants:revoke";

template <typename Response>
Response refuse(std::string message) {
    Response r;
    r.success = false;
    r.message = std::move(message);
    return r;
}

bool is_active(const domain::run_grant& g) {
    return g.revoked_at == std::chrono::system_clock::time_point{};
}

std::optional<boost::uuids::uuid> session_account(const ores::database::context& ctx) {
    if (ctx.actor().empty())
        return std::nullopt;
    repository::account_repository repo;
    const auto accounts = repo.read_latest_by_username(ctx, ctx.actor());
    if (accounts.empty())
        return std::nullopt;
    return accounts.front().id;
}

std::optional<domain::run_grant> grant_for(run_grant_service& svc,
                                           const boost::uuids::uuid& grantor,
                                           const boost::uuids::uuid& party,
                                           const std::string& resource) {
    const auto grants = svc.list_grants_by_grantor_account_id(
        boost::uuids::to_string(grantor), 0, std::numeric_limits<std::uint32_t>::max());
    for (const auto& g : grants)
        if (g.party_id == party && g.resource == resource)
            return g;
    return std::nullopt;
}

}

run_grant_operations_service::run_grant_operations_service(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

messaging::create_run_grant_response
run_grant_operations_service::create_run_grant(const messaging::create_run_grant_request& request) {
    using response = messaging::create_run_grant_response;

    const auto party = ctx_.party_id();
    if (!party || party->is_nil())
        return refuse<response>("A run grant needs a party: act for the party the runs belong to.");
    const auto& held = ctx_.roles();
    if (!held)
        return refuse<response>(
            "A run grant needs the person's permissions: send the request on their behalf.");
    if (request.resource.empty() || request.role.empty() || request.audience.empty())
        return refuse<response>("A run grant needs a resource, a role and an audience.");
    if (request.max_runs < 0 || request.valid_seconds < 0)
        return refuse<response>("max_runs and valid_seconds cannot be negative.");

    const auto grantor = session_account(ctx_);
    if (!grantor)
        return refuse<response>("No account matches the session's user, " + ctx_.actor() + ".");

    authorization_service auth(ctx_);
    const auto role = auth.find_role_by_name(request.role);
    if (!role)
        return refuse<response>("No role is named " + request.role + ".");
    std::vector<std::string> missing;
    for (const auto& code : auth.get_role_permissions(role->id))
        if (!ores::security::authorization::grants(*held, code))
            missing.push_back(code);
    if (!missing.empty()) {
        std::string list;
        for (const auto& code : missing)
            list += (list.empty() ? "" : ", ") + code;
        return refuse<response>("You cannot grant the role " + request.role +
                                ", because you do not hold: " + list + ".");
    }

    run_grant_service svc(ctx_);
    auto existing = grant_for(svc, *grantor, *party, request.resource);
    if (existing && is_active(*existing)) {
        response r;
        r.success = true;
        r.grant_id = boost::uuids::to_string(existing->id);
        r.message = "The grant is already active.";
        return r;
    }

    domain::run_grant grant;
    if (existing) {
        grant = *existing;
        grant.change_reason_code = std::string(update);
        grant.change_commentary = "Re-activated by a new consent.";
    } else {
        grant.id = boost::uuids::random_generator()();
        grant.change_reason_code = std::string(new_record);
    }
    grant.resource = request.resource;
    grant.grantor_account_id = *grantor;
    grant.role_id = role->id;
    grant.audience = request.audience;
    grant.max_runs = request.max_runs;
    grant.not_after = request.valid_seconds > 0
        ? std::chrono::system_clock::now() + std::chrono::seconds(request.valid_seconds)
        : std::chrono::system_clock::time_point{};
    grant.revoked_at = {};
    grant.revoked_by.clear();
    grant.revoke_reason.clear();
    stamp(grant, ctx_, grant.change_reason_code);
    svc.save_grant(grant);

    BOOST_LOG_SEV(lg(), info) << "Run grant " << grant.id << " for " << grant.resource
                              << " granted by " << ctx_.actor()
                              << (existing ? " (re-activated)" : "");
    response r;
    r.success = true;
    r.created = true;
    r.grant_id = boost::uuids::to_string(grant.id);
    return r;
}

messaging::revoke_run_grant_response
run_grant_operations_service::revoke_run_grant(const messaging::revoke_run_grant_request& request) {
    using response = messaging::revoke_run_grant_response;

    boost::uuids::uuid id;
    try {
        id = boost::uuids::string_generator()(request.grant_id);
    } catch (const std::exception&) {
        return refuse<response>("Not a grant id: " + request.grant_id + ".");
    }

    run_grant_service svc(ctx_);
    auto grant = svc.find_grant(id);
    if (!grant)
        return refuse<response>("No run grant has the id " + request.grant_id + ".");

    if (!has_permission(ctx_, revoke_permission) && session_account(ctx_) != grant->grantor_account_id)
        return refuse<response>("Only the grantor, or a holder of " + std::string(revoke_permission) +
                                ", may revoke a run grant.");

    response r;
    r.success = true;
    if (!is_active(*grant)) {
        r.message = "The grant was already revoked.";
        return r;
    }

    const auto party = grant->party_id;
    grant->revoked_at = std::chrono::system_clock::now();
    grant->revoked_by = ctx_.actor().empty() ? ctx_.service_account() : ctx_.actor();
    grant->revoke_reason = request.reason.empty() ? "revoked" : request.reason;
    grant->change_reason_code = std::string(update);
    grant->change_commentary = "Revoked: " + grant->revoke_reason;
    stamp(*grant, ctx_, update);
    // stamp() writes the session's party; a revoke by an administrator acting
    // for another party must not move the grant.
    grant->party_id = party;
    svc.save_grant(*grant);

    BOOST_LOG_SEV(lg(), info) << "Run grant " << grant->id << " revoked by " << grant->revoked_by
                              << ": " << grant->revoke_reason;
    return r;
}

}
