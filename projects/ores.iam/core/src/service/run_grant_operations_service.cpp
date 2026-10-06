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
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/run_token_issue_repository.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/run_grant_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/authorization/grants.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <cctype>
#include <chrono>
#include <cstddef>
#include <limits>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::service {

using namespace ores::logging;
using ores::service::messaging::change_reasons::new_record;
using ores::service::messaging::change_reasons::update;
using ores::service::messaging::has_permission;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.iam.service.run_grant_operations_service");
    return instance;
}

constexpr std::string_view revoke_permission = "iam::run_grants:revoke";
constexpr std::string_view exchange_permission = "iam::run_grants:exchange";

/// The role-name suffix that makes an account a service.
constexpr std::string_view service_role_suffix = "Service";

/// The outcome words the issue log records, and the reason a rate-limited
/// exchange carries: it is not one of the five checks, so it is not a refusal
/// value.
constexpr std::string_view issued_outcome = "issued";
constexpr std::string_view refused_outcome = "refused";
constexpr std::string_view rate_limited_reason = "unavailable";

/// How many exchanges one calling service may make per period, and how many
/// one tenant may make. IAM serves thousands of exchanges a minute, so the
/// limits are set to stop one caller exhausting it rather than to ration the
/// fleet; a burst of the same size lets a service that has been quiet catch up.
constexpr int exchanges_per_service = 120;
constexpr int exchanges_per_tenant = 600;
constexpr std::chrono::seconds exchange_limit_period{60};

constexpr run_grant_operations_service::exchange_limits default_exchange_limits{
    .per_service = exchanges_per_service,
    .per_tenant = exchanges_per_tenant,
    .burst = exchanges_per_service,
    .period = exchange_limit_period};

template <typename Response>
Response refuse(std::string message) {
    Response r;
    r.success = false;
    r.message = std::move(message);
    return r;
}

/// A refusal's answer, whose message names the check that produced it.
template <typename Response>
Response refuse(std::string_view detail, exchange_refusal reason) {
    return refuse<Response>("[" + std::string(exchange_refusal_reason(reason)) + "] " +
                            std::string(detail));
}

bool is_active(const domain::run_grant& g) {
    const std::chrono::system_clock::time_point never{};
    return g.revoked_at == never &&
           (g.not_after == never || g.not_after > std::chrono::system_clock::now());
}

bool same_consent(const domain::run_grant& g,
                  const boost::uuids::uuid& role_id,
                  const messaging::create_run_grant_request& request) {
    return g.role_id == role_id && g.audience == request.audience &&
           g.max_runs == request.max_runs && request.valid_seconds == 0 &&
           g.not_after == std::chrono::system_clock::time_point{};
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

/// The lock a grant's run budget is read and spent under.
///
/// Two exchanges for one grant must not both read the same served count and
/// then both issue, so the read and the write are one critical section. The
/// stripes keep unrelated grants from contending. It serializes within one IAM
/// process; a deployment that runs several replicas needs a store-level guard
/// as well.
std::mutex& run_budget_lock(std::string_view grant_key) {
    constexpr std::size_t stripe_count = 64;
    static std::array<std::mutex, stripe_count> stripes;
    return stripes[std::hash<std::string_view>{}(grant_key) % stripe_count];
}

}

run_grant_operations_service::exchange_limiters::exchange_limiters(const exchange_limits& limits)
    : per_service(limits.per_service, limits.period, limits.burst)
    , per_tenant(limits.per_tenant, limits.period, limits.burst) {}

const std::shared_ptr<run_grant_operations_service::exchange_limiters>&
run_grant_operations_service::shared_limiters() {
    static const auto instance =
        std::make_shared<exchange_limiters>(default_exchange_limits);
    return instance;
}

run_grant_operations_service::run_grant_operations_service(
    ores::database::context ctx,
    std::optional<security::jwt::jwt_authenticator> signer,
    std::optional<exchange_limits> limits)
    : ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , limiters_(limits ? std::make_shared<exchange_limiters>(*limits) : shared_limiters()) {}

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
    if (existing && is_active(*existing) && same_consent(*existing, role->id, request)) {
        response r;
        r.success = true;
        r.grant_id = boost::uuids::to_string(existing->id);
        r.message = "The grant is already active with this consent.";
        return r;
    }

    domain::run_grant grant;
    if (existing) {
        grant = *existing;
        grant.change_reason_code = std::string(update);
        grant.change_commentary =
            is_active(*existing) ? "Replaced by a new consent." : "Re-activated by a new consent.";
    } else {
        grant.id = boost::uuids::random_generator()();
        grant.change_reason_code = std::string(new_record);
    }
    grant.resource = request.resource;
    grant.grantor_account_id = *grantor;
    grant.role_id = role->id;
    grant.audience = request.audience;
    grant.max_runs = request.max_runs;
    grant.not_after = request.valid_seconds > 0 ? std::chrono::system_clock::now() +
                                                      std::chrono::seconds(request.valid_seconds) :
                                                  std::chrono::system_clock::time_point{};
    grant.revoked_at = {};
    grant.revoked_by.clear();
    grant.revoke_reason.clear();
    try {
        svc.save_grant(grant);
    } catch (const std::exception& e) {
        // A concurrent create for the same party, resource and grantor won the
        // unique index on the natural key; its grant is the answer.
        if (existing)
            throw;
        auto winner = grant_for(svc, *grantor, *party, request.resource);
        if (!winner)
            throw;
        BOOST_LOG_SEV(lg(), info) << "Run grant for " << request.resource
                                  << " was created concurrently: " << e.what();
        response r;
        r.success = true;
        r.grant_id = boost::uuids::to_string(winner->id);
        r.message = "The grant was created by a concurrent request.";
        return r;
    }

    BOOST_LOG_SEV(lg(), info) << "Run grant " << grant.id << " for " << grant.resource
                              << " granted by " << ctx_.actor() << ": "
                              << (existing ? grant.change_commentary : "new");
    response r;
    r.success = true;
    r.created = true;
    r.grant_id = boost::uuids::to_string(grant.id);
    if (existing)
        r.message = grant.change_commentary;
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

    // has_permission() passes a context that carries no permission list: IAM's
    // own base context, for internal callers. Create refuses such a context,
    // because a grant needs a person's permissions; a revoke only removes one.
    if (!has_permission(ctx_, revoke_permission) &&
        session_account(ctx_) != grant->grantor_account_id)
        return refuse<response>("Only the grantor, or a holder of " +
                                std::string(revoke_permission) + ", may revoke a run grant.");

    response r;
    r.success = true;
    if (grant->revoked_at != std::chrono::system_clock::time_point{}) {
        r.message = "The grant was already revoked.";
        return r;
    }

    grant->revoked_at = std::chrono::system_clock::now();
    grant->revoked_by = ctx_.actor().empty() ? ctx_.service_account() : ctx_.actor();
    grant->revoke_reason = request.reason.empty() ? "revoked" : request.reason;
    grant->change_reason_code = std::string(update);
    grant->change_commentary = "Revoked: " + grant->revoke_reason;
    // A save stamps the session's party on the row, so an administrator acting
    // for another party saves through a context narrowed to the grant's party,
    // keeping their identity and permissions, or the revoke would move it.
    run_grant_service(
        ctx_.with_party(ctx_.tenant_id(), grant->party_id, ctx_.visible_party_ids(), ctx_.actor()))
        .save_grant(*grant);

    BOOST_LOG_SEV(lg(), info) << "Run grant " << grant->id << " revoked by " << grant->revoked_by
                              << ": " << grant->revoke_reason;
    return r;
}

std::string_view exchange_refusal_reason(exchange_refusal refusal) {
    switch (refusal) {
        case exchange_refusal::not_permitted:
            return "not_permitted";
        case exchange_refusal::no_service_identity:
            return "no_service_identity";
        case exchange_refusal::outside_audience:
            return "outside_audience";
        case exchange_refusal::grant_unknown:
            return "grant_unknown";
        case exchange_refusal::grant_revoked:
            return "grant_revoked";
        case exchange_refusal::grant_expired:
            return "grant_expired";
        case exchange_refusal::grant_exhausted:
            return "grant_exhausted";
        case exchange_refusal::tenant_unreadable:
            return "tenant_unreadable";
        case exchange_refusal::tenant_mismatch:
            return "tenant_mismatch";
        case exchange_refusal::grantor_inactive:
            return "grantor_inactive";
        case exchange_refusal::grant_lapsed:
            return "grant_lapsed";
    }
    return "refused";
}

std::optional<std::string> service_name_of(std::span<const std::string> role_names) {
    for (const auto& name : role_names)
        if (name.size() > service_role_suffix.size() && name.ends_with(service_role_suffix))
            return name;
    return std::nullopt;
}

bool audience_admits(std::string_view audience, std::string_view service_name) {
    if (service_name.empty())
        return false;
    std::size_t start = 0;
    while (start <= audience.size()) {
        const auto comma = audience.find(',', start);
        const auto end = comma == std::string_view::npos ? audience.size() : comma;
        auto token = audience.substr(start, end - start);
        while (!token.empty() && std::isspace(static_cast<unsigned char>(token.front())))
            token.remove_prefix(1);
        while (!token.empty() && std::isspace(static_cast<unsigned char>(token.back())))
            token.remove_suffix(1);
        if (token == service_name)
            return true;
        if (comma == std::string_view::npos)
            break;
        start = comma + 1;
    }
    return false;
}

exchange_decision decide_exchange(const exchange_facts& facts,
                                  std::chrono::system_clock::time_point now) {
    const auto refuse_with = [](exchange_refusal reason) {
        return exchange_decision{.refusal = reason, .permissions = {}};
    };

    // 1. The caller holds the exchange permission.
    if (!facts.holds_exchange_permission)
        return refuse_with(exchange_refusal::not_permitted);

    // 2. The caller's service name is in the grant's audience. The audience
    // lives on the grant, so a request that names no grant cannot be placed in
    // one; that refusal is check 3's, and it is the only way round.
    if (facts.service_name.empty())
        return refuse_with(exchange_refusal::no_service_identity);
    if (!facts.grant)
        return refuse_with(exchange_refusal::grant_unknown);
    if (!audience_admits(facts.grant->audience, facts.service_name))
        return refuse_with(exchange_refusal::outside_audience);

    // 3. The grant is live: not revoked, inside its time limit, and inside its
    // run budget. A standing grant states no limit of either kind.
    const auto& grant = *facts.grant;
    const std::chrono::system_clock::time_point never{};
    if (grant.revoked_at != never)
        return refuse_with(exchange_refusal::grant_revoked);
    if (grant.not_after != never && grant.not_after <= now)
        return refuse_with(exchange_refusal::grant_expired);
    if (grant.max_runs > 0 &&
        facts.runs_served >= static_cast<std::size_t>(grant.max_runs) &&
        !facts.run_already_served)
        return refuse_with(exchange_refusal::grant_exhausted);

    // 4. The grant acts in the tenant the request names.
    const auto target = utility::uuid::tenant_id::from_string(facts.request_tenant_id);
    if (!target)
        return refuse_with(exchange_refusal::tenant_unreadable);
    if (grant.tenant_id.to_string() != target->to_string())
        return refuse_with(exchange_refusal::tenant_mismatch);

    // 5. The grantor is active.
    if (facts.grantor_status != "active")
        return refuse_with(exchange_refusal::grantor_inactive);

    // The permissions are the role's, narrowed to what the grantor holds now:
    // a grantor who loses a permission loses it for their runs too, and one
    // who loses all of them has nothing left to lend.
    std::vector<std::string> permissions;
    for (const auto& code : facts.role_permissions)
        if (ores::security::authorization::grants(facts.grantor_permissions, code))
            permissions.push_back(code);
    std::ranges::sort(permissions);
    permissions.erase(std::unique(permissions.begin(), permissions.end()), permissions.end());
    if (permissions.empty())
        return refuse_with(exchange_refusal::grant_lapsed);

    exchange_decision decision;
    decision.permissions = std::move(permissions);
    return decision;
}

security::jwt::jwt_claims run_token_claims(const domain::run_grant& grant,
                                           std::string_view grantor_username,
                                           std::string_view service_name,
                                           std::string_view run_id,
                                           std::span<const std::string> permissions,
                                           std::chrono::system_clock::time_point now) {
    security::jwt::jwt_claims claims;
    claims.subject = boost::uuids::to_string(grant.grantor_account_id);
    claims.username = std::string(grantor_username);
    claims.tenant_id = grant.tenant_id.to_string();
    const auto party = boost::uuids::to_string(grant.party_id);
    claims.party_id = party;
    claims.visible_party_ids = {party};
    claims.roles.assign(permissions.begin(), permissions.end());
    claims.act = std::string(service_name);
    claims.audience = std::string(service_name);
    claims.grant_id = boost::uuids::to_string(grant.id);
    claims.run_id = std::string(run_id);
    // No session: a run token opens none, so the sessions table does not grow
    // with the number of runs.
    claims.issued_at = now;
    claims.expires_at = now + run_token_lifetime;
    return claims;
}

void run_grant_operations_service::log_refusal(
    const std::string& tenant_id,
    const messaging::exchange_run_grant_request& request,
    const std::string& service_name,
    const std::string& grantor_account_id,
    std::string_view reason) {
    try {
        repository::run_token_issue_repository(ctx_).record(std::chrono::system_clock::now(),
                                                            tenant_id,
                                                            "",
                                                            request.grant_id,
                                                            request.run_id,
                                                            service_name,
                                                            grantor_account_id,
                                                            std::string(refused_outcome),
                                                            std::string(reason));
    } catch (const std::exception& e) {
        // The refusal is the answer; a log that cannot be written must not
        // replace it with a failure.
        BOOST_LOG_SEV(lg(), warn) << "Could not record the refusal of run " << request.run_id
                                  << " for grant " << request.grant_id << ": " << e.what();
    }
}

messaging::exchange_run_grant_response
run_grant_operations_service::issue(const domain::run_grant& grant,
                                    const std::string& grantor_username,
                                    const std::string& service_name,
                                    const std::string& run_id,
                                    std::vector<std::string> permissions) {
    using response = messaging::exchange_run_grant_response;

    if (!signer_)
        return refuse<response>("This deployment cannot issue run tokens.");
    const auto now = std::chrono::system_clock::now();
    const auto claims =
        run_token_claims(grant, grantor_username, service_name, run_id, permissions, now);
    const auto token = signer_->create_token(claims);
    if (!token)
        return refuse<response>("The run token could not be signed.");

    // The row is appended before the token is handed over, so no run token
    // exists that the issue log does not name.
    repository::run_token_issue_repository(ctx_).record(now,
                                                        grant.tenant_id.to_string(),
                                                        boost::uuids::to_string(grant.party_id),
                                                        boost::uuids::to_string(grant.id),
                                                        run_id,
                                                        service_name,
                                                        boost::uuids::to_string(
                                                            grant.grantor_account_id),
                                                        std::string(issued_outcome),
                                                        "");

    BOOST_LOG_SEV(lg(), info) << "Issued a run token for grant " << grant.id << " run " << run_id
                              << " to " << service_name << " as "
                              << boost::uuids::to_string(grant.grantor_account_id);

    response r;
    r.success = true;
    r.token = *token;
    r.expires_at = std::chrono::duration_cast<std::chrono::seconds>(
                       claims.expires_at.time_since_epoch())
                       .count();
    return r;
}

messaging::exchange_run_grant_response run_grant_operations_service::exchange_run_grant(
    const messaging::exchange_run_grant_request& request) {
    using response = messaging::exchange_run_grant_response;

    // A deployment that holds no signer cannot issue a run token, and says so
    // before it reads anything: there is nothing to exchange for.
    if (!signer_)
        return refuse<response>("This deployment cannot issue run tokens.");

    // 1. The caller holds iam::run_grants:exchange. Read from the token's
    // permission list before anything else, so a caller without it learns
    // nothing about any grant.
    const bool permitted = has_permission(ctx_, exchange_permission);
    if (!permitted)
        return refuse<response>("The caller needs " + std::string(exchange_permission) + ".",
                                exchange_refusal::not_permitted);

    // The request's tenant scopes every read the checks make, because a run
    // grant is readable only from inside its tenant. A request that names no
    // tenant id is refused before anything is read, so no read ever falls back
    // to the caller's own tenant and answers with another tenant's grant.
    const auto target = utility::uuid::tenant_id::from_string(request.tenant_id);
    if (!target) {
        log_refusal("", request, "", "", exchange_refusal_reason(exchange_refusal::tenant_unreadable));
        return refuse<response>("The request names no tenant: " + request.tenant_id + ".",
                                exchange_refusal::tenant_unreadable);
    }
    const auto logged_tenant = target->to_string();
    const auto grant_ctx = ctx_.with_tenant(*target, ctx_.actor());

    // The limits are keyed on the validated tenant, so an unparseable string
    // cannot mint a fresh bucket and grow the limiter without bound.
    const auto tenant_limit = limiters_->per_tenant.allow(logged_tenant);
    if (!tenant_limit.allowed) {
        log_refusal(logged_tenant, request, "", "", rate_limited_reason);
        return refuse<response>("[unavailable] Too many exchanges for this tenant; retry after " +
                                std::to_string(tenant_limit.retry_after.count()) + " ms.");
    }

    authorization_service auth(ctx_);
    const auto caller = auth.caller_account();

    // 2. The caller's service name, which is the identity the audience names.
    // Every service role the account holds is a candidate, and the grant's
    // audience picks among them, so an account with two service roles is not
    // refused for the grant that names its other one.
    std::vector<std::string> service_roles;
    if (caller) {
        for (const auto& role : auth.get_account_roles(*caller))
            if (role.name.size() > service_role_suffix.size() &&
                role.name.ends_with(service_role_suffix))
                service_roles.push_back(role.name);
        std::ranges::sort(service_roles);
    }

    boost::uuids::uuid grant_id{};
    bool readable_id = true;
    try {
        grant_id = boost::uuids::string_generator()(request.grant_id);
    } catch (const std::exception&) {
        readable_id = false;
    }

    exchange_facts facts;
    facts.holds_exchange_permission = permitted;
    facts.request_tenant_id = logged_tenant;
    if (readable_id)
        facts.grant = run_grant_service(grant_ctx).find_grant(grant_id);

    std::string service_name = service_roles.empty() ? "" : service_roles.front();
    if (facts.grant) {
        for (const auto& name : service_roles)
            if (audience_admits(facts.grant->audience, name)) {
                service_name = name;
                break;
            }
    }
    facts.service_name = service_name;

    const auto service_limit = limiters_->per_service.allow(service_name);
    if (!service_limit.allowed) {
        log_refusal(logged_tenant, request, service_name, "", rate_limited_reason);
        return refuse<response>("[unavailable] Too many exchanges for " + service_name +
                                "; retry after " + std::to_string(service_limit.retry_after.count()) +
                                " ms.");
    }

    // 3. The two permission lists the last check and the mint read. The role
    // and the grantor live in the grant's tenant, so they are read there, not
    // in the calling service's own tenant.
    std::optional<repository::run_token_issue_repository> issues;
    std::string grant_key;
    if (facts.grant) {
        issues.emplace(grant_ctx);
        grant_key = boost::uuids::to_string(facts.grant->id);
        authorization_service grant_auth(grant_ctx);
        facts.role_permissions = grant_auth.get_role_permissions(facts.grant->role_id);
        facts.grantor_permissions =
            grant_auth.get_effective_permissions(facts.grant->grantor_account_id);
    }

    // 5. The grantor, read in the tenant the grant acts in.
    std::optional<domain::account> grantor;
    if (facts.grant) {
        const auto accounts = repository::account_repository().read_latest(
            grant_ctx, boost::uuids::to_string(facts.grant->grantor_account_id));
        if (!accounts.empty()) {
            grantor = accounts.front();
            facts.grantor_status = grantor->account_status;
        }
    }

    // The max_runs read and the issue write are one critical section per
    // grant, so two concurrent exchanges cannot both spend the last run.
    std::unique_lock<std::mutex> run_budget;
    if (facts.grant) {
        run_budget = std::unique_lock(run_budget_lock(grant_key));
        facts.runs_served = issues->distinct_runs(grant_key);
        facts.run_already_served = issues->exists(grant_key, request.run_id);
    }

    const auto decision = decide_exchange(facts, std::chrono::system_clock::now());
    if (decision.refusal) {
        const auto reason = *decision.refusal;
        log_refusal(logged_tenant,
                    request,
                    service_name,
                    grantor ? boost::uuids::to_string(grantor->id) : "",
                    exchange_refusal_reason(reason));
        BOOST_LOG_SEV(lg(), info) << "Refused a run token for grant " << request.grant_id
                                  << " run " << request.run_id << " to " << service_name << ": "
                                  << exchange_refusal_reason(reason);
        std::string detail = "The run token was refused: " +
                             std::string(exchange_refusal_reason(reason)) + ".";
        if (reason == exchange_refusal::grant_lapsed)
            detail = "The grantor holds none of the role's permissions any more, so the grant has "
                     "lapsed.";
        return refuse<response>(std::move(detail), reason);
    }

    return issue(*facts.grant, grantor->username, service_name, request.run_id, decision.permissions);
}

}
