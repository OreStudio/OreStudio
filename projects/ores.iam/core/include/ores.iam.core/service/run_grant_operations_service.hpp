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
#ifndef ORES_IAM_CORE_SERVICE_RUN_GRANT_OPERATIONS_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_RUN_GRANT_OPERATIONS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/run_grant.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.service/service/rate_limiter.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::service {

/// How long a run token lasts.
inline constexpr std::chrono::seconds run_token_lifetime{300};

/**
 * @brief Why a run grant exchange was refused.
 *
 * One value per check, plus the sub-cases of check 3. The words a caller reads
 * are @ref exchange_refusal_reason's, so a refusal always names the check that
 * produced it.
 */
enum class exchange_refusal {
    /// 1. The caller does not hold iam::run_grants:exchange.
    not_permitted,
    /// 2. The caller's account holds no service role, so it names no service.
    no_service_identity,
    /// 2. The caller's service name is not in the grant's audience.
    outside_audience,
    /// 3. No run grant has the id the request names.
    grant_unknown,
    /// 3. The grant has been revoked.
    grant_revoked,
    /// 3. The grant is past its not_after.
    grant_expired,
    /// 3. The grant has served its max_runs, and not this run.
    grant_exhausted,
    /// 4. The request's tenant is not a tenant id.
    tenant_unreadable,
    /// 4. The grant belongs to another tenant.
    tenant_mismatch,
    /// 5. The grantor's account is not active.
    grantor_inactive,
    /// The grantor holds none of the role's permissions any more.
    grant_lapsed
};

/**
 * @brief The sentence a caller reads for a refusal. It names the check.
 */
ORES_IAM_CORE_EXPORT std::string_view exchange_refusal_reason(exchange_refusal refusal);

/**
 * @brief Everything the exchange's checks decide on, read from the store.
 *
 * The facts are gathered first and the decision is taken from them alone, so
 * the five checks are one ordered function of values rather than a sequence of
 * queries. A caller that can supply the facts proves the checks without a
 * database.
 */
struct exchange_facts {
    /// 1. Whether the caller's token carries iam::run_grants:exchange.
    bool holds_exchange_permission = false;

    /// 2. The caller's service name: the service role its account holds. Empty
    /// when it holds none.
    std::string service_name;

    /// 3. The grant the request names, or nothing when no grant has that id.
    std::optional<domain::run_grant> grant;

    /// 4. The tenant id the request names, exactly as it arrived.
    std::string request_tenant_id;

    /// 5. The grantor account's status. Empty when no account has its id.
    std::string grantor_status;

    /// 3. How many distinct runs the grant has been served.
    std::size_t runs_served = 0;

    /// 3. Whether the grant was already served for the request's run.
    bool run_already_served = false;

    /// The role's permissions, and the permissions the grantor holds now.
    std::vector<std::string> role_permissions;
    std::vector<std::string> grantor_permissions;
};

/**
 * @brief The outcome of the checks: the refusal, or the permissions to carry.
 */
struct exchange_decision {
    /// Nothing when the exchange may proceed.
    std::optional<exchange_refusal> refusal;

    /// The role's permissions the grantor still holds. Empty on a refusal.
    std::vector<std::string> permissions;
};

/**
 * @brief Makes the five checks, in order, and computes the permissions.
 *
 * 1. The caller holds iam::run_grants:exchange.
 * 2. The caller's service name is in the grant's audience.
 * 3. The grant exists, is not revoked, and is inside not_after and max_runs.
 * 4. The grant's tenant is the request's tenant.
 * 5. The grantor is active.
 *
 * The first check that fails is the refusal, so a caller that fails two checks
 * reads the earlier one. The one departure from the stated order is forced by
 * the data: the audience lives on the grant, so a request that names no grant
 * is refused as @c grant_unknown before the audience is tested.
 *
 * Check 4 is inside this function for a caller that supplies the facts. The
 * service reads the grant through a context scoped to the request's tenant, so
 * the read itself enforces it and @c tenant_mismatch is reachable only here.
 *
 * When none of the five fails, the permissions are the role's intersected with
 * the grantor's, and an empty intersection is @c grant_lapsed: the grantor no
 * longer holds what they consented to give.
 *
 * @param now The instant the grant's not_after is read against.
 */
ORES_IAM_CORE_EXPORT exchange_decision decide_exchange(const exchange_facts& facts,
                                                       std::chrono::system_clock::time_point now);

/**
 * @brief The caller's service name among the roles its account holds.
 *
 * A service's identity is its @c <Component>Service role, which is bound to
 * its account and stable across environments. Nothing when the account holds
 * no such role, so a person's account never names a service.
 */
ORES_IAM_CORE_EXPORT std::optional<std::string>
service_name_of(std::span<const std::string> role_names);

/**
 * @brief Whether a grant's audience names a service.
 *
 * The audience is a comma-separated list of service names; the tokens are
 * compared with their surrounding spaces removed.
 */
ORES_IAM_CORE_EXPORT bool audience_admits(std::string_view audience, std::string_view service_name);

/**
 * @brief The claims a run token carries.
 *
 * The token is the grantor's identity, narrowed: its subject is the grantor's
 * account, its tenant and party are the grant's, and its only visible party is
 * the grant's party, so row-level security returns one party's rows. Its actor
 * and audience are the requesting service, and it names the grant and the run
 * it serves. It opens no session.
 *
 * @param now The issue instant; the expiry is @ref run_token_lifetime after it.
 */
ORES_IAM_CORE_EXPORT security::jwt::jwt_claims
run_token_claims(const domain::run_grant& grant,
                 std::string_view grantor_username,
                 std::string_view service_name,
                 std::string_view run_id,
                 std::span<const std::string> permissions,
                 std::chrono::system_clock::time_point now);

/**
 * @brief Creates and revokes run grants, and exchanges one for a run token.
 *
 * Each write makes a check no row write can express. A create needs a session
 * that acts for a party and carries the person's permissions, and the person
 * must hold every permission of the role. A revoke needs the grantor, or a
 * holder of iam::run_grants:revoke. The exchange needs a step service's own
 * token, a live grant that names that service in its audience, and a grantor
 * who still holds the role. The context is the request's, so the tenant, the
 * party and the person come from the token, never the request.
 */
class ORES_IAM_CORE_EXPORT run_grant_operations_service {
public:
    /**
     * @brief IAM's exchange limits: one per calling service, one per tenant.
     *
     * A limit of nothing lets every request through, which is how a
     * deployment states that it sets none. The burst is the most one key may
     * spend at once.
     */
    struct exchange_limits {
        int per_service = 0;
        int per_tenant = 0;
        int burst = 0;
        std::chrono::seconds period{60};
    };

    /**
     * @param ctx The request's context: the caller and its permissions.
     * @param signer Signs the run token. The exchange refuses without one, so
     * a caller that only creates and revokes grants need not supply it.
     * @param limits The exchange limits; the process-wide ones when absent. A
     * test passes its own to reach a rate-limit refusal.
     */
    explicit run_grant_operations_service(
        ores::database::context ctx,
        std::optional<security::jwt::jwt_authenticator> signer = std::nullopt,
        std::optional<exchange_limits> limits = std::nullopt);

    /**
     * @brief Creates a grant, returns the active one, or re-activates a
     * revoked one for the same party, resource and grantor.
     */
    messaging::create_run_grant_response
    create_run_grant(const messaging::create_run_grant_request& request);

    /**
     * @brief Revokes a grant. Revoking a revoked grant succeeds and changes
     * nothing.
     */
    messaging::revoke_run_grant_response
    revoke_run_grant(const messaging::revoke_run_grant_request& request);

    /**
     * @brief Exchanges a run grant for a run token.
     *
     * The caller is a step service presenting its own Client Credentials
     * token. On success the answer carries a token that lasts
     * @ref run_token_lifetime, and one row is appended to the issue log. A
     * refusal names the check that produced it; a caller that exceeds one of
     * the limits is answered as unavailable, with the wait in the message.
     */
    messaging::exchange_run_grant_response
    exchange_run_grant(const messaging::exchange_run_grant_request& request);

private:
    /**
     * @brief The service's two limiters, held across requests.
     *
     * The handler builds a service per request, so the limiters live beside it
     * rather than in it: one process, one count per key.
     */
    struct exchange_limiters {
        explicit exchange_limiters(const exchange_limits& limits);
        ores::service::service::rate_limiter per_service;
        ores::service::service::rate_limiter per_tenant;
    };

    /// The limiters every service instance shares, built from the constants in
    /// this component's translation unit.
    static const std::shared_ptr<exchange_limiters>& shared_limiters();

    /// Appends a refusal's row. A failure to write it is logged and swallowed:
    /// the caller still reads the refusal the check produced.
    void log_refusal(const std::string& tenant_id,
                     const messaging::exchange_run_grant_request& request,
                     const std::string& service_name,
                     const std::string& grantor_account_id,
                     std::string_view reason);

    /// Signs the run token, appends the issued row, and answers.
    messaging::exchange_run_grant_response issue(const domain::run_grant& grant,
                                                 const std::string& grantor_username,
                                                 const std::string& service_name,
                                                 const std::string& run_id,
                                                 std::vector<std::string> permissions);

    ores::database::context ctx_;
    std::optional<security::jwt::jwt_authenticator> signer_;
    std::shared_ptr<exchange_limiters> limiters_;
};

}

#endif
