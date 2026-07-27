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
#ifndef ORES_IAM_SERVICE_INTERNAL_IMPERSONATION_SERVICE_HPP
#define ORES_IAM_SERVICE_INTERNAL_IMPERSONATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <functional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Mints short-lived JWTs for server-side actor impersonation.
 *
 * Trusted server-side code occasionally needs to drive the *real* NATS
 * request/handler pipeline as a specific account/party -- not by calling
 * service classes in-process (which bypasses the same authorization,
 * validation, and audit-trail code a real client's request goes through),
 * and not by re-authenticating with a password (this server never has one
 * for an arbitrary account -- passwords are hashed, by design, even for the
 * server's own use). This service mints the same jwt_claims shape the real
 * login path produces (see auth_handler.hpp's login()), including the
 * account's actual effective permissions and visible-party set, without a
 * password check.
 *
 * Deliberately NOT exposed on any NATS subject -- callable only from
 * trusted in-process server code (e.g. an orchestrator impersonating a
 * tenant admin and then each operating-company party in turn to drive
 * their own provisioning). Tokens are minted with a short TTL and meant to
 * be used once and discarded.
 *
 * First consumer: the Acme holding-group provisioning orchestrator (see
 * "Rework Acme provisioning: simulate real per-party setup via internal
 * actor impersonation"). Designed as a named, reusable component for any
 * future multi-actor server-side workflow, not a one-off for Acme.
 */
class ORES_IAM_CORE_EXPORT internal_impersonation_service {
public:
    using context = ores::database::context;

    /**
     * @brief Resolves the visible-party set for a (tenant, party) pair --
     * same shape as service::cache::party_cache::compute_visible_party_ids,
     * injected rather than depending on party_cache directly so this
     * service stays unit-testable without a live NATS connection (the
     * cache's only constructor requires one). Production wiring passes a
     * lambda closing over the shared party_cache instance.
     */
    using visible_party_ids_fn =
        std::function<std::vector<boost::uuids::uuid>(const std::string&, const boost::uuids::uuid&)>;

    /**
     * @param signer            Same JWT signer the service's real auth handlers
     *                          use -- tokens minted here must validate
     *                          identically to a real login's token downstream.
     * @param visible_party_ids Resolves the impersonated party's visible-party
     *                          set, exactly as login does (see party_cache).
     */
    internal_impersonation_service(ores::security::jwt::jwt_authenticator signer,
                                   visible_party_ids_fn visible_party_ids);

    /**
     * @brief Mints a token for @p account_id acting as @p party_id in @p tenant_id.
     *
     * @param ctx        Database context used to compute effective permissions;
     *                   any tenant/party scoping already on it is ignored --
     *                   permissions are looked up for @p account_id directly.
     * @param tenant_id  Tenant the account belongs to (UUID string).
     * @param account_id Account being impersonated.
     * @param party_id   Party the impersonated session is scoped to.
     * @param username   Username claim (for modified_by/audit trails downstream).
     * @param ttl        Token lifetime. Keep short -- this is a use-once,
     *                   discard-immediately token, not a session credential.
     *
     * @return The signed JWT, or an empty string if signing fails.
     */
    [[nodiscard]] std::string mint_token(const context& ctx,
                                         const std::string& tenant_id,
                                         const boost::uuids::uuid& account_id,
                                         const boost::uuids::uuid& party_id,
                                         const std::string& username,
                                         std::chrono::seconds ttl = std::chrono::seconds{60});

private:
    ores::security::jwt::jwt_authenticator signer_;
    visible_party_ids_fn visible_party_ids_;
};

}

#endif
