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
#ifndef ORES_IAM_CLIENT_CLIENT_SERVICE_TOKEN_PROVIDER_HPP
#define ORES_IAM_CLIENT_CLIENT_SERVICE_TOKEN_PROVIDER_HPP

#include "ores.iam.client/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <string>

namespace ores::nats::service {
class client;
}

namespace ores::iam::client {

/**
 * @brief Create a token provider for NATS-authenticated service accounts.
 *
 * The returned callable performs iam.v1.ops.service_login immediately on
 * construction of the internal state, so the first authenticated request
 * does not incur a login round-trip. On each subsequent invocation the
 * provider returns the cached token, refreshing proactively when the token
 * is within @p refresh_margin of expiry (falling back to re-authentication
 * if the refresh fails).
 *
 * @param nats            Connected raw NATS client (must outlive the provider).
 * @param username        Service account name (= database user name).
 * @param password        Service account plaintext database password.
 * @param refresh_margin  Proactively refresh this far before token expiry.
 */
ORES_IAM_CLIENT_EXPORT ores::nats::service::nats_client::token_provider
make_service_token_provider(ores::nats::service::client& nats,
                            std::string username,
                            std::string password,
                            std::chrono::seconds refresh_margin = std::chrono::seconds(120));

/**
 * @brief A token provider that gives a service account a session inside one tenant.
 *
 * A service that serves a tenant's work needs a session scoped to that tenant,
 * because every write names the tenant and the party from the caller's claims.
 * The provider asks =iam.v1.ops.enter_tenant_as_service= with the service's own
 * token, caches the session, and enters again when the session is within
 * @p refresh_margin of expiry or when the caller forces it.
 *
 * The session carries the service's own permissions, so it writes where the
 * service's role writes. Ask for a tenant at a time: a provider is bound to the
 * tenant it was made for.
 *
 * @param nats          Connected raw NATS client (must outlive the provider).
 * @param service_token Supplies the service's own token, as
 *                      @ref make_service_token_provider does.
 * @param tenant_id     The tenant to serve.
 * @param refresh_margin Enter again this far before the session expires.
 */
ORES_IAM_CLIENT_EXPORT ores::nats::service::nats_client::token_provider
make_tenant_session_provider(ores::nats::service::client& nats,
                             ores::nats::service::nats_client::token_provider service_token,
                             std::string tenant_id,
                             std::chrono::seconds refresh_margin = std::chrono::seconds(60));

} // namespace ores::iam::client

#endif
