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

#include <chrono>
#include <string>
#include "ores.nats/service/nats_client.hpp"

namespace ores::nats::service { class client; }

namespace ores::iam::client {

/**
 * @brief Create a token provider for NATS-authenticated service accounts.
 *
 * The returned callable performs iam.v1.auth.service-login immediately on
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
ores::nats::service::nats_client::token_provider
make_service_token_provider(
    ores::nats::service::client& nats,
    std::string username,
    std::string password,
    std::chrono::seconds refresh_margin = std::chrono::seconds(120));

} // namespace ores::iam::client

#endif
