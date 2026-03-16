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
#ifndef ORES_NATS_SERVICE_JWKS_HPP
#define ORES_NATS_SERVICE_JWKS_HPP

#include <string>
#include <chrono>
#include <boost/asio/awaitable.hpp>
#include "ores.nats/service/client.hpp"

namespace ores::nats::service {

/**
 * @brief Fetches the RS256 public key from the IAM JWKS endpoint via NATS.
 *
 * Sends a request to <prefix>.iam.v1.auth.jwks and parses the JSON response.
 * Retries with exponential backoff (1s → 2s → 4s … capped at 30s) if IAM is
 * unavailable or returns an error. The coroutine will not return until a valid
 * non-empty public key has been obtained.
 *
 * IAM returns an error (not an empty key) when no RSA private key is
 * configured — the caller will simply keep retrying until the key is set up.
 *
 * @param nats Connected NATS client (subject prefix already applied).
 * @param per_request_timeout Timeout for each individual NATS request.
 * @return PEM-encoded RSA public key string (never empty).
 */
boost::asio::awaitable<std::string>
fetch_jwks_public_key(client& nats,
    std::chrono::seconds per_request_timeout = std::chrono::seconds(10));

} // namespace ores::nats::service

#endif
