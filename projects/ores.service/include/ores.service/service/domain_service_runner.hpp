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
#ifndef ORES_SERVICE_SERVICE_DOMAIN_SERVICE_RUNNER_HPP
#define ORES_SERVICE_SERVICE_DOMAIN_SERVICE_RUNNER_HPP

#include <functional>
#include <string_view>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.nats/service/client.hpp"

namespace ores::service::service {

/**
 * @brief Runs the standard domain service lifecycle.
 *
 * Encapsulates the boilerplate common to every NATS domain service:
 *
 *  1. Installs SIGINT/SIGTERM handler early so stop works at any startup phase.
 *  2. Fetches the JWKS RS256 public key from IAM with exponential backoff.
 *  3. Creates an RS256 JWT verifier from the key.
 *  4. Calls @p register_fn(nats, ctx, verifier) to register subscriptions.
 *  5. Waits for a shutdown signal.
 *  6. Drains NATS and logs "Shutdown complete: <service_name>".
 *
 * If a signal arrives while fetching the JWKS key the retry loop is cancelled
 * immediately and the service exits cleanly (no drain needed — no subscriptions
 * are registered yet).
 *
 * @param io_ctx    The service's io_context.
 * @param nats      Connected NATS client (connect() already called).
 * @param ctx       Database context.
 * @param name      Service name used in log messages (e.g. "ores.assets.service").
 * @param register_fn Callable with signature
 *                  @code
 *                  auto register_fn(
 *                      ores::nats::service::client& nats,
 *                      ores::database::context ctx,
 *                      std::optional<ores::security::jwt::jwt_authenticator> verifier);
 *                  @endcode
 *                  The return value is ignored (subscriptions are kept alive via
 *                  the returned vector held inside this coroutine).
 * @param on_started Optional callback invoked after subscriptions are registered
 *                   and before the service starts waiting for shutdown. Called
 *                   with the io_context so the caller can co_spawn additional
 *                   coroutines (e.g. background pollers).
 */
template<typename RegisterFn>
boost::asio::awaitable<void>
run(boost::asio::io_context& io_ctx,
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::string_view name,
    RegisterFn&& register_fn,
    std::function<void(boost::asio::io_context&)> on_started = {});

} // namespace ores::service::service

#include "ores.service/service/domain_service_runner_impl.hpp"

#endif
