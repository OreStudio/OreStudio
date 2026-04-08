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
#ifndef ORES_SERVICE_SERVICE_SIGNING_SERVICE_RUNNER_HPP
#define ORES_SERVICE_SERVICE_SIGNING_SERVICE_RUNNER_HPP

#include <string>
#include <functional>
#include <string_view>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.nats/service/client.hpp"

namespace ores::service::service {

/**
 * @brief Runs the signing service lifecycle (IAM only).
 *
 * Identical to domain_service_runner::run() except that step 2 (JWKS fetch)
 * is replaced by constructing an RS256 signer from the service's own private
 * key.  This is the correct pattern for IAM, which cannot fetch a public key
 * from itself.
 *
 * Lifecycle:
 *  1. Installs SIGINT/SIGTERM/SIGQUIT handler early.
 *  2. Creates an RS256 JWT signer from @p jwt_private_key.
 *  3. Calls @p register_fn(nats, ctx, signer) to register subscriptions.
 *  4. Calls @p on_started(io_ctx) if provided.
 *  5. Waits for a shutdown signal.
 *  6. Calls @p on_shutdown() if provided.
 *  7. Drains NATS and logs "Shutdown complete: <name>".
 *
 * @param io_ctx          The service's io_context.
 * @param nats            Connected NATS client (connect() already called).
 * @param ctx             Database context.
 * @param name            Service name used in log messages.
 * @param jwt_private_key PEM-encoded RSA private key for JWT signing.
 * @param register_fn     Callable: (client&, context, jwt_authenticator signer)
 * @param on_started      Optional: called after registration, before signal wait.
 * @param on_shutdown     Optional: called after signal, before nats.drain().
 */
template<typename RegisterFn>
boost::asio::awaitable<void>
run_signing(boost::asio::io_context& io_ctx,
            ores::nats::service::client& nats,
            ores::database::context ctx,
            std::string_view name,
            const std::string& jwt_private_key,
            RegisterFn&& register_fn,
            std::function<void(boost::asio::io_context&)> on_started  = {},
            std::function<void()>                          on_shutdown = {});

} // namespace ores::service::service

#include "ores.service/service/signing_service_runner_impl.hpp"

#endif
