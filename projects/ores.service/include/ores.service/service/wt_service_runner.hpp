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
#ifndef ORES_SERVICE_SERVICE_WT_SERVICE_RUNNER_HPP
#define ORES_SERVICE_SERVICE_WT_SERVICE_RUNNER_HPP

#include <thread>
#include <functional>
#include <string_view>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/use_future.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/jwks.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::service::service {

/**
 * @brief Runs the service lifecycle for Wt-hosted services.
 *
 * Wt's WServer::waitForShutdown() is a blocking call that owns signal
 * handling, so the standard coroutine-based domain_service_runner cannot be
 * used directly.  This runner wraps Wt's blocking lifecycle symmetrically
 * around the NATS infrastructure:
 *
 *  1. Fetches the JWKS RS256 public key from IAM (runs io_context to
 *     completion for this phase; retries with exponential backoff).
 *  2. Creates an RS256 JWT verifier.
 *  3. Calls @p register_fn(nats, ctx, verifier) to register NATS subscriptions.
 *  4. Calls @p on_started(io_ctx) if provided (e.g. to co_spawn heartbeat).
 *  5. Restarts the io_context and runs it on a background thread so that NATS
 *     callbacks and the heartbeat timer are serviced throughout the Wt lifetime.
 *  6. Calls @p wt_setup_fn() — this is expected to start the Wt WServer and
 *     block until WServer::waitForShutdown() returns.
 *  7. Stops the background io_context thread.
 *  8. Drains NATS and logs "Shutdown complete: <name>".
 *
 * @param nats        Connected NATS client (connect() already called).
 * @param ctx         Database context.
 * @param name        Service name used in log messages.
 * @param register_fn Callable: (client&, context, optional<jwt_authenticator>)
 * @param wt_setup_fn Callable: () -> void; starts and blocks on Wt server.
 * @param on_started  Optional: called after registration, before wt_setup_fn.
 *                    Receives the io_context for co_spawning coroutines.
 */
template<typename RegisterFn, typename WtSetupFn>
void run_wt(ores::nats::service::client& nats,
            ores::database::context ctx,
            std::string_view name,
            RegisterFn&& register_fn,
            WtSetupFn&& wt_setup_fn,
            std::function<void(boost::asio::io_context&)> on_started = {}) {

    using namespace ores::logging;
    static const std::string_view logger_name = "ores.service.service.wt_runner";
    static auto& lg = []() -> auto& {
        static auto instance = make_logger(logger_name);
        return instance;
    }();

    boost::asio::io_context io_ctx;

    // Phase 1: fetch JWKS synchronously by running the io_context to
    // completion.  The future resolves when fetch_jwks_public_key returns.
    BOOST_LOG_SEV(lg, info) << "Fetching JWKS public key from IAM...";
    auto jwks_future = boost::asio::co_spawn(
        io_ctx,
        ores::nats::service::fetch_jwks_public_key(nats),
        boost::asio::use_future);
    io_ctx.run();

    const std::string pub_key = jwks_future.get();
    BOOST_LOG_SEV(lg, info) << "Fetched JWKS public key from IAM";

    std::optional<ores::security::jwt::jwt_authenticator> verifier =
        ores::security::jwt::jwt_authenticator::create_rs256_verifier(pub_key);

    auto subs = register_fn(nats, std::move(ctx), std::move(verifier));
    BOOST_LOG_SEV(lg, info) << "Registered " << subs.size() << " subscription(s).";
    for (const auto& sub : subs)
        BOOST_LOG_SEV(lg, info) << "NATS subscribe: " << sub.subject();

    // Phase 2: restart the io_context and run it on a background thread so
    // that the heartbeat and any NATS coroutines are serviced while Wt blocks
    // the main thread.
    io_ctx.restart();

    if (on_started)
        on_started(io_ctx);

    // Keep the io_context alive even when it has no pending timers.
    auto work_guard = boost::asio::make_work_guard(io_ctx);
    std::thread io_thread([&io_ctx]() { io_ctx.run(); });

    BOOST_LOG_SEV(lg, info) << "Service ready.";

    // Phase 3: hand control to Wt; blocks until WServer::waitForShutdown().
    wt_setup_fn();

    // Phase 4: Wt has shut down — stop the background NATS/heartbeat thread.
    work_guard.reset();
    io_ctx.stop();
    io_thread.join();

    BOOST_LOG_SEV(lg, info) << "Shutdown signal received. Draining...";
    nats.drain();
    BOOST_LOG_SEV(lg, info) << "Shutdown complete: " << name;
}

} // namespace ores::service::service

#endif
