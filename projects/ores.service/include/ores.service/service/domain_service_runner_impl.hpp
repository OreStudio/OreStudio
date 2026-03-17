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
#ifndef ORES_SERVICE_SERVICE_DOMAIN_SERVICE_RUNNER_IMPL_HPP
#define ORES_SERVICE_SERVICE_DOMAIN_SERVICE_RUNNER_IMPL_HPP

#include <csignal>
#include <functional>
#include <optional>
#include <boost/asio/bind_cancellation_slot.hpp>
#include <boost/asio/cancellation_signal.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/error.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/error_code.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/jwks.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::service::service {

template<typename RegisterFn>
boost::asio::awaitable<void>
run(boost::asio::io_context& io_ctx,
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::string_view name,
    RegisterFn&& register_fn,
    std::function<void(boost::asio::io_context&)> on_started) {

    using namespace ores::logging;
    static const std::string_view logger_name = "ores.service.service.runner";
    static auto& lg = []() -> auto& {
        static auto instance = make_logger(logger_name);
        return instance;
    }();

    // During the JWKS startup phase, use a cancellation_signal rather than
    // io_ctx.stop() so that the coroutine can unwind cleanly (log, co_return)
    // before the io_context exits. Calling io_ctx.stop() here would cause
    // io_ctx.run() in main() to return before the catch block executes.
    boost::asio::cancellation_signal startup_cancel;
    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    signals.async_wait([&startup_cancel](const boost::system::error_code& ec, int) {
        if (!ec) startup_cancel.emit(boost::asio::cancellation_type::all);
    });

    // co_spawn with bind_cancellation_slot propagates the slot to the spawned
    // coroutine: when startup_cancel.emit() fires, the coroutine is cancelled
    // at its next async point (the backoff timer) with operation_aborted.
    std::string pub_key;
    try {
        pub_key = co_await boost::asio::co_spawn(
            io_ctx,
            ores::nats::service::fetch_jwks_public_key(nats),
            boost::asio::bind_cancellation_slot(
                startup_cancel.slot(), boost::asio::use_awaitable));
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted)
            throw;
        BOOST_LOG_SEV(lg, info) << "Shutdown signal received during startup.";
        BOOST_LOG_SEV(lg, info) << "Shutdown complete: " << name;
        co_return;
    }
    BOOST_LOG_SEV(lg, info) << "Fetched JWKS public key from IAM";

    // Dismiss the startup handler before re-arming for the operational phase.
    // signals.cancel() posts the handler with operation_aborted (ignored by !ec).
    // The subsequent async_wait(use_awaitable) then waits for the real signal.
    signals.cancel();

    std::optional<ores::security::jwt::jwt_authenticator> verifier =
        ores::security::jwt::jwt_authenticator::create_rs256_verifier(pub_key);

    auto subs = register_fn(nats, std::move(ctx), std::move(verifier));
    BOOST_LOG_SEV(lg, info) << "Registered " << subs.size() << " subscription(s).";
    for (const auto& sub : subs)
        BOOST_LOG_SEV(lg, info) << "NATS subscribe: " << sub.subject();

    if (on_started)
        on_started(io_ctx);

    BOOST_LOG_SEV(lg, info) << "Service ready. Waiting for requests...";
    co_await signals.async_wait(boost::asio::use_awaitable);

    BOOST_LOG_SEV(lg, info) << "Shutdown signal received. Draining...";
    nats.drain();
    BOOST_LOG_SEV(lg, info) << "Shutdown complete: " << name;
    co_return;
}

} // namespace ores::service::service

#endif
