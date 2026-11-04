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

#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/service/service_lifecycle.hpp"
#include <boost/asio/io_context.hpp>
#include <boost/asio/signal_set.hpp>
#include <functional>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::service::service {

namespace detail {

using subscriptions = std::vector<ores::nats::service::subscription>;
using verifier_type = std::optional<ores::security::jwt::jwt_authenticator>;
using register_with_context = std::function<subscriptions(
    ores::nats::service::client&, ores::database::context, verifier_type)>;
using register_without_context =
    std::function<subscriptions(ores::nats::service::client&, verifier_type)>;

/*
 * The coroutines behind run(). They take the caller's register function as a
 * std::function, and bind it into a named std::function before awaiting,
 * because a coroutine's frame holds its parameters and the temporaries of an
 * await: a caller's lambda, or a lambda of this scope, has no linkage, and a
 * frame holding one is what gcc refuses under -Wsubobject-linkage.
 */

/// Fetches the IAM public key, or returns nothing when shutdown came first.
inline boost::asio::awaitable<verifier_type>
await_verifier(boost::asio::io_context& io_ctx,
               ores::nats::service::client& nats,
               boost::asio::signal_set& signals,
               boost::asio::cancellation_signal& startup_cancel,
               std::string_view name,
               ores::logging::logger_t& lg) {
    using namespace ores::logging;

    add_shutdown_signals(signals);
    arm_startup_cancellation(signals, startup_cancel);

    auto pub_key = co_await fetch_public_key_or_shutdown(io_ctx, nats, startup_cancel, name, lg);
    if (!pub_key)
        co_return std::nullopt;
    BOOST_LOG_SEV(lg, info) << "Fetched JWKS public key from IAM";

    // Dismiss the startup handler before re-arming for the operational phase.
    // signals.cancel() posts the handler with operation_aborted (ignored by !ec).
    // The subsequent async_wait(use_awaitable) then waits for the real signal.
    signals.cancel();

    co_return ores::security::jwt::jwt_authenticator::create_rs256_verifier(*pub_key);
}

inline boost::asio::awaitable<void>
run_domain_service(boost::asio::io_context& io_ctx,
                   ores::nats::service::client& nats,
                   ores::database::context ctx,
                   std::string_view name,
                   register_with_context register_fn,
                   std::function<void(boost::asio::io_context&)> on_started,
                   std::function<void()> on_shutdown) {
    auto& lg = runner_logger();
    // Outlives the startup phase, as the signal handler that emits it does.
    boost::asio::cancellation_signal startup_cancel;
    boost::asio::signal_set signals(io_ctx);
    auto verifier = co_await await_verifier(io_ctx, nats, signals, startup_cancel, name, lg);
    if (!verifier)
        co_return;

    std::function<subscriptions()> bound =
        [&nats, ctx = std::move(ctx), verifier = std::move(verifier), &register_fn]() mutable {
            return register_fn(nats, std::move(ctx), std::move(verifier));
        };
    co_await announce_ready_and_drain(
        io_ctx, nats, name, signals, std::move(bound), std::move(on_started),
        std::move(on_shutdown), lg);
}

inline boost::asio::awaitable<void>
run_domain_service(boost::asio::io_context& io_ctx,
                   ores::nats::service::client& nats,
                   std::string_view name,
                   register_without_context register_fn,
                   std::function<void(boost::asio::io_context&)> on_started,
                   std::function<void()> on_shutdown) {
    auto& lg = runner_logger();
    // Outlives the startup phase, as the signal handler that emits it does.
    boost::asio::cancellation_signal startup_cancel;
    boost::asio::signal_set signals(io_ctx);
    auto verifier = co_await await_verifier(io_ctx, nats, signals, startup_cancel, name, lg);
    if (!verifier)
        co_return;

    std::function<subscriptions()> bound =
        [&nats, verifier = std::move(verifier), &register_fn]() mutable {
            return register_fn(nats, std::move(verifier));
        };
    co_await announce_ready_and_drain(
        io_ctx, nats, name, signals, std::move(bound), std::move(on_started),
        std::move(on_shutdown), lg);
}

}

/*
 * Not a coroutine: it converts the register function and returns the service's
 * awaitable, so a caller binds the result before awaiting it and the lambdas of
 * the call end at that statement instead of living in the caller's frame.
 */
template <typename RegisterFn>
boost::asio::awaitable<void> run(boost::asio::io_context& io_ctx,
                                 ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::string_view name,
                                 RegisterFn&& register_fn,
                                 std::function<void(boost::asio::io_context&)> on_started,
                                 std::function<void()> on_shutdown) {
    return detail::run_domain_service(io_ctx,
                                      nats,
                                      std::move(ctx),
                                      name,
                                      detail::register_with_context(
                                          std::forward<RegisterFn>(register_fn)),
                                      std::move(on_started),
                                      std::move(on_shutdown));
}

template <typename RegisterFn>
boost::asio::awaitable<void> run(boost::asio::io_context& io_ctx,
                                 ores::nats::service::client& nats,
                                 std::string_view name,
                                 RegisterFn&& register_fn,
                                 std::function<void(boost::asio::io_context&)> on_started,
                                 std::function<void()> on_shutdown) {
    return detail::run_domain_service(io_ctx,
                                      nats,
                                      name,
                                      detail::register_without_context(
                                          std::forward<RegisterFn>(register_fn)),
                                      std::move(on_started),
                                      std::move(on_shutdown));
}

}

#endif
