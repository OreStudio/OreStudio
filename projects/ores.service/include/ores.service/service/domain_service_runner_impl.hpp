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
#include <functional>
#include <optional>
#include <string_view>
#include <utility>

namespace ores::service::service {

template <typename RegisterFn>
boost::asio::awaitable<void> run(boost::asio::io_context& io_ctx,
                                 ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::string_view name,
                                 RegisterFn&& register_fn,
                                 std::function<void(boost::asio::io_context&)> on_started,
                                 std::function<void()> on_shutdown) {

    using namespace ores::logging;
    auto& lg = detail::runner_logger();

    boost::asio::cancellation_signal startup_cancel;
    boost::asio::signal_set signals(io_ctx);
    detail::add_shutdown_signals(signals);
    detail::arm_startup_cancellation(signals, startup_cancel);

    auto pub_key =
        co_await detail::fetch_public_key_or_shutdown(io_ctx, nats, startup_cancel, name, lg);
    if (!pub_key)
        co_return;
    BOOST_LOG_SEV(lg, info) << "Fetched JWKS public key from IAM";

    // Dismiss the startup handler before re-arming for the operational phase.
    // signals.cancel() posts the handler with operation_aborted (ignored by !ec).
    // The subsequent async_wait(use_awaitable) then waits for the real signal.
    signals.cancel();

    std::optional<ores::security::jwt::jwt_authenticator> verifier =
        ores::security::jwt::jwt_authenticator::create_rs256_verifier(*pub_key);

    co_await detail::announce_ready_and_drain(
        io_ctx,
        nats,
        name,
        signals,
        [&nats, ctx = std::move(ctx), verifier = std::move(verifier), &register_fn]() mutable {
            return register_fn(nats, std::move(ctx), std::move(verifier));
        },
        std::move(on_started),
        std::move(on_shutdown),
        lg);
}

template <typename RegisterFn>
boost::asio::awaitable<void> run(boost::asio::io_context& io_ctx,
                                 ores::nats::service::client& nats,
                                 std::string_view name,
                                 RegisterFn&& register_fn,
                                 std::function<void(boost::asio::io_context&)> on_started,
                                 std::function<void()> on_shutdown) {

    using namespace ores::logging;
    auto& lg = detail::runner_logger();

    boost::asio::cancellation_signal startup_cancel;
    boost::asio::signal_set signals(io_ctx);
    detail::add_shutdown_signals(signals);
    detail::arm_startup_cancellation(signals, startup_cancel);

    auto pub_key =
        co_await detail::fetch_public_key_or_shutdown(io_ctx, nats, startup_cancel, name, lg);
    if (!pub_key)
        co_return;
    BOOST_LOG_SEV(lg, info) << "Fetched JWKS public key from IAM";

    signals.cancel();

    std::optional<ores::security::jwt::jwt_authenticator> verifier =
        ores::security::jwt::jwt_authenticator::create_rs256_verifier(*pub_key);

    co_await detail::announce_ready_and_drain(
        io_ctx,
        nats,
        name,
        signals,
        [&nats, verifier = std::move(verifier), &register_fn]() mutable {
            return register_fn(nats, std::move(verifier));
        },
        std::move(on_started),
        std::move(on_shutdown),
        lg);
}

}

#endif
