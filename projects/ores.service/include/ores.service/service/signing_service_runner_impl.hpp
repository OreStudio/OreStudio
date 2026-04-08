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
#ifndef ORES_SERVICE_SERVICE_SIGNING_SERVICE_RUNNER_IMPL_HPP
#define ORES_SERVICE_SERVICE_SIGNING_SERVICE_RUNNER_IMPL_HPP

#include <csignal>
#include <functional>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/error_code.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::service::service {

template<typename RegisterFn>
boost::asio::awaitable<void>
run_signing(boost::asio::io_context& io_ctx,
            ores::nats::service::client& nats,
            ores::database::context ctx,
            std::string_view name,
            const std::string& jwt_private_key,
            RegisterFn&& register_fn,
            std::function<void(boost::asio::io_context&)> on_started,
            std::function<void()> on_shutdown) {

    using namespace ores::logging;
    static const std::string_view logger_name =
        "ores.service.service.signing_runner";
    static auto& lg = []() -> auto& {
        static auto instance = make_logger(logger_name);
        return instance;
    }();

    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM, SIGQUIT);

    auto signer = ores::security::jwt::jwt_authenticator::create_rs256_signer(
        jwt_private_key);
    BOOST_LOG_SEV(lg, info) << "RS256 signer initialised.";

    auto subs = register_fn(nats, std::move(ctx), std::move(signer));
    BOOST_LOG_SEV(lg, info) << "Registered " << subs.size() << " subscription(s).";
    for (const auto& sub : subs)
        BOOST_LOG_SEV(lg, info) << "NATS subscribe: " << sub.subject();

    if (on_started)
        on_started(io_ctx);

    BOOST_LOG_SEV(lg, info) << "Service ready.";
    BOOST_LOG_SEV(lg, info) << "Waiting for requests...";
    co_await signals.async_wait(boost::asio::use_awaitable);

    BOOST_LOG_SEV(lg, info) << "Shutdown signal received. Draining...";
    if (on_shutdown) on_shutdown();
    nats.drain();
    BOOST_LOG_SEV(lg, info) << "Shutdown complete: " << name;
    co_return;
}

} // namespace ores::service::service

#endif
