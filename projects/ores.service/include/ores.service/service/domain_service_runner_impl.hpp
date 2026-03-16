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
#include <optional>
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
    RegisterFn&& register_fn) {

    using namespace ores::logging;
    static const std::string_view logger_name = "ores.service.service.runner";
    static auto& lg = []() -> auto& {
        static auto instance = make_logger(logger_name);
        return instance;
    }();

    // Install signal handler before the JWKS fetch so that SIGINT/SIGTERM
    // cancels the retry loop instead of hitting the OS default handler.
    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    signals.async_wait([&io_ctx](const boost::system::error_code& ec, int) {
        if (!ec) io_ctx.stop();
    });

    std::string pub_key;
    try {
        pub_key = co_await ores::nats::service::fetch_jwks_public_key(nats);
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted)
            throw;
        BOOST_LOG_SEV(lg, info) << "Shutdown signal received during startup.";
        co_return;
    }
    BOOST_LOG_SEV(lg, info) << "Fetched JWKS public key from IAM";

    std::optional<ores::security::jwt::jwt_authenticator> verifier =
        ores::security::jwt::jwt_authenticator::create_rs256_verifier(pub_key);

    auto subs = register_fn(nats, std::move(ctx), std::move(verifier));
    BOOST_LOG_SEV(lg, info) << "Registered " << subs.size() << " subscription(s).";

    BOOST_LOG_SEV(lg, info) << "Service ready. Waiting for requests...";
    signals.cancel();
    co_await signals.async_wait(boost::asio::use_awaitable);

    BOOST_LOG_SEV(lg, info) << "Shutdown signal received. Draining...";
    nats.drain();
    BOOST_LOG_SEV(lg, info) << "Shutdown complete: " << name;
    co_return;
}

} // namespace ores::service::service

#endif
