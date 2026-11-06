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

#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/service/service_lifecycle.hpp"
#include <boost/asio/io_context.hpp>
#include <boost/asio/signal_set.hpp>
#include <functional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::service::service {

namespace detail {

using register_with_signer = std::function<std::vector<ores::nats::service::subscription>(
    ores::nats::service::client&, ores::database::context, ores::security::jwt::jwt_authenticator)>;

/*
 * The coroutine behind run_signing(). It takes the register function as a
 * std::function, and binds it into a named one before awaiting, because a
 * coroutine's frame holds its parameters and the temporaries of an await: a
 * lambda has no linkage, and a frame holding one is what gcc refuses under
 * -Wsubobject-linkage.
 */
inline boost::asio::awaitable<void>
run_signing_service(boost::asio::io_context& io_ctx,
                    ores::nats::service::client& nats,
                    ores::database::context ctx,
                    std::string_view name,
                    const std::string& jwt_private_key,
                    register_with_signer register_fn,
                    std::function<void(boost::asio::io_context&)> on_started,
                    std::function<void()> on_shutdown) {
    using namespace ores::logging;
    auto& lg = signing_runner_logger();

    boost::asio::signal_set signals(io_ctx);
    add_shutdown_signals(signals);

    auto signer = ores::security::jwt::jwt_authenticator::create_rs256_signer(jwt_private_key);
    BOOST_LOG_SEV(lg, info) << "RS256 signer initialised.";

    std::function<std::vector<ores::nats::service::subscription>()> bound =
        [&nats, ctx = std::move(ctx), signer = std::move(signer), &register_fn]() mutable {
            return register_fn(nats, std::move(ctx), std::move(signer));
        };
    co_await announce_ready_and_drain(io_ctx,
                                      nats,
                                      name,
                                      signals,
                                      std::move(bound),
                                      std::move(on_started),
                                      std::move(on_shutdown),
                                      lg);
}

}

/*
 * Not a coroutine: it converts the register function and returns the service's
 * awaitable, so a caller binds the result before awaiting it and the lambdas of
 * the call end at that statement instead of living in the caller's frame.
 */
template <typename RegisterFn>
boost::asio::awaitable<void> run_signing(boost::asio::io_context& io_ctx,
                                         ores::nats::service::client& nats,
                                         ores::database::context ctx,
                                         std::string_view name,
                                         const std::string& jwt_private_key,
                                         RegisterFn&& register_fn,
                                         std::function<void(boost::asio::io_context&)> on_started,
                                         std::function<void()> on_shutdown) {
    return detail::run_signing_service(
        io_ctx,
        nats,
        std::move(ctx),
        name,
        jwt_private_key,
        detail::register_with_signer(std::forward<RegisterFn>(register_fn)),
        std::move(on_started),
        std::move(on_shutdown));
}

}

#endif
