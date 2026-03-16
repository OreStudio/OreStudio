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
#include "ores.telemetry.service/app/application.hpp"

#include <chrono>
#include <span>
#include <csignal>
#include <optional>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/throw_exception.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.database/service/context_factory.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.telemetry.service/app/application_exception.hpp"
#include "ores.telemetry.service/app/nats_poller.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.telemetry/messaging/registrar.hpp"
#include "ores.nats/service/jwks.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::telemetry::service::app {

using namespace ores::logging;

ores::database::context application::make_context(
    const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg {
        .database_options = db_opts,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void>
application::run(boost::asio::io_context& io_ctx,
    const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.telemetry.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url
                              << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" : cfg.nats.subject_prefix)
                              << "')";

    auto ctx = make_context(cfg.database);
    auto pub_key = co_await ores::nats::service::fetch_jwks_public_key(nats);
    BOOST_LOG_SEV(lg(), info) << "Fetched JWKS public key from IAM";
    std::optional<ores::security::jwt::jwt_authenticator> verifier =
        ores::security::jwt::jwt_authenticator::create_rs256_verifier(pub_key);
    auto subs = ores::telemetry::messaging::registrar::register_handlers(
        nats, ctx, std::move(verifier));
    BOOST_LOG_SEV(lg(), info) << "Registered " << subs.size() << " subscription(s).";

    if (!cfg.nats_monitor_url.empty()) {
        auto poller = std::make_shared<nats_poller>(
            cfg.nats_monitor_url, cfg.nats_monitor_interval_seconds, ctx);
        boost::asio::co_spawn(io_ctx,
            [poller]() { return poller->run(); },
            boost::asio::detached);
        BOOST_LOG_SEV(lg(), info) << "NATS metrics poller started ("
                                  << cfg.nats_monitor_interval_seconds << "s interval).";
    }

    BOOST_LOG_SEV(lg(), info) << "Service ready. Waiting for requests...";
    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    co_await signals.async_wait(boost::asio::use_awaitable);

    BOOST_LOG_SEV(lg(), info) << "Shutdown signal received. Draining...";
    nats.drain();
    BOOST_LOG_SEV(lg(), info) << "Shutdown complete: ores.telemetry.service";
    co_return;
}

}
