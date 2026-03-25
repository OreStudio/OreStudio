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

#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include "ores.database/service/context_factory.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.telemetry.service/app/application_exception.hpp"
#include "ores.telemetry.service/app/nats_poller.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.telemetry.service/messaging/registrar.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"

namespace ores::telemetry::service::app {

using namespace ores::logging;

namespace {
constexpr std::string_view service_name = "ores.telemetry.service";
constexpr std::string_view service_version = ORES_VERSION;
}

ores::database::context application::make_context(
    const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg {
        .database_options = db_opts,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1,
        .service_account = db_opts.user
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

    // Create a separate context for the poller so the main ctx can be moved
    // into run() for the subscription handlers.
    auto poller_ctx = make_context(cfg.database);
    const auto monitor_url = cfg.nats_monitor_url;
    const auto monitor_interval = cfg.nats_monitor_interval_seconds;

    co_await ores::service::service::run(
        io_ctx, nats, make_context(cfg.database), "ores.telemetry.service",
        [](auto& n, auto c, auto v) {
            return ores::telemetry::service::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
        },
        [monitor_url, monitor_interval, poller_ctx = std::move(poller_ctx), &nats]
        (boost::asio::io_context& ioc) mutable {
            if (!monitor_url.empty()) {
                auto poller = std::make_shared<nats_poller>(
                    monitor_url, monitor_interval, std::move(poller_ctx));
                boost::asio::co_spawn(ioc,
                    [poller]() { return poller->run(); },
                    boost::asio::detached);
            }
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc,
                [hb]() { return hb->run(); },
                boost::asio::detached);
        });
    co_return;
}

}
