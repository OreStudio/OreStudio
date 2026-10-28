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
#include "ores.marketdata.official/app/application.hpp"
#include "ores.http.client/net/transport.hpp"
#include "ores.marketdata.official/app/official_adapter.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>

namespace ores::marketdata::official::app {

using namespace ores::logging;

namespace {

constexpr std::string_view service_name = "ores.marketdata.official";
constexpr std::string_view service_version = ORES_VERSION;

}

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        std::string(service_name), 0, 1);
    BOOST_LOG_SEV(lg(), info) << "Frankfurter : " << cfg.frankfurter_url;
    BOOST_LOG_SEV(lg(), info) << "Sources     : " << cfg.sources.size();
    BOOST_LOG_SEV(lg(), info) << "Backfill    : " << cfg.backfill_days << " days";
    BOOST_LOG_SEV(lg(), info) << "Poll        : every " << cfg.poll_minutes << " minutes";

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    http::client::net::beast_transport transport;
    domain::frankfurter_fetcher fetcher(transport, cfg.frankfurter_url);
    adapter_timing timing;
    timing.poll_interval = std::chrono::minutes(cfg.poll_minutes);
    official_adapter adapter(
        nats, cfg.sources, fetcher, static_cast<int>(cfg.backfill_days), timing);

    co_await ores::service::service::run(
        io_ctx,
        nats,
        service_name,
        [&adapter](auto& /*n*/, auto /*verifier*/) { return adapter.subscribe(); },
        [&](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats, std::string());
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
            adapter.start();
        },
        [&]() { adapter.stop(); });
    co_return;
}

}
