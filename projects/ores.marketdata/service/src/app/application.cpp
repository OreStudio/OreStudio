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
#include "ores.marketdata.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.eventing.core/service/registrar.hpp"
#include "ores.marketdata.api/eventing/feed_binding_changed_event.hpp"
#include "ores.marketdata.core/messaging/registrar.hpp"
#include "ores.marketdata.service/app/feed_ingest_loop.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <memory>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

constexpr std::string_view service_name = "ores.marketdata.service";
constexpr std::string_view service_version = ORES_VERSION;

} // namespace

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = 4,
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.marketdata.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" :
                                                                    cfg.nats.subject_prefix)
                              << "')";

    try {
        auto admin = nats.make_admin();
        admin.ensure_stream(nats.make_stream_name("synthetic_ticks"),
                            {nats.make_subject("synthetic.v1.tick.>")});
        admin.ensure_stream(nats.make_stream_name("marketdata_ticks"),
                            {nats.make_subject("marketdata.v1.tick.>")});
        BOOST_LOG_SEV(lg(), info) << "JetStream streams ready: synthetic_ticks, marketdata_ticks";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ensure JetStream streams: " << e.what();
        throw;
    }

    auto ingest = std::make_shared<feed_ingest_loop>(nats, make_context(cfg.database));

    namespace ev = ores::eventing;
    namespace mdev = ores::marketdata::eventing;
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);
    ev::service::registrar::register_mapping<mdev::feed_binding_changed_event>(
        event_source, "ores.marketdata.feed_binding", "ores_marketdata_feed_bindings");
    auto feed_binding_sub = event_bus.subscribe<mdev::feed_binding_changed_event>(
        [ingest](const mdev::feed_binding_changed_event&) {
            BOOST_LOG_SEV(lg(), info) << "Feed binding changed — refreshing ingest loop.";
            ingest->refresh();
        });
    event_source.start();

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.marketdata.service",
        [&cfg, ingest](auto& n, auto c, auto v) {
            return ores::marketdata::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v), cfg.http_base_url,
                [ingest]() { ingest->refresh(); });
        },
        [&nats, ingest](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
            ingest->start();
        });

    event_source.stop();
    co_return;
}

}
