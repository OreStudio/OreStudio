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
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.marketdata.api/eventing/feed_binding_changed_event.hpp"
#include "ores.marketdata.api/messaging/crm_protocol.hpp"
#include "ores.marketdata.core/messaging/registrar.hpp"
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.marketdata.service/app/feed_ingest_loop.hpp"
#include "ores.marketdata.service/messaging/crm_handler.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/eventing/crm_driver_pair_changed_event.hpp"
#include "ores.refdata.api/eventing/crm_enabled_derived_pair_changed_event.hpp"
#include "ores.refdata.api/eventing/crm_topology_config_changed_event.hpp"
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

    // Authenticated client for service-to-service calls. Currently used
    // only by import_handler, to fetch ores.refdata's currency_pair
    // reference data for FX quote convention checking during import. The
    // token provider authenticates with the service's own database
    // account credentials.
    ores::nats::service::nats_client svc_nats(
        nats,
        ores::iam::client::make_service_token_provider(
            nats, cfg.database.user, cfg.database.password()));

    auto crm_bridge = std::make_shared<crm_ingest_bridge>(make_context(cfg.database));
    auto ingest = std::make_shared<feed_ingest_loop>(nats, make_context(cfg.database), crm_bridge);

    namespace ev = ores::eventing;
    namespace mdev = ores::marketdata::eventing;
    namespace rdev = ores::refdata::eventing;
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);
    ev::service::registrar::register_mapping<mdev::feed_binding_changed_event>(
        event_source, "ores.marketdata.feed_binding", "ores_marketdata_feed_bindings");
    auto feed_binding_sub = event_bus.subscribe<mdev::feed_binding_changed_event>(
        [ingest](const mdev::feed_binding_changed_event&) {
            BOOST_LOG_SEV(lg(), info) << "Feed binding changed — refreshing ingest loop.";
            ingest->refresh();
        });

    ev::service::registrar::register_mapping<rdev::crm_topology_config_changed_event>(
        event_source, "ores.refdata.crm_topology_config", "ores_refdata_crm_topology_configs");
    ev::service::registrar::register_mapping<rdev::crm_driver_pair_changed_event>(
        event_source, "ores.refdata.crm_driver_pair", "ores_refdata_crm_driver_pairs");
    ev::service::registrar::register_mapping<rdev::crm_enabled_derived_pair_changed_event>(
        event_source,
        "ores.refdata.crm_enabled_derived_pair",
        "ores_refdata_crm_enabled_derived_pairs");
    auto crm_topology_sub = event_bus.subscribe<rdev::crm_topology_config_changed_event>(
        [crm_bridge](const rdev::crm_topology_config_changed_event&) {
            BOOST_LOG_SEV(lg(), info) << "CRM topology config changed — refreshing CRM engines.";
            crm_bridge->refresh();
        });
    auto crm_driver_pair_sub = event_bus.subscribe<rdev::crm_driver_pair_changed_event>(
        [crm_bridge](const rdev::crm_driver_pair_changed_event&) {
            BOOST_LOG_SEV(lg(), info) << "CRM driver pair changed — refreshing CRM engines.";
            crm_bridge->refresh();
        });
    auto crm_enabled_derived_pair_sub =
        event_bus.subscribe<rdev::crm_enabled_derived_pair_changed_event>(
            [crm_bridge](const rdev::crm_enabled_derived_pair_changed_event&) {
                BOOST_LOG_SEV(lg(), info)
                    << "CRM enabled derived pair changed — refreshing CRM engines.";
                crm_bridge->refresh();
            });
    event_source.start();
    crm_bridge->refresh();

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.marketdata.service",
        [&cfg, ingest, &svc_nats, crm_bridge](auto& n, auto c, auto v) {
            auto subs = ores::marketdata::messaging::registrar::register_handlers(
                n, c, svc_nats, v, cfg.http_base_url);

            namespace mm = ores::marketdata::messaging;
            constexpr auto queue = "ores.marketdata.service";
            subs.push_back(
                n.queue_subscribe(std::string(mm::get_crm_rate_request::nats_subject),
                                  queue,
                                  [&n, c, v, crm_bridge](ores::nats::message msg) mutable {
                                      mm::crm_handler h(n, c, v, crm_bridge);
                                      h.rate(std::move(msg));
                                  }));
            subs.push_back(
                n.queue_subscribe(std::string(mm::get_crm_rates_request::nats_subject),
                                  queue,
                                  [&n, c, v, crm_bridge](ores::nats::message msg) mutable {
                                      mm::crm_handler h(n, c, v, crm_bridge);
                                      h.rates(std::move(msg));
                                  }));
            return subs;
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
