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
#include "ores.compute.service/app/application.hpp"
#include "ores.compute.api/eventing/app_changed_event.hpp"
#include "ores.compute.api/eventing/app_version_changed_event.hpp"
#include "ores.compute.api/eventing/batch_changed_event.hpp"
#include "ores.compute.api/eventing/host_changed_event.hpp"
#include "ores.compute.api/eventing/result_changed_event.hpp"
#include "ores.compute.api/eventing/workunit_changed_event.hpp"
#include "ores.compute.core/messaging/registrar.hpp"
#include "ores.compute.service/app/application_exception.hpp"
#include "ores.compute.service/app/batch_workflow_bridge.hpp"
#include "ores.compute.service/app/compute_grid_poller.hpp"
#include "ores.compute.service/app/workunit_dispatcher.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.eventing.core/service/registrar.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <memory>

namespace ores::compute::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;
namespace cev = ores::compute::eventing;

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = static_cast<std::size_t>(db_opts.pool_size),
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

namespace {

constexpr std::string_view service_name = "ores.compute.service";
constexpr std::string_view service_version = ORES_VERSION;

void publish_entity_event(ores::nats::service::client& nats,
                          const std::string& subject,
                          const ev::domain::entity_change_event& notif) {
    // Delegate to the shared hardened publisher: it rethrows on failure so
    // the event_bus surfaces the lost notification at error.
    ev::service::publish_entity_event(nats, subject, notif);
}

} // namespace

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.compute.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);

    ev::service::registrar::register_mapping<cev::app_changed_event>(
        event_source, "ores.compute.app", "ores_compute_apps");
    ev::service::registrar::register_mapping<cev::app_version_changed_event>(
        event_source, "ores.compute.app_version", "ores_compute_app_versions");
    ev::service::registrar::register_mapping<cev::batch_changed_event>(
        event_source, "ores.compute.batch", "ores_compute_batches");
    ev::service::registrar::register_mapping<cev::workunit_changed_event>(
        event_source, "ores.compute.workunit", "ores_compute_workunits");
    ev::service::registrar::register_mapping<cev::result_changed_event>(
        event_source, "ores.compute.result", "ores_compute_results");
    ev::service::registrar::register_mapping<cev::host_changed_event>(
        event_source, "ores.compute.host", "ores_compute_hosts");

    // Grid dispatch seam: creates the result rows and publishes the JetStream
    // assignments for shell-saved workunits. Lives here, subscribed to the
    // in-process event bus (not in the generated workunit handler, which
    // regeneration would strip — see workunit_dispatcher.hpp). Must be
    // constructed before event_source.start() so no change is missed.
    app::workunit_dispatcher dispatcher(nats, make_context(cfg.database));

    auto app_sub =
        event_bus.subscribe<cev::app_changed_event>([&nats](const cev::app_changed_event& e) {
            publish_entity_event(nats,
                                 "ores.compute.app_changed",
                                 ev::domain::entity_change_event{.entity = "ores.compute.app",
                                                                 .timestamp = e.timestamp,
                                                                 .entity_ids = e.app_ids,
                                                                 .tenant_id = e.tenant_id});
        });

    auto app_version_sub = event_bus.subscribe<cev::app_version_changed_event>(
        [&nats](const cev::app_version_changed_event& e) {
            publish_entity_event(
                nats,
                "ores.compute.app_version_changed",
                ev::domain::entity_change_event{.entity = "ores.compute.app_version",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.app_version_ids,
                                                .tenant_id = e.tenant_id});
        });

    auto batch_sub =
        event_bus.subscribe<cev::batch_changed_event>([&nats](const cev::batch_changed_event& e) {
            publish_entity_event(nats,
                                 "ores.compute.batch_changed",
                                 ev::domain::entity_change_event{.entity = "ores.compute.batch",
                                                                 .timestamp = e.timestamp,
                                                                 .entity_ids = e.batch_ids,
                                                                 .tenant_id = e.tenant_id});
        });

    auto workunit_sub = event_bus.subscribe<cev::workunit_changed_event>(
        [&nats](const cev::workunit_changed_event& e) {
            publish_entity_event(nats,
                                 "ores.compute.workunit_changed",
                                 ev::domain::entity_change_event{.entity = "ores.compute.workunit",
                                                                 .timestamp = e.timestamp,
                                                                 .entity_ids = e.workunit_ids,
                                                                 .tenant_id = e.tenant_id});
        });

    auto dispatch_sub = event_bus.subscribe<cev::workunit_changed_event>(
        [&dispatcher](const cev::workunit_changed_event& e) { dispatcher.dispatch(e); });

    auto result_sub =
        event_bus.subscribe<cev::result_changed_event>([&nats](const cev::result_changed_event& e) {
            publish_entity_event(nats,
                                 "ores.compute.result_changed",
                                 ev::domain::entity_change_event{.entity = "ores.compute.result",
                                                                 .timestamp = e.timestamp,
                                                                 .entity_ids = e.result_ids,
                                                                 .tenant_id = e.tenant_id});
        });

    auto host_sub =
        event_bus.subscribe<cev::host_changed_event>([&nats](const cev::host_changed_event& e) {
            publish_entity_event(nats,
                                 "ores.compute.host_changed",
                                 ev::domain::entity_change_event{.entity = "ores.compute.host",
                                                                 .timestamp = e.timestamp,
                                                                 .entity_ids = e.host_ids,
                                                                 .tenant_id = e.tenant_id});
        });

    event_source.start();
    BOOST_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    // Create separate contexts for pollers so the main context can be
    // moved into run() for the subscription handlers.
    auto poller_ctx = make_context(cfg.database);
    auto bridge_ctx = make_context(cfg.database);
    const auto telemetry_interval = cfg.telemetry_interval_seconds;

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.compute.service",
        [](auto& n, auto c, auto v) {
            return ores::compute::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
        },
        [telemetry_interval,
         poller_ctx = std::move(poller_ctx),
         bridge_ctx = std::move(bridge_ctx),
         &nats](boost::asio::io_context& ioc) mutable {
            if (telemetry_interval > 0) {
                auto poller = std::make_shared<app::compute_grid_poller>(telemetry_interval,
                                                                         std::move(poller_ctx));
                boost::asio::co_spawn(
                    ioc, [poller]() { return poller->run(); }, boost::asio::detached);
            }
            // Async bridge: fires step_completed_event when batches close.
            constexpr std::uint32_t bridge_interval_seconds = 10;
            auto bridge = std::make_shared<app::batch_workflow_bridge>(
                bridge_interval_seconds, nats, std::move(bridge_ctx));
            boost::asio::co_spawn(ioc, [bridge]() { return bridge->run(); }, boost::asio::detached);
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });

    event_source.stop();
    co_return;
}

}
