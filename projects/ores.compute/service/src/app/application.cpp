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
#include "ores.compute.api/eventing/workunit_changed_event.hpp"
#include "ores.compute.core/messaging/registrar.hpp"
#include "ores.compute.service/app/application_exception.hpp"
#include "ores.compute.service/app/batch_workflow_bridge.hpp"
#include "ores.compute.service/app/compute_grid_poller.hpp"
#include "ores.compute.service/app/workunit_dispatcher.hpp"
#include "ores.compute.service/messaging/app_event_registrar.hpp"
#include "ores.compute.service/messaging/app_version_event_registrar.hpp"
#include "ores.compute.service/messaging/batch_event_registrar.hpp"
#include "ores.compute.service/messaging/host_event_registrar.hpp"
#include "ores.compute.service/messaging/platform_event_registrar.hpp"
#include "ores.compute.service/messaging/result_event_registrar.hpp"
#include "ores.compute.service/messaging/workunit_event_registrar.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
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

    // The generated registrars own each entity's mapping and its NATS
    // publication: each reads the trigger's channel and publishes the
    // canonical event the dispatcher's subscriber consumes.
    auto app_sub = ores::compute::service::messaging::register_app_event_mapping(
        event_source, event_bus, nats);
    auto app_version_sub = ores::compute::service::messaging::register_app_version_event_mapping(
        event_source, event_bus, nats);
    auto batch_sub = ores::compute::service::messaging::register_batch_event_mapping(
        event_source, event_bus, nats);
    auto workunit_sub = ores::compute::service::messaging::register_workunit_event_mapping(
        event_source, event_bus, nats);
    auto result_sub = ores::compute::service::messaging::register_result_event_mapping(
        event_source, event_bus, nats);
    auto host_sub = ores::compute::service::messaging::register_host_event_mapping(
        event_source, event_bus, nats);
    auto platform_sub = ores::compute::service::messaging::register_platform_event_mapping(
        event_source, event_bus, nats);

    // The dispatch seam reads the in-process bus, and the bus event it wants
    // carries the tenant and the changed workunit ids. The canonical event the
    // registrar publishes carries no tenant, so the workunit channel is mapped
    // a second time, to the older domain event. One channel serves both.
    ev::service::registrar::register_mapping<cev::workunit_changed_event>(
        event_source, "ores.compute.workunit", "ores_compute_workunits");

    // Must be constructed before event_source.start() so no change is missed.
    app::workunit_dispatcher dispatcher(nats, make_context(cfg.database));

    // Grid dispatch seam: creates the result rows and publishes the JetStream
    // assignments for shell-saved workunits. It reads the in-process bus rather
    // than the NATS publication, so it stays beside the registrars rather than
    // inside one.
    auto dispatch_sub = event_bus.subscribe<cev::workunit_changed_event>(
        [&dispatcher](const cev::workunit_changed_event& e) { dispatcher.dispatch(e); });
    (void)app_sub;
    (void)app_version_sub;
    (void)batch_sub;
    (void)workunit_sub;
    (void)result_sub;
    (void)host_sub;
    (void)platform_sub;
    (void)dispatch_sub;

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
