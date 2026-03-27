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
#include "ores.reporting.service/app/application.hpp"

#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/post.hpp>
#include "ores.database/service/context_factory.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.core/messaging/registrar.hpp"
#include "ores.reporting.core/service/report_scheduling_service.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.reporting.api/eventing/report_definition_changed_event.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"

namespace ores::reporting::service::app {

using namespace ores::logging;
namespace ev   = ores::eventing;
namespace rdev = ores::reporting::eventing;

namespace {
constexpr std::string_view service_name = "ores.reporting.service";
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
        "ores.reporting.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url
                              << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" : cfg.nats.subject_prefix)
                              << "')";

    auto db_ctx = make_context(cfg.database);

    // Listen for report definition changes so that definitions created or
    // activated after service startup are scheduled without a service restart.
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(
        make_context(cfg.database), event_bus);
    ev::service::registrar::register_mapping<rdev::report_definition_changed_event>(
        event_source, "ores.reporting.report_definition",
        "ores_reporting_report_definitions");

    auto def_changed_sub = event_bus.subscribe<rdev::report_definition_changed_event>(
        [&io_ctx, db_ctx, &nats](const rdev::report_definition_changed_event&) {
            // Post a reconcile task to the io_context so it runs in the
            // coroutine executor rather than the postgres listener thread.
            auto reconciler = std::make_shared<report_scheduling_service>(db_ctx, nats);
            boost::asio::post(io_ctx, [reconciler, &io_ctx]() {
                boost::asio::co_spawn(io_ctx,
                    [reconciler]() { return reconciler->reconcile(); },
                    boost::asio::detached);
            });
        });

    event_source.start();

    co_await ores::service::service::run(
        io_ctx, nats, db_ctx, "ores.reporting.service",
        [](auto& n, auto c, auto v) {
            return ores::reporting::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
        },
        [&nats, db_ctx](boost::asio::io_context& ioc) {
            // Reconcile scheduler jobs for all unscheduled report definitions.
            auto reconciler = std::make_shared<report_scheduling_service>(db_ctx, nats);
            boost::asio::co_spawn(ioc,
                [reconciler]() { return reconciler->reconcile(); },
                boost::asio::detached);

            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc,
                [hb]() { return hb->run(); },
                boost::asio::detached);
        });

    event_source.stop();
    co_return;
}

}
