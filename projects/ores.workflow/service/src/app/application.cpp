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
#include "ores.workflow.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include "ores.workflow.core/messaging/registrar.hpp"
#include "ores.workflow.service/messaging/workflow_instance_event_registrar.hpp"
#include "ores.workflow.service/messaging/workflow_step_event_registrar.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>

namespace ores::workflow::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;

namespace {
constexpr std::string_view service_name = "ores.workflow.service";
constexpr std::string_view service_version = ORES_VERSION;
}

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

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.workflow.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // Ensure the durable workflow stream exists before subscribing.
    // Idempotent — safe to call on every startup and across multiple instances.
    try {
        using ores::workflow::messaging::start_workflow_message;
        using ores::workflow::messaging::step_completed_event;
        const auto stream_name = nats.make_stream_name("workflow");
        auto admin = nats.make_admin();
        admin.ensure_stream(stream_name,
                            nats.covering_subjects({start_workflow_message::nats_subject,
                                                    step_completed_event::nats_subject}));
        BOOST_LOG_SEV(lg(), info) << "Workflow JetStream stream ready: " << stream_name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ensure workflow stream: " << e.what();
        // The service cannot run without the stream, so the failure propagates.
        throw;
    }

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);

    // Each generated registrar owns one entity's mapping: it reads the trigger's
    // channel and publishes the canonical event on the subject its action names.
    // Both subscriptions are held here rather than discarded, because the source
    // delivers into them and they must outlive start().
    auto instance_event_sub =
        ores::workflow::service::messaging::register_workflow_instance_event_mapping(
            event_source, event_bus, nats);
    auto step_event_sub = ores::workflow::service::messaging::register_workflow_step_event_mapping(
        event_source, event_bus, nats);
    (void)instance_event_sub;
    (void)step_event_sub;

    // Subscriptions are in place before the source starts, so no change that
    // lands during startup is missed.
    event_source.start();
    BOOST_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.workflow.service",
        [](auto& n, auto c, auto v) {
            return ores::workflow::messaging::registrar::register_handlers(
                n, std::move(c), std::move(*v));
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });

    event_source.stop();
    co_return;
}

}
