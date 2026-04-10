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

#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include "ores.database/service/context_factory.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.workflow/messaging/registrar.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"

namespace ores::workflow::service::app {

using namespace ores::logging;

namespace {
constexpr std::string_view service_name = "ores.workflow.service";
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
        "ores.workflow.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url
                              << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" : cfg.nats.subject_prefix)
                              << "')";

    // Ensure the durable workflow stream exists before subscribing.
    // Idempotent — safe to call on every startup and across multiple instances.
    try {
        using ores::workflow::messaging::start_workflow_message;
        using ores::workflow::messaging::step_completed_event;
        const auto stream_name = nats.make_stream_name("workflow");
        auto admin = nats.make_admin();
        admin.ensure_stream(stream_name, {
            nats.make_subject(start_workflow_message::nats_subject),
            nats.make_subject(step_completed_event::nats_subject)
        });
        BOOST_LOG_SEV(lg(), info) << "Workflow JetStream stream ready: " << stream_name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ensure workflow stream: " << e.what();
        throw; // propagate — service cannot run without the stream
    }

    ores::nats::service::nats_client svc_nats(nats,
        ores::iam::client::make_service_token_provider(
            nats, cfg.database.user, cfg.database.password()));

    co_await ores::service::service::run(
        io_ctx, nats, make_context(cfg.database), "ores.workflow.service",
        [&svc_nats](auto& n, auto c, auto v) {
            return ores::workflow::messaging::registrar::register_handlers(
                n, std::move(c), std::move(*v), svc_nats);
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc,
                [hb]() { return hb->run(); },
                boost::asio::detached);
        });
    co_return;
}

}
