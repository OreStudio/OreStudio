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
#include "ores.iam.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.iam.core/messaging/registrar.hpp"
#include "ores.iam.core/service/role_grant_applier.hpp"
#include "ores.iam.service/app/application_exception.hpp"
#include "ores.iam.service/messaging/event_registrar.hpp"
#include "ores.inbox.api/eventing/approval_request_event.hpp"
#include "ores.inbox.api/messaging/approval_request_protocol.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.service/service/signing_service_runner.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/throw_exception.hpp>
#include <memory>

namespace ores::iam::service::app {

using namespace ores::logging;

namespace ev = ores::eventing;

namespace {
constexpr std::string_view service_name = "ores.iam.service";
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
        "ores.iam.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY -> NATS publish.
    // The per-entity registrars are composed in event_registrar.cpp. Without
    // this call iam's change events stay on their Postgres channels and never
    // reach their subjects.
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);
    auto event_subs =
        messaging::event_registrar::register_event_mappings(event_source, event_bus, nats);

    // An approved role request is granted here, by IAM, which owns the roles.
    // The inbox's request channel is only the nudge: each change runs the
    // reconciliation, which grants every approved role not yet held, so a
    // missed or repeated event changes nothing. A run at start catches what
    // was approved while this service was down.
    using approval_request_event = ores::inbox::messaging::approval_request_event;
    auto role_grants = std::make_shared<ores::iam::service::role_grant_applier>(
        make_context(cfg.database), &event_bus);
    event_source.register_entity_event_mapping<approval_request_event>(
        "ores_inbox_approval_requests");
    auto role_grant_sub = event_bus.subscribe<approval_request_event>(
        [role_grants](const approval_request_event&) { role_grants->apply(); });
    event_source.start();
    role_grants->apply();

    co_await ores::service::service::run_signing(
        io_ctx,
        nats,
        make_context(cfg.database),
        service_name,
        cfg.jwt_private_key,
        [password = cfg.database.password()](auto& n, auto c, auto s) {
            return ores::iam::messaging::registrar::register_handlers(
                n, std::move(c), std::move(s), password);
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
