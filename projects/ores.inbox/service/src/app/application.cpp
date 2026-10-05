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
#include "ores.inbox.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.inbox.core/messaging/registrar.hpp"
#include "ores.inbox.service/app/application_exception.hpp"
#include "ores.inbox.service/messaging/approval_decision_event_registrar.hpp"
#include "ores.inbox.service/messaging/approval_decision_type_event_registrar.hpp"
#include "ores.inbox.service/messaging/approval_kind_event_registrar.hpp"
#include "ores.inbox.service/messaging/approval_request_event_registrar.hpp"
#include "ores.inbox.service/messaging/approval_request_state_event_registrar.hpp"
#include "ores.inbox.service/messaging/delivery_outcome_type_event_registrar.hpp"
#include "ores.inbox.service/messaging/notification_channel_event_registrar.hpp"
#include "ores.inbox.service/messaging/notification_delivery_event_registrar.hpp"
#include "ores.inbox.service/messaging/notification_event_registrar.hpp"
#include "ores.inbox.service/messaging/notification_kind_event_registrar.hpp"
#include "ores.inbox.service/messaging/notification_preference_event_registrar.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <vector>

namespace ores::inbox::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;

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

constexpr std::string_view service_name = "ores.inbox.service";
constexpr std::string_view service_version = ORES_VERSION;

} // namespace

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.inbox.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY to NATS publish.
    // Each generated registrar owns one entity's mapping and its publication.
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);
    namespace msg = ores::inbox::service::messaging;
    std::vector<ev::service::subscription> events;
    events.push_back(msg::register_approval_decision_event_mapping(event_source, event_bus, nats));
    events.push_back(
        msg::register_approval_decision_type_event_mapping(event_source, event_bus, nats));
    events.push_back(msg::register_approval_kind_event_mapping(event_source, event_bus, nats));
    events.push_back(msg::register_approval_request_event_mapping(event_source, event_bus, nats));
    events.push_back(
        msg::register_approval_request_state_event_mapping(event_source, event_bus, nats));
    events.push_back(
        msg::register_delivery_outcome_type_event_mapping(event_source, event_bus, nats));
    events.push_back(msg::register_notification_event_mapping(event_source, event_bus, nats));
    events.push_back(
        msg::register_notification_channel_event_mapping(event_source, event_bus, nats));
    events.push_back(
        msg::register_notification_delivery_event_mapping(event_source, event_bus, nats));
    events.push_back(msg::register_notification_kind_event_mapping(event_source, event_bus, nats));
    events.push_back(
        msg::register_notification_preference_event_mapping(event_source, event_bus, nats));
    event_source.start();
    BOOST_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.inbox.service",
        [](auto& n, auto c, auto v) {
            return ores::inbox::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
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
