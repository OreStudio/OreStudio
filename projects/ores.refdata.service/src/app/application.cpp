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
#include "ores.refdata.service/app/application.hpp"

#include <algorithm>
#include <rfl/json.hpp>
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/service_accounts.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.refdata.service/app/application_exception.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.eventing/domain/entity_change_event.hpp"
#include "ores.refdata/eventing/book_changed_event.hpp"
#include "ores.refdata/eventing/portfolio_changed_event.hpp"
#include "ores.refdata/messaging/registrar.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"

namespace ores::refdata::service::app {

using namespace ores::logging;
namespace ev  = ores::eventing;
namespace rdev = ores::refdata::eventing;

ores::database::context application::make_context(
    const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg {
        .database_options = db_opts,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1,
        .service_account = std::string(ores::database::service::service_accounts::refdata_service)
    };

    return context_factory::make_context(cfg);
}

application::application() = default;

namespace {

auto& pub_lg() {
    static auto instance = make_logger("ores.refdata.service.app");
    return instance;
}

void publish_entity_event(ores::nats::service::client& nats,
                          const std::string& subject,
                          const ev::domain::entity_change_event& notif) {
    try {
        const auto json = rfl::json::write(notif);
        std::vector<std::byte> data(json.size());
        std::transform(json.begin(), json.end(), data.begin(),
                       [](char c) { return std::byte(c); });
        nats.publish(subject, std::move(data), {});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(pub_lg(), error)
            << "Failed to publish event to '" << subject << "': " << e.what();
    }
}

} // namespace

boost::asio::awaitable<void>
application::run(boost::asio::io_context& io_ctx,
    const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.refdata.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url
                              << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" : cfg.nats.subject_prefix)
                              << "')";

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(
        make_context(cfg.database), event_bus);

    ev::service::registrar::register_mapping<rdev::book_changed_event>(
        event_source, "ores.refdata.book", "ores_books");
    ev::service::registrar::register_mapping<rdev::portfolio_changed_event>(
        event_source, "ores.refdata.portfolio", "ores_portfolios");

    auto book_sub = event_bus.subscribe<rdev::book_changed_event>(
        [&nats](const rdev::book_changed_event& e) {
            publish_entity_event(nats, "ores.refdata.book_changed",
                ev::domain::entity_change_event{
                    .entity     = "ores.refdata.book",
                    .timestamp  = e.timestamp,
                    .entity_ids = e.ids,
                    .tenant_id  = e.tenant_id
                });
        });

    auto portfolio_sub = event_bus.subscribe<rdev::portfolio_changed_event>(
        [&nats](const rdev::portfolio_changed_event& e) {
            publish_entity_event(nats, "ores.refdata.portfolio_changed",
                ev::domain::entity_change_event{
                    .entity     = "ores.refdata.portfolio",
                    .timestamp  = e.timestamp,
                    .entity_ids = e.ids,
                    .tenant_id  = e.tenant_id
                });
        });

    event_source.start();
    BOOST_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    co_await ores::service::service::run(
        io_ctx, nats, make_context(cfg.database), "ores.refdata.service",
        [](auto& n, auto c, auto v) {
            return ores::refdata::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                "ores.refdata.service", "1.0", nats);
            boost::asio::co_spawn(ioc,
                [hb]() { return hb->run(); },
                boost::asio::detached);
        });

    event_source.stop();
    co_return;
}

}
