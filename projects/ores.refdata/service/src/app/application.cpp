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
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.core/messaging/registrar.hpp"
#include "ores.refdata.core/service/calendar_materialisation_service.hpp"
#include "ores.refdata.service/config/options.hpp"
#include "ores.refdata.service/messaging/book_proposal_registrar.hpp"
#include "ores.refdata.service/messaging/event_registrar.hpp"
#include "ores.refdata.service/messaging/provisioning_step_handler.hpp"
#include "ores.refdata.service/messaging/provisioning_step_registrar.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <chrono>
#include <cstddef>
#include <exception>
#include <memory>
#include <string_view>
#include <utility>

namespace ores::refdata::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_min = static_cast<std::size_t>(db_opts.pool_min),
                                       .pool_max = static_cast<std::size_t>(db_opts.pool_max),
                                       .pool_idle_seconds =
                                           static_cast<std::size_t>(db_opts.pool_idle),
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

namespace {

constexpr std::string_view service_name = "ores.refdata.service";
constexpr std::string_view service_version = ORES_VERSION;

/*
 * Provisioning builds each tenant's calendars, but no run provisions the system tenant. Its
 * generators date what they quote with its calendars, so they are built here, before any of them
 * can ask. A build only extends past the dates already there, so a restart rewrites nothing.
 */
void build_system_calendars(const ores::database::context& ctx) {
    static auto log = ores::logging::make_logger("ores.refdata.service.app.system_calendars");
    using ores::database::service::tenant_context;
    const auto system = tenant_context::with_system_tenant(ctx);
    const auto acting = system.with_tenant(system.tenant_id(), system.service_account());
    const auto started = std::chrono::steady_clock::now();
    try {
        ores::refdata::service::calendar_materialisation_service calendars(acting);
        const auto rows = calendars.regenerate_in_use(std::chrono::year{
            ores::refdata::messaging::provisioning_step_handler::last_generated_holiday_year});
        const auto took = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - started);
        ORES_LOG_SEV(log, info) << "Built the system tenant's calendars: wrote " << rows
                                << " day(s) in " << took.count() << " ms.";
    } catch (const std::exception& e) {
        ORES_LOG_SEV(log, error) << "The system tenant's calendars were not built: " << e.what();
    }
}

} // namespace

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    ORES_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.refdata.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);

    // Every entity's event mapping comes from the generated per-entity registrars.
    auto generated_event_subs =
        messaging::event_registrar::register_event_mappings(event_source, event_bus, nats);

    event_source.start();
    ORES_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    build_system_calendars(make_context(cfg.database));

    // Bound before the await, so no lambda of the call lives in this frame; see run().
    auto service_run = ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.refdata.service",
        [](auto& n, auto c, auto v) {
            auto subs = ores::refdata::messaging::registrar::register_handlers(n, c, v);
            // The provisioning run's calendar step names refdata's own work, so
            // it is served here and its writes reach the store directly. It is
            // registered before the proposals take the context.
            auto provisioning =
                ores::refdata::messaging::provisioning_step_registrar::register_handlers(n, c);
            for (auto& sub : provisioning)
                subs.push_back(std::move(sub));
            // The proposal operations raise approval requests, so they are
            // registered here and not with the entity handlers.
            auto proposals = ores::refdata::messaging::book_proposal_registrar::register_handlers(
                n, std::move(c), std::move(v));
            for (auto& sub : proposals)
                subs.push_back(std::move(sub));
            return subs;
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });
    co_await std::move(service_run);

    event_source.stop();
    co_return;
}

}
