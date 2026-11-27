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
#include "ores.vault.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.vault.core/messaging/registrar.hpp"
#include "ores.vault.service/app/application_exception.hpp"
#include "ores.vault.service/app/provisioning.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::vault::service::app {

using namespace ores::logging;

namespace {
constexpr std::string_view service_name = "ores.vault.service";
constexpr std::string_view service_version = ORES_VERSION;
}

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

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    ORES_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.vault.service", 0, 1);

    // The deployment's own step runs before the bus, because the first value it
    // stores is the password of the service that issues the tokens the bus
    // would ask for. It answers no request, so it opens no subscription.
    if (!cfg.provision.empty()) {
        auto ctx = make_context(cfg.database);
        const auto system_ctx = ores::database::service::tenant_context::with_system_tenant(ctx);
        if (provision_from(system_ctx, cfg.provision, std::cout, std::cerr) != 0) {
            throw std::runtime_error("Provisioning did not write every entry");
        }
        co_return;
    }

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // The registrar loads the two keys the deployment placed for this unit, and
    // refuses to register anything when they are absent, so a vault with no
    // keys fails to start rather than answering a request it cannot serve.
    //
    // Bound before the await, so no lambda of the call lives in this frame.
    auto service_run = ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        service_name,
        [](auto& n, auto c, auto v) {
            return ores::vault::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });
    co_await std::move(service_run);
    co_return;
}

}
