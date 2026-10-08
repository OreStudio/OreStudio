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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service_app_application.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.storage.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include "ores.storage.service/app/application_exception.hpp"
#include "ores.storage.service/messaging/registrar.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/throw_exception.hpp>
#include <memory>

namespace ores::storage::service::app {

using namespace ores::logging;

namespace {
constexpr std::string_view service_name = "ores.storage.service";
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
        "ores.storage.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // The store is the one the HTTP routes are given: two front doors onto the
    // same tree, which is what makes a put over the bus and a put over HTTP
    // leave the same object.
    ores::storage::filesystem::local_store store(cfg.storage_dir);

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.storage.service",
        [store](auto& n, auto c, auto v) {
            return ores::storage::service::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v), store);
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });
    co_return;
}

}
