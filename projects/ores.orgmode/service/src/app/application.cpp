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
#include "ores.orgmode.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.orgmode.service/app/application_exception.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/throw_exception.hpp>

namespace ores::orgmode::service::app {

using namespace ores::logging;


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
        "ores.orgmode.service", 0, 1);

    auto ctx = make_context(cfg.database);
    (void)ctx;
    (void)io_ctx;

    // TODO: register domain handlers and run NATS server
    ORES_LOG_SEV(lg(), warn) << "ores.orgmode.service service is not yet implemented.";
    co_return;
}

}
