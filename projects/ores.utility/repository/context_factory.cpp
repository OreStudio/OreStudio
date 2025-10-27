/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <rfl/json.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context_factory.hpp"

namespace {

using namespace ores::utility::log;
auto lg(make_logger("ores.utility.repository.context_factory"));

}

namespace ores::utility::repository {

std::ostream&
operator<<(std::ostream& s, const context_factory::configuration& v) {
    rfl::json::write(v, s);
    return(s);
}

context context_factory::make_context(const configuration& cfg) {
    BOOST_LOG_SEV(lg, debug) << "Creating context. Configuration: " << cfg;

    const auto credentials = sqlgen::postgres::Credentials {
        .user = cfg.user,
        .password = cfg.password.value(),
        .host = cfg.host,
        .dbname = cfg.database,
        .port = cfg.port
    };

    sqlgen::ConnectionPoolConfig pool_config {
        .size = cfg.pool_size,
        .num_attempts = cfg.num_attempts,
        .wait_time_in_seconds = cfg.wait_time_in_seconds
    };

    context r(make_connection_pool<context::connection_type>(
            pool_config, credentials));

    BOOST_LOG_SEV(lg, debug) << "Finished creating context.";
    return r;
}

}
