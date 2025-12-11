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
#include "ores.utility/database/context_factory.hpp"

#include <rfl/json.hpp>
#include "ores.utility/database/database_options.hpp"

namespace ores::utility::database {

using namespace ores::utility::log;

std::ostream&
operator<<(std::ostream& s, const context_factory::configuration& v) {
    rfl::json::write(v, s);
    return s;
}

context context_factory::make_context(const configuration& cfg) {
    BOOST_LOG_SEV(lg(), debug) << "Creating context. Configuration: " << cfg;

    const auto credentials = to_credentials(cfg.database_options);

    sqlgen::ConnectionPoolConfig pool_config {
        .size = cfg.pool_size,
        .num_attempts = cfg.num_attempts,
        .wait_time_in_seconds = cfg.wait_time_in_seconds
    };

    auto pool = make_connection_pool<context::connection_type>(
        pool_config, credentials);
    auto single_conn = sqlgen::postgres::connect(credentials);

    context r(std::move(pool), std::move(single_conn), credentials);

    BOOST_LOG_SEV(lg(), debug) << "Finished creating context.";
    return r;
}

}
