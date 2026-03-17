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
#include "ores.database/domain/database_options.hpp"

#include <ostream>
#include <string_view>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"

namespace ores::database {

namespace {

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.utility.database.postgres");
    return instance;
}

}

std::ostream& operator<<(std::ostream& s, const database_options& v) {
    rfl::json::write(v, s);
    return s;
}

sqlgen::postgres::Credentials to_credentials(const database_options& opts) {
    return sqlgen::postgres::Credentials {
        .user = opts.user,
        .password = opts.password(),
        .host = opts.host,
        .dbname = opts.database,
        .port = opts.port,
        .notice_handler = [](const char* msg) {
            using namespace ores::logging;
            // libpq appends a trailing newline; strip it before logging.
            std::string_view sv(msg);
            if (!sv.empty() && sv.back() == '\n')
                sv.remove_suffix(1);
            BOOST_LOG_SEV(lg(), info) << sv;
        }
    };
}

}
