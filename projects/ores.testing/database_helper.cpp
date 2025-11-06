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
#include "ores.testing/database_helper.hpp"

#include "ores.testing/test_database_manager.hpp"

namespace ores::testing {

using namespace ores::utility::log;
using ores::testing::test_database_manager;

database_helper::database_helper()
    : context_(test_database_manager::make_context()) {}

void database_helper::truncate_table(const std::string& table_name) {
    BOOST_LOG_SEV(lg(), info) << "Truncating table: " << table_name;

    const auto truncate_sql = "TRUNCATE TABLE " + table_name;
    const auto execute_truncate = [&](auto&& session) {
        return session->execute(truncate_sql);
    };

    const auto r = sqlgen::session(context_.connection_pool())
        .and_then(execute_truncate);

    if (!r) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to truncate table " << table_name
            << ": " << r.error().what();
    } else {
        BOOST_LOG_SEV(lg(), info)
            << "Successfully truncated table: " << table_name;
    }
}

}
