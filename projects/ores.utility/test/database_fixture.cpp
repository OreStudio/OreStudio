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
#include "ores.utility/test/database_fixture.hpp"
#include "ores.utility/environment/environment.hpp"
#include "ores.utility/repository/context_factory.hpp"

namespace ores::utility::test {

using namespace ores::utility::log;
using utility::repository::context;
using utility::repository::context_factory;

database_fixture::database_fixture() : context_(make_context()) {}

database_fixture::database_options database_fixture::make_database_options() {
    // TEST prefix to avoid clashing with real vars.
    return database_options {
        .user = environment::environment::get_value_or_default(
            "TEST_ORES_DB_USER", "ores"),
        .password = environment::environment::get_value_or_default(
            "TEST_ORES_DB_PASSWORD", ""),
        .host = environment::environment::get_value_or_default(
            "TEST_ORES_DB_HOST", "localhost"),
        .database = environment::environment::get_value_or_default(
            "TEST_ORES_DB_DATABASE", "oresdb"),
        .port = environment::environment::get_int_value_or_default(
            "TEST_ORES_DB_PORT", 5432)
    };
}

context database_fixture::make_context() {
    const auto opts = make_database_options();
    context_factory::configuration db_cfg{
        .user = opts.user,
        .password = opts.password,
        .host = opts.host,
        .database = opts.database,
        .port = opts.port,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    context ctx = context_factory::make_context(db_cfg);
    BOOST_LOG_SEV(lg(), info) << "Database context created successfully";
    return ctx;
}

void database_fixture::truncate_table(const std::string& table_name) {
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
