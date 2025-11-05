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
#include "ores.testing/test_database_manager.hpp"
#include "ores.testing/database_lifecycle_listener.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::testing {

using namespace ores::utility::log;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.utility.test.database_lifecycle_listener");
    return instance;
}

}

void database_lifecycle_listener::testRunStarting(
    Catch::TestRunInfo const& /*testRunInfo*/) {
    BOOST_LOG_SEV(lg(), debug) << "Test run starting, creating test database";

    // Generate unique test database name for this process
    test_db_name_ = test_database_manager::generate_test_database_name();

    // Create test database from template
    test_database_manager::create_test_database(test_db_name_);

    // Set environment variable so tests use the isolated database
    test_database_manager::set_test_database_env(test_db_name_);

    BOOST_LOG_SEV(lg(), debug) << "Test database ready: " << test_db_name_;
}

void database_lifecycle_listener::testRunEnded(
    Catch::TestRunStats const& /*testRunStats*/) {
    BOOST_LOG_SEV(lg(), debug) << "Test run ended, dropping test database: " << test_db_name_;

    if (!test_db_name_.empty()) {
        test_database_manager::drop_test_database(test_db_name_);
        test_db_name_.clear();
    }
}

}
