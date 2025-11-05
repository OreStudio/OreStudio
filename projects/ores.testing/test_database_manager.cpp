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

#include <random>
#include <sstream>
#include <stdexcept>
#include <boost/log/attributes/scoped_attribute.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/environment/environment.hpp"
#include "ores.utility/repository/context_factory.hpp"

#ifdef _WIN32
#include <process.h>
#define getpid _getpid
#else
#include <unistd.h>
#endif

namespace ores::testing {

using namespace ores::utility::log;
using utility::repository::context;
using utility::repository::context_factory;
using utility::environment::environment;

namespace {

auto& lg() {
    static auto instance = make_logger(
        "ores.utility.test.test_database_manager");
    return instance;
}

/**
 * @brief Creates a database context connected to the postgres database.
 *
 * This is needed for admin operations like CREATE/DROP DATABASE.
 */
context make_admin_context() {
    context_factory::configuration db_cfg{
        .user = environment::get_value_or_default("TEST_ORES_DB_USER", "ores"),
        .password = environment::get_value_or_default("TEST_ORES_DB_PASSWORD", ""),
        .host = environment::get_value_or_default("TEST_ORES_DB_HOST", "localhost"),
        .database = "postgres",  // Connect to admin database
        .port = environment::get_int_value_or_default("TEST_ORES_DB_PORT", 5432),
        .pool_size = 1,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    return context_factory::make_context(db_cfg);
}

}

std::string test_database_manager::generate_test_database_name() {
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    // Use process ID for uniqueness across parallel processes
    const auto pid = getpid();

    // Add random suffix for additional uniqueness
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1000, 9999);
    const auto random_suffix = dis(gen);

    std::ostringstream oss;
    oss << "oresdb_test_" << pid << "_" << random_suffix;

    const auto db_name = oss.str();
    BOOST_LOG_SEV(lg(), info) << "Generated test database name: " << db_name;

    return db_name;
}

void test_database_manager::create_test_database(const std::string& db_name) {
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    BOOST_LOG_SEV(lg(), info) << "Creating test database: " << db_name;

    try {
        auto admin_ctx = make_admin_context();

        // Create database from template
        const auto create_sql = "CREATE DATABASE " + db_name +
                              " WITH TEMPLATE = oresdb_template";

        const auto execute_create = [&](auto&& session) {
            return session->execute(create_sql);
        };

        const auto result = sqlgen::session(admin_ctx.connection_pool())
            .and_then(execute_create);

        if (!result) {
            const auto error_msg = "Failed to create test database " + db_name +
                                 ": " + result.error().what();
            BOOST_LOG_SEV(lg(), error) << error_msg;
            throw std::runtime_error(error_msg);
        }

        BOOST_LOG_SEV(lg(), info) << "Successfully created test database: " << db_name;

    } catch (const std::exception& e) {
        const auto error_msg = "Exception while creating test database " +
            db_name + ": " + e.what();
        BOOST_LOG_SEV(lg(), error) << error_msg;
        throw std::runtime_error(error_msg);
    }
}

void test_database_manager::drop_test_database(const std::string& db_name) {
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    BOOST_LOG_SEV(lg(), info) << "Dropping test database: " << db_name;

    try {
        auto admin_ctx = make_admin_context();

        // First, terminate any active connections to the database
        const auto terminate_sql =
            "SELECT pg_terminate_backend(pid) FROM pg_stat_activity "
            "WHERE datname = '" + db_name + "' AND pid <> pg_backend_pid()";

        const auto execute_terminate = [&](auto&& session) {
            return session->execute(terminate_sql);
        };

        auto result = sqlgen::session(admin_ctx.connection_pool())
            .and_then(execute_terminate);

        if (!result) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to terminate connections for " << db_name
                << ": " << result.error().what();
            // Continue with drop even if terminate fails
        }

        // Drop the database
        const auto drop_sql = "DROP DATABASE IF EXISTS " + db_name;

        const auto execute_drop = [&](auto&& session) {
            return session->execute(drop_sql);
        };

        result = sqlgen::session(admin_ctx.connection_pool())
            .and_then(execute_drop);

        if (!result) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to drop test database " << db_name
                << ": " << result.error().what();
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Successfully dropped test database: " << db_name;
        }

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "Exception while dropping test database " << db_name
            << ": " << e.what();
        // Don't throw - cleanup should be best-effort
    }
}

void test_database_manager::set_test_database_env(const std::string& db_name) {
    BOOST_LOG_SEV(lg(), info)
        << "Setting TEST_ORES_DB_DATABASE environment variable to: " << db_name;

#ifdef _WIN32
    _putenv_s("TEST_ORES_DB_DATABASE", db_name.c_str());
#else
    setenv("TEST_ORES_DB_DATABASE", db_name.c_str(), 1);
#endif

    BOOST_LOG_SEV(lg(), info) << "Environment variable set successfully";
}

}
