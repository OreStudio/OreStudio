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

#include <iostream>
#include <random>
#include <sstream>
#include <stdexcept>
#include <boost/log/attributes/scoped_attribute.hpp>
#include "ores.platform/environment/environment.hpp"
#include "ores.database/service/context_factory.hpp"

#ifdef _WIN32
#include <process.h>
#define getpid _getpid
#else
#include <unistd.h>
#endif

namespace {

const std::string prefix = "ORES_TEST_DB_";

}

namespace ores::testing {

using namespace ores::logging;
using ores::database::context;
using ores::database::context_factory;
using ores::platform::environment::environment;

context test_database_manager::make_admin_context() {
    // Use separate credentials for DDL user (database creation/dropping)
    database::database_options opts {
        .user = environment::environment::get_value_or_default(
            prefix + "DDL_USER", "ores_test_ddl_user"),
        .password = environment::environment::get_value_or_default(
            prefix + "DDL_PASSWORD", ""),
        .host = environment::environment::get_value_or_default(
            prefix + "HOST", "localhost"),
        .database = "postgres",  // Connect to admin database for CREATE/DROP
        .port = environment::environment::get_int_value_or_default(
            prefix + "PORT", 5432)
    };

    context_factory::configuration db_cfg{
        .database_options = opts,
        .pool_size = 1,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    return context_factory::make_context(db_cfg);
}

context test_database_manager::make_context() {
    const auto opts = make_database_options();
    context_factory::configuration db_cfg{
        .database_options = opts,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    context ctx = context_factory::make_context(db_cfg);
    BOOST_LOG_SEV(lg(), info) << "Database context created successfully";
    return ctx;
}

database::database_options test_database_manager::make_database_options() {
    return database::database_options {
        .user = environment::environment::get_value_or_default(
            prefix + "USER", "ores_test_dml_user"),
        .password = environment::environment::get_value_or_default(
            prefix + "PASSWORD", ""),
        .host = environment::environment::get_value_or_default(
            prefix + "HOST", "localhost"),
        .database = environment::environment::get_value_or_default(
            prefix + "DATABASE", "ores_default"),
        .port = environment::environment::get_int_value_or_default(
            prefix + "PORT", 5432)
    };
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
    oss << "ores_test_" << pid << "_" << random_suffix;

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
                              " WITH TEMPLATE = ores_template";

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
            BOOST_LOG_SEV(lg(), warn) << "Failed to terminate connections for "
                                      << db_name << ": " << result.error().what();
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
            BOOST_LOG_SEV(lg(), warn) << "Failed to drop test database "
                                      << db_name << ": " << result.error().what();
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
    const std::string variable = prefix + "DATABASE";
    BOOST_LOG_SEV(lg(), info) << "Setting " << variable
                              << " environment variable to: "
                              << db_name;

    environment::set_value(variable, db_name);

    BOOST_LOG_SEV(lg(), info) << "Environment variable set successfully";
}

}
