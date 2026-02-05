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

#include <chrono>
#include <iomanip>
#include <random>
#include <sstream>
#include <stdexcept>
#include <boost/log/attributes/scoped_attribute.hpp>
#include "ores.platform/environment/environment.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

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
    // Get test tenant ID, fall back to system tenant if not set
    auto tenant_id = get_test_tenant_id_env();
    if (tenant_id.empty()) {
        tenant_id = system_tenant_id;
    }

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
            prefix + "PORT", 5432),
        .tenant = tenant_id
    };
}

std::string test_database_manager::generate_test_tenant_code(
    const std::string& test_suite_name) {
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    // Get current timestamp
    const auto now = std::chrono::system_clock::now();
    const auto time_t_now = std::chrono::system_clock::to_time_t(now);
    std::tm tm_now{};
#ifdef _WIN32
    localtime_s(&tm_now, &time_t_now);
#else
    localtime_r(&time_t_now, &tm_now);
#endif

    // Use process ID for uniqueness across parallel processes
    const auto pid = getpid();

    // Add random suffix for additional uniqueness
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1000, 9999);
    const auto random_suffix = dis(gen);

    std::ostringstream oss;
    oss << test_suite_name << "_"
        << std::put_time(&tm_now, "%Y%m%d_%H%M%S")
        << "_" << pid << "_" << random_suffix;

    const auto tenant_code = oss.str();
    BOOST_LOG_SEV(lg(), info) << "Generated test tenant code: " << tenant_code;

    return tenant_code;
}

std::string test_database_manager::provision_test_tenant(
    database::context& ctx, const std::string& tenant_code,
    const std::string& description) {
    using database::service::tenant_context;
    using database::repository::execute_parameterized_string_query;
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    BOOST_LOG_SEV(lg(), info) << "Provisioning test tenant: " << tenant_code
                              << " (" << description << ")";

    ctx = tenant_context::with_system_tenant(ctx);

    // Call the provisioner function - it returns the new tenant's UUID
    const auto results = execute_parameterized_string_query(ctx,
        "SELECT ores_iam_provision_tenant_fn($1, $2, $3, $4, $5)::text",
        {"test", tenant_code, tenant_code,
         tenant_code + ".localhost", description},
        lg(), "Provisioning test tenant");

    if (results.empty()) {
        const auto error_msg = "Provisioner returned no tenant_id for " + tenant_code;
        BOOST_LOG_SEV(lg(), error) << error_msg;
        throw std::runtime_error(error_msg);
    }

    const auto& tenant_id = results[0];
    BOOST_LOG_SEV(lg(), info) << "Successfully provisioned test tenant: "
                              << tenant_code << " (id: " << tenant_id << ")";

    return tenant_id;
}

void test_database_manager::terminate_test_tenant(
    database::context& ctx, const std::string& tenant_id) {
    using database::service::tenant_context;
    using database::repository::execute_parameterized_command;
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    BOOST_LOG_SEV(lg(), info) << "Terminating test tenant: " << tenant_id;

    try {
        ctx = tenant_context::with_system_tenant(ctx);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to set system tenant context for "
                                  << "termination: " << e.what();
        return;
    }

    // Call terminator - marks tenant as terminated but preserves all data
    try {
        execute_parameterized_command(ctx,
            "SELECT ores_iam_terminate_tenant_fn($1::uuid)",
            {tenant_id},
            lg(), "Terminating test tenant");

        BOOST_LOG_SEV(lg(), info) << "Successfully terminated test tenant: "
                                  << tenant_id << ". Data preserved for analysis.";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to terminate test tenant "
                                  << tenant_id << ": " << e.what();
        // Don't throw - cleanup should be best-effort
    }
}

void test_database_manager::set_test_tenant_id_env(const std::string& tenant_id) {
    const std::string variable = prefix + "TENANT_ID";
    BOOST_LOG_SEV(lg(), info) << "Setting " << variable
                              << " environment variable to: "
                              << tenant_id;

    environment::set_value(variable, tenant_id);
}

std::string test_database_manager::get_test_tenant_id_env() {
    return environment::get_value_or_default(prefix + "TENANT_ID", "");
}

}
