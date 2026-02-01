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
#ifndef ORES_TESTING_TEST_DATABASE_MANAGER_HPP
#define ORES_TESTING_TEST_DATABASE_MANAGER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/database_options.hpp"

namespace ores::testing {

/**
 * @brief Manages test tenant isolation for parallel test execution.
 *
 * This class provides utilities for provisioning and deprovisioning test
 * tenants. Each test suite gets its own isolated tenant, allowing multiple
 * test processes to run concurrently without interference.
 *
 * Tenant isolation uses PostgreSQL Row Level Security (RLS) to ensure
 * complete data separation between test suites.
 */
class test_database_manager {
private:
    inline static std::string_view logger_name =
        "ores.testing.test_database_manager";

    static auto& lg() {
        static auto instance = ores::logging::make_logger(logger_name);
        return instance;
    }

  public:
    /**
     * @brief Creates a database context from environment variables.
     */
    static database::context make_context();

    /**
     * @brief Creates database options from environment variables.
     */
    static database::database_options make_database_options();

    /**
     * @brief Generates a unique tenant code for this test run.
     *
     * The tenant code includes the test suite name and timestamp:
     * {test_suite}_{YYYYMMDD}_{HHMMSS}_{pid}_{random}
     *
     * @param test_suite_name Name of the test suite (e.g., "ores.cli.tests")
     * @return A unique tenant code string
     */
    static std::string generate_test_tenant_code(const std::string& test_suite_name);

    /**
     * @brief Provisions a test tenant using the SQL provisioner function.
     *
     * Creates a new tenant with all required base data (permissions, roles,
     * refdata) copied from the system tenant. The caller must have a valid
     * database context with system tenant access.
     *
     * @param ctx The database context to use
     * @param tenant_code The unique tenant code
     * @param description Human-readable description (e.g., test suite name + version)
     * @return The UUID of the created tenant as a string
     * @throws std::runtime_error if provisioning fails
     */
    static std::string provision_test_tenant(database::context& ctx,
                                             const std::string& tenant_code,
                                             const std::string& description);

    /**
     * @brief Deprovisions a test tenant using the SQL deprovisioner function.
     *
     * Soft-deletes the tenant and all its data. The caller must have system
     * tenant context.
     *
     * @param ctx The database context to use
     * @param tenant_id The UUID of the tenant to deprovision
     */
    static void deprovision_test_tenant(database::context& ctx,
                                        const std::string& tenant_id);

    /**
     * @brief Sets the ORES_TEST_DB_TENANT_ID environment variable.
     *
     * This allows tests to retrieve the test tenant ID.
     *
     * @param tenant_id The tenant ID to set
     */
    static void set_test_tenant_id_env(const std::string& tenant_id);

    /**
     * @brief Gets the test tenant ID from environment variable.
     *
     * @return The tenant ID, or empty string if not set
     */
    static std::string get_test_tenant_id_env();

    /**
     * @brief System tenant ID constant.
     */
    static constexpr auto system_tenant_id =
        "00000000-0000-0000-0000-000000000000";
};

}

#endif
