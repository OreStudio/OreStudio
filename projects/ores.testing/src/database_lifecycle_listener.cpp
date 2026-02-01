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
#include "ores.testing/database_lifecycle_listener.hpp"

#include <sstream>
#include <boost/log/attributes/scoped_attribute.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.testing/test_database_manager.hpp"

namespace ores::testing {

using namespace ores::logging;

void database_lifecycle_listener::
testRunStarting(Catch::TestRunInfo const& testRunInfo) {
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    BOOST_LOG_SEV(lg(), info) << "Test run starting, provisioning test tenant";

    try {
        // Create database context
        auto ctx = test_database_manager::make_context();

        // Get test suite name from Catch2
        const std::string test_suite_name(testRunInfo.name.data(),
                                          testRunInfo.name.size());

        // Generate unique test tenant code for this process
        const auto tenant_code =
            test_database_manager::generate_test_tenant_code(test_suite_name);

        // Build human-readable description with version info
        std::ostringstream desc;
        desc << "v" << ORES_VERSION << " (" << ORES_BUILD_INFO << ")";
        const auto description = desc.str();

        // Provision the test tenant (copies refdata from system tenant)
        test_tenant_id_ = test_database_manager::provision_test_tenant(
            ctx, tenant_code, description);

        // Set environment variable so tests use this tenant
        test_database_manager::set_test_tenant_id_env(test_tenant_id_);

        BOOST_LOG_SEV(lg(), info) << "Test tenant ready: " << test_tenant_id_;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to provision test tenant: " << e.what();
        throw;
    }
}

void database_lifecycle_listener::testRunEnded(
    Catch::TestRunStats const& /*testRunStats*/) {
    BOOST_LOG_SCOPED_LOGGER_TAG(lg(), "Tag", "TestSuite");

    if (test_tenant_id_.empty()) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Test run ended, deprovisioning test tenant: "
                              << test_tenant_id_;

    try {
        auto ctx = test_database_manager::make_context();
        test_database_manager::deprovision_test_tenant(ctx, test_tenant_id_);
        test_tenant_id_.clear();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to deprovision test tenant: " << e.what();
        // Don't throw - cleanup should be best-effort
    }
}

}
