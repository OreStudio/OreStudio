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
#ifndef ORES_TESTING_SCOPED_DATABASE_HELPER_HPP
#define ORES_TESTING_SCOPED_DATABASE_HELPER_HPP

#include "ores.database/domain/database_options.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::testing {

/**
 * @brief Provides database context for tests with optional RBAC seeding.
 *
 * Each test run uses an isolated tenant, so no table truncation is needed.
 */
class scoped_database_helper {
public:
    /**
     * @brief Constructs a scoped database helper.
     *
     * @param seed_rbac If true, seeds minimal RBAC data (Admin role with wildcard permission)
     */
    explicit scoped_database_helper(bool seed_rbac = false) {
        if (seed_rbac) {
            helper_.seed_rbac();
        }
    }

    /**
     * @brief Seeds minimal RBAC data for tests.
     *
     * Call this if you need RBAC data but didn't pass seed_rbac=true to constructor.
     */
    void seed_rbac() {
        helper_.seed_rbac();
    }

    /**
     * @brief Gets the database context.
     */
    database::context& context() {
        return helper_.context();
    }

    /**
     * @brief Gets the test tenant ID.
     */
    utility::uuid::tenant_id tenant_id() {
        return utility::uuid::tenant_id::from_string(
                   test_database_manager::get_test_tenant_id_env())
            .value();
    }

    /**
     * @brief Gets the database user for the current connection.
     */
    std::string db_user() {
        return helper_.db_user();
    }

    /**
     * @brief Switches the context to party-scoped isolation.
     *
     * Delegates to database_helper::set_party(). Call once per test after
     * finding the target party, before any party-isolated reads/writes.
     */
    void set_party(const boost::uuids::uuid& party_id) {
        helper_.set_party(party_id);
    }

    /**
     * @brief A context that acts as a service, with that service's own grants.
     *
     * The deployment keeps a login beside each service role, named after it
     * with @c _test appended, that starts as the service. This signs in with
     * that fixture, so a statement runs under the service's grants while no
     * service password reaches the test process. The fixture holds the
     * deployment's test password, which the environment file carries.
     *
     * @param service_user_variable The environment variable that names the
     * service's database user, such as @c ORES_INBOX_SERVICE_DB_USER.
     * @param tenant The tenant to scope to; empty means the test's own.
     */
    database::context as_service(std::string_view service_user_variable,
                                 std::string_view tenant = {}) {
        using ores::platform::environment::environment;
        const std::string service_user =
            environment::get_value_or_throw(std::string(service_user_variable));
        const database::database_options options{
            .user = service_user + "_test",
            .password = environment::get_value_or_throw("ORES_TEST_DB_PASSWORD"),
            .host = environment::get_value_or_default("ORES_TEST_DB_HOST", "localhost"),
            .database = environment::get_value_or_throw("ORES_TEST_DB_DATABASE"),
            .port = environment::get_int_value_or_default("ORES_TEST_DB_PORT", 5432),
            .tenant = tenant.empty() ? tenant_id().to_string() : std::string(tenant)};
        const database::context_factory::configuration config{.database_options = options,
                                                              .pool_min = 2,
                                                              .pool_max = 2,
                                                              .pool_idle_seconds = 30,
                                                              .num_attempts = 3,
                                                              .wait_time_in_seconds = 1,
                                                              .service_account = service_user};
        const auto base = database::context_factory::make_context(config);
        return base.with_tenant(base.tenant_id(), service_user);
    }

private:
    database_helper helper_;
};

}

#endif
