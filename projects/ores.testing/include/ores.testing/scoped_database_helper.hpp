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

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.testing/database_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

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
    void seed_rbac() { helper_.seed_rbac(); }

    /**
     * @brief Gets the database context.
     */
    database::context& context() { return  helper_.context(); }

    /**
     * @brief Gets the test tenant ID.
     */
    utility::uuid::tenant_id tenant_id() {
        return utility::uuid::tenant_id::from_string(
            test_database_manager::get_test_tenant_id_env()).value();
    }

    /**
     * @brief Gets the database user for the current connection.
     */
    std::string db_user() { return helper_.db_user(); }

private:
    database_helper helper_;
};

}

#endif
