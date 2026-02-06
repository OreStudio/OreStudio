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
#ifndef ORES_TESTING_DATABASE_HELPER_HPP
#define ORES_TESTING_DATABASE_HELPER_HPP

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::testing {

/**
 * @brief Provides database setup and cleanup utilities for tests.
 */
class database_helper {
private:
    inline static std::string_view logger_name =
        "ores.testing.database_helper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    database_helper();

    /**
     * @brief Seeds minimal RBAC data for tests.
     *
     * Creates the essential permissions and roles needed for IAM tests:
     * - Wildcard permission (*)
     * - Admin role with wildcard permission
     *
     * This is idempotent - safe to call multiple times.
     */
    void seed_rbac();

    /**
     * @brief Sets the tenant context for tests.
     *
     * Uses the test tenant ID from the environment variable if set
     * (set by database_lifecycle_listener), otherwise falls back to the
     * system tenant.
     */
    void set_tenant_context();

    /**
     * @brief Gets the database context.
     */
    database::context& context() { return context_; }

    /**
     * @brief Gets the test tenant ID.
     */
    utility::uuid::tenant_id tenant_id() {
        return utility::uuid::tenant_id::from_string(
            test_database_manager::get_test_tenant_id_env()).value();
    }

private:
    database::context context_;
};

}

#endif
