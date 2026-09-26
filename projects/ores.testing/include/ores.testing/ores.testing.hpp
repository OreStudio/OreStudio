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
#ifndef ORES_TESTING_ORES_TESTING_HPP
#define ORES_TESTING_ORES_TESTING_HPP

/**
 * @brief Testing infrastructure common to all test projects.
 *
 * Provides comprehensive testing utilities for ORE Studio components using
 * Catch2. Key features:
 *
 * - Tenant isolation: each test run provisions its own PostgreSQL tenant on a
 *   shared database, so parallel suites cannot see each other's rows
 * - Catch2 listeners: automatic logging and test-tenant lifecycle management
 * - Logging integration: Boost.Log configured per-test-case with suite-based organization
 * - Database helpers: context management, RBAC seeding and party-scoped isolation
 * - Environment-driven: database configuration from ORES_TEST_DB_* environment variables
 *
 * Usage: Register the listeners in each test's main.cpp:
 *   CATCH_REGISTER_LISTENER(ores::testing::logging_listener)
 *   CATCH_REGISTER_LISTENER(ores::testing::database_lifecycle_listener)
 *
 * The logging_listener initializes logging per test case, and the
 * database_lifecycle_listener provisions and terminates the test tenant.
 */
namespace ores::testing {}

#endif
